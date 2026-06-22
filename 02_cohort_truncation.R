# ==============================================================================
# SCRIPT: 02_cohort_truncation.R (INTEGRITY AUDIT & TIMELINE REPAIR)
# ==============================================================================
library(data.table)

message("\n==================================================================")
message("⚙️ STARTING SCRIPT 02: REPAIRING COHORT TIMELINES & AUDITING SYSTEM")
message("==================================================================")

# Ensure project root tracking is active
if (!requireNamespace("here", quietly = TRUE)) stop("CRITICAL: 'here' package missing.")

# ─── SECTION 1: EXPLICIT DEPENDENCY INGESTION ───
# Define database target context explicitly matching your extraction script layout
DB_LABEL      <- "GOLD" # Toggle to "AURUM" as needed
is_aurum_mode <- (DB_LABEL == "AURUM")

# Safe Haven structured paths relative to root directory
PATH_DICTIONARY <- here::here("data", ifelse(is_aurum_mode, "Define_MedicalDictionary.txt", "medical.txt"))
PATH_PATIENT    <- here::here("data", "Project_Extract_Patient_01.txt")
PATH_EVENTS     <- here::here("data", ifelse(is_aurum_mode, "Project_Extract_Observation_01.txt", "Project_Extract_Clinical_01.txt"))
PATH_PRACTICE   <- here::here("data", "practice.txt")

# Define target paths vector for system profiling
target_paths <- c(dictionary = PATH_DICTIONARY, patient = PATH_PATIENT, events = PATH_EVENTS)
if (!is_aurum_mode) target_paths <- c(target_paths, practice = PATH_PRACTICE)

# LOAD CHECKPOINT: Drop memory-dependent check; read straight from script 1 checkpoint
input_checkpoint <- here::here(sprintf("extracted_raw_comprehensive_%s_events.rds", tolower(DB_LABEL)))

if (!file.exists(input_checkpoint)) {
  stop(paste("CRITICAL INTERMEDIATE PIPELINE FAILURE: Missing input data tracker at", input_checkpoint))
}

message("⏳ Ingesting cohort subset extractor file...")
cohort_dt <- readRDS(input_checkpoint)


# ─── SECTION 2: HARDENED PRE-RUN SYSTEM STORAGE AUDIT ───
audit_safe_haven_inputs <- function(paths_vector) {
  message("\n📦 RUNNING PRE-RUN STORAGE & VOLUME INTEGRITY AUDIT...")
  audit_results <- list()
  
  for (file_role in names(paths_vector)) {
    current_path <- paths_vector[[file_role]]
    
    if (!file.exists(current_path)) {
      stop(sprintf("CRITICAL FAILURE: Core database file path missing!\n  Role: %s\n  Path: %s", 
                   toupper(file_role), current_path))
    }
    
    file_info  <- file.info(current_path)
    size_mb    <- round(file_info$size / (1024^2), 2)
    
    if (file_info$size == 0) stop(paste("CRITICAL ERROR: Source file empty:", file_role))
    
    message(paste("  -> Profiling size metrics for:", basename(current_path)))
    
    # SAFE ROW COUNTING: Avoids allocating massive datasets into RAM memory channels
    # Counts newline segments directly via system streaming connections
    con <- file(current_path, "r")
    row_count <- 0
    while(length(chunk <- readLines(con, n = 500000, warn = FALSE)) > 0) {
      row_count <- row_count + length(chunk)
    }
    close(con)
    
    audit_results[[file_role]] <- data.table(
      File_Role          = file_role,
      File_Name          = basename(current_path),
      Size_MB            = size_mb,
      Total_Row_Count    = row_count,
      Last_Modified_Date = file_info$mtime
    )
  }
  
  audit_matrix <- rbindlist(audit_results)
  return(audit_matrix)
}

file_system_audit_log <- audit_safe_haven_inputs(target_paths)
print(file_system_audit_log)

# Commit trace log file for study management validation
fwrite(file_system_audit_log, here::here("INTERNAL_STORAGE_AUDIT_LOG.csv"))


# ─── SECTION 3: CHRONOLOGICAL TIME-TRAVEL REPAIRS ───
# Note: For this repair matrix to trigger, cohort_dt must contain 'observation_start' 
# and 'index_date'. (Ensure these variables are mapped/joined before this file executes).
if (all(c("observation_start", "index_date") %in% names(cohort_dt))) {
  
  # Ensure clean date type conversion properties
  cohort_dt[, `:=`(observation_start = as.Date(observation_start), index_date = as.Date(index_date))]
  
  anomalous_n <- cohort_dt[observation_start > index_date, .N]
  
  if (anomalous_n > 0) {
    message(paste("\n⚠️ Found", anomalous_n, "patients with timeline date anomalies."))
    initial_total <- cohort_dt[, .N]
    
    cohort_dt[, date_lag_days := as.numeric(observation_start - index_date)]
    
    # --- CORRECTION A: SHIFT ≤ 90 DAY CONSULTATION DELAYS ---
    cohort_dt[observation_start > index_date & date_lag_days <= 90, index_date := observation_start]
    
    # --- CORRECTION B: EXCLUDE > 90 DAY RECORDS ---
    cohort_dt <- cohort_dt[!(observation_start > index_date & date_lag_days > 90)]
    
    cohort_dt[, date_lag_days := NULL]
    message(sprintf("📈 Slicing Complete: Initial N = %d | Cleaned N = %d", initial_total, cohort_dt[, .N]))
  } else {
    message("\n✔ [DATE CHECK]: Confirmed perfect chronological time ordering across records.")
  }
  
  # Validate system boundaries
  na_count <- cohort_dt[is.na(observation_start), .N]
  if (na_count > 0) stop(paste("CRITICAL FAILURE:", na_count, "records are missing observation_start markers."))
  
} else {
  message("\n⚠️ WARNING: 'observation_start' or 'index_date' fields missing in raw events extraction.")
  message("Skipping timeline repair step. Ensure demographic features are joined before truncation.")
}

# ─── SECTION 4: RE-PERSISTING COHORT CHECKPOINT ───
output_path <- here::here(sprintf("staged_cohort_checkpoint_%s.rds", honest_label <- tolower(DB_LABEL)))
saveRDS(cohort_dt, output_path)

# Clear environment RAM
rm(cohort_dt, file_system_audit_log)
gc(verbose = FALSE, full = TRUE)

message(sprintf("\n✔ [SAVED CLEANED CHECKPOINT]: Successfully written data to: %s", output_path))
message("==================================================================\n")
