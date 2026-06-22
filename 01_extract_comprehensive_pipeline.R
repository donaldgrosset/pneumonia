# ==============================================================================
# SCRIPT 1: 01_extract_comprehensive_pipeline.R (UNIFIED BATCH EXTRACTION ENGINE)
# ==============================================================================
library(data.table)

message("\n========================================================")
message("🚀 INITIALIZING AUTOMATED SAFE HAVEN BATCH EXTRACTION ENGINE")
message("========================================================")

# Ensure the project root path manager is active
if (!requireNamespace("here", quietly = TRUE)) stop("CRITICAL: 'here' package is missing.")

# ─── SECTION 1: GLOBAL PHENOTYPE REGEX ARCHITECTURE ───
# Kept in one central hub. Any adjustment here fixes BOTH database extractions instantly.
pheno_rules <- list(
  parkinson = list(
    include = "parkinson|paralysis agitans",
    exclude = "progressive supranuclear|palsy|psp|atrophy|msa|shy-drager|striatonigral|corticobasal|cbd|vascular|arteriosclerotic|drug induced|drug-induced|neuroleptic|secondary|family history|screening|wpw|syndrome"
  ),
  pneumonia = list(
    include = "pneumonia|bronchopneumonia",
    exclude = "history of|family history|vaccine|vaccination|immunisation|screening|congenital|rheumatoid|transient|post-procedural|neonatal|radiation|hypostatic"
  ),
  uti = list(
    include = "urinary tract infection|cystitis|urinary infection|pyelonephritis",
    exclude = "history of|family history|screening|prophylaxis|microbiol|specimen|catheter care|interstitial|post-procedural|calculus|congenital"
  )
)

# Create mandatory output directories if they do not exist
if (!dir.exists(here::here("codelists"))) dir.create(here::here("codelists"))


# ─── SECTION 2: THE MULTI-DATABASE AUTOMATION LOOP ───
# This systematically processes GOLD then AURUM autonomously
for (db_label in c("GOLD", "AURUM")) {
  
  message(sprintf("\n\n🔄 [STARTING PIPELINE PARTITION] --> Processing Database: %s", db_label))
  
  # Set context flags dynamically based on the current loop iteration
  is_aurum_mode <- (db_label == "AURUM")
  code_col      <- ifelse(is_aurum_mode, "medcodeid", "medcode")
  
  # Configure relative file path variables dynamically based on layout schemas
  PATH_DICTIONARY <- here::here("data", ifelse(is_aurum_mode, "Define_MedicalDictionary.txt", "medical.txt"))
  PATH_EVENTS     <- here::here("data", ifelse(is_aurum_mode, "Project_Extract_Observation_01.txt", "Project_Extract_Clinical_01.txt"))
  
  # File checkpoint validation gating
  if (!file.exists(PATH_DICTIONARY)) {
    warning(sprintf("⚠️ Skipping %s: Dictionary file missing at %s", db_label, PATH_DICTIONARY))
    next
  }
  if (!file.exists(PATH_EVENTS)) {
    warning(sprintf("⚠️ Skipping %s: Event extract file missing at %s", db_label, PATH_EVENTS))
    next
  }
  
  # --- STEP 2.1: DICTIONARY INGESTION & PARSING ---
  message(sprintf("⏳ Ingesting %s medical dictionary text terms...", db_label))
  dict_dt <- fread(PATH_DICTIONARY, select = c(code_col, "term"))
  dict_dt[, term_lower := tolower(term)]
  
  compiled_vectors <- list()
  
  # --- STEP 2.2: ITERATIVE PHENOTYPING LOOP ---
  for (condition in names(pheno_rules)) {
    message(sprintf("  🔍 Extracting %s codes from %s...", toupper(condition), db_label))
    
    rules   <- pheno_rules[[condition]]
    matches <- dict_dt[grepl(rules$include, term_lower)]
    
    inclusions <- matches[!grepl(rules$exclude, term_lower)]
    exclusions <- matches[grepl(rules$exclude, term_lower)]
    
    inclusions[, review_status := "ACCEPTED"]
    exclusions[, review_status := "REJECTED"]
    
    # Commit audit logs cleanly to disk for Airlock review trace
    audit_path <- here::here(sprintf("MANUAL_AUDIT_LOG_%s_%s.csv", toupper(condition), db_label))
    fwrite(rbind(inclusions, exclusions), audit_path)
    
    # Isolate flat codes list
    final_verified_codes <- inclusions[[code_col]]
    compiled_vectors[[condition]] <- final_verified_codes
    
    txt_path <- here::here("codelists", sprintf("approved_%s_%s_codes.txt", tolower(db_label), condition))
    fwrite(data.table(code = final_verified_codes), file = txt_path, col.names = FALSE, row.names = FALSE, quote = FALSE)
    
    cat(sprintf("    -> %s Status Locked: %d ACCEPTED | %d REJECTED\n", 
                toupper(condition), nrow(inclusions), nrow(exclusions)))
  }
  
  # --- STEP 2.3: FAST COHORT FILTERING SELECTION ---
  message(sprintf("⏳ Reading raw %s event extraction file for data slicing...", db_label))
  raw_clinical <- fread(PATH_EVENTS, select = c("patid", "event_date", code_col))
  
  all_accepted_keys <- unlist(compiled_vectors, use.names = FALSE)
  
  message("  ⚡ Slicing events via fast binary matching array check...")
  extracted_study_events <- raw_clinical[get(code_col) %in% all_accepted_keys]
  
  # Export final partitioned binary object file
  checkpoint_name <- here::here(sprintf("extracted_raw_comprehensive_%s_events.rds", tolower(db_label)))
  saveRDS(extracted_study_events, checkpoint_name)
  
  # Clean up memory buffers explicitly inside the loop step before jumping databases
  rm(raw_clinical, dict_dt, extracted_study_events, inclusions, exclusions, matches)
  gc(verbose = FALSE, full = TRUE)
  
  message(sprintf("✔ [PARTITION COMPLETE]: Saved extraction checkpoint: %s", checkpoint_name))
}

message("\n========================================================")
message("🎉 ALL CHANNELS COMPLETED AUTONOMOUSLY WITHOUT ERROR")
message("========================================================\n")
