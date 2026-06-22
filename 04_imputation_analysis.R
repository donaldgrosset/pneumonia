# ==============================================================================
# SCRIPT: 04_imputation_analysis.R (CONDITIONAL MULTIPLE IMPUTATION FRAMEWORK)
# ==============================================================================
library(data.table)

message("\n==================================================================")
message("⚙️ RUNNING SCRIPT 04: CONDITIONAL MULTIPLE IMPUTATION FRAMEWORK")
message("==================================================================")

# Ensure project root tracking is active
if (!requireNamespace("here", quietly = TRUE)) stop("CRITICAL: 'here' package missing.")

# ─── SECTION 1: LOAD RE-PERSISTED SYSTEM STATES ───
DB_LABEL      <- "GOLD" # Toggle to "AURUM" as needed
is_aurum_mode <- (DB_LABEL == "AURUM")

# Locate persistent assets on disk from prior scripts
input_checkpoint <- here::here(sprintf("staged_cohort_checkpoint_%s.rds", tolower(DB_LABEL)))
routing_meta_path <- here::here(sprintf("pipeline_routing_config_%s.rds", tolower(DB_LABEL)))

if (!file.exists(input_checkpoint)) stop(paste("Missing dataset checkpoint:", input_checkpoint))
if (!file.exists(routing_meta_path)) stop(paste("Missing pipeline routing configuration:", routing_meta_path))

# Load data tables cleanly into isolated environment slots
cohort_dt       <- readRDS(input_checkpoint)
routing_config  <- readRDS(routing_meta_path)
PIPELINE_ROUTE  <- routing_config$PIPELINE_ROUTE

message(sprintf("✔ Loaded System Context. Mode: %s | Route Target: %s", DB_LABEL, PIPELINE_ROUTE))


# ─── SECTION 2: HARDENED CONDITIONAL EXECUTOR ENGINE ───
execute_imputation_routing <- function(cohort, route) {
  
  if (route == "RUN_TESTING_MODE") {
    message("🚀 PIPELINE SWITCH: Generating Temporary Linkage Placeholders for Testing")
    dt_test <- copy(cohort)
    
    # Generate Mock IMD Quintiles safely using explicit integer casting
    if (!"imd_quintile" %in% names(dt_test)) {
      set.seed(42)
      dt_test[, imd_quintile := as.integer(sample(1:5, .N, replace = TRUE))]
      message("  -> Staged placeholder variable: 'imd_quintile' (Mock Integers 1-5)")
    }
    
    # Generate Mock Rural-Urban indicators
    if (!"is_urban" %in% names(dt_test)) {
      dt_test[, is_urban := as.integer(sample(c(0, 1), .N, replace = TRUE, prob = c(0.2, 0.8)))]
      message("  -> Staged placeholder variable: 'is_urban' (0=Rural, 1=Urban)")
    }
    
    # Clean up clinical missing values using safe type-checked formatting arrays
    if (any(is.na(dt_test$bmi) | dt_test$bmi == -9)) {
      clean_bmi <- dt_test$bmi[dt_test$bmi != -9 & !is.na(dt_test$bmi)]
      med_bmi   <- if (length(clean_bmi) > 0) median(clean_bmi, na.rm = TRUE) else 25.0
      dt_test[is.na(bmi) | bmi == -9, bmi := as.numeric(med_bmi)]
    }
    
    # Handle categorical variables safely
    for (cat_var in c("smoking", "alcohol")) {
      if (cat_var %in% names(dt_test)) {
        dt_test[is.na(get(cat_var)) | get(cat_var) == -9, (cat_var) := 0L]
      }
    }
    
    message("✔ [TESTING PASS-THROUGH COMPLETED]: Continuous tracking list matrix staged.")
    return(list(Dataset_1 = dt_test))
    
  } else if (route == "RUN_PASS_THROUGH") {
    message("🚀 PIPELINE SWITCH: Executing Direct Pass-Through (Data 100% Complete)")
    return(list(Dataset_1 = copy(cohort)))
    
  } else {
    message("🔄 PIPELINE SWITCH: Executing Live Multiple Imputation (Linkages Merged)")
    if (!requireNamespace("mice", quietly = TRUE)) stop("Package 'mice' is required.")
    
    impute_target <- copy(cohort)
    
    # Cast variables back to standard NA definitions for package integration
    impute_target[bmi == -9, bmi := NA]
    impute_target[smoking == -9, smoking := NA]
    impute_target[alcohol == -9, alcohol := NA]
    
    # Convert analytical predictors to explicit structural factors for MICE optimization
    factor_cols <- c("smoking", "alcohol", "diabetes", "chronic_lung", "ethnicity", "imd_quintile", "is_urban")
    present_factors <- factor_cols[factor_cols %in% names(impute_target)]
    impute_target[, (present_factors) := lapply(.SD, as.factor), .SDcols = present_factors]
    
    message("⏳ Constructing optimization matrix layers for safe execution...")
    # Initialize mice parameter setups to block index field tracking errors
    init_mice <- mice(impute_target, m = 1, maxit = 0, printFlag = FALSE)
    pred_mat  <- init_mice$predictorMatrix
    
    # Force system identifier fields to 0 so they aren't calculated as variables
    id_fields <- c("patid", "case_patid")
    present_ids <- id_fields[id_fields %in% colnames(pred_mat)]
    if (length(present_ids) > 0) pred_mat[present_ids, ] <- 0
    if (length(present_ids) > 0) pred_mat[, present_ids] <- 0
    
    message("⏳ Commencing mice iterations across 5 parallel datasets...")
    mice_object <- mice(impute_target, m = 5, method = "pmm", predictorMatrix = pred_mat, seed = 42, printFlag = FALSE)
    
    imputed_list <- list()
    for (i in 1:5) {
      imputed_list[[paste0("Dataset_", i)]] <- as.data.table(mice::complete(mice_object, i))
    }
    
    message("✔ [IMPUTATION COMPLETED]: Staged 5 separate imputed dataframes.")
    return(imputed_list)
  }
}

# --- PROCESS AND PERSIST ANALYTICAL COHORT OBJECTS ---
staged_analysis_data <- execute_imputation_routing(cohort_dt, PIPELINE_ROUTE)

# Safe Data Persistence: Avoids volatile memory dependencies (<<-)
# Writes out your complete multi-dataset data structure to an RDS object
output_path <- here::here(sprintf("staged_analysis_list_%s.rds", tolower(DB_LABEL)))
saveRDS(staged_analysis_data, output_path)

# Clear background variables to save virtual machine RAM
rm(cohort_dt, staged_analysis_data)
gc(verbose = FALSE, full = TRUE)

message(sprintf("✔ [SAVED DATA LAYER]: Imputation list saved successfully to: %s", output_path))
message("==================================================================\n")
