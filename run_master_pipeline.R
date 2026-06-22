# ==============================================================================
# MASTER RUN CONTROLLER: COMPREHENSIVE HARDENED BATCH PIPELINE EXECUTION
# ==============================================================================
library(data.table)

# ─── FORCE THE PATH ENGINE TO ANCHOR TO THIS NEW FOLDER ───
if (!requireNamespace("here", quietly = TRUE)) stop("Install 'here' package.")
here::i_am("run_master_pipeline.R")

message("\n==================================================================")
message("🚀 LAUNCHING MASTER AUTOMATED BATCH RUN WITH MEMORY PURGING GATES")
message("==================================================================")

# Define your file sequence array in the new chronological order
pipeline_sequence <- c(
  "00_initial_cleanup_script.R",
  "01_extract_comprehensive_pipeline.R",
  "02_cohort_truncation.R",
  "03_pre_imputation_check.R",
  "04_imputation_analysis.R",
  "05_generate_manuscript_table1.R",
  "06_generate_lifestyle_distributions.R",
  "07_conditional_logistic_regression.R",
  "08_matching_balance_validation.R",
  "09_negative_control_validation.R",
  "10_stratified_subgroup_analysis.R",
  "11_generate_forest_plot.R",        # <-- NEW LINK 11 (Generates your PDF Chart)
  "12_generate_multi_tab_excel.R"     # <-- RENUMBERED LINK 12 (Bundles everything)
)

for (script_file in pipeline_sequence) {
  script_path <- here::here(script_file)
  
  if (file.exists(script_path)) {
    
    # ─── 🛡️ THE MEMORY CLEARING GATEWAY ───
    # Before opening a new file, force R to destroy any lingering background environments,
    # un-evicted table cache buffers, or RStudio interface memory histories.
    if (script_file != "00_initial_cleanup_script.R") {
      message("⏳ RAM Purge Gate: Evicting memory buffers before loading next script...")
      
      # Clear the background .GlobalEnv variables *except* our automation loop variables
      rm(list = setdiff(ls(all.names = TRUE), c("script_file", "script_path", "pipeline_sequence")), envir = .GlobalEnv)
      
      # Force a deep, aggressive virtual memory flush back to the operating system
      gc(verbose = FALSE, full = TRUE)
    }
    
    message(sprintf("\n➡️ [EXECUTING COMPONENT]: %s", script_file))
    
    # source() loads and runs the entire script in the correct background sequence
    source(script_path, local = FALSE, echo = FALSE)
    
  } else {
    stop(sprintf("CRITICAL PIPELINE INTERRUPTION: File missing from folder: %s", script_file))
  }
}

# Absolute final system wipe
rm(list = ls(all.names = TRUE))
gc(verbose = FALSE, full = TRUE)

message("\n==================================================================")
message("🎉 PIPELINE AUTOMATION COMPLETE: All components successfully built under strict memory caps!")
message("==================================================================\n")
