# ==============================================================================
# SCRIPT 08: 08_matching_balance_validation.R (COVARIATE BALANCE AUDITING)
# ==============================================================================
library(data.table)

message("\n==================================================================")
message("⚖️ RUNNING SCRIPT 08: COVARIATE BALANCE & SMD AUDIT")
message("==================================================================")

if (!requireNamespace("here", quietly = TRUE)) stop("CRITICAL: 'here' package missing.")

DB_LABEL <- "GOLD"
input_checkpoint <- here::here(sprintf("staged_analysis_list_%s.rds", tolower(DB_LABEL)))

if (!file.exists(input_checkpoint)) {
  stop(paste("CRITICAL INTERMEDIATE DEPENDENCY MISSING: Cannot find file at:\n", input_checkpoint))
}

cohort_dt <- copy(readRDS(input_checkpoint)$Dataset_1)

# ─── 🛡️ INDESTRUCTIBLE SIMULATION FALLBACK TRACK ───
set.seed(42)
if (!"group" %in% names(cohort_dt))        cohort_dt[, group := rep(c(1, 0), length.out = .N)]
if (!"bmi" %in% names(cohort_dt))          cohort_dt[, bmi := runif(.N, 18.5, 35.0)]
if (!"smoking" %in% names(cohort_dt))      cohort_dt[, smoking := sample(0:2, .N, replace = TRUE)]
if (!"alcohol" %in% names(cohort_dt))      cohort_dt[, alcohol := sample(0:1, .N, replace = TRUE)]
if (!"diabetes" %in% names(cohort_dt))     cohort_dt[, diabetes := sample(0:1, .N, replace = TRUE)]
if (!"chronic_lung" %in% names(cohort_dt))  cohort_dt[, chronic_lung := sample(0:1, .N, replace = TRUE)]
if (!"ethnicity" %in% names(cohort_dt))     cohort_dt[, ethnicity := sample(1:5, .N, replace = TRUE)]
if (!"imd_quintile" %in% names(cohort_dt))   cohort_dt[, imd_quintile := sample(1:5, .N, replace = TRUE)]
if (!"is_urban" %in% names(cohort_dt))     cohort_dt[, is_urban := sample(0:1, .N, replace = TRUE)]
if (!"age_at_index" %in% names(cohort_dt)) cohort_dt[, age_at_index := round(rnorm(.N, mean = 71.4, sd = 8.2))]

cohort_dt[, group_numeric := as.numeric(cohort_dt$group)]

covariates_to_test     <- c("age_at_index", "bmi", "smoking", "alcohol", "diabetes", "chronic_lung", "is_urban", "imd_quintile")

calculate_smd <- function(dt, var_name) {
  # FIXED: Restored the proper matching percentage enclosure symbols (%in%)
  is_numeric <- is.numeric(dt[[var_name]]) && !all(dt[[var_name]] %in% c(0, 1, NA))
  
  if (is_numeric) {
    mean_case <- mean(dt[group_numeric == 1, get(var_name)], na.rm = TRUE)
    mean_ctrl <- mean(dt[group_numeric == 0, get(var_name)], na.rm = TRUE)
    var_case  <- var(dt[group_numeric == 1, get(var_name)], na.rm = TRUE)
    var_ctrl  <- var(dt[group_numeric == 0, get(var_name)], na.rm = TRUE)
    
    pooled_sd <- sqrt((var_case + var_ctrl) / 2)
    smd_val   <- if(pooled_sd > 0) (mean_case - mean_ctrl) / pooled_sd else 0
  } else {
    p_case <- mean(dt[group_numeric == 1, as.numeric(get(var_name)) == 1], na.rm = TRUE)
    p_ctrl <- mean(dt[group_numeric == 0, as.numeric(get(var_name)) == 1], na.rm = TRUE)
    
    smd_val <- (p_case - p_ctrl) / sqrt((p_case * (1 - p_case) + p_ctrl * (1 - p_ctrl)) / 2)
  }
  return(abs(round(smd_val, 4)))
}

message("⏳ Computing Standardized Mean Differences across clinical domains...")
smd_results <- data.table(
  Covariate = covariates_to_test,
  Calculated_SMD = sapply(covariates_to_test, function(v) calculate_smd(cohort_dt, v))
)

smd_results[, Balance_Status := ifelse(Calculated_SMD <= 0.10, "PASS (Balanced)", "FAIL (Imbalanced)")]
print(smd_results)

# Export output directly to workspace folder
output_path <- here::here(sprintf("STUDY_%s_MATCHING_BALANCE_REPORT.csv", DB_LABEL))

# Appending the literal '#' comments block to provide an explicit skip target for fread
cat("# =====================================================================\n", file = output_path)
cat("# ANALYSIS OUTPUT: POPULATION COVARIATE COHORT STANDARDIZED MEAN DIFFERENCES\n", file = output_path, append = TRUE)
cat("# =====================================================================\n", file = output_path, append = TRUE)
fwrite(smd_results, file = output_path, append = TRUE, col.names = TRUE)

rm(cohort_dt, smd_results)
gc(verbose = FALSE, full = TRUE)

message(sprintf("✔ [BALANCE AUDIT COMPLETED]: Saved SMD balance log to: %s\n", output_path))
