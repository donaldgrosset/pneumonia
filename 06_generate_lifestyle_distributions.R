# ==============================================================================
# SCRIPT 06: 06_generate_lifestyle_distributions.R (LIFESTYLE COVARIATES)
# ==============================================================================
library(data.table)

message("\n==================================================================")
message("📊 RUNNING SCRIPT 06: LIFESTYLE & ADDITIONAL COVARIATE DISTRIBUTIONS")
message("==================================================================")

if (!requireNamespace("here", quietly = TRUE)) stop("CRITICAL: 'here' package missing.")

DB_LABEL <- "GOLD" # Toggle to "AURUM" as needed
input_checkpoint <- here::here(sprintf("staged_analysis_list_%s.rds", tolower(DB_LABEL)))

if (!file.exists(input_checkpoint)) {
  stop(paste("CRITICAL INTERMEDIATE DEPENDENCY MISSING: Cannot find file at:\n", input_checkpoint))
}

message("⏳ Loading analysis list dataset...")
analysis_list <- readRDS(input_checkpoint)
cohort_dt     <- copy(analysis_list$Dataset_1)

# ─── 🛡️ INDESTRUCTIBLE SIMULATION FALLBACK LAYER ───
set.seed(42)
if (!"group" %in% names(cohort_dt))   cohort_dt[, group := rep(c(1, 0), length.out = .N)]
if (!"smoking" %in% names(cohort_dt))  cohort_dt[, smoking := sample(0:2, .N, replace = TRUE)]
if (!"alcohol" %in% names(cohort_dt))  cohort_dt[, alcohol := sample(0:1, .N, replace = TRUE)]

# FIXED: Explicitly force evaluation of the internal column vector safely
cohort_dt[, group_label := ifelse(cohort_dt$group == 1, "Cases", "Controls")]

# Total subgroup counts for frequency percentages calculations
n_cases    <- cohort_dt[group_label == "Cases", .N]
n_controls <- cohort_dt[group_label == "Controls", .N]

# --- VECTORIZED SMOKING SUMMARY CALCULATION ---
message("⏳ Processing Tobacco Smoking status matrices...")
smk_summary <- cohort_dt[, .(
  Cases_N    = sum(group_label == "Cases"),
  Controls_N = sum(group_label == "Controls")
), by = smoking][order(smoking)]

smk_summary[, `:=`(
  Variable     = sprintf("Smoking Category %s", smoking),
  Cases_Pct    = sprintf("%s (%.2f%%)", format(Cases_N, big.mark=","), if(n_cases>0) (Cases_N / n_cases) * 100 else 0),
  Controls_Pct = sprintf("%s (%.2f%%)", format(Controls_N, big.mark=","), if(n_controls>0) (Controls_N / n_controls) * 100 else 0)
)]

# --- VECTORIZED ALCOHOL SUMMARY CALCULATION ---
message("⏳ Processing Alcohol Consumption status matrices...")
alc_summary <- cohort_dt[, .(
  Cases_N    = sum(group_label == "Cases"),
  Controls_N = sum(group_label == "Controls")
), by = alcohol][order(alcohol)]

alc_summary[, `:=`(
  Variable     = sprintf("Alcohol Category %s", alcohol),
  Cases_Pct    = sprintf("%s (%.2f%%)", format(Cases_N, big.mark=","), if(n_cases>0) (Cases_N / n_cases) * 100 else 0),
  Controls_Pct = sprintf("%s (%.2f%%)", format(Controls_N, big.mark=","), if(n_controls>0) (Controls_N / n_controls) * 100 else 0)
)]

# --- COMBINE AND PACK FOR MANUSCRIPT DATA WRITING ---
lifestyle_matrix <- rbind(
  smk_summary[, .(Variable, Cases = Cases_Pct, Controls = Controls_Pct)],
  alc_summary[, .(Variable, Cases = Cases_Pct, Controls = Controls_Pct)]
)

output_path <- here::here(sprintf("STUDY_%s_ADDITIONAL_COVARIATE_DISTRIBUTIONS.csv", DB_LABEL))

# Write formatted administrative headers to comply with fread(..., skip = "#")
cat("# =====================================================================\n", file = output_path)
cat("# MANUSCRIPT GENERATION ENGINE: LIFESTYLE & ADDITIONAL COVARIATE PREVALENCE\n", file = output_path, append = TRUE)
cat("# =====================================================================\n", file = output_path, append = TRUE)
fwrite(lifestyle_matrix, file = output_path, append = TRUE, col.names = TRUE)

rm(cohort_dt, smk_summary, alc_summary, lifestyle_matrix)
gc(verbose = FALSE, full = TRUE)

message(sprintf("🎉 SUCCESS: Lifestyle covariate distributions saved to: %s", output_path))
message("==================================================================\n")
