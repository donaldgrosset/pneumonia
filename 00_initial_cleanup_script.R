# ==============================================================================
# SCRIPT 0: 00_initial_cleanup_script.R (SYSTEM ENV CLEANUP & REBOOT)
# ==============================================================================

message("\n==================================================================")
message("🧼 RUNNING SCRIPT 00: INITIAL HEALTH CLEANUP & SESSION RESET")
message("==================================================================")

# ─── 1. GLOBAL WORKSPACE ENVIRONMENT RESET ───
# Closes any active background graphing windows
graphics.off()                  

# Clears the visible text on the active console screen
cat("\014")                     


# ─── 2. DYNAMIC WORKSPACE ANCHORING (NO SETWD) ───
# Ensure the 'here' package is available for safe, project-relative path building.
# Inside the Safe Haven, 'here' will automatically find the repository root.
if (!requireNamespace("here", quietly = TRUE)) {
  stop("CRITICAL ENVIRONMENT FAILURE: The 'here' package must be installed.")
}

message("✔ Workspace Root Anchored Path -> ", here::here())


# ─── 3. SYSTEM GARBAGE COLLECTION PURGE ───
message("⏳ Invoking deep garbage collection across system memory channels...")

# Forces R to immediately release unused memory back to the Operating System.
# Crucial before loading heavy CPRD data tables.
gc(verbose = FALSE, full = TRUE) 

message("==================================================================")
message("✔ ENVIRONMENT READY: System workspace is sanitized for File 01.")
message("==================================================================\n")
