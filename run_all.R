# Run from the root of the provided project folder:
#   Rscript run_all.R           # run 01-07; skip 04 if GIS transitions absent
#   Rscript run_all.R 05 06 07  # run just the trajectory/path-dependence stages
# Or open the project root in RStudio and source("run_all.R").

files <- c(
  `01` = "scripts/01_prepare_training_data.R",
  `02` = "scripts/02_random_forest_typology_classification.R",
  `03` = "scripts/03_external_validation.R",
  `04` = "scripts/04_apply_rf_typologies.R",
  `05` = "scripts/05_prepare_reconstructed_trajectories.R",
  `06` = "scripts/06_analyze_fragmentation_trajectories.R",
  `07` = "scripts/07_information_theoretic_path_dependence.R"
)
args <- commandArgs(trailingOnly = TRUE)
selected <- if (!length(args) || identical(args, "all")) names(files) else args
if (any(!selected %in% names(files)))
  stop("Specify script numbers from 01 through 07, e.g. Rscript run_all.R 05 06 07")
for (step in selected) {
  if (step == "04" && !file.exists("data/processed/ALL_PATCH_TRANSITIONS.csv") &&
      (!length(args) || identical(args, "all"))) {
    message("Skipping 04: data/processed/ALL_PATCH_TRANSITIONS.csv was not provided.")
    next
  }
  message("\n===== RUNNING ", step, ": ", files[[step]], " =====")
  source(files[[step]], local = new.env(parent = globalenv()), chdir = FALSE)
}
message("\nRequested pipeline stages completed.")
