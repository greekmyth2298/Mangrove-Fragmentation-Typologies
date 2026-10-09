# Execute from the PROJECT ROOT in RStudio or a terminal.
#   source("run_all.R")             # Run scripts 01-07 + benchmark tests
#   Rscript run_all.R               # Same from terminal
#   Rscript run_all.R 05 06 07      # Only analyses using existing trajectories
#   Rscript run_all.R test          # Test existing output files
# Speed-up smoke test:
#   Sys.setenv(MANGROVE_BOOTSTRAP_REPS = "100")
#   source("run_all.R")
if (!file.exists("scripts/00_shared.R")) {
  stop("Working directory must be the repository root containing scripts/ and data/.\n",
       "In RStudio open the .Rproj file, or use Session > Set Working Directory.")
}
stages <- c(
  `01` = "scripts/01_prepare_training_data.R",
  `02` = "scripts/02_random_forest_typology_classification.R",
  `03` = "scripts/03_external_validation.R",
  `04` = "scripts/04_apply_rf_typologies.R",
  `05` = "scripts/05_prepare_reconstructed_trajectories.R",
  `06` = "scripts/06_analyze_fragmentation_trajectories.R",
  `07` = "scripts/07_information_theoretic_path_dependence.R"
)
args <- commandArgs(trailingOnly = TRUE)
selection <- if (!length(args) || identical(args, "all")) names(stages) else args
if (identical(selection, "test")) {
  source("tests/test_pipeline.R", local = new.env(parent = globalenv()))
} else {
  if (any(!selection %in% names(stages))) {
    stop("Select stage numbers 01-07, 'all', or 'test'.")
  }
  for (nm in selection) {
    message("\n===== RUNNING STAGE ", nm, ": ", stages[[nm]], " =====")
    elapsed <- system.time(
      tryCatch(source(stages[[nm]], local = new.env(parent = globalenv()),
                      chdir = FALSE),
               error = function(e) stop("Pipeline stopped at Stage ", nm,
                                        " (", stages[[nm]], "): ",
                                        conditionMessage(e), call. = FALSE))
    )
    message("===== FINISHED STAGE ", nm, " in ", round(elapsed[["elapsed"]], 2), " sec =====")
  }
  if (identical(selection, names(stages))) {
    message("\n===== RUNNING PUBLISHED NUMERICAL VERIFICATION =====")
    source("tests/test_pipeline.R", local = new.env(parent = globalenv()))
  }
  message("\nCompleted requested stages: ", paste(selection, collapse = ", "))
}
