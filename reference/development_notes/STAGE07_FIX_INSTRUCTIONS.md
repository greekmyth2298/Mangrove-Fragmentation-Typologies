# Fix for Stage 07: missing/non-finite published baseline LL

Replace `scripts/00_shared.R` and `scripts/07_information_theoretic_path_dependence.R` with the two files from this package. Keep all input CSVs unchanged.

From the project root in RStudio:

```r
source("scripts/05_prepare_reconstructed_trajectories.R")  # only if its output is missing
source("tests/test_stage07_point_estimate_names.R")
Sys.setenv(MANGROVE_BOOTSTRAP_REPS = "100", MANGROVE_COMPARE_WHOLE_AREA = "false")
source("scripts/07_information_theoretic_path_dependence.R")
source("tests/test_pipeline.R")
```

If the named-statistic test fails, share its full error and the output of `getwd()`; do not re-run the bootstrap until that test passes. For publication-quality uncertainty, change the environment setting to 1000 after the smoke test passes.

The original uploaded external-validation CSV warning persists, independently of this issue.
