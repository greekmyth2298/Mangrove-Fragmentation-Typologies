# Start here: complete GitHub package

This ZIP contains the **entire repository-ready pipeline** with all source CSVs, 30 interval prediction files, Scripts 01–07, original historical code, the latest verification fixes, and the author-supplied `ALL CITIES COMPILED.csv` preserved under `data/original/`.

1. Extract the archive into a new, empty directory.
2. Open `Mangrove-Fragmentation-Typologies.Rproj` in RStudio.
3. Install the only additional R package once with `install.packages("randomForest")` if necessary.
4. Run the full pipeline:

```r
Sys.setenv(MANGROVE_BOOTSTRAP_REPS = "1000",
           MANGROVE_COMPARE_WHOLE_AREA = "true")
source("run_all.R")
```

The author previously confirmed that all seven analytical stages and 15 publication checks completed successfully at B=1,000. This package also adds **source-provenance assertions**; these need an RStudio check on the newly extracted archive. For a quick separate test, run `source("tests/test_original_source_provenance.R")`.

The historical script is archived for reference at `reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R`; do not execute it as part of the pipeline. The historical CSV is **identical** to the processed trajectories already used by Stage 05, so no analytical inputs have been changed.

For a summary of remaining publication/validation caveats, read `README.md` and `reference/ORIGINAL_ANALYSIS_PROVENANCE.md`.
