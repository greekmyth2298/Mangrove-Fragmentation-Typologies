# Final executable verification checklist

1. Extract the release ZIP; double-click `Mangrove-Fragmentation-Typologies.Rproj`.
2. Console: `getwd()` should end with the extracted project folder.
3. Console: `file.exists('data/interval_predictions/manifest.csv')` and `file.exists('scripts/00_shared.R')` should both yield `TRUE`.
4. Install the only R package if needed: `install.packages('randomForest')`.
5. Smoke test:

   ```r
   Sys.setenv(MANGROVE_BOOTSTRAP_REPS='100', MANGROVE_COMPARE_WHOLE_AREA='false')
   source('run_all.R')
   ```

6. Full run:

   ```r
   Sys.setenv(MANGROVE_BOOTSTRAP_REPS='1000', MANGROVE_COMPARE_WHOLE_AREA='true')
   source('run_all.R')
   ```

7. Verify: `source('tests/test_pipeline.R')`. It should print all `PASS` checks and finish with `All specified deterministic publication checks PASSED`.
8. Compare stochastic bootstrap summaries with published Table 5; differences must be reported rather than hard-coded away.
9. Inspect the original-versus-refitted classification disagreement table and the external validation differences from the published manuscript.
10. If any stage errors, record the FIRST `Pipeline stopped at Stage XX` message and include a few preceding console lines.

Commands run as `Rscript run_all.R` or `Rscript run_all.R 05 06 07` in a terminal opened at the project root.
