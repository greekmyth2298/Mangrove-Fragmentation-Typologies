# Audit serialization fix

The source of the false failure was:

```r
Result = c(nrow(observed), nrow(predicted), all(comparison$Equal))
```

R coerces `TRUE` to `1` in a numeric vector, so the test expecting `"TRUE"`
failed even when the first-interval classifications agreed exactly.

Script 05 now uses explicit `as.character(...)` for each value. The pipeline
regression test verifies both values from the summary CSV and every row of
`1996_2007_prediction_multiset_audit.csv`. No scientific estimates or dataset
values were changed.

From a clean extracted project folder, run `source("run_all.R")` after
setting `MANGROVE_BOOTSTRAP_REPS=100` for the quick test.
