# Run from project root after Script 05:
# source("tests/test_stage07_point_estimate_names.R")
source("scripts/00_shared.R")
d <- mf_load_trajectories()
stopifnot(nrow(d) == 3186L)
expected_fields <- c("H_recent", "H_history", "Delta_H",
                     "LL_recent", "LL_history", "LR")
check <- function(x, tag) {
  stopifnot(identical(names(x), expected_fields),
            length(x) == 6L, is.numeric(x), all(is.finite(x)))
  for (nm in expected_fields) {
    z <- x[nm]
    stopifnot(length(z) == 1L, is.finite(unname(z)),
              identical(names(z), nm))
  }
  message("PASS: ", tag, ": ", paste(names(x), collapse = ", "))
}
pooled <- mf_joint_stats(d)
check(pooled, "pooled statistic names and finite values")
stopifnot(abs(unname(pooled["LL_recent"]) - (-4010.9)) < 0.1,
          abs(unname(pooled["LL_history"]) - (-3760.9)) < 0.1,
          abs(unname(pooled["LR"]) - 500.08) < 0.01,
          abs(unname(pooled["Delta_H"]) - 0.113) < 0.0006)
d$w <- mf_equal_area_weights(d)
weighted <- mf_joint_stats(d, weighted = TRUE)
check(weighted, "equal-urban-area-weighted statistic names and finite values")
stopifnot(abs(unname(weighted["Delta_H"]) - 0.130) < 0.0006)
checks <- lapply(expected_fields, function(nm)
  mf_report_check(pooled[nm], pooled[[nm]], paste("interface test", nm)))
stopifnot(all(vapply(checks, function(z) isTRUE(z$pass), logical(1))))
message("Stage 07 point-estimate interfaces and manuscript benchmarks PASSED.")
