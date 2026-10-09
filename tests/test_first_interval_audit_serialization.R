# Focused regression test for first-interval audit output and TRUE serialization.
# Run from the project root AFTER Script 05, or source Script 05 below.
source("scripts/00_shared.R")
if (!file.exists("outputs/trajectories/1996_2007_prediction_audit_summary.csv")) {
  source("scripts/05_prepare_reconstructed_trajectories.R")
}
audit <- mf_read("outputs/trajectories/1996_2007_prediction_audit_summary.csv")
detail <- mf_read("outputs/trajectories/1996_2007_prediction_multiset_audit.csv")
stopifnot(
  length(audit$Result[audit$Check == "reconstructed_first_interval_rows"]) == 1L,
  identical(audit$Result[audit$Check == "reconstructed_first_interval_rows"], "3186"),
  identical(audit$Result[audit$Check == "archived_first_interval_rows"], "3186"),
  identical(audit$Result[audit$Check == "city_ancestor_class_multiset_matches"], "TRUE"),
  nrow(detail) > 0L,
  all(detail$Equal == "TRUE")
)
message("PASS: 1996-2007 multiset audit and TRUE serialization.")
