# Reproducibility verification for OUTPUTS after executing 01-07.
# Fails if required outputs are missing or published trajectory benchmarks differ.
source("scripts/00_shared.R")
read_output <- function(path) {
  mf_need_file(path, "Run the entire pipeline first: source('run_all.R')")
  mf_read(path)
}
assert <- function(value, explanation) {
  if (!isTRUE(value)) stop("REPRODUCTION CHECK FAILED: ", explanation, call. = FALSE)
  message("  PASS  ", explanation)
}
message("Checking output completeness and published manuscript benchmarks...")
train <- read_output("data/processed/TRAINING_DATA.csv")
assert(nrow(train) == 1212L, "1,212 training records")
model <- readRDS("outputs/random_forest/rf_model_bundle.rds")
assert(identical(model$predictors, MF_PREDICTORS), "RF predictor definitions")
validate <- read_output("outputs/external_validation/external_validation_city_metrics.csv")
assert(nrow(validate) == 5L, "five independently validated urban areas")
full <- read_output("outputs/typology_predictions/PATCH_TRANSITIONS_1996_2020_PREDICTED.csv")
assert(nrow(full) == 4584L, "4,584 classified 1996-2020 records")
interval <- read_output("outputs/typology_predictions/interval_transitions_historical_vs_refitted.csv")
assert(nrow(interval) == 8909L, "8,909 archived interval transition records")
assert(length(unique(interval$Urban_Area)) == 10L, "ten interval study urban areas")
assert(length(unique(interval$Interval)) == 3L, "three intermediate intervals")
traj <- read_output("outputs/trajectories/FRAGMENTATION_TRAJECTORIES.csv")
assert(nrow(traj) == 3186L, "3,186 closed-cohort descendant trajectories")
# The original historical CSV and code are archived unchanged.
source("tests/test_original_source_provenance.R",
       local = new.env(parent = globalenv()))
first <- read_output("outputs/trajectories/1996_2007_prediction_audit_summary.csv")
# Verify both the recorded comparison and the underlying key-level audit.
# The previous Script 05 accidentally serialized TRUE as numeric 1 because
# c(3186, 3186, TRUE) coerces all values to numeric.
lookup_result <- function(name) {
  rows <- first$Result[first$Check == name]
  if (length(rows) != 1L) stop("Missing or repeated 1996-2007 audit check: ", name)
  trimws(as.character(rows))
}
assert(identical(lookup_result("reconstructed_first_interval_rows"), "3186") &&
       identical(lookup_result("archived_first_interval_rows"), "3186"),
       "1996-2007 audit records 3,186 predictions and trajectories")
first_detail <- read_output("outputs/trajectories/1996_2007_prediction_multiset_audit.csv")
assert(nrow(first_detail) > 0L &&
       all(first_detail$Equal == "TRUE") &&
       identical(lookup_result("city_ancestor_class_multiset_matches"), "TRUE"),
       "1996-2007 archived predictions match first trajectory states")
t3 <- read_output("outputs/trajectory_analysis/publication_table3_reproduction_check.csv")
t4 <- read_output("outputs/trajectory_analysis/publication_table4_reproduction_check.csv")
assert(nrow(t3) == 10L && all(t3$Matches == "TRUE"), "published Table 3 counts")
assert(nrow(t4) == 10L && all(t4$Matches == "TRUE"), "published Table 4 counts")
effects <- read_output("outputs/path_dependence/publication_tables5_6_reproduction_checks.csv")
assert(all(effects$pass == "TRUE"), "published Tables 5-6 point estimates")
n <- read_output("outputs/path_dependence/publication_table6_sample_size_checks.csv")
assert(all(n$Matches == "TRUE"), "published Table 6 cohort sizes")
boot <- read_output("outputs/path_dependence/bootstrap_legacy_two_stage_summary.csv")
assert(nrow(boot) == 4L, "two-stage bootstrap pooled and equal-city summaries")
message("All specified deterministic publication checks PASSED.")
message("NOTE: bootstrap CIs are stochastic and may not equal rounded published values.")
message("NOTE: validation CSV may differ from published Table 2 accuracies.")
message("NOTE: the spatial lineage construction itself precedes these CSVs.")
