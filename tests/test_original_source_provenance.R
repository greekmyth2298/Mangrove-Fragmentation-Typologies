# Confirm the exact original files provided by the author are archived unchanged.
# Base R only; run from the repository root.
source("scripts/00_shared.R")
archived_data <- "data/original/ALL CITIES COMPILED.csv"
pipeline_data <- "data/processed/RECONSTRUCTED_TRAJECTORIES.csv"
archived_code <- "reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R"
previous_code <- "reference/historical_code/path_dependence_original_recovered.txt"
for (p in c(archived_data, pipeline_data, archived_code, previous_code)) mf_need_file(p)

# Compare contents without requiring digest or command-line SHA utilities.
# This verifies both the original header and all 3,186 source trajectories.
original <- mf_read(archived_data)
prepared <- mf_read(pipeline_data)
if (!identical(original, prepared) || nrow(original) != 3186L ||
    !identical(names(original), c("Country", "City", "City_ID_1996",
                                  "S_1996_2007", "S_2007_2015",
                                  "S_2015_2020", "Boundary"))) {
  stop("Original ALL CITIES COMPILED.csv differs from the trajectory input.",
       call. = FALSE)
}
if (unname(tools::md5sum(archived_data)) != unname(tools::md5sum(pipeline_data))) {
  stop("Archived trajectory CSV bytes differ from the supplied original.",
       call. = FALSE)
}
message("  PASS  original ALL CITIES COMPILED.csv preserved without changes")

if (unname(tools::md5sum(archived_code)) != unname(tools::md5sum(previous_code))) {
  stop("Original historical path-dependence R script has changed.",
       call. = FALSE)
}
message("  PASS  original historical R script preserved without changes")
