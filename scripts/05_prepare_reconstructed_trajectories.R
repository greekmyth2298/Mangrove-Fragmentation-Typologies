# 05 | Import GIS-reconstructed, manually checked multi-step patch lineages.
# IMPORTANT: this file is an independent GIS-derived input, NOT a join on Script 04.
# One row is a descendant pathway; duplicated 1996 ancestry IDs and fully
# identical records are expected possible outcomes of branching and retained.
source("scripts/00_shared.R")
input <- "data/processed/RECONSTRUCTED_TRAJECTORIES.csv"
outdir <- mf_dir("outputs/trajectories")
d <- mf_read(input)
mf_check_cols(d, c("Country", "City", "City_ID_1996", "S_1996_2007",
                   "S_2007_2015", "S_2015_2020", "Boundary"), input)
if (!nrow(d)) stop("Trajectory input is empty.")
clean <- data.frame(
  country = trimws(d$Country), city = trimws(d$City),
  original_patch_id_1996 = trimws(d$City_ID_1996),
  s1 = mf_canonical_state(d$S_1996_2007),
  s2 = mf_canonical_state(d$S_2007_2015),
  s3 = mf_canonical_state(d$S_2015_2020),
  boundary_patch = mf_binary_boundary(d$Boundary),
  stringsAsFactors = FALSE
)
if (anyNA(clean[c("country", "city", "original_patch_id_1996", "s1", "s2", "s3")]) ||
    any(clean$country == "" | clean$city == "" | clean$original_patch_id_1996 == "")) {
  stop("The supplied closed-cohort CSV has incomplete trajectory/identifier fields.")
}
if (any((clean$s1 == "Lost" & clean$s2 != "Lost") |
        (clean$s2 == "Lost" & clean$s3 != "Lost"))) {
  stop("A trajectory departs the absorbing Lost state.")
}
# Synthetic row ID identifies a CSV entry, not an independently verified descendant.
clean$pathway_row_id <- seq_len(nrow(clean))
clean$urban_area_id <- clean$city  # Treat transboundary MUA labels as one unit.
clean$ancestral_patch_key <- paste(clean$urban_area_id,
                                   clean$original_patch_id_1996, sep = " | ")
clean$trajectory <- paste(clean$s1, clean$s2, clean$s3, sep = " -> ")
clean$trajectory_short <- paste(clean$s1, clean$s2, clean$s3, sep = "_")
clean$final_state <- clean$s3
clean$starts_lost <- clean$s1 == "Lost"
clean$has_lost_state <- clean$s1 == "Lost" | clean$s2 == "Lost" | clean$s3 == "Lost"
clean$loss_terminating <- clean$s3 == "Lost"
clean$closed_cohort <- TRUE
# Compatibility field names for readers of the original Script 05 output.
clean$city_id_1996 <- clean$original_patch_id_1996
clean$state_1996_2007 <- clean$s1
clean$state_2007_2015 <- clean$s2
clean$state_2015_2020 <- clean$s3
mf_write(clean, file.path(outdir, "FRAGMENTATION_TRAJECTORIES.csv"))

paths <- as.data.frame(table(Trajectory = clean$trajectory), stringsAsFactors = FALSE)
paths <- paths[order(-paths$Freq, paths$Trajectory), , drop = FALSE]
paths$Percent <- 100 * paths$Freq / nrow(clean)
mf_write(paths, file.path(outdir, "recurrent_trajectory_summary.csv"))
loss <- as.data.frame(table(Trajectory = clean$trajectory[clean$loss_terminating]),
                      stringsAsFactors = FALSE)
loss <- loss[order(-loss$Freq, loss$Trajectory), , drop = FALSE]
loss$Percent_Of_Lost <- 100 * loss$Freq / sum(clean$loss_terminating)
mf_write(loss, file.path(outdir, "loss_terminating_trajectory_summary.csv"))
city_summary <- as.data.frame(table(City = clean$city, Trajectory = clean$trajectory),
                              stringsAsFactors = FALSE)
city_summary <- city_summary[city_summary$Freq > 0, , drop = FALSE]
mf_write(city_summary, file.path(outdir, "city_trajectory_summary.csv"))

key_cols <- c("country", "city", "original_patch_id_1996",
              "s1", "s2", "s3", "boundary_patch")
audit <- data.frame(
  metric = c("pathway_rows", "urban_areas", "unique_1996_ancestors",
             "additional_rows_sharing_ancestor", "additional_identical_rows",
             "boundary_flagged", "lost_at_final_interval"),
  value = c(nrow(clean), length(unique(clean$urban_area_id)),
            length(unique(clean$ancestral_patch_key)),
            nrow(clean) - length(unique(clean$ancestral_patch_key)),
            sum(duplicated(clean[, key_cols, drop = FALSE])),
            sum(clean$boundary_patch), sum(clean$loss_terminating))
)
mf_write(audit, file.path(outdir, "trajectory_input_audit.csv"))
mf_session(file.path(outdir, "session_info.txt"))
message("05 complete: ", nrow(clean), " descendant pathway rows; ",
        length(unique(clean$urban_area_id)), " urban areas.")
