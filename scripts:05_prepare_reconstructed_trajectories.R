############################################################
# Preparing Reconstructed Fragmentation Trajectories
#
# Manuscript:
# Fragmentation Typologies, Trajectories, and Path Dependence
# in Urban Mangrove Landscapes of Southeast Asia
#
# Author: Allen Glen Gil
#
# 05_prepare_reconstructed_trajectories.R
#
# Purpose:
#   Prepare and standardize a pre-constructed trajectory dataset
#   for path-dependence and fragmentation trajectory analyses.
#
# IMPORTANT:
#   The input dataset (RECONSTRUCTED_TRAJECTORIES.csv) already
#   contains:
#     - patch lineages reconstructed across time
#     - typology states for each interval:
#         1996–2007, 2007–2015, 2015–2020
#     - a boundary flag used for sensitivity analysis
#
#   Lineage reconstruction was performed prior to this step
#   using GIS-based patch tracking and manual validation outside R.
#   This script does NOT reconstruct patch lineages from spatial data.
#
# Input:
#   data/processed/RECONSTRUCTED_TRAJECTORIES.csv
#
# Output:
#   outputs/trajectories/
#     FRAGMENTATION_TRAJECTORIES.csv
#     recurrent_trajectory_summary.csv
#     loss_terminating_trajectory_summary.csv
#     city_trajectory_summary.csv
#     session_info.txt
############################################################

# ==========================================================
# 0. USER CONFIGURATION
# ==========================================================

input_path <- "data/processed/RECONSTRUCTED_TRAJECTORIES.csv"

output_dir <- "outputs/trajectories"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

state_cols <- c(
  "S_1996_2007",
  "S_2007_2015",
  "S_2015_2020"
)

valid_states <- c(
  "Nibbling",
  "Clearing",
  "Shattering",
  "Displacing",
  "Expanding",
  "Stabilizing",
  "Lost"
)

# ==========================================================
# 1. LOAD PACKAGES
# ==========================================================

required_packages <- c("tidyverse", "janitor")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
})

# ==========================================================
# 2. LOAD DATA
# ==========================================================

message("Loading reconstructed trajectory data...")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

raw_data <- readr::read_csv(input_path, show_col_types = FALSE)

message("Input records: ", nrow(raw_data))

# ==========================================================
# 3. STANDARDIZE COLUMN NAMES
# ==========================================================

original_names <- names(raw_data)

trajectory_data <- raw_data %>%
  janitor::clean_names()

state_cols_clean <- janitor::make_clean_names(state_cols)

required_cols <- c(
  "country",
  "city",
  "city_id_1996",
  state_cols_clean
)

missing_cols <- setdiff(required_cols, names(trajectory_data))

if (length(missing_cols) > 0) {
  stop(
    "The reconstructed trajectory dataset is missing required columns after cleaning names: ",
    paste(missing_cols, collapse = ", "),
    "\nOriginal column names were: ",
    paste(original_names, collapse = ", ")
  )
}

# ==========================================================
# 4. CLEAN AND STANDARDIZE TYPOLOGY STATES
# ==========================================================

message("Cleaning typology state columns...")

standardize_state <- function(x) {
  x <- stringr::str_squish(as.character(x))
  
  dplyr::case_when(
    stringr::str_to_lower(x) == "nibbling" ~ "Nibbling",
    stringr::str_to_lower(x) == "clearing" ~ "Clearing",
    stringr::str_to_lower(x) == "shattering" ~ "Shattering",
    stringr::str_to_lower(x) == "displacing" ~ "Displacing",
    stringr::str_to_lower(x) == "expanding" ~ "Expanding",
    stringr::str_to_lower(x) == "stabilizing" ~ "Stabilizing",
    stringr::str_to_lower(x) == "stabilising" ~ "Stabilizing",
    stringr::str_to_lower(x) == "lost" ~ "Lost",
    TRUE ~ x
  )
}

trajectory_data <- trajectory_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(state_cols_clean),
      standardize_state
    )
  )

invalid_states <- trajectory_data %>%
  dplyr::select(dplyr::all_of(state_cols_clean)) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "interval",
    values_to = "state"
  ) %>%
  dplyr::filter(!is.na(state), !(state %in% valid_states)) %>%
  dplyr::distinct(interval, state)

if (nrow(invalid_states) > 0) {
  print(invalid_states)
  stop(
    "Invalid typology states detected. ",
    "Please correct these values before continuing."
  )
}

# ==========================================================
# 5. CLEAN BOUNDARY COLUMN
# ==========================================================

if ("boundary" %in% names(trajectory_data)) {
  
  trajectory_data <- trajectory_data %>%
    dplyr::mutate(
      boundary_patch = dplyr::case_when(
        is.na(boundary) ~ FALSE,
        stringr::str_to_lower(as.character(boundary)) %in%
          c("true", "t", "yes", "y", "1", "boundary") ~ TRUE,
        stringr::str_to_lower(as.character(boundary)) %in%
          c("false", "f", "no", "n", "0", "non-boundary", "nonboundary") ~ FALSE,
        TRUE ~ as.logical(boundary)
      )
    )
  
} else {
  
  warning(
    "No boundary column found. ",
    "A boundary_patch column will be created and set to FALSE for all records."
  )
  
  trajectory_data <- trajectory_data %>%
    dplyr::mutate(boundary_patch = FALSE)
}

# ==========================================================
# 6. CREATE TRAJECTORY FIELDS
# ==========================================================

message("Creating standardized trajectory fields...")

trajectory_data <- trajectory_data %>%
  dplyr::mutate(
    patch_lineage_id = as.character(city_id_1996),
    
    state_1996_2007 = .data[[state_cols_clean[1]]],
    state_2007_2015 = .data[[state_cols_clean[2]]],
    state_2015_2020 = .data[[state_cols_clean[3]]],
    
    trajectory = paste(
      state_1996_2007,
      state_2007_2015,
      state_2015_2020,
      sep = " -> "
    ),
    
    trajectory_short = paste(
      state_1996_2007,
      state_2007_2015,
      state_2015_2020,
      sep = "_"
    ),
    
    final_state = state_2015_2020,
    starts_lost = state_1996_2007 == "Lost",
    has_lost_state = if_any(
      dplyr::all_of(c("state_1996_2007", "state_2007_2015", "state_2015_2020")),
      ~ .x == "Lost"
    ),
    loss_terminating = final_state == "Lost",
    
    # All records in RECONSTRUCTED_TRAJECTORIES.csv are assumed
    # to be part of the closed cohort traceable from 1996.
    closed_cohort = !if_any(
      dplyr::all_of(c("state_1996_2007", "state_2007_2015", "state_2015_2020")),
      is.na
    )
  )

# ==========================================================
# 7. EXPORT CLEANED TRAJECTORY DATA
# ==========================================================

trajectory_export <- trajectory_data %>%
  dplyr::select(
    country,
    city,
    patch_lineage_id,
    city_id_1996,
    state_1996_2007,
    state_2007_2015,
    state_2015_2020,
    trajectory,
    trajectory_short,
    final_state,
    starts_lost,
    has_lost_state,
    loss_terminating,
    boundary_patch,
    closed_cohort,
    dplyr::everything(),
    -dplyr::any_of(c(
      "s_1996_2007",
      "s_2007_2015",
      "s_2015_2020",
      "boundary"
    ))
  )

trajectory_output_path <- file.path(
  output_dir,
  "FRAGMENTATION_TRAJECTORIES.csv"
)

readr::write_csv(trajectory_export, trajectory_output_path)

message("Cleaned trajectory dataset saved to: ", trajectory_output_path)

# ==========================================================
# 8. RECURRENT TRAJECTORY SUMMARY
# ==========================================================

recurrent_summary <- trajectory_export %>%
  dplyr::filter(closed_cohort) %>%
  dplyr::count(trajectory, name = "patches_n") %>%
  dplyr::mutate(percent = patches_n / sum(patches_n) * 100) %>%
  dplyr::arrange(dplyr::desc(patches_n))

readr::write_csv(
  recurrent_summary,
  file.path(output_dir, "recurrent_trajectory_summary.csv")
)

# ==========================================================
# 9. LOSS-TERMINATING TRAJECTORY SUMMARY
# ==========================================================

loss_terminating_summary <- trajectory_export %>%
  dplyr::filter(closed_cohort, loss_terminating) %>%
  dplyr::count(trajectory, name = "patches_n") %>%
  dplyr::mutate(percent = patches_n / sum(patches_n) * 100) %>%
  dplyr::arrange(dplyr::desc(patches_n))

readr::write_csv(
  loss_terminating_summary,
  file.path(output_dir, "loss_terminating_trajectory_summary.csv")
)

# ==========================================================
# 10. CITY-LEVEL TRAJECTORY SUMMARY
# ==========================================================

city_trajectory_summary <- trajectory_export %>%
  dplyr::filter(closed_cohort) %>%
  dplyr::count(city, trajectory, name = "patches_n") %>%
  dplyr::group_by(city) %>%
  dplyr::mutate(percent_city = patches_n / sum(patches_n) * 100) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(city, dplyr::desc(patches_n))

readr::write_csv(
  city_trajectory_summary,
  file.path(output_dir, "city_trajectory_summary.csv")
)

# ==========================================================
# 11. BASIC SUMMARY OUTPUT
# ==========================================================

message("\n=== Reconstructed trajectory preparation summary ===")
message("Total rows: ", nrow(trajectory_export))
message("Closed cohort rows: ", sum(trajectory_export$closed_cohort, na.rm = TRUE))
message("Boundary patches: ", sum(trajectory_export$boundary_patch, na.rm = TRUE))
message("Loss-terminating trajectories: ", sum(trajectory_export$loss_terminating, na.rm = TRUE))

message("\nTop recurrent trajectories:")
print(head(recurrent_summary, 10))

message("\nTop loss-terminating trajectories:")
print(head(loss_terminating_summary, 10))

# ==========================================================
# 12. SAVE SESSION INFORMATION
# ==========================================================

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_dir, "session_info.txt")
)

message("\nReconstructed trajectory preparation complete.")