############################################################
# Preparation of Training Data
#
# Manuscript:
# Fragmentation Typologies, Trajectories, and Path Dependence
# in Urban Mangrove Landscapes of Southeast Asia
#
# Author: Allen Glen Gil
#
# Purpose:
#   Clean and prepare the Random Forest training dataset for
#   mangrove fragmentation typology classification.
#
# Input:
#   data/raw/TRAINING_DATA.csv
#
# Output:
#   data/processed/TRAINING_DATA.csv
#
# Notes:
#   Patch metrics and transformed predictors were calculated
#   before this step. This script retains only the fields needed
#   for Random Forest classification and removes incomplete rows.
############################################################

# ==========================================================
# 0. USER CONFIGURATION
# ==========================================================

input_path  <- "data/raw/TRAINING_DATA.csv"
output_path <- "data/processed/TRAINING_DATA.csv"

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 1. LOAD PACKAGES
# ==========================================================

required_packages <- c("tidyverse")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(tidyverse)

# ==========================================================
# 2. DEFINE REQUIRED COLUMNS
# ==========================================================

id_cols <- c(
  "City",
  "Patch_ID_1996",
  "Patch_ID_2020"
)

predictor_cols <- c(
  "log_area_parent",
  "log_area_child",
  "log_change_area",
  "log_change_ED",
  "log_change_shape_index",
  "log_change_ENN",
  "logit_retention_ratio",
  "log_centroid_shift"
)

target_col <- "Manual_Typology"

required_cols <- c(id_cols, predictor_cols, target_col)

# ==========================================================
# 3. LOAD DATA
# ==========================================================

message("Loading raw training data...")

raw_data <- readr::read_csv(input_path, show_col_types = FALSE)

missing_cols <- setdiff(required_cols, names(raw_data))

if (length(missing_cols) > 0) {
  stop(
    "The input dataset is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

message("Raw records: ", nrow(raw_data))

# ==========================================================
# 4. CLEAN AND RETAIN MODELING COLUMNS
# ==========================================================

training_data <- raw_data %>%
  dplyr::select(dplyr::all_of(required_cols)) %>%
  dplyr::mutate(
    City = as.character(City),
    Patch_ID_1996 = as.character(Patch_ID_1996),
    Patch_ID_2020 = as.character(Patch_ID_2020),
    Manual_Typology = as.factor(Manual_Typology)
  ) %>%
  dplyr::filter(!if_any(dplyr::all_of(required_cols), is.na))

# ==========================================================
# 5. QUALITY CHECKS
# ==========================================================

message("Cleaned records: ", nrow(training_data))
message("Rows removed due to missing required values: ", nrow(raw_data) - nrow(training_data))

# Check for non-finite predictor values
nonfinite_check <- training_data %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(predictor_cols),
      ~ sum(!is.finite(.x)),
      .names = "nonfinite_{.col}"
    )
  )

if (sum(nonfinite_check) > 0) {
  print(nonfinite_check)
  stop("Non-finite values detected in predictor columns. Please inspect transformations.")
}

# Class balance summary
class_summary <- training_data %>%
  dplyr::count(Manual_Typology, name = "n") %>%
  dplyr::mutate(percent = n / sum(n) * 100)

city_class_summary <- training_data %>%
  dplyr::count(City, Manual_Typology, name = "n") %>%
  dplyr::arrange(City, Manual_Typology)

message("\nTypology class summary:")
print(class_summary)

message("\nCity × typology summary:")
print(city_class_summary)

# ==========================================================
# 6. EXPORT CLEANED TRAINING DATA AND SUMMARIES
# ==========================================================

readr::write_csv(training_data, output_path)

readr::write_csv(
  class_summary,
  "data/processed/TRAINING_DATA_class_summary.csv"
)

readr::write_csv(
  city_class_summary,
  "data/processed/TRAINING_DATA_city_typology_summary.csv"
)

message("\nCleaned training data saved to: ", output_path)
message("Class summaries saved to: data/processed/")
message("Done.")