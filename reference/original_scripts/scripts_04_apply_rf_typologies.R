############################################################
# Applying the Random Forest Model to Classify Typologies
#
# Manuscript:
# Fragmentation Typologies, Trajectories, and Path Dependence
# in Urban Mangrove Landscapes of Southeast Asia
#
# Author: Allen Glen Gil
#
# Purpose:
#   Apply the trained Random Forest typology classifier to the
#   full patch-transition dataset and export predicted mangrove
#   fragmentation typologies.
#
# Input:
#   outputs/random_forest/rf_typology_model.rds
#   data/processed/ALL_PATCH_TRANSITIONS.csv
#
# Output:
#   outputs/typology_predictions/
#     ALL_PATCH_TRANSITIONS_PREDICTED.csv
#     typology_prediction_summary.csv
#     session_info.txt
############################################################

# ==========================================================
# 0. USER CONFIGURATION
# ==========================================================

model_path <- "outputs/random_forest/rf_typology_model.rds"
prediction_data_path <- "data/processed/ALL_PATCH_TRANSITIONS.csv"

output_dir <- "outputs/typology_predictions"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

predictors <- c(
  "log_area_parent",
  "log_area_child",
  "log_change_area",
  "log_change_ED",
  "log_change_shape_index",
  "log_change_ENN",
  "logit_retention_ratio",
  "log_centroid_shift"
)

class_labels <- c(
  "0" = "Nibbling",
  "1" = "Clearing",
  "2" = "Shattering",
  "3" = "Displacing",
  "4" = "Expanding"
)

# ==========================================================
# 1. LOAD PACKAGES
# ==========================================================

required_packages <- c("tidyverse", "randomForest")

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
  library(randomForest)
})

# ==========================================================
# 2. LOAD MODEL AND DATA
# ==========================================================

message("Loading trained Random Forest model...")

if (!file.exists(model_path)) {
  stop(
    "Model file not found: ", model_path,
    "\nPlease run 02_random_forest_typology_classification.R first, ",
    "or update model_path to the correct location."
  )
}

rf_model <- readRDS(model_path)

message("Loading full patch-transition dataset...")

if (!file.exists(prediction_data_path)) {
  stop(
    "Prediction dataset not found: ", prediction_data_path,
    "\nPlease provide the full patch-transition dataset or update prediction_data_path."
  )
}

prediction_data <- readr::read_csv(prediction_data_path, show_col_types = FALSE)

# ==========================================================
# 3. CHECK REQUIRED COLUMNS
# ==========================================================

missing_predictors <- setdiff(predictors, names(prediction_data))

if (length(missing_predictors) > 0) {
  stop(
    "The prediction dataset is missing required predictor columns: ",
    paste(missing_predictors, collapse = ", ")
  )
}

message("Input records: ", nrow(prediction_data))

# ==========================================================
# 4. HANDLE COMPLETE AND INCOMPLETE ROWS
# ==========================================================

prediction_data <- prediction_data %>%
  dplyr::mutate(Row_ID_for_prediction = dplyr::row_number())

complete_rows <- prediction_data %>%
  dplyr::filter(!if_any(dplyr::all_of(predictors), is.na))

incomplete_rows <- prediction_data %>%
  dplyr::filter(if_any(dplyr::all_of(predictors), is.na)) %>%
  dplyr::mutate(
    Predicted_Typology_Code = NA_character_,
    Predicted_Typology_Label = NA_character_
  )

message("Rows with complete predictors: ", nrow(complete_rows))
message("Rows with incomplete predictors: ", nrow(incomplete_rows))

# ==========================================================
# 5. PREDICT TYPOLOGIES
# ==========================================================

message("Predicting fragmentation typologies...")

pred_classes <- predict(rf_model, newdata = complete_rows, type = "class")
pred_probs <- predict(rf_model, newdata = complete_rows, type = "prob")

predicted_complete <- complete_rows %>%
  dplyr::mutate(
    Predicted_Typology_Code = as.character(pred_classes),
    Predicted_Typology_Label = dplyr::recode(
      Predicted_Typology_Code,
      !!!class_labels,
      .default = Predicted_Typology_Code
    )
  ) %>%
  dplyr::bind_cols(
    as.data.frame(pred_probs) %>%
      dplyr::rename_with(~ paste0("Prob_", .x))
  )

# Add probability columns to incomplete rows so binding works cleanly.
prob_cols <- names(pred_probs)

if (nrow(incomplete_rows) > 0) {
  for (prob_col in prob_cols) {
    incomplete_rows[[paste0("Prob_", prob_col)]] <- NA_real_
  }
}

prediction_results <- dplyr::bind_rows(
  predicted_complete,
  incomplete_rows
) %>%
  dplyr::arrange(Row_ID_for_prediction) %>%
  dplyr::select(-Row_ID_for_prediction)

# ==========================================================
# 6. EXPORT PREDICTIONS
# ==========================================================

prediction_output_path <- file.path(
  output_dir,
  "ALL_PATCH_TRANSITIONS_PREDICTED.csv"
)

readr::write_csv(prediction_results, prediction_output_path)

message("Predictions saved to: ", prediction_output_path)

# ==========================================================
# 7. EXPORT PREDICTION SUMMARY
# ==========================================================

prediction_summary <- prediction_results %>%
  dplyr::count(
    Predicted_Typology_Code,
    Predicted_Typology_Label,
    name = "n"
  ) %>%
  dplyr::arrange(dplyr::desc(n))

summary_output_path <- file.path(
  output_dir,
  "typology_prediction_summary.csv"
)

readr::write_csv(prediction_summary, summary_output_path)

message("Prediction summary saved to: ", summary_output_path)

print(prediction_summary)

# ==========================================================
# 8. OPTIONAL CITY/INTERVAL SUMMARY
# ==========================================================

summary_group_cols <- intersect(
  c("City", "Urban_Area", "Interval", "Time_Interval", "Year_Interval"),
  names(prediction_results)
)

if (length(summary_group_cols) > 0) {
  
  grouped_summary <- prediction_results %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(summary_group_cols)),
      Predicted_Typology_Code,
      Predicted_Typology_Label,
      name = "n"
    ) %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(summary_group_cols)),
      Predicted_Typology_Code
    )
  
  grouped_summary_path <- file.path(
    output_dir,
    "typology_prediction_summary_by_group.csv"
  )
  
  readr::write_csv(grouped_summary, grouped_summary_path)
  
  message("Grouped prediction summary saved to: ", grouped_summary_path)
}

# ==========================================================
# 9. SAVE SESSION INFORMATION
# ==========================================================

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_dir, "session_info.txt")
)

message("\nRandom Forest typology prediction complete.")