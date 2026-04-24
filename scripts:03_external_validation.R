############################################################
# External Validation
#
# Manuscript:
# Fragmentation Typologies, Trajectories, and Path Dependence
# in Urban Mangrove Landscapes of Southeast Asia
#
# Author: Allen Glen Gil
#
# Purpose:
#   Evaluate external validation performance using an
#   external validation dataset that already contains both
#   manually assigned typologies and predicted typologies.
#
# Input:
#   data/processed/EXTERNAL_VALIDATION.csv
#
# Required columns:
#   City
#   Manual_Typology
#   Predicted_Typology
#
# Output:
#   outputs/external_validation/
#     external_validation_overall_metrics.csv
#     external_validation_city_metrics.csv
#     external_validation_confusion_matrix_raw.csv
#     external_validation_confusion_matrix_by_city.csv
#     external_validation_by_class_metrics.csv
#     external_validation_city_typology_summary.csv
#     session_info.txt
############################################################

# ==========================================================
# 0. USER CONFIGURATION
# ==========================================================

validation_data_path <- "data/processed/EXTERNAL_VALIDATION.csv"

output_dir <- "outputs/external_validation"
figure_dir <- "figures/external_validation"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

city_col <- "City"
target_col <- "Manual_Typology"
pred_col <- "Predicted_Typology"

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

required_packages <- c(
  "tidyverse",
  "caret",
  "e1071"
)

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
  library(caret)
  library(e1071)
})

# ==========================================================
# 2. LOAD AND CHECK DATA
# ==========================================================

message("Loading external validation data...")

if (!file.exists(validation_data_path)) {
  stop("External validation dataset not found: ", validation_data_path)
}

validation_data <- readr::read_csv(validation_data_path, show_col_types = FALSE)

required_cols <- c(city_col, target_col, pred_col)
missing_cols <- setdiff(required_cols, names(validation_data))

if (length(missing_cols) > 0) {
  stop(
    "The external validation dataset is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

validation_data <- validation_data %>%
  dplyr::filter(!if_any(dplyr::all_of(required_cols), is.na)) %>%
  dplyr::mutate(
    City = as.character(.data[[city_col]]),
    Manual_Typology = as.character(.data[[target_col]]),
    Predicted_Typology = as.character(.data[[pred_col]])
  )

message("External validation records after removing incomplete cases: ", nrow(validation_data))

# ==========================================================
# 3. STANDARDIZE CLASS LEVELS
# ==========================================================

# Use class labels if the typology columns are stored as numeric codes.
# If typology columns already contain labels, this step keeps them unchanged.

standardize_typology <- function(x, class_labels) {
  x <- as.character(x)
  
  dplyr::recode(
    x,
    !!!class_labels,
    .default = x
  )
}

validation_data <- validation_data %>%
  dplyr::mutate(
    Manual_Typology_Label = standardize_typology(Manual_Typology, class_labels),
    Predicted_Typology_Label = standardize_typology(Predicted_Typology, class_labels)
  )

typology_levels <- unique(c(
  class_labels,
  validation_data$Manual_Typology_Label,
  validation_data$Predicted_Typology_Label
))

typology_levels <- typology_levels[!is.na(typology_levels)]

validation_data <- validation_data %>%
  dplyr::mutate(
    Manual_Typology_Label = factor(Manual_Typology_Label, levels = typology_levels),
    Predicted_Typology_Label = factor(Predicted_Typology_Label, levels = typology_levels)
  )

# ==========================================================
# 4. OVERALL EXTERNAL VALIDATION METRICS
# ==========================================================

message("Calculating overall external validation metrics...")

overall_conf_mat <- caret::confusionMatrix(
  data = validation_data$Predicted_Typology_Label,
  reference = validation_data$Manual_Typology_Label
)

overall_metrics <- tibble::tibble(
  Metric = c(
    "Accuracy",
    "Kappa",
    "AccuracyLower",
    "AccuracyUpper",
    "NoInformationRate",
    "PValue_AccuracyGreaterThanNIR"
  ),
  Value = c(
    unname(overall_conf_mat$overall["Accuracy"]),
    unname(overall_conf_mat$overall["Kappa"]),
    unname(overall_conf_mat$overall["AccuracyLower"]),
    unname(overall_conf_mat$overall["AccuracyUpper"]),
    unname(overall_conf_mat$overall["AccuracyNull"]),
    unname(overall_conf_mat$overall["AccuracyPValue"])
  )
)

readr::write_csv(
  overall_metrics,
  file.path(output_dir, "external_validation_overall_metrics.csv")
)

raw_confusion_matrix <- as.data.frame(overall_conf_mat$table)

readr::write_csv(
  raw_confusion_matrix,
  file.path(output_dir, "external_validation_confusion_matrix_raw.csv")
)

by_class_metrics <- overall_conf_mat$byClass

if (is.null(dim(by_class_metrics))) {
  by_class_metrics <- as.data.frame(t(by_class_metrics))
  by_class_metrics$Class <- rownames(by_class_metrics)
  rownames(by_class_metrics) <- NULL
} else {
  by_class_metrics <- as.data.frame(by_class_metrics)
  by_class_metrics$Class <- rownames(by_class_metrics)
  rownames(by_class_metrics) <- NULL
}

by_class_metrics <- by_class_metrics %>%
  dplyr::select(Class, dplyr::everything())

readr::write_csv(
  by_class_metrics,
  file.path(output_dir, "external_validation_by_class_metrics.csv")
)

# ==========================================================
# 5. CITY-LEVEL EXTERNAL VALIDATION METRICS
# ==========================================================

message("Calculating city-level external validation metrics...")

city_metrics_list <- list()
city_confusion_list <- list()

for (city_name in sort(unique(validation_data[[city_col]]))) {
  
  city_df <- validation_data %>%
    dplyr::filter(.data[[city_col]] == city_name)
  
  city_reference <- factor(
    city_df$Manual_Typology_Label,
    levels = typology_levels
  )
  
  city_prediction <- factor(
    city_df$Predicted_Typology_Label,
    levels = typology_levels
  )
  
  city_conf_mat <- caret::confusionMatrix(
    data = city_prediction,
    reference = city_reference
  )
  
  city_metrics_list[[city_name]] <- tibble::tibble(
    City = city_name,
    N = nrow(city_df),
    Accuracy = unname(city_conf_mat$overall["Accuracy"]),
    Kappa = unname(city_conf_mat$overall["Kappa"]),
    AccuracyLower = unname(city_conf_mat$overall["AccuracyLower"]),
    AccuracyUpper = unname(city_conf_mat$overall["AccuracyUpper"]),
    NoInformationRate = unname(city_conf_mat$overall["AccuracyNull"]),
    PValue_AccuracyGreaterThanNIR = unname(city_conf_mat$overall["AccuracyPValue"])
  )
  
  city_confusion_list[[city_name]] <- as.data.frame(city_conf_mat$table) %>%
    dplyr::mutate(City = city_name) %>%
    dplyr::select(City, dplyr::everything())
}

city_metrics <- dplyr::bind_rows(city_metrics_list)
city_confusion_matrices <- dplyr::bind_rows(city_confusion_list)

readr::write_csv(
  city_metrics,
  file.path(output_dir, "external_validation_city_metrics.csv")
)

readr::write_csv(
  city_confusion_matrices,
  file.path(output_dir, "external_validation_confusion_matrix_by_city.csv")
)

# ==========================================================
# 6. CITY × TYPOLOGY SUMMARY
# ==========================================================

city_typology_summary <- validation_data %>%
  dplyr::count(
    City,
    Observed = Manual_Typology_Label,
    Predicted = Predicted_Typology_Label,
    name = "n"
  ) %>%
  dplyr::arrange(City, Observed, Predicted)

readr::write_csv(
  city_typology_summary,
  file.path(output_dir, "external_validation_city_typology_summary.csv")
)

# ==========================================================
# 7. EXPORT VALIDATION DATA WITH STANDARDIZED LABELS
# ==========================================================

validation_output <- validation_data %>%
  dplyr::select(
    City,
    Manual_Typology,
    Predicted_Typology,
    Manual_Typology_Label,
    Predicted_Typology_Label,
    dplyr::everything()
  )

readr::write_csv(
  validation_output,
  file.path(output_dir, "external_validation_predictions_standardized.csv")
)

# ==========================================================
# 8. FIGURE: EXTERNAL VALIDATION ACCURACY BY CITY
# ==========================================================

city_accuracy_plot <- city_metrics %>%
  ggplot(aes(x = reorder(City, Accuracy), y = Accuracy)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "External Validation Accuracy by Urban Area",
    x = "Urban area",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(figure_dir, "external_validation_accuracy_by_city.png"),
  plot = city_accuracy_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# ==========================================================
# 9. PRINT SUMMARY
# ==========================================================

message("\n=== Overall External Validation Metrics ===")
print(overall_metrics)

message("\n=== City-Level External Validation Metrics ===")
print(city_metrics)

message("\nOutputs saved to: ", output_dir)
message("Figures saved to: ", figure_dir)

# ==========================================================
# 10. SAVE SESSION INFORMATION
# ==========================================================

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_dir, "session_info.txt")
)

message("\nExternal validation complete.")