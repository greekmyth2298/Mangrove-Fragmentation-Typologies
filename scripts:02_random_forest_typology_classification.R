############################################################
# Random Forest Classification of Mangrove Fragmentation Typologies
#
# Manuscript:
# Fragmentation Typologies, Trajectories, and Path Dependence
# in Urban Mangrove Landscapes of Southeast Asia
#
# Author: Allen Glen Gil
#
# Purpose:
#   1. Load manually classified training data
#   2. Conduct stratified train/test split by City × Typology
#   3. Train Random Forest classifier
#   4. Evaluate performance using confusion matrix, accuracy, Kappa, NIR
#   5. Export model outputs, evaluation tables, and figures
#   6. Optionally apply trained model to a new dataset
############################################################

# ==========================================================
# 0. USER CONFIGURATION
# ==========================================================

# ---- Input files ----
# Manually classified dataset used for model training and validation
training_data_path <- "data/processed/TRAINING_DATA.csv"

# Optional prediction dataset.
# Set to NULL if not applying the model to new data.
new_data_path <- NULL
# Example:
# new_data_path <- "data/processed/NORMALIZED_KOTA_KINABALU_1996_2020.csv"

# ---- Output folders ----
output_dir <- "outputs/random_forest"
figure_dir <- "figures/random_forest"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Reproducibility ----
random_seed <- 42
set.seed(random_seed)

# ---- Model settings ----
train_prop <- 0.70
ntree_value <- 500

target_col <- "Manual_Typology"
city_col   <- "City"

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

required_packages <- c(
  "tidyverse",
  "caret",
  "randomForest",
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

library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

# ==========================================================
# 2. LOAD AND CHECK DATA
# ==========================================================

message("Loading training data...")

dat <- readr::read_csv(training_data_path, show_col_types = FALSE)

required_cols <- c(predictors, target_col, city_col)
missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop(
    "The training dataset is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

dat <- dat %>%
  dplyr::filter(!if_any(all_of(required_cols), is.na)) %>%
  dplyr::mutate(
    !!target_col := as.factor(.data[[target_col]]),
    !!city_col := as.factor(.data[[city_col]])
  )

if (!all(levels(dat[[target_col]]) %in% names(class_labels))) {
  warning(
    "Some target levels do not match the class_labels object. ",
    "Check typology labels before interpreting outputs."
  )
}

message("Training records after removing incomplete cases: ", nrow(dat))

# ==========================================================
# 3. STRATIFIED TRAIN/TEST SPLIT BY CITY × TYPOLOGY
# ==========================================================

message("Creating stratified train/test split by City × Typology...")

split_data <- dat %>%
  dplyr::group_by(.data[[city_col]], .data[[target_col]]) %>%
  dplyr::group_modify(~ {
    n_cell <- nrow(.x)
    
    if (n_cell == 1) {
      .x$Set <- "train"
    } else {
      n_train <- max(1, min(n_cell - 1, round(train_prop * n_cell)))
      train_idx <- sample(seq_len(n_cell), n_train)
      .x$Set <- ifelse(seq_len(n_cell) %in% train_idx, "train", "test")
    }
    
    .x
  }) %>%
  dplyr::ungroup()

train_data <- split_data %>%
  dplyr::filter(Set == "train") %>%
  dplyr::select(-Set)

test_data <- split_data %>%
  dplyr::filter(Set == "test") %>%
  dplyr::select(-Set)

train_data[[target_col]] <- droplevels(as.factor(train_data[[target_col]]))
test_data[[target_col]] <- factor(
  test_data[[target_col]],
  levels = levels(train_data[[target_col]])
)

split_summary <- split_data %>%
  dplyr::count(.data[[city_col]], Set, name = "n") %>%
  tidyr::pivot_wider(
    names_from = Set,
    values_from = n,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    total = train + test,
    train_prop_observed = train / total,
    test_prop_observed = test / total
  )

readr::write_csv(
  train_data,
  file.path(output_dir, "rf_train_data.csv")
)

readr::write_csv(
  test_data,
  file.path(output_dir, "rf_test_data.csv")
)

readr::write_csv(
  split_summary,
  file.path(output_dir, "rf_train_test_split_summary.csv")
)

message("Training records: ", nrow(train_data))
message("Testing records: ", nrow(test_data))

# ==========================================================
# 4. TRAIN RANDOM FOREST MODEL
# ==========================================================

message("Training Random Forest model...")

rf_formula <- as.formula(
  paste(target_col, "~", paste(predictors, collapse = " + "))
)

rf_model <- randomForest::randomForest(
  formula = rf_formula,
  data = train_data,
  ntree = ntree_value,
  importance = TRUE,
  na.action = na.omit
)

saveRDS(
  rf_model,
  file.path(output_dir, "rf_typology_model.rds")
)

# ==========================================================
# 5. MODEL EVALUATION
# ==========================================================

message("Evaluating model on held-out test data...")

test_pred <- predict(rf_model, newdata = test_data, type = "class")

conf_mat <- caret::confusionMatrix(
  data = factor(test_pred, levels = levels(train_data[[target_col]])),
  reference = factor(test_data[[target_col]], levels = levels(train_data[[target_col]]))
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
    unname(conf_mat$overall["Accuracy"]),
    unname(conf_mat$overall["Kappa"]),
    unname(conf_mat$overall["AccuracyLower"]),
    unname(conf_mat$overall["AccuracyUpper"]),
    unname(conf_mat$overall["AccuracyNull"]),
    unname(conf_mat$overall["AccuracyPValue"])
  )
)

readr::write_csv(
  overall_metrics,
  file.path(output_dir, "rf_overall_metrics.csv")
)

raw_confusion_matrix <- as.data.frame(conf_mat$table)

readr::write_csv(
  raw_confusion_matrix,
  file.path(output_dir, "rf_confusion_matrix_raw.csv")
)

by_class_metrics <- conf_mat$byClass

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
  file.path(output_dir, "rf_by_class_metrics.csv")
)

message("Overall model performance:")
print(overall_metrics)

# ==========================================================
# 6. CONFUSION MATRIX HEATMAP
# ==========================================================

message("Creating confusion matrix heatmap...")

cm_df <- as.data.frame(conf_mat$table)
colnames(cm_df) <- c("Prediction", "Reference", "Freq")

cm_df <- cm_df %>%
  dplyr::mutate(
    Reference = factor(
      Reference,
      levels = names(class_labels),
      labels = class_labels[names(class_labels)]
    ),
    Prediction = factor(
      Prediction,
      levels = names(class_labels),
      labels = class_labels[names(class_labels)]
    )
  )

cm_norm <- cm_df %>%
  dplyr::group_by(Reference) %>%
  dplyr::mutate(
    Percent = Freq / sum(Freq) * 100
  ) %>%
  dplyr::ungroup()

confusion_plot <- ggplot(
  cm_norm,
  aes(x = Reference, y = Prediction, fill = Percent)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), size = 4) +
  scale_fill_gradient(low = "white", high = "grey30") +
  labs(
    title = "Random Forest Confusion Matrix",
    subtitle = "Row-normalized by observed typology",
    x = "Observed typology",
    y = "Predicted typology",
    fill = "% of observed class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  filename = file.path(figure_dir, "rf_confusion_matrix_heatmap.png"),
  plot = confusion_plot,
  width = 7,
  height = 6,
  dpi = 300
)

# ==========================================================
# 7. VARIABLE IMPORTANCE
# ==========================================================

message("Exporting variable importance...")

importance_matrix <- randomForest::importance(rf_model)

variable_importance <- tibble::tibble(
  Variable = rownames(importance_matrix),
  MeanDecreaseGini = importance_matrix[, "MeanDecreaseGini"]
) %>%
  dplyr::arrange(dplyr::desc(MeanDecreaseGini))

readr::write_csv(
  variable_importance,
  file.path(output_dir, "rf_variable_importance.csv")
)

varimp_plot <- ggplot(
  variable_importance,
  aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance",
    x = "Predictor",
    y = "Mean decrease in Gini"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(figure_dir, "rf_variable_importance.png"),
  plot = varimp_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# ==========================================================
# 8. OPTIONAL: APPLY MODEL TO NEW DATA
# ==========================================================

if (!is.null(new_data_path)) {
  
  message("Applying trained model to new data...")
  
  new_data <- readr::read_csv(new_data_path, show_col_types = FALSE)
  
  missing_new_cols <- setdiff(predictors, names(new_data))
  
  if (length(missing_new_cols) > 0) {
    stop(
      "The new dataset is missing required predictor columns: ",
      paste(missing_new_cols, collapse = ", ")
    )
  }
  
  new_data_clean <- new_data %>%
    dplyr::filter(!if_any(all_of(predictors), is.na))
  
  pred_classes <- predict(rf_model, newdata = new_data_clean, type = "class")
  pred_probs <- predict(rf_model, newdata = new_data_clean, type = "prob")
  
  pred_results <- new_data_clean %>%
    dplyr::mutate(
      Predicted_Typology_Code = as.character(pred_classes),
      Predicted_Typology_Label = dplyr::recode(
        Predicted_Typology_Code,
        !!!class_labels
      )
    ) %>%
    dplyr::bind_cols(
      as.data.frame(pred_probs) %>%
        dplyr::rename_with(~ paste0("Prob_", .x))
    )
  
  prediction_output_path <- file.path(
    output_dir,
    "rf_predictions_new_data.csv"
  )
  
  readr::write_csv(pred_results, prediction_output_path)
  
  prediction_summary <- pred_results %>%
    dplyr::count(Predicted_Typology_Code, Predicted_Typology_Label, name = "n") %>%
    dplyr::arrange(desc(n))
  
  readr::write_csv(
    prediction_summary,
    file.path(output_dir, "rf_predictions_summary.csv")
  )
  
  message("Predictions saved to: ", prediction_output_path)
}

# ==========================================================
# 9. SAVE SESSION INFORMATION
# ==========================================================

session_info_path <- file.path(output_dir, "session_info.txt")

writeLines(
  capture.output(sessionInfo()),
  con = session_info_path
)

message("Random Forest typology classification complete.")
message("Outputs saved to: ", output_dir)
message("Figures saved to: ", figure_dir)