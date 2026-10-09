# 02 | Train/test stratified Random Forest classifier (500 trees, seed 42).
# Follows the published five-class internal validation, 70/30 within City x Typology.
# Predictor construction and GIS metrics are upstream and are NOT recreated here.
source("scripts/00_shared.R")
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Install the required package once: install.packages('randomForest')")
}

input <- "data/processed/TRAINING_DATA.csv"
outdir <- mf_dir("outputs/random_forest")
figdir <- mf_dir("figures/random_forest")
d <- mf_read(input)
mf_check_cols(d, c("City", MF_PREDICTORS, "Manual_Typology"), "Training dataset")
d$Manual_Typology <- mf_typology_code(d$Manual_Typology)
for (nm in MF_PREDICTORS) {
  d[[nm]] <- as.numeric(d[[nm]])
  if (anyNA(d[[nm]]) || any(!is.finite(d[[nm]]))) stop("Bad RF predictor: ", nm)
}
if (anyNA(d$City)) stop("City cannot be missing.")

seed <- 42L; train_fraction <- 0.70; trees <- 500L
set.seed(seed)
# Keep at least one train and test sample in each stratum with n >= 2.
# Singleton strata enter the training set, as in the original code.
strata <- interaction(d$City, d$Manual_Typology, drop = TRUE)
parts <- split(seq_len(nrow(d)), strata)
train_indices <- unlist(lapply(parts, function(ids) {
  if (length(ids) == 1L) return(ids)
  k <- max(1L, min(length(ids) - 1L, round(train_fraction * length(ids))))
  sample(ids, size = k, replace = FALSE)
}), use.names = FALSE)
train_mask <- seq_len(nrow(d)) %in% train_indices
train <- d[train_mask, , drop = FALSE]
test <- d[!train_mask, , drop = FALSE]
if (!nrow(test)) stop("Training/testing split left no test records.")
train$Manual_Typology <- factor(train$Manual_Typology, levels = names(MF_LABELS))
test$Manual_Typology <- factor(test$Manual_Typology, levels = names(MF_LABELS))
if (any(table(train$Manual_Typology) == 0)) stop("A target class is absent from training.")
mf_write(train, file.path(outdir, "rf_train_data.csv"))
mf_write(test, file.path(outdir, "rf_test_data.csv"))
split_summary <- as.data.frame(table(City = d$City,
                                     Set = ifelse(train_mask, "train", "test")),
                               stringsAsFactors = FALSE)
mf_write(split_summary, file.path(outdir, "rf_train_test_split_summary.csv"))

formula <- stats::reformulate(MF_PREDICTORS, response = "Manual_Typology")
fit <- randomForest::randomForest(formula, data = train, ntree = trees,
                                  importance = TRUE, na.action = stats::na.fail)
saveRDS(fit, file.path(outdir, "rf_typology_model.rds"))
saveRDS(list(model = fit, predictors = MF_PREDICTORS,
             labels = MF_LABELS, seed = seed, ntree = trees,
             training_rows = nrow(train), testing_rows = nrow(test)),
        file.path(outdir, "rf_model_bundle.rds"))
pred <- as.character(stats::predict(fit, newdata = test, type = "class"))
test$Predicted_Typology <- pred
test$Predicted_Typology_Label <- unname(MF_LABELS[pred])
mf_write(test, file.path(outdir, "rf_test_predictions.csv"))
metrics <- mf_accuracy_stats(as.character(test$Manual_Typology), pred)
mf_write(metrics$metrics, file.path(outdir, "rf_overall_metrics.csv"))
mf_write(metrics$by_class, file.path(outdir, "rf_by_class_metrics.csv"))
mf_write(mf_confusion_long(metrics$confusion),
         file.path(outdir, "rf_confusion_matrix_raw.csv"))

imp <- randomForest::importance(fit)
importance <- data.frame(Variable = rownames(imp),
                         MeanDecreaseGini = imp[, "MeanDecreaseGini"],
                         row.names = NULL)
importance <- importance[order(-importance$MeanDecreaseGini), , drop = FALSE]
mf_write(importance, file.path(outdir, "rf_variable_importance.csv"))

# Plot with base graphics so only randomForest is required by the entire pipeline.
grDevices::png(file.path(figdir, "rf_confusion_matrix_heatmap.png"),
              width = 1200, height = 1000, res = 150)
mat <- metrics$confusion
col_tot <- colSums(mat)
percent <- sweep(mat, 2, pmax(col_tot, 1), "/") * 100
par(mar = c(8, 8, 4, 2))
image(seq_len(ncol(mat)), seq_len(nrow(mat)), t(percent),
      col = gray(seq(1, 0.25, length.out = 50)), zlim = c(0, 100),
      axes = FALSE, xlab = "", ylab = "", main = "RF internal validation")
axis(1, at = seq_len(ncol(mat)), labels = MF_LABELS, las = 2)
axis(2, at = seq_len(nrow(mat)), labels = MF_LABELS, las = 2)
mtext("Observed typology", side = 1, line = 6)
mtext("Predicted typology", side = 2, line = 6)
for (j in seq_len(ncol(mat))) for (i in seq_len(nrow(mat))) {
  text(j, i, paste0(format(round(percent[i, j], 1), nsmall = 1), "%"), cex = 0.8)
}
grDevices::dev.off()
grDevices::png(file.path(figdir, "rf_variable_importance.png"),
              width = 1200, height = 800, res = 150)
par(mar = c(5, 13, 3, 1))
barplot(rev(importance$MeanDecreaseGini), names.arg = rev(importance$Variable),
        horiz = TRUE, las = 1, col = "gray50", xlab = "Mean decrease in Gini",
        main = "Random Forest predictor importance")
grDevices::dev.off()
mf_session(file.path(outdir, "session_info.txt"))
message("02 complete: test accuracy = ", round(metrics$metrics$Accuracy, 4),
        "; n train/test = ", nrow(train), "/", nrow(test))
