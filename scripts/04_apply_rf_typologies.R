# 04 | Apply the five-class RF model to an independently constructed
# ALL_PATCH_TRANSITIONS dataset. Never fabricate this GIS-derived dataset.
# This stage is optional if the required input has not been supplied.
source("scripts/00_shared.R")
model_file <- "outputs/random_forest/rf_model_bundle.rds"
input <- "data/processed/ALL_PATCH_TRANSITIONS.csv"
outdir <- mf_dir("outputs/typology_predictions")
mf_need_file(model_file, "Run Script 02 to train the classifier.")
mf_need_file(input,
  paste0("This GIS-derived all-transition dataset was not among the uploaded files.\n",
         "Supply the eight transformed predictors before running Script 04."))

bundle <- readRDS(model_file)
if (!all(c("model", "predictors", "labels") %in% names(bundle)))
  stop("Model bundle is missing required metadata.")
d <- mf_read(input)
mf_check_cols(d, bundle$predictors, input)
if (anyDuplicated(names(d))) stop("Prediction input contains duplicate column names.")
for (nm in bundle$predictors) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
usable <- stats::complete.cases(d[, bundle$predictors, drop = FALSE]) &
  apply(d[, bundle$predictors, drop = FALSE], 1L, function(x) all(is.finite(x)))
# Retain all records and original order. Rows without usable metrics remain unclassified.
d$Predicted_Typology_Code <- NA_character_
d$Predicted_Typology_Label <- NA_character_
prob_names <- paste0("Prob_", names(bundle$labels))
for (nm in prob_names) d[[nm]] <- NA_real_
if (any(usable)) {
  p <- stats::predict(bundle$model, newdata = d[usable, , drop = FALSE], type = "class")
  pp <- stats::predict(bundle$model, newdata = d[usable, , drop = FALSE], type = "prob")
  d$Predicted_Typology_Code[usable] <- as.character(p)
  d$Predicted_Typology_Label[usable] <- unname(bundle$labels[as.character(p)])
  for (code in names(bundle$labels)) {
    if (code %in% colnames(pp)) d[[paste0("Prob_", code)]][usable] <- pp[, code]
  }
}
mf_write(d, file.path(outdir, "ALL_PATCH_TRANSITIONS_PREDICTED.csv"))
summary <- as.data.frame(table(Code = factor(d$Predicted_Typology_Code,
                                               levels = names(bundle$labels))),
                         stringsAsFactors = FALSE)
summary$Typology <- unname(bundle$labels[summary$Code])
mf_write(summary, file.path(outdir, "typology_prediction_summary.csv"))
mf_write(data.frame(metric = c("input_rows", "classified_rows", "unclassified_rows"),
                    value = c(nrow(d), sum(usable), sum(!usable))),
         file.path(outdir, "prediction_audit.csv"))
mf_session(file.path(outdir, "session_info.txt"))
message("04 complete: ", sum(usable), "/", nrow(d), " transitions classified.")
