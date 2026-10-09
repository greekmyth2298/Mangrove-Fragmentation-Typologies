# 04 | Apply the trained Random Forest to all supplied transition datasets.
# Inputs: 21-urban-area 1996-2020 predictor table AND 30 original interval
#         prediction exports (10 urban areas x three observation intervals).
# Two independent products are kept separate:
#   (a) Archived historical predictions in the uploaded CSV files;
#   (b) New model predictions obtained by fitting Script 02 in this run.
# Differences do NOT imply that either dataset was relabeled or repaired.
# The 30 historical files have already been GIS-linked and classified; this
# script does NOT create missing patch lineages, Lost or Stabilizing states.
source("scripts/00_shared.R")
if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop("Script 04 requires randomForest; install.packages('randomForest').")
}
model_path <- "outputs/random_forest/rf_model_bundle.rds"
full_input <- "data/processed/PATCH_TRANSITIONS_1996_2020.csv"
manifest_path <- "data/interval_predictions/manifest.csv"
outdir <- mf_dir("outputs/typology_predictions")
for (path in c(model_path, full_input, manifest_path)) mf_need_file(path)
bundle <- readRDS(model_path)
if (!is.list(bundle) || !all(c("model", "predictors", "labels") %in% names(bundle))) {
  stop("Saved RF bundle is invalid; rerun Script 02.")
}
if (!identical(bundle$predictors, MF_PREDICTORS) ||
    !identical(bundle$labels, MF_LABELS)) {
  stop("Saved RF model metadata differs from the shared predictor/class specification.")
}

# Applies the newly trained model while retaining every input observation.
classify <- function(d, source_label) {
  mf_check_cols(d, MF_PREDICTORS, source_label)
  if (anyDuplicated(names(d))) stop("Duplicated column names: ", source_label)
  x <- d[, MF_PREDICTORS, drop = FALSE]
  for (nm in MF_PREDICTORS) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  ok <- stats::complete.cases(x) & apply(x, 1L, function(z) all(is.finite(z)))
  code <- rep(NA_character_, nrow(d))
  probs <- matrix(NA_real_, nrow(d), length(MF_LABELS),
                  dimnames = list(NULL, paste0("New_Prob_", names(MF_LABELS))))
  if (any(ok)) {
    predicted <- stats::predict(bundle$model, newdata = x[ok, , drop = FALSE],
                                type = "class")
    scores <- stats::predict(bundle$model, newdata = x[ok, , drop = FALSE],
                             type = "prob")
    code[ok] <- as.character(predicted)
    probs[ok, ] <- scores[, names(MF_LABELS), drop = FALSE]
  }
  list(code = code, label = unname(MF_LABELS[code]), scores = probs,
       eligible = ok)
}

# 04A. The original long-interval classification (21 urban areas, 1996-2020).
full <- mf_read(full_input)
if (!nrow(full)) stop("The full 1996-2020 transition dataset is empty.")
full_rf <- classify(full, full_input)
full$Predicted_Typology_Code <- full_rf$code
full$Predicted_Typology_Label <- full_rf$label
for (nm in colnames(full_rf$scores)) full[[nm]] <- full_rf$scores[, nm]
mf_write(full, file.path(outdir, "PATCH_TRANSITIONS_1996_2020_PREDICTED.csv"))
full_summary <- as.data.frame(table(City = full$City,
                                    Typology = full$Predicted_Typology_Label),
                              stringsAsFactors = FALSE)
full_summary <- full_summary[full_summary$Freq > 0L, , drop = FALSE]
mf_write(full_summary, file.path(outdir, "typology_1996_2020_by_urban_area.csv"))
mf_write(data.frame(Scope = "1996-2020, 21 areas", N = nrow(full),
                    New_Predicted = sum(full_rf$eligible),
                    Missing_Predictors = sum(!full_rf$eligible)),
         file.path(outdir, "classification_1996_2020_coverage.csv"))

# 04B. The original 30 interval-specific prediction files, imported via a
# manifest so city/period information is not inferred from fragile filenames.
manifest <- mf_read(manifest_path)
required <- c("urban_area", "interval", "file", "expected_rows")
mf_check_cols(manifest, required, "interval manifest")
if (nrow(manifest) != 30L || anyDuplicated(manifest$file)) {
  stop("Expected exactly 30 distinct interval files in the manifest.")
}
intervals <- c("1996_2007", "2007_2015", "2015_2020")
if (!setequal(unique(manifest$interval), intervals) ||
    length(unique(manifest$urban_area)) != 10L ||
    any(table(manifest$urban_area) != 3L)) {
  stop("Manifest must describe 3 intervals for each of 10 urban areas.")
}
expected <- suppressWarnings(as.integer(manifest$expected_rows))
if (anyNA(expected) || any(expected < 1L)) stop("Invalid manifest row counts.")

combined <- vector("list", nrow(manifest))
coverage <- vector("list", nrow(manifest))
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, , drop = FALSE]
  mf_need_file(row$file)
  d <- mf_read(row$file)
  if (nrow(d) != expected[i]) stop("Manifest row count mismatch for ", row$file)
  mf_check_cols(d, c("Predicted_Typology", paste0("Prob_", 0:4)), row$file)
  archival <- mf_typology_code(d$Predicted_Typology,
                               paste("historical predictions", row$file))
  pr <- classify(d, row$file)
  from <- substr(row$interval, 1, 4)
  to <- substr(row$interval, 6, 9)
  find_id <- function(year) {
    possibilities <- c(paste0("ID_", year), paste0("Patch_ID_", year))
    hit <- intersect(possibilities, names(d))
    if (length(hit) != 1L) stop("Expected exactly one ", year,
                               " identifier in ", row$file)
    d[[hit]]
  }
  orig_probs <- sapply(paste0("Prob_", 0:4), function(nm)
    suppressWarnings(as.numeric(d[[nm]])))
  if (is.null(dim(orig_probs))) orig_probs <- matrix(orig_probs, ncol = 5)
  if (any(!is.finite(orig_probs)) || any(orig_probs < -1e-8) ||
      any(orig_probs > 1 + 1e-8) ||
      any(abs(rowSums(orig_probs) - 1) > 0.015)) {
    stop("Invalid historical class probability distribution: ", row$file)
  }
  z <- data.frame(
    Source_File = row$file, Input_Row = seq_len(nrow(d)),
    Urban_Area = mf_canonical_city(rep(row$urban_area, nrow(d))),
    Interval = row$interval,
    Parent_Year = from, Child_Year = to,
    Parent_ID = as.character(find_id(from)),
    Child_ID = as.character(find_id(to)),
    Historical_Code = archival,
    Historical_Label = unname(MF_LABELS[archival]),
    New_RF_Code = pr$code,
    New_RF_Label = pr$label,
    Match = !is.na(pr$code) & archival == pr$code,
    stringsAsFactors = FALSE
  )
  for (j in 0:4) {
    z[[paste0("Historical_Prob_", j)]] <- orig_probs[, j + 1L]
    z[[paste0("New_Prob_", j)]] <- pr$scores[, j + 1L]
  }
  for (nm in MF_PREDICTORS) z[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  combined[[i]] <- z
  coverage[[i]] <- data.frame(Urban_Area = row$urban_area,
    Interval = row$interval, File = row$file,
    Rows = nrow(d), New_Classified = sum(pr$eligible),
    Model_Agrees = sum(z$Match),
    Match_Rate = mean(z$Match), stringsAsFactors = FALSE)
}
all_interval <- do.call(rbind, combined)
rownames(all_interval) <- NULL
coverage <- do.call(rbind, coverage)
rownames(coverage) <- NULL
if (nrow(all_interval) != 8909L) {
  warning("Interval transition count differs from 8,909 archived rows.")
}
mf_write(all_interval, file.path(outdir, "interval_transitions_historical_vs_refitted.csv"))
mf_write(coverage, file.path(outdir, "interval_prediction_comparison_by_file.csv"))
mf_write(all_interval[!all_interval$Match, , drop = FALSE],
         file.path(outdir, "interval_prediction_disagreements.csv"))
historical_class <- as.data.frame(table(Urban_Area = all_interval$Urban_Area,
    Interval = all_interval$Interval, Typology = all_interval$Historical_Label),
    stringsAsFactors = FALSE)
historical_class <- historical_class[historical_class$Freq > 0, , drop = FALSE]
mf_write(historical_class,
         file.path(outdir, "historical_interval_typology_composition.csv"))
writeLines(c(
  "The uploaded interval CSVs are the authoritative ARCHIVED historical predictions.",
  "New_RF_* columns were generated by the model trained in the current run.",
  "Disagreement can arise because the historical fitted RF model is not provided.",
  "Original class probabilities are never replaced by new probabilities.",
  "The 1996-2020 table covers 21 urban areas; the 30 shorter-interval files",
  "cover the 10 urban areas entering the path-dependence cohort.",
  "Script 05 imports GIS/manual reconstructed lineages independently;",
  "these classified transition tables do not encode every disappearance",
  "or rule used to establish Stabilizing and Lost states."
), file.path(outdir, "prediction_provenance_notes.txt"))
mf_session(file.path(outdir, "session_info.txt"))
message("04 complete: 1996-2020 records=", nrow(full),
        "; archived interval records=", nrow(all_interval),
        "; new RF agreement with archived labels=",
        sprintf("%.1f%%", 100 * mean(all_interval$Match)), ".")
