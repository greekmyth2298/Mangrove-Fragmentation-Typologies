# 01 | Prepare manually classified Random Forest training records.
# Input: data/raw/TRAINING_DATA.csv
# Output: data/processed/TRAINING_DATA.csv and audit summaries.
source("scripts/00_shared.R")

input <- "data/raw/TRAINING_DATA.csv"
out <- "data/processed/TRAINING_DATA.csv"
required <- c("City", "Patch_ID_1996", "Patch_ID_2020", MF_PREDICTORS,
              "Manual_Typology")
d <- mf_read(input)
mf_check_cols(d, required, "Raw training data")
raw_n <- nrow(d)
d <- d[, required, drop = FALSE]
for (nm in names(d)) d[[nm]] <- trimws(d[[nm]])
# Log all omitted rows; avoid silently changing the sampling frame.
missing_rows <- !stats::complete.cases(d) | apply(d, 1L, function(x) any(x == "", na.rm = TRUE))
if (any(missing_rows)) {
  mf_write(cbind(original_row = which(missing_rows) + 1L,
                 d[missing_rows, , drop = FALSE]),
           "outputs/data_quality/training_excluded_rows.csv")
  warning(sum(missing_rows), " incomplete records excluded from training.")
}
d <- d[!missing_rows, , drop = FALSE]
d$Manual_Typology <- mf_typology_code(d$Manual_Typology, "Manual_Typology")
for (nm in MF_PREDICTORS) {
  d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  if (any(!is.finite(d[[nm]]))) stop("Non-finite/non-numeric predictor: ", nm)
}
if (!nrow(d)) stop("Training data are empty after cleaning.")
mf_write(d, out)
classes <- as.data.frame(table(Typology_Code = d$Manual_Typology),
                         stringsAsFactors = FALSE)
classes$Typology <- unname(MF_LABELS[classes$Typology_Code])
classes$Percent <- 100 * classes$Freq / nrow(d)
mf_write(classes, "outputs/data_quality/training_class_balance.csv")
mf_write(as.data.frame(table(City = d$City, Typology_Code = d$Manual_Typology),
                       stringsAsFactors = FALSE),
         "outputs/data_quality/training_city_class_balance.csv")
mf_write(data.frame(metric = c("input_rows", "prepared_rows", "excluded_rows",
                                "urban_areas", "typology_classes"),
                    value = c(raw_n, nrow(d), raw_n - nrow(d),
                              length(unique(d$City)), length(unique(d$Manual_Typology)))),
         "outputs/data_quality/training_audit.csv")
mf_session("outputs/data_quality/session_info_01.txt")
message("01 complete: ", nrow(d), " cleaned training records -> ", out)
