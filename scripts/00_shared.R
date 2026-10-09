# Shared helpers for Gil & Seto (2026), Landscape Ecology 41:188.
# Source from the PROJECT ROOT. All files and paths are project-relative.
# R >= 4.2; base R only, except the RF stage requires randomForest.

MF_STATES <- c("Nibbling", "Clearing", "Shattering", "Displacing",
               "Expanding", "Stabilizing", "Lost")
MF_LABELS <- setNames(MF_STATES[1:5], as.character(0:4))
MF_PREDICTORS <- c("log_area_parent", "log_area_child", "log_change_area",
                   "log_change_ED", "log_change_shape_index", "log_change_ENN",
                   "logit_retention_ratio", "log_centroid_shift")
MF_COLORS <- c(Nibbling = "#237CB8", Clearing = "#D97A29",
               Shattering = "#C8A428", Displacing = "#20816F",
               Expanding = "#BC6FA5", Stabilizing = "#8768AC", Lost = "#909090")

mf_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}
mf_need_file <- function(path, explanation = NULL) {
  if (!file.exists(path)) stop("Required file is missing: ", path,
                               if (!is.null(explanation)) paste0("\n", explanation) else "",
                               call. = FALSE)
  invisible(path)
}
mf_check_cols <- function(x, cols, name = "input") {
  absent <- setdiff(cols, names(x))
  if (length(absent)) stop(name, " lacks required columns: ",
                           paste(absent, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
mf_read <- function(path) {
  mf_need_file(path)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), colClasses = "character")
}
mf_write <- function(x, path) {
  mf_dir(dirname(path))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}
mf_session <- function(path) {
  mf_dir(dirname(path))
  writeLines(c(paste("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
               capture.output(sessionInfo())), con = path)
}
mf_typology_code <- function(x, field = "typology") {
  x <- trimws(as.character(x))
  # Accept either original numeric codes or their plain-language names.
  replacement <- match(tolower(x), tolower(MF_LABELS)) - 1L
  use_label <- !is.na(replacement)
  x[use_label] <- as.character(replacement[use_label])
  if (anyNA(x) || any(!x %in% names(MF_LABELS))) {
    stop("Invalid/missing class code in ", field, ". Allowed codes: 0..4.", call. = FALSE)
  }
  x
}
mf_canonical_state <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) == "stabilising"] <- "Stabilizing"
  idx <- match(tolower(x), tolower(MF_STATES))
  bad <- !is.na(x) & is.na(idx)
  if (any(bad)) stop("Unrecognized state(s): ",
                     paste(unique(x[bad]), collapse = ", "), call. = FALSE)
  ifelse(is.na(x), NA_character_, MF_STATES[idx])
}
mf_binary_boundary <- function(x) {
  x <- tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  yes <- c("yes", "y", "true", "t", "1", "boundary")
  no <- c("", "no", "n", "false", "f", "0", "nonboundary", "non-boundary")
  if (any(!x %in% c(yes, no))) stop("Unexpected boundary flag(s): ",
                                   paste(unique(x[!x %in% c(yes, no)]), collapse = ", "))
  x %in% yes  # Empty fields mean 'not flagged', per the supplied GIS export.
}
mf_accuracy_stats <- function(observed, predicted) {
  observed <- factor(observed, levels = names(MF_LABELS))
  predicted <- factor(predicted, levels = names(MF_LABELS))
  tab <- table(Prediction = predicted, Reference = observed)
  n <- sum(tab)
  if (!n) stop("Cannot evaluate an empty validation sample.")
  acc <- sum(diag(tab)) / n
  expected <- sum(rowSums(tab) * colSums(tab)) / (n * n)
  kappa <- if (expected >= 1) NA_real_ else (acc - expected) / (1 - expected)
  nir <- max(colSums(tab)) / n
  exact <- stats::binom.test(sum(diag(tab)), n, p = nir, alternative = "greater")
  # caret confusionMatrix uses an unadjusted two-sided exact CI for accuracy.
  ci <- stats::binom.test(sum(diag(tab)), n)$conf.int
  main <- data.frame(N = n, Accuracy = acc, Kappa = kappa,
                     AccuracyLower = ci[1], AccuracyUpper = ci[2],
                     NoInformationRate = nir,
                     PValue_AccuracyGreaterThanNIR = exact$p.value)
  by_class <- do.call(rbind, lapply(seq_along(MF_LABELS), function(i) {
    tp <- tab[i, i]; fn <- sum(tab[, i]) - tp
    fp <- sum(tab[i, ]) - tp; tn <- n - tp - fn - fp
    sens <- if (tp + fn) tp / (tp + fn) else NA_real_
    spec <- if (tn + fp) tn / (tn + fp) else NA_real_
    prec <- if (tp + fp) tp / (tp + fp) else NA_real_
    f1 <- if (is.na(prec) || is.na(sens) || prec + sens == 0) NA_real_ else
      2 * prec * sens / (prec + sens)
    data.frame(Code = names(MF_LABELS)[i], Typology = unname(MF_LABELS[i]),
               Support = sum(tab[, i]), Precision = prec, Recall = sens,
               Specificity = spec, F1 = f1)
  }))
  list(metrics = main, by_class = by_class, confusion = tab)
}
mf_confusion_long <- function(mat, group = NULL) {
  z <- as.data.frame(mat, responseName = "n", stringsAsFactors = FALSE)
  z$Prediction_Label <- unname(MF_LABELS[as.character(z$Prediction)])
  z$Reference_Label <- unname(MF_LABELS[as.character(z$Reference)])
  if (!is.null(group)) z$City <- group
  z
}

# Input for Scripts 06-07 is ALWAYS output by Script 05.
mf_load_trajectories <- function(path = "outputs/trajectories/FRAGMENTATION_TRAJECTORIES.csv") {
  d <- mf_read(path)
  needed <- c("country", "city", "original_patch_id_1996", "pathway_row_id",
              "s1", "s2", "s3", "boundary_patch", "urban_area_id", "closed_cohort")
  mf_check_cols(d, needed, basename(path))
  for (nm in c("s1", "s2", "s3")) d[[nm]] <- mf_canonical_state(d[[nm]])
  if (anyNA(d[c("country", "city", "original_patch_id_1996", "s1", "s2", "s3")]))
    stop("Incomplete trajectories encountered after Script 05.")
  if (any((d$s1 == "Lost" & d$s2 != "Lost") |
          (d$s2 == "Lost" & d$s3 != "Lost")))
    stop("An absorbing Lost state transitions to a different state.")
  if (anyDuplicated(d$pathway_row_id)) stop("Synthetic pathway_row_id is not unique.")
  if (!all(tolower(d$closed_cohort) %in% c("true", "t", "1")))
    stop("Script 05 output contains non-closed trajectories.")
  d$boundary_patch <- mf_binary_boundary(d$boundary_patch)
  d$lost_final <- d$s3 == "Lost"
  d
}

# Weighted conditional entropy / empirical multinomial log-likelihood.
# These are finite-sample empirical quantities, not out-of-sample predictive scores.
mf_joint_stats <- function(d, weighted = FALSE) {
  if (!nrow(d)) return(c(H_recent = NA_real_, H_history = NA_real_,
                         Delta_H = NA_real_, LL_recent = NA_real_,
                         LL_history = NA_real_, LR = NA_real_))
  w <- if (weighted) d$w else rep(1, nrow(d))
  if (length(w) != nrow(d) || anyNA(w) || any(w <= 0) || any(!is.finite(w)))
    stop("Invalid analysis weights.")
  recent <- factor(d$s2, levels = MF_STATES)
  hist <- interaction(factor(d$s1, levels = MF_STATES),
                      factor(d$s2, levels = MF_STATES), drop = TRUE)
  y <- factor(d$s3, levels = MF_STATES)
  t_recent <- stats::xtabs(w ~ recent + y)
  t_history <- stats::xtabs(w ~ hist + y)
  entropy_from_counts <- function(mat) {
    denom <- rowSums(mat)
    keep <- which(denom > 0)
    total <- sum(mat)
    H <- 0
    for (i in keep) {
      p <- mat[i, ] / denom[i]
      p <- p[p > 0]
      H <- H - (denom[i] / total) * sum(p * log2(p))
    }
    H
  }
  h0 <- entropy_from_counts(t_recent)
  h1 <- entropy_from_counts(t_history)
  # Empirical LL in nats; valid for integer counts or weighted pseudo counts.
  ll0 <- -sum(w) * log(2) * h0
  ll1 <- -sum(w) * log(2) * h1
  # Enforce a stable six-field API. R can propagate names from scalar
  # intermediates through c(), making an exact lookup such as
  # stats["LL_recent"] return NA even though the estimate is finite.
  values <- unname(as.numeric(c(h0, h1, h0 - h1,
                                ll0, ll1, 2 * (ll1 - ll0))))
  fields <- c("H_recent", "H_history", "Delta_H",
              "LL_recent", "LL_history", "LR")
  if (length(values) != length(fields) || any(!is.finite(values))) {
    stop("Information-theory point estimates are missing/non-finite; ",
         "inspect trajectories and weights.", call. = FALSE)
  }
  stats::setNames(values, fields)
}
mf_equal_area_weights <- function(d, group_col = "urban_area_id") {
  counts <- table(d[[group_col]])
  as.numeric(1 / counts[d[[group_col]]])
}
# Compare an observed scalar to a manuscript reference value.
# Named vectors (including named NA results from a misspelled lookup) can
# cause data.frame() to infer invalid row names. Remove names explicitly and
# fail with a meaningful message if the estimate is missing or non-finite.
mf_report_check <- function(actual, expected, label, tol = 0.0005) {
  if (length(label) != 1L || is.na(label) || !nzchar(as.character(label))) {
    stop("Publication check requires one nonempty metric label.", call. = FALSE)
  }
  if (length(actual) != 1L || !is.numeric(actual)) {
    stop("Publication check '", label, "' expected one numeric observed value.",
         call. = FALSE)
  }
  if (length(expected) != 1L || !is.numeric(expected) ||
      length(tol) != 1L || !is.numeric(tol)) {
    stop("Publication check '", label, "' has invalid reference/tolerance.",
         call. = FALSE)
  }
  observed_value <- unname(as.numeric(actual))
  expected_value <- unname(as.numeric(expected))
  tolerance <- unname(as.numeric(tol))
  if (!is.finite(observed_value)) {
    stop("Publication check '", label, "' received a missing/non-finite estimate. ",
         "Check the metric name and the stage-07 point-estimate output.",
         call. = FALSE)
  }
  if (!is.finite(expected_value) || !is.finite(tolerance) || tolerance < 0) {
    stop("Publication check '", label, "' has a non-finite reference or invalid tolerance.",
         call. = FALSE)
  }
  difference <- abs(observed_value - expected_value)
  data.frame(metric = as.character(label), expected = expected_value,
             observed = observed_value, absolute_error = difference,
             pass = difference <= tolerance, row.names = NULL,
             stringsAsFactors = FALSE)
}

# Normalize documented variants in city/urban-area spelling for auditing only.
# Never modify raw spatial identifiers or merge areas based on fuzzy matching.
mf_canonical_city <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("vung tau", "vungtau")] <- "Vungtau"
  x[tolower(x) %in% c("chonburi", "chon buri")] <- "Chon Buri"
  x[tolower(x) %in% c("bangkok", "krungthep", "krung thep")] <- "Bangkok"
  x
}
