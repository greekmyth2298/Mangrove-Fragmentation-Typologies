# 07 | Information-theoretic path dependence along three-stage trajectories.
# Published estimates: Gil & Seto (2026), Table 5, Table 6, Figure 7.
# Recovered historical code: two-stage area + within-area trajectory bootstrap.
# Method text in the published paper describes whole-urban-area bootstrap.
# BOTH are reported, with explicit names. Historical two-stage is the default
# primary comparison for reproducing the published percentile intervals.
source("scripts/00_shared.R")
d <- mf_load_trajectories()
outdir <- mf_dir("outputs/path_dependence")
figdir <- mf_dir("figures/path_dependence")
write_out <- function(z, name) mf_write(z, file.path(outdir, name))

# Runtime settings. Publication's historical code used B=1000, seed=42.
seed <- 42L
B <- as.integer(Sys.getenv("MANGROVE_BOOTSTRAP_REPS", "1000"))
if (is.na(B) || B < 50L) stop("MANGROVE_BOOTSTRAP_REPS must be >= 50.")
run_comparator <- tolower(Sys.getenv("MANGROVE_COMPARE_WHOLE_AREA", "true")) == "true"

# A) Published pooled patch-weighted estimates.
pooled <- mf_joint_stats(d)
# Equal URBAN AREA weight means every MUA unit receives equal total weight;
# it does not refer to physical urban area (km^2) or original 1996 patch count.
d$w <- mf_equal_area_weights(d)
weighted <- mf_joint_stats(d, weighted = TRUE)
primary <- data.frame(Specification = c("pooled_pathway_weighted",
                                        "equal_urban_area_weighted"),
                      N_Pathways = nrow(d),
                      N_Urban_Areas = length(unique(d$urban_area_id)),
                      rbind(pooled, weighted), row.names = NULL)
write_out(primary, "table5_primary_information_theory.csv")

# B) Per-urban-area estimates. Descriptive; no city-specific p-values.
city_effects <- do.call(rbind, lapply(split(d, d$urban_area_id), function(x) {
  data.frame(City = x$urban_area_id[1], N_Pathways = nrow(x),
             t(mf_joint_stats(x)), P_Lost = mean(x$s3 == "Lost"))
}))
city_effects <- city_effects[order(-city_effects$Delta_H), , drop = FALSE]
rownames(city_effects) <- NULL
write_out(city_effects, "figure7_urban_area_effect_sizes.csv")

# C) Rerecovered two-stage bootstrap, and independently specified whole-area bootstrap.
# Resample m urban areas with replacement. The two-stage option ALSO samples
# n trajectories within each selected urban area, with replacement. This is
# documented in the recovered code but omitted from the paper's prose.
# For legacy equal-area-weighted bootstrap, duplicated urban areas are merged
# by their original identity when assigning weights. This precisely mirrors
# the original code's group_by(City) weighting; it is reported as a legacy
# estimator and must not be confused with weighting each DRAWN area block.
bootstrap_once <- function(areas, split_by_area, mode = c("two_stage", "whole_area"),
                           legacy_equal_prob = FALSE) {
  mode <- match.arg(mode)
  chosen <- if (legacy_equal_prob) {
    sample(areas, size = length(areas), replace = TRUE,
           prob = rep(1 / length(areas), length(areas)))
  } else {
    sample(areas, size = length(areas), replace = TRUE)
  }
  chunks <- lapply(seq_along(chosen), function(i) {
    group <- split_by_area[[chosen[i]]]
    if (mode == "two_stage") group <- group[sample.int(nrow(group), nrow(group),
                                                replace = TRUE), , drop = FALSE]
    group$bootstrap_draw_id <- i
    group
  })
  boot <- do.call(rbind, chunks)
  rownames(boot) <- NULL
  boot
}
bootstrap_replicates <- function(data, reps, mode = "two_stage", weighted = FALSE,
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  groups <- split(data, data$urban_area_id)
  areas <- sort(names(groups))
  if (length(areas) < 2L) stop("Bootstrap requires at least two urban areas.")
  results <- matrix(NA_real_, nrow = reps, ncol = 2,
                    dimnames = list(NULL, c("LR", "Delta_H")))
  for (b in seq_len(reps)) {
    boot <- bootstrap_once(areas, groups, mode,
                           legacy_equal_prob = weighted && mode == "two_stage")
    if (weighted) {
      # Exact legacy weighting merges duplicated CITY labels from resampling.
      # Strict whole-area resampling instead gives each drawn BLOCK equal weight.
      by_group <- if (mode == "two_stage") "urban_area_id" else "bootstrap_draw_id"
      boot$w <- mf_equal_area_weights(boot, by_group)
    }
    est <- mf_joint_stats(boot, weighted = weighted)
    results[b, ] <- c(est["LR"], est["Delta_H"])
    if (b %% 250L == 0L) message("07 bootstrap ", mode,
                                 if (weighted) " weighted" else " pooled",
                                 ": ", b, "/", reps)
  }
  as.data.frame(results)
}

summarize_bootstrap <- function(draws, mode, specification) {
  do.call(rbind, lapply(names(draws), function(nm) {
    x <- draws[[nm]]
    data.frame(Bootstrap = mode, Specification = specification,
               Metric = nm, Replicates = length(x),
               Mean = mean(x), SD = stats::sd(x),
               CI_2.5 = unname(stats::quantile(x, 0.025)),
               CI_97.5 = unname(stats::quantile(x, 0.975)))
  }))
}
# The original source ran two bootstrap loops in sequence, after one set.seed(42).
# Do not reset the random generator between them; this preserves R RNG order.
set.seed(seed)
legacy_pooled <- bootstrap_replicates(d, B, "two_stage", weighted = FALSE)
legacy_weighted <- bootstrap_replicates(d, B, "two_stage", weighted = TRUE)
write_out(legacy_pooled, "bootstrap_legacy_two_stage_pooled_draws.csv")
write_out(legacy_weighted, "bootstrap_legacy_two_stage_equal_area_draws.csv")
legacy_summary <- rbind(
  summarize_bootstrap(legacy_pooled, "two_stage_original_code", "pooled"),
  summarize_bootstrap(legacy_weighted, "two_stage_original_code", "equal_area"))
write_out(legacy_summary, "bootstrap_legacy_two_stage_summary.csv")
if (run_comparator) {
  # Independent random stream ensures the comparator does not perturb legacy runs.
  set.seed(seed)
  whole_pooled <- bootstrap_replicates(d, B, "whole_area", weighted = FALSE)
  whole_weighted <- bootstrap_replicates(d, B, "whole_area", weighted = TRUE)
  write_out(whole_pooled, "bootstrap_whole_urban_area_pooled_draws.csv")
  write_out(whole_weighted, "bootstrap_whole_urban_area_equal_area_draws.csv")
  whole_summary <- rbind(
    summarize_bootstrap(whole_pooled, "whole_area_as_paper_describes", "pooled"),
    summarize_bootstrap(whole_weighted, "whole_area_as_paper_describes", "equal_area"))
  write_out(whole_summary, "bootstrap_whole_urban_area_summary.csv")
  write_out(rbind(legacy_summary, whole_summary), "bootstrap_methods_comparison.csv")
}

# D) Three prespecified robustness samples, all pooled (as in Table 6).
no_early_lost <- d[d$s1 != "Lost" & d$s2 != "Lost", , drop = FALSE]
# A 'history' is the pair (s1,s2), with frequency counted WITHIN each city.
key <- paste(d$urban_area_id, d$s1, d$s2, sep = " || ")
count_by_history <- table(key)
keep_rare <- as.numeric(count_by_history[key]) >= 5L
trimmed <- d[keep_rare, , drop = FALSE]
no_boundary <- d[!d$boundary_patch, , drop = FALSE]
all_samples <- list(primary = d, exclude_early_lost = no_early_lost,
                    trim_histories_within_city_n_ge_5 = trimmed,
                    exclude_boundary_patches = no_boundary)
robustness <- do.call(rbind, lapply(names(all_samples), function(nm) {
  x <- all_samples[[nm]]
  data.frame(Specification = nm, N_Urban_Areas = length(unique(x$urban_area_id)),
             N_Pathways = nrow(x), t(mf_joint_stats(x)))
}))
rownames(robustness) <- NULL
write_out(robustness, "table6_robustness_checks.csv")

# E) Reference comparisons use printed precision; no results are overwritten.
checks <- do.call(rbind, list(
  mf_report_check(pooled["LL_recent"], -4010.9, "Published LL baseline", 0.1),
  mf_report_check(pooled["LL_history"], -3760.9, "Published LL history", 0.1),
  mf_report_check(pooled["LR"], 500.08, "Published LR", 0.01),
  mf_report_check(pooled["H_recent"], 1.816, "Published H given recent", 0.0006),
  mf_report_check(pooled["H_history"], 1.703, "Published H given full history", 0.0006),
  mf_report_check(pooled["Delta_H"], 0.113, "Published pooled Delta H", 0.0006),
  mf_report_check(weighted["Delta_H"], 0.130, "Published equal-area Delta H", 0.0006),
  mf_report_check(robustness$Delta_H[2], 0.153, "Published early Lost sensitivity", 0.0006),
  mf_report_check(robustness$Delta_H[3], 0.126, "Published rare history sensitivity", 0.0006),
  mf_report_check(robustness$Delta_H[4], 0.119, "Published boundary sensitivity", 0.0006)
))
write_out(checks, "publication_tables5_6_reproduction_checks.csv")
if (any(!checks$pass)) warning("At least one manuscript point estimate did not match within tolerance.")
# Additional n benchmarks reported in Table 6.
check_sizes <- data.frame(Specification = robustness$Specification,
                          Published_n = c(3186L, 2359L, 2882L, 2965L),
                          Observed_n = robustness$N_Pathways)
check_sizes$Matches <- check_sizes$Published_n == check_sizes$Observed_n
write_out(check_sizes, "publication_table6_sample_size_checks.csv")

# F) Figure 7: three displayed city-level metrics (no synthetic city-level CIs).
grDevices::png(file.path(figdir, "figure7_city_dependence_panels.png"),
              width = 1550, height = 1050, res = 145)
par(mfrow = c(1, 3), mar = c(6, 10, 4, 1), oma = c(0, 0, 1, 0))
vars <- c("Delta_H", "LR", "P_Lost")
labels <- c("Entropy reduction (bits)", "Likelihood-ratio statistic", "Final-interval P(Lost)")
for (j in seq_along(vars)) {
  y <- rev(seq_len(nrow(city_effects)))
  x <- city_effects[[vars[j]]]
  plot(x, y, xlim = c(0, max(x) * 1.10 + 1e-8), ylim = c(0.4, length(y) + 0.6),
       yaxt = "n", ylab = "", xlab = labels[j], pch = 19,
       col = "#485F6B", main = c("A. Historical memory", "B. Model evidence",
                                 "C. Patch loss")[j])
  axis(2, at = y, labels = city_effects$City, las = 2, cex.axis = 0.68)
  segments(0, y, x, y, col = "gray65")
  points(x, y, pch = 19, col = "#485F6B")
}
grDevices::dev.off()

# G) Analysis note and provenance of estimator choices.
notes <- c(
  "Path dependence uses empirical transition probabilities among seven states.",
  "Closed cohort: complete 1996-baseline-traceable descendant pathways; repeated rows retained.",
  "Pooled estimand: each descendant pathway is weighted equally.",
  "Weighted estimand: each morphological urban-area unit has equal total weight.",
  "Historical original script specified seed 42 and B=1000.",
  "Historical original bootstrap draws cities, then resamples pathways within each city.",
  "Historical weighted bootstrap collapses repeated sampled cities to one weight unit.",
  "Published Methods say to resample whole areas and retain their pathways.",
  "Both bootstrap methods are implemented and labeled separately.",
  "City-specific results are descriptive; p values are not calculated for cities.",
  "LR based on empirical log-likelihood is a model-fit statistic, not a causal test.",
  "The 1996-2007, 2007-2015, and 2015-2020 periods differ in duration.",
  "No inference claims independent sibling descendants or causal mechanisms."
)
writeLines(notes, file.path(outdir, "analysis_methodology_notes.txt"))
mf_session(file.path(outdir, "session_info.txt"))
message("07 complete: pooled Delta H = ", round(pooled["Delta_H"], 5),
        "; equal-city Delta H = ", round(weighted["Delta_H"], 5),
        "; manuscript point checks pass = ", all(checks$pass))
