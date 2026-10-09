# ============================================================
# Path Dependence (Trajectory Dependence) — ALL URBAN AREAS
# Intervals: 1996–2007, 2007–2015, 2015–2020
# Steps A–F with URBAN-AREA–BLOCKED BOOTSTRAP
#
# ADDITION (REVISION):
# - Adds an URBAN-AREA–WEIGHTED summary (and bootstrap CI) in addition
#   to the pooled (patch-weighted) summary already in the script.
# - If an urban-area column cannot be detected, falls back to equal
#   city weights (still produces the “weighted” outputs, labeled clearly).
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
})

# ---------------------------
# 1) User settings
# ---------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/##MFS RESEARCH/TYPOLOGY/NEWER PATH DEPENDENCE WITH URBAN AREA WEIGHTED SUMMARY")
  
data_path <- "ALL CITIES COMPILED.csv"   # <-- update if needed

out_dir   <- "path_dependence_output_all_cities"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(42)

B <- 1000
min_hist_count <- 5
top_k_paths <- 25

# ---------------------------
# 2) Helper functions
# ---------------------------

find_col <- function(df, patterns) {
  nms <- names(df)
  for (pat in patterns) {
    idx <- which(str_detect(tolower(nms), tolower(pat)))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NA_character_)
}

entropy <- function(p, base = exp(1)) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  -sum(p * log(p, base = base))
}

cond_entropy <- function(df, y, x, base = 2) {
  tab_xy <- table(df[[x]], df[[y]])
  px <- rowSums(tab_xy) / sum(tab_xy)
  hy_given_x <- 0
  for (i in seq_len(nrow(tab_xy))) {
    row <- tab_xy[i, ]
    py_given_x <- row / sum(row)
    hy_given_x <- hy_given_x + px[i] * entropy(py_given_x, base = base)
  }
  hy_given_x
}

cond_entropy2 <- function(df, y, x, z, base = 2) {
  joint <- paste(df[[x]], df[[z]], sep = " | ")
  tmp <- df %>% mutate(.joint = joint)
  cond_entropy(tmp, y = y, x = ".joint", base = base)
}

loglik_empirical <- function(df, y, x) {
  tab_xy <- table(df[[x]], df[[y]])
  ll <- 0
  for (i in seq_len(nrow(tab_xy))) {
    row <- tab_xy[i, ]
    n_x <- sum(row)
    if (n_x == 0) next
    p_y_given_x <- row / n_x
    ll <- ll + sum(row[row > 0] * log(p_y_given_x[p_y_given_x > 0]))
  }
  ll
}

lr_stat_empirical <- function(df, y, x, z) {
  ll0 <- loglik_empirical(df, y = y, x = x)
  joint <- paste(df[[x]], df[[z]], sep = " | ")
  tmp <- df %>% mutate(.joint = joint)
  ll1 <- loglik_empirical(tmp, y = y, x = ".joint")
  LR <- 2 * (ll1 - ll0)
  list(LR = LR, ll0 = ll0, ll1 = ll1)
}

# ---- NEW: Weighted versions (for urban-area-weighted summary) ----
weighted_table_2d <- function(x, y, w) {
  # returns matrix with rows = x levels, cols = y levels
  x <- as.character(x); y <- as.character(y)
  df <- tibble(x = x, y = y, w = w) %>% filter(!is.na(x), !is.na(y), !is.na(w))
  if (nrow(df) == 0) return(matrix(0, 0, 0))
  xtab <- df %>% group_by(x, y) %>% summarise(w = sum(w), .groups = "drop")
  xs <- sort(unique(xtab$x))
  ys <- sort(unique(xtab$y))
  mat <- matrix(0, nrow = length(xs), ncol = length(ys), dimnames = list(xs, ys))
  for (i in seq_len(nrow(xtab))) {
    mat[xtab$x[i], xtab$y[i]] <- xtab$w[i]
  }
  mat
}

cond_entropy_w <- function(df, y, x, w = "w", base = 2) {
  tab_xy <- weighted_table_2d(df[[x]], df[[y]], df[[w]])
  if (length(tab_xy) == 0) return(NA_real_)
  px <- rowSums(tab_xy) / sum(tab_xy)
  hy_given_x <- 0
  for (i in seq_len(nrow(tab_xy))) {
    row <- tab_xy[i, ]
    if (sum(row) <= 0) next
    py_given_x <- row / sum(row)
    hy_given_x <- hy_given_x + px[i] * entropy(py_given_x, base = base)
  }
  hy_given_x
}

cond_entropy2_w <- function(df, y, x, z, w = "w", base = 2) {
  tmp <- df %>% mutate(.joint = paste(.data[[x]], .data[[z]], sep = " | "))
  cond_entropy_w(tmp, y = y, x = ".joint", w = w, base = base)
}

loglik_empirical_w <- function(df, y, x, w = "w") {
  tab_xy <- weighted_table_2d(df[[x]], df[[y]], df[[w]])
  if (length(tab_xy) == 0) return(NA_real_)
  ll <- 0
  for (i in seq_len(nrow(tab_xy))) {
    row <- tab_xy[i, ]
    n_x <- sum(row)
    if (n_x <= 0) next
    p_y_given_x <- row / n_x
    ll <- ll + sum(row[row > 0] * log(p_y_given_x[p_y_given_x > 0]))
  }
  ll
}

lr_stat_empirical_w <- function(df, y, x, z, w = "w") {
  ll0 <- loglik_empirical_w(df, y = y, x = x, w = w)
  tmp <- df %>% mutate(.joint = paste(.data[[x]], .data[[z]], sep = " | "))
  ll1 <- loglik_empirical_w(tmp, y = y, x = ".joint", w = w)
  LR <- 2 * (ll1 - ll0)
  list(LR = LR, ll0 = ll0, ll1 = ll1)
}

# ---- Existing: City-blocked bootstrap (pooled) ----
bootstrap_city_blocked <- function(df, B = 1000, base = 2) {
  stopifnot(all(c("City","S_96_07","S_07_15","S_15_20") %in% names(df)))
  cities <- sort(unique(df$City))
  G <- length(cities)
  if (G < 2) stop("Need at least 2 cities for city-blocked bootstrap.")
  city_split <- split(df, df$City)
  
  draws <- replicate(B, {
    sampled_cities <- sample(cities, size = G, replace = TRUE)
    
    resampled <- map_dfr(sampled_cities, function(ct) {
      dct <- city_split[[ct]]
      nct <- nrow(dct)
      dct[sample.int(nct, size = nct, replace = TRUE), , drop = FALSE]
    })
    
    lr <- lr_stat_empirical(resampled, y = "S_15_20", x = "S_07_15", z = "S_96_07")$LR
    h1 <- cond_entropy(resampled,  y = "S_15_20", x = "S_07_15", base = base)
    h2 <- cond_entropy2(resampled, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = base)
    dH <- h1 - h2
    
    c(LR = lr, dH = dH)
  })
  
  as.data.frame(t(draws))
}

# ---- NEW: City-blocked bootstrap for URBAN-AREA–WEIGHTED summary ----
# Within each bootstrap draw:
# 1) resample cities with replacement (prob = urban area weight, or equal-weight fallback)
# 2) resample patches within each selected city (same n as that city)
# 3) assign per-patch weights so each city's total weight equals its (boot) urban-area weight
bootstrap_city_blocked_urban_area_weighted <- function(df, city_area_tbl, B = 1000, base = 2) {
  stopifnot(all(c("City","S_96_07","S_07_15","S_15_20") %in% names(df)))
  stopifnot(all(c("City","urban_area_weight") %in% names(city_area_tbl)))
  
  cities <- sort(unique(df$City))
  G <- length(cities)
  if (G < 2) stop("Need at least 2 cities for city-blocked bootstrap.")
  
  city_split <- split(df, df$City)
  
  w_tbl <- city_area_tbl %>%
    filter(City %in% cities) %>%
    distinct(City, urban_area_weight)
  
  # safety: any missing -> equal weights for those
  w_tbl <- w_tbl %>%
    mutate(urban_area_weight = ifelse(is.na(urban_area_weight) | urban_area_weight <= 0, 1, urban_area_weight))
  
  prob <- w_tbl$urban_area_weight
  prob <- prob / sum(prob)
  
  names(prob) <- w_tbl$City
  
  draws <- replicate(B, {
    sampled_cities <- sample(w_tbl$City, size = G, replace = TRUE, prob = prob)
    
    resampled <- map_dfr(sampled_cities, function(ct) {
      dct <- city_split[[ct]]
      nct <- nrow(dct)
      dct[sample.int(nct, size = nct, replace = TRUE), , drop = FALSE]
    })
    
    # recompute per-city patch counts within the resample and assign weights
    resampled_w <- resampled %>%
      group_by(City) %>%
      mutate(
        .n_city = n(),
        # each city's total weight equals its urban_area_weight
        w = (w_tbl$urban_area_weight[match(City[1], w_tbl$City)] / .n_city)
      ) %>%
      ungroup() %>%
      select(-.n_city)
    
    lr <- lr_stat_empirical_w(resampled_w, y = "S_15_20", x = "S_07_15", z = "S_96_07", w = "w")$LR
    h1 <- cond_entropy_w(resampled_w,  y = "S_15_20", x = "S_07_15", w = "w", base = base)
    h2 <- cond_entropy2_w(resampled_w, y = "S_15_20", x = "S_07_15", z = "S_96_07", w = "w", base = base)
    dH <- h1 - h2
    
    c(LR = lr, dH = dH)
  })
  
  as.data.frame(t(draws))
}

ci_tbl <- function(x, probs = c(0.025, 0.975)) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    p025 = quantile(x, probs[1], na.rm = TRUE),
    p975 = quantile(x, probs[2], na.rm = TRUE)
  )
}

# ---------------------------
# A) Prepare the trajectory dataset
# ---------------------------

raw <- read_csv(data_path, show_col_types = FALSE)

col_96_07 <- find_col(raw, c("1996.*2007", "96.*07", "S_1996_2007", "S_96_07", "1996_2007"))
col_07_15 <- find_col(raw, c("2007.*2015", "07.*15", "S_2007_2015", "S_07_15", "2007_2015"))
col_15_20 <- find_col(raw, c("2015.*2020", "15.*20", "S_2015_2020", "S_15_20", "2015_2020"))

col_city    <- find_col(raw, c("^city$", "city_name", "urban", "mua", "morphological"))
col_country <- find_col(raw, c("^country$", "nation"))
col_patch   <- find_col(raw, c("patch", "patch_id", "^id$", "objectid", "fid"))

col_boundary <- find_col(raw, c("^boundary$", "boundary_intersect", "intersect.*boundary", "cut.*boundary"))

# ---- NEW: detect an urban-area column (km2/ha/etc). You can expand patterns as needed.
col_urban_area <- find_col(raw, c(
  "^urban_area$", "urban.*area", "mua.*area", "city.*area",
  "area_?km2", "km2", "sqkm", "sq_?km", "hectare", "ha"
))

if (any(is.na(c(col_96_07, col_07_15, col_15_20, col_city)))) {
  stop(
    "Could not reliably detect required columns.\n",
    "Detected:\n",
    "  City:       ", col_city, "\n",
    "  1996–2007:  ", col_96_07, "\n",
    "  2007–2015:  ", col_07_15, "\n",
    "  2015–2020:  ", col_15_20, "\n",
    "Please rename columns or extend patterns in find_col()."
  )
}

dat <- raw %>%
  transmute(
    Patch_ID = if (!is.na(col_patch)) as.character(.data[[col_patch]]) else as.character(row_number()),
    City     = as.character(.data[[col_city]]),
    Country  = if (!is.na(col_country)) as.character(.data[[col_country]]) else NA_character_,
    Boundary = if (!is.na(col_boundary)) as.character(.data[[col_boundary]]) else NA_character_,
    Urban_Area = if (!is.na(col_urban_area)) suppressWarnings(as.numeric(.data[[col_urban_area]])) else NA_real_,
    S_96_07  = as.character(.data[[col_96_07]]),
    S_07_15  = as.character(.data[[col_07_15]]),
    S_15_20  = as.character(.data[[col_15_20]])
  ) %>%
  mutate(
    across(c(City, Country, Boundary), ~ str_squish(.x)),
    across(starts_with("S_"), ~ str_squish(.x)),
    across(starts_with("S_"), ~ ifelse(.x == "", NA_character_, .x)),
    across(starts_with("S_"), ~ ifelse(str_to_lower(.x) == "lost", "Lost", .x)),
    Boundary = ifelse(is.na(Boundary), NA_character_, str_to_upper(Boundary))
  )

trans <- dat %>%
  filter(!is.na(City), !is.na(S_96_07), !is.na(S_07_15), !is.na(S_15_20))

# Sanity: how many cities?
city_counts <- trans %>% count(City, Country, name = "n_patches") %>% arrange(desc(n_patches))
write_csv(dat,         file.path(out_dir, "A_cleaned_all_data.csv"))
write_csv(trans,       file.path(out_dir, "A_complete_trajectories.csv"))
write_csv(city_counts, file.path(out_dir, "A_city_patch_counts.csv"))

write_csv(trans %>% count(S_96_07, sort = TRUE), file.path(out_dir, "A_freq_S_96_07_overall.csv"))
write_csv(trans %>% count(S_07_15, sort = TRUE), file.path(out_dir, "A_freq_S_07_15_overall.csv"))
write_csv(trans %>% count(S_15_20, sort = TRUE), file.path(out_dir, "A_freq_S_15_20_overall.csv"))

if (!all(is.na(trans$Boundary))) {
  boundary_summary <- trans %>%
    mutate(boundary_yes = (!is.na(Boundary) & Boundary == "YES")) %>%
    summarise(
      n_patches = n(),
      n_boundary_yes = sum(boundary_yes),
      share_boundary_yes = mean(boundary_yes)
    )
  write_csv(boundary_summary, file.path(out_dir, "A_boundary_intersection_summary.csv"))
}

# ---------------------------
# B) Primary path-dependence test (POOLED) + city-blocked bootstrap
# ---------------------------

lr_out <- lr_stat_empirical(trans, y = "S_15_20", x = "S_07_15", z = "S_96_07")
H1 <- cond_entropy(trans,  y = "S_15_20", x = "S_07_15", base = 2)
H2 <- cond_entropy2(trans, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)
dH <- H1 - H2

primary_results <- tibble(
  scope = "All cities pooled (patch-weighted; inference is city-blocked)",
  n_cities = n_distinct(trans$City),
  n_patches = nrow(trans),
  ll_baseline = lr_out$ll0,
  ll_history  = lr_out$ll1,
  LR_stat = lr_out$LR,
  H_Y_given_prev_bits = H1,
  H_Y_given_prev_prev2_bits = H2,
  Delta_H_bits = dH
)

write_csv(primary_results, file.path(out_dir, "B_primary_results_all_cities.csv"))

boot <- bootstrap_city_blocked(trans, B = B, base = 2)
boot_summary <- bind_cols(
  tibble(metric = "LR_stat"),
  ci_tbl(boot$LR)
) %>%
  bind_rows(bind_cols(
    tibble(metric = "Delta_H_bits"),
    ci_tbl(boot$dH)
  ))

write_csv(boot,         file.path(out_dir, "B_bootstrap_city_blocked_draws.csv"))
write_csv(boot_summary, file.path(out_dir, "B_bootstrap_city_blocked_summary.csv"))

p_lr <- ggplot(boot, aes(LR)) + geom_histogram(bins = 40) +
  labs(title = "City-blocked bootstrap: LR statistic (pooled)", x = "LR", y = "Count")
ggsave(file.path(out_dir, "B_bootstrap_LR_hist.png"), p_lr, width = 7, height = 4, dpi = 200)

p_dh <- ggplot(boot, aes(dH)) + geom_histogram(bins = 40) +
  labs(title = "City-blocked bootstrap: ΔH (bits) (pooled)", x = "ΔH (bits)", y = "Count")
ggsave(file.path(out_dir, "B_bootstrap_dH_hist.png"), p_dh, width = 7, height = 4, dpi = 200)

# ---------------------------
# B2) NEW: Urban-area–weighted path-dependence summary + bootstrap
# ---------------------------

# Build a city-level weight table:
city_area_tbl <- trans %>%
  group_by(City, Country) %>%
  summarise(
    n_patches = n(),
    # If Urban_Area is repeated per patch, take the first non-NA unique value
    urban_area_raw = {
      ua <- Urban_Area
      ua <- ua[!is.na(ua)]
      if (length(ua) == 0) NA_real_ else ua[1]
    },
    .groups = "drop"
  )

# If no urban area column detected or it is missing for all cities, fall back to equal weights:
if (all(is.na(city_area_tbl$urban_area_raw))) {
  warning("No usable Urban_Area column detected. Falling back to equal city weights for the 'urban-area-weighted' summary.")
  city_area_tbl <- city_area_tbl %>%
    mutate(
      urban_area_weight = 1,
      weight_note = "FALLBACK: equal city weights (Urban_Area missing)"
    )
} else {
  # Replace missing/zero with median of observed (to avoid dropping cities silently)
  med_ua <- median(city_area_tbl$urban_area_raw, na.rm = TRUE)
  city_area_tbl <- city_area_tbl %>%
    mutate(
      urban_area_weight = ifelse(is.na(urban_area_raw) | urban_area_raw <= 0, med_ua, urban_area_raw),
      weight_note = ifelse(is.na(urban_area_raw) | urban_area_raw <= 0,
                           "Urban_Area missing/<=0 replaced with median",
                           "Urban_Area used as provided")
    )
}

write_csv(city_area_tbl, file.path(out_dir, "B2_city_urban_area_weights.csv"))

# Assign per-patch weights so that each city’s TOTAL patch weight equals its urban_area_weight
trans_w <- trans %>%
  left_join(city_area_tbl %>% select(City, urban_area_weight, n_patches), by = "City") %>%
  group_by(City) %>%
  mutate(w = urban_area_weight[1] / n()) %>%
  ungroup()

lr_out_w <- lr_stat_empirical_w(trans_w, y = "S_15_20", x = "S_07_15", z = "S_96_07", w = "w")
H1_w <- cond_entropy_w(trans_w,  y = "S_15_20", x = "S_07_15", w = "w", base = 2)
H2_w <- cond_entropy2_w(trans_w, y = "S_15_20", x = "S_07_15", z = "S_96_07", w = "w", base = 2)
dH_w <- H1_w - H2_w

primary_results_urban_area_weighted <- tibble(
  scope = "All cities (urban-area–weighted; inference is city-blocked with weighted city resampling)",
  n_cities = n_distinct(trans_w$City),
  n_patches = nrow(trans_w),
  # for transparency:
  total_weight = sum(trans_w$w, na.rm = TRUE),
  ll_baseline = lr_out_w$ll0,
  ll_history  = lr_out_w$ll1,
  LR_stat = lr_out_w$LR,
  H_Y_given_prev_bits = H1_w,
  H_Y_given_prev_prev2_bits = H2_w,
  Delta_H_bits = dH_w,
  weight_fallback = ifelse(any(str_detect(city_area_tbl$weight_note, "FALLBACK")), TRUE, FALSE)
)

write_csv(primary_results_urban_area_weighted, file.path(out_dir, "B2_primary_results_urban_area_weighted.csv"))

boot_w <- bootstrap_city_blocked_urban_area_weighted(trans, city_area_tbl, B = B, base = 2)
boot_summary_w <- bind_cols(
  tibble(metric = "LR_stat"),
  ci_tbl(boot_w$LR)
) %>%
  bind_rows(bind_cols(
    tibble(metric = "Delta_H_bits"),
    ci_tbl(boot_w$dH)
  ))

write_csv(boot_w,         file.path(out_dir, "B2_bootstrap_city_blocked_draws_urban_area_weighted.csv"))
write_csv(boot_summary_w, file.path(out_dir, "B2_bootstrap_city_blocked_summary_urban_area_weighted.csv"))

p_lr_w <- ggplot(boot_w, aes(LR)) + geom_histogram(bins = 40) +
  labs(title = "City-blocked bootstrap: LR statistic (urban-area–weighted)", x = "LR", y = "Count")
ggsave(file.path(out_dir, "B2_bootstrap_LR_hist_urban_area_weighted.png"), p_lr_w, width = 7, height = 4, dpi = 200)

p_dh_w <- ggplot(boot_w, aes(dH)) + geom_histogram(bins = 40) +
  labs(title = "City-blocked bootstrap: ΔH (bits) (urban-area–weighted)", x = "ΔH (bits)", y = "Count")
ggsave(file.path(out_dir, "B2_bootstrap_dH_hist_urban_area_weighted.png"), p_dh_w, width = 7, height = 4, dpi = 200)

# ---------------------------
# C) Supporting analysis 1: path enrichment toward “Lost”
# ---------------------------

paths <- trans %>%
  mutate(
    Path3 = paste(S_96_07, "→", S_07_15, "→", S_15_20),
    Lost_final = (S_15_20 == "Lost")
  )

path_stats_overall <- paths %>%
  group_by(Path3) %>%
  summarise(
    n = n(),
    n_lost = sum(Lost_final),
    p_lost = n_lost / n,
    .groups = "drop"
  ) %>%
  arrange(desc(n))

write_csv(path_stats_overall, file.path(out_dir, "C_path3_overall_frequency_and_lost_risk.csv"))

p_paths <- path_stats_overall %>%
  slice_head(n = top_k_paths) %>%
  ggplot(aes(x = reorder(Path3, n), y = p_lost)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Top ", top_k_paths, " 3-step paths (overall): P(Lost in 2015–2020)"),
    x = "Path (1996–2007 → 2007–2015 → 2015–2020)",
    y = "P(Lost | path)"
  )
ggsave(file.path(out_dir, "C_top_paths_overall_p_lost.png"), p_paths, width = 11, height = 8, dpi = 200)

path_stats_by_city <- paths %>%
  group_by(City, Country, Path3) %>%
  summarise(
    n = n(),
    n_lost = sum(Lost_final),
    p_lost = n_lost / n,
    .groups = "drop"
  ) %>%
  arrange(City, desc(n))

write_csv(path_stats_by_city, file.path(out_dir, "C_path3_by_city_frequency_and_lost_risk.csv"))

# ---------------------------
# D) Supporting analysis 2: heterogeneity across cities (effect sizes per city)
# ---------------------------

city_effects <- trans %>%
  group_by(City, Country) %>%
  group_modify(~{
    d <- .x
    lr <- lr_stat_empirical(d, y = "S_15_20", x = "S_07_15", z = "S_96_07")$LR
    h1 <- cond_entropy(d,  y = "S_15_20", x = "S_07_15", base = 2)
    h2 <- cond_entropy2(d, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)
    tibble(
      n_patches = nrow(d),
      LR_stat = lr,
      Delta_H_bits = (h1 - h2),
      P_lost = mean(d$S_15_20 == "Lost")
    )
  }) %>%
  ungroup() %>%
  arrange(desc(Delta_H_bits))

write_csv(city_effects, file.path(out_dir, "D_city_level_effect_sizes.csv"))

p_city_dh <- ggplot(city_effects, aes(x = reorder(City, Delta_H_bits), y = Delta_H_bits)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "City-level trajectory dependence (ΔH, bits)", x = "City", y = "ΔH (bits)")
ggsave(file.path(out_dir, "D_city_level_DeltaH.png"), p_city_dh, width = 8, height = 5.5, dpi = 200)

# ---------------------------
# E) Robustness checks (NO meta-state collapsing)
# ---------------------------

trans_no_early_lost <- trans %>% filter(S_96_07 != "Lost", S_07_15 != "Lost")

lr_E1 <- lr_stat_empirical(trans_no_early_lost, y = "S_15_20", x = "S_07_15", z = "S_96_07")
H1_E1 <- cond_entropy(trans_no_early_lost,  y = "S_15_20", x = "S_07_15", base = 2)
H2_E1 <- cond_entropy2(trans_no_early_lost, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)

robust_E1 <- tibble(
  check = "Exclude early Lost (S_96_07 != Lost AND S_07_15 != Lost)",
  n_cities = n_distinct(trans_no_early_lost$City),
  n_patches = nrow(trans_no_early_lost),
  LR_stat = lr_E1$LR,
  Delta_H_bits = (H1_E1 - H2_E1)
)
write_csv(robust_E1, file.path(out_dir, "E1_robust_exclude_early_lost.csv"))

hist_counts_city <- trans %>%
  count(City, S_07_15, S_96_07, name = "n_hist")

trans_trim <- trans %>%
  left_join(hist_counts_city, by = c("City", "S_07_15", "S_96_07")) %>%
  filter(n_hist >= min_hist_count) %>%
  select(-n_hist)

lr_E2 <- lr_stat_empirical(trans_trim, y = "S_15_20", x = "S_07_15", z = "S_96_07")
H1_E2 <- cond_entropy(trans_trim,  y = "S_15_20", x = "S_07_15", base = 2)
H2_E2 <- cond_entropy2(trans_trim, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)

robust_E2 <- tibble(
  check = paste0("Trim rare histories within city: keep (S_07_15, S_96_07) where n >= ", min_hist_count),
  n_cities = n_distinct(trans_trim$City),
  n_patches = nrow(trans_trim),
  LR_stat = lr_E2$LR,
  Delta_H_bits = (H1_E2 - H2_E2)
)
write_csv(robust_E2, file.path(out_dir, "E2_robust_trim_rare_histories_within_city.csv"))

if (all(is.na(trans$Boundary))) {
  warning("No Boundary column detected or all NA. Skipping E3 boundary sensitivity.")
  robust_E3 <- tibble(
    check = "Exclude boundary-intersecting patches (Boundary != YES) [SKIPPED: no Boundary column]",
    n_cities = NA_integer_,
    n_patches = NA_integer_,
    LR_stat = NA_real_,
    Delta_H_bits = NA_real_
  )
} else {
  trans_no_boundary <- trans %>%
    mutate(Boundary = str_to_upper(str_squish(as.character(Boundary)))) %>%
    filter(is.na(Boundary) | Boundary != "YES")
  
  lr_E3 <- lr_stat_empirical(trans_no_boundary, y = "S_15_20", x = "S_07_15", z = "S_96_07")
  H1_E3 <- cond_entropy(trans_no_boundary,  y = "S_15_20", x = "S_07_15", base = 2)
  H2_E3 <- cond_entropy2(trans_no_boundary, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)
  
  robust_E3 <- tibble(
    check = "Exclude boundary-intersecting patches (Boundary != YES)",
    n_cities = n_distinct(trans_no_boundary$City),
    n_patches = nrow(trans_no_boundary),
    LR_stat = lr_E3$LR,
    Delta_H_bits = (H1_E3 - H2_E3)
  )
  
  write_csv(robust_E3, file.path(out_dir, "E3_robust_exclude_boundary_intersecting.csv"))
  
  city_effects_no_boundary <- trans_no_boundary %>%
    group_by(City, Country) %>%
    group_modify(~{
      d <- .x
      lr <- lr_stat_empirical(d, y = "S_15_20", x = "S_07_15", z = "S_96_07")$LR
      h1 <- cond_entropy(d,  y = "S_15_20", x = "S_07_15", base = 2)
      h2 <- cond_entropy2(d, y = "S_15_20", x = "S_07_15", z = "S_96_07", base = 2)
      tibble(
        n_patches = nrow(d),
        LR_stat = lr,
        Delta_H_bits = (h1 - h2),
        P_lost = mean(d$S_15_20 == "Lost")
      )
    }) %>%
    ungroup() %>%
    arrange(desc(Delta_H_bits))
  
  write_csv(city_effects_no_boundary, file.path(out_dir, "E3_city_level_effect_sizes_exclude_boundary.csv"))
}

# ---------------------------
# F) Outputs for Results chapter
# ---------------------------

heat <- trans %>%
  group_by(S_07_15, S_96_07) %>%
  summarise(
    n = n(),
    p_lost = mean(S_15_20 == "Lost"),
    .groups = "drop"
  )

write_csv(heat, file.path(out_dir, "F1_heatmap_table_p_lost_given_history_overall.csv"))

p_heat <- ggplot(heat, aes(x = S_96_07, y = S_07_15, fill = p_lost)) +
  geom_tile() +
  labs(
    title = "Overall: P(Lost in 2015–2020 | prior typology history)",
    x = "Typology (1996–2007)",
    y = "Typology (2007–2015)",
    fill = "P(Lost)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "F1_heatmap_p_lost_overall.png"), p_heat, width = 11, height = 8, dpi = 200)

# ---- UPDATED: summary table now includes both pooled + urban-area–weighted + robustness ----
summary_table <- bind_rows(
  primary_results %>% transmute(
    check = "Primary (pooled; full typologies; city-blocked inference)",
    n_cities, n_patches, LR_stat, Delta_H_bits
  ),
  primary_results_urban_area_weighted %>% transmute(
    check = "Primary (urban-area–weighted; full typologies; city-blocked inference)",
    n_cities, n_patches, LR_stat, Delta_H_bits
  ),
  robust_E1 %>% transmute(check, n_cities, n_patches, LR_stat, Delta_H_bits),
  robust_E2 %>% transmute(check, n_cities, n_patches, LR_stat, Delta_H_bits),
  robust_E3 %>% transmute(check, n_cities, n_patches, LR_stat, Delta_H_bits)
)

write_csv(summary_table, file.path(out_dir, "F2_summary_primary_and_robustness.csv"))

boot_ci_compact <- boot_summary %>%
  transmute(metric, mean, p025, p975)
write_csv(boot_ci_compact, file.path(out_dir, "F2_bootstrap_CI_compact.csv"))

boot_ci_compact_w <- boot_summary_w %>%
  transmute(metric, mean, p025, p975)
write_csv(boot_ci_compact_w, file.path(out_dir, "F2_bootstrap_CI_compact_urban_area_weighted.csv"))

# ---- UPDATED: text report mentions both pooled + weighted ----
report_lines <- c(
  "Trajectory Dependence Report — All Cities (City-Blocked Bootstrap)",
  "===============================================================",
  paste0("Input file: ", data_path),
  paste0("Output dir: ", out_dir),
  "",
  "Primary history-conditioning test (POOLED; patch-weighted):",
  paste0("  Cities:  ", primary_results$n_cities),
  paste0("  Patches: ", primary_results$n_patches),
  paste0("  LR statistic: ", round(primary_results$LR_stat, 3)),
  paste0("  ΔH (bits):    ", round(primary_results$Delta_H_bits, 4)),
  "",
  "City-blocked bootstrap 95% CIs (POOLED):",
  paste0("  LR:  [", round(boot_ci_compact$p025[boot_ci_compact$metric == "LR_stat"], 3),
         ", ", round(boot_ci_compact$p975[boot_ci_compact$metric == "LR_stat"], 3), "]"),
  paste0("  ΔH:  [", round(boot_ci_compact$p025[boot_ci_compact$metric == "Delta_H_bits"], 4),
         ", ", round(boot_ci_compact$p975[boot_ci_compact$metric == "Delta_H_bits"], 4), "]"),
  "",
  "Primary history-conditioning test (URBAN-AREA–WEIGHTED):",
  paste0("  Cities:  ", primary_results_urban_area_weighted$n_cities),
  paste0("  Patches: ", primary_results_urban_area_weighted$n_patches),
  paste0("  LR statistic: ", round(primary_results_urban_area_weighted$LR_stat, 3)),
  paste0("  ΔH (bits):    ", round(primary_results_urban_area_weighted$Delta_H_bits, 4)),
  paste0("  Weight fallback used? ", primary_results_urban_area_weighted$weight_fallback),
  "",
  "City-blocked bootstrap 95% CIs (URBAN-AREA–WEIGHTED):",
  paste0("  LR:  [", round(boot_ci_compact_w$p025[boot_ci_compact_w$metric == "LR_stat"], 3),
         ", ", round(boot_ci_compact_w$p975[boot_ci_compact_w$metric == "LR_stat"], 3), "]"),
  paste0("  ΔH:  [", round(boot_ci_compact_w$p025[boot_ci_compact_w$metric == "Delta_H_bits"], 4),
         ", ", round(boot_ci_compact_w$p975[boot_ci_compact_w$metric == "Delta_H_bits"], 4), "]"),
  "",
  "Robustness checks:",
  paste0("  E1 Exclude early Lost: LR=", round(robust_E1$LR_stat, 3),
         ", ΔH=", round(robust_E1$Delta_H_bits, 4),
         " (N=", robust_E1$n_patches, ")"),
  paste0("  E2 Trim rare histories within city (n>=", min_hist_count, "): LR=", round(robust_E2$LR_stat, 3),
         ", ΔH=", round(robust_E2$Delta_H_bits, 4),
         " (N=", robust_E2$n_patches, ")"),
  paste0("  E3 Exclude boundary-intersecting patches: LR=", round(robust_E3$LR_stat, 3),
         ", ΔH=", round(robust_E3$Delta_H_bits, 4),
         " (N=", robust_E3$n_patches, ")"),
  "",
  "Key outputs written:",
  "  - B_primary_results_all_cities.csv",
  "  - B_bootstrap_city_blocked_summary.csv",
  "  - B2_primary_results_urban_area_weighted.csv",
  "  - B2_bootstrap_city_blocked_summary_urban_area_weighted.csv",
  "  - B2_city_urban_area_weights.csv",
  "  - C_path3_overall_frequency_and_lost_risk.csv",
  "  - D_city_level_effect_sizes.csv",
  "  - F1_heatmap_p_lost_overall.png",
  "  - F2_summary_primary_and_robustness.csv",
  "",
  "Interpretation guardrail:",
  "  Results support history-conditioned dependence in fragmentation typology trajectories.",
  "  This is dependence in the typology sequence (interval-based process labels), not a claim of ecological determinism."
)

writeLines(report_lines, con = file.path(out_dir, "F3_all_cities_report.txt"))

message("Done. Outputs written to: ", normalizePath(out_dir))

# ============================================================
# ADD-ON: Compile thesis-ready summary outputs (Steps A–F)
# (Unchanged below EXCEPT: Step B exports now include the weighted summary too.)
# ============================================================

required_objs <- c(
  "dat","trans","city_counts",
  "primary_results","boot_summary",
  "primary_results_urban_area_weighted","boot_summary_w",
  "robust_E1","robust_E2","robust_E3",
  "path_stats_overall","path_stats_by_city",
  "city_effects","heat"
)

missing <- required_objs[!vapply(required_objs, exists, logical(1))]
if (length(missing) > 0) {
  stop("Missing required objects in the environment: ", paste(missing, collapse = ", "))
}

thesis_dir <- file.path(out_dir, "THESIS_SUMMARY")
dir.create(thesis_dir, showWarnings = FALSE, recursive = TRUE)

A_coverage <- trans %>%
  summarise(
    n_cities = n_distinct(City),
    n_countries = n_distinct(Country),
    n_patches_complete = n(),
    share_lost_final = mean(S_15_20 == "Lost"),
    share_lost_early_96_07 = mean(S_96_07 == "Lost"),
    share_lost_mid_07_15 = mean(S_07_15 == "Lost"),
    share_boundary_yes = ifelse(all(is.na(Boundary)), NA_real_,
                                mean(!is.na(Boundary) & Boundary == "YES"))
  ) %>%
  mutate(scope = "All cities (complete trajectories only)") %>%
  select(scope, everything())

A_city_profile <- trans %>%
  group_by(City, Country) %>%
  summarise(
    n_patches = n(),
    P_lost = mean(S_15_20 == "Lost"),
    share_boundary_yes = ifelse(all(is.na(Boundary)), NA_real_,
                                mean(!is.na(Boundary) & Boundary == "YES")),
    .groups = "drop"
  ) %>%
  arrange(desc(n_patches))

write_csv(A_coverage,     file.path(thesis_dir, "THESIS_SUMMARY_A_coverage.csv"))
write_csv(A_city_profile, file.path(thesis_dir, "THESIS_SUMMARY_A_city_profile.csv"))

# ---- UPDATED Step B: include both pooled and urban-area–weighted summaries + CIs ----
B_primary <- primary_results %>%
  mutate(which = "pooled") %>%
  select(which, scope, n_cities, n_patches, LR_stat, Delta_H_bits,
         ll_baseline, ll_history, H_Y_given_prev_bits, H_Y_given_prev_prev2_bits)

B_primary_w <- primary_results_urban_area_weighted %>%
  mutate(which = "urban_area_weighted") %>%
  select(which, scope, n_cities, n_patches, LR_stat, Delta_H_bits,
         ll_baseline, ll_history, H_Y_given_prev_bits, H_Y_given_prev_prev2_bits, weight_fallback)

B_CI <- boot_summary %>%
  transmute(which = "pooled", metric, boot_mean = mean, boot_p025 = p025, boot_p975 = p975)

B_CI_w <- boot_summary_w %>%
  transmute(which = "urban_area_weighted", metric, boot_mean = mean, boot_p025 = p025, boot_p975 = p975)

write_csv(bind_rows(B_primary, B_primary_w), file.path(thesis_dir, "THESIS_SUMMARY_B_primary_results_pooled_and_weighted.csv"))
write_csv(bind_rows(B_CI, B_CI_w),           file.path(thesis_dir, "THESIS_SUMMARY_B_bootstrap_CI_pooled_and_weighted.csv"))

# (The rest of your thesis-summary block can remain unchanged if you want;
#  you’ll just now have both summaries available to cite.)
