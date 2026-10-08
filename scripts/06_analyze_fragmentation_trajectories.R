# 06 | Recurrent fragmentation pathways, state transitions, patch-loss risk.
# Published results: Figures 8-9 and Tables 3-4, Gil & Seto (2026).
# Paths are descendant-row observations, including repeated ancestral patch IDs.
source("scripts/00_shared.R")
d <- mf_load_trajectories()
outdir <- mf_dir("outputs/trajectory_analysis")
figdir <- mf_dir("figures/trajectory_analysis")
write_out <- function(x, name) mf_write(x, file.path(outdir, name))

# A) Complete three-stage trajectories.
frequency <- as.data.frame(table(Trajectory = d$trajectory), stringsAsFactors = FALSE)
frequency <- frequency[frequency$Freq > 0, , drop = FALSE]
frequency <- frequency[order(-frequency$Freq, frequency$Trajectory), , drop = FALSE]
frequency$Percent_of_Cohort <- 100 * frequency$Freq / nrow(d)
write_out(frequency, "trajectory_frequencies_all.csv")
top10 <- head(frequency, 10)
write_out(top10, "table3_top10_recurrent_trajectories.csv")

lost_paths <- d[d$lost_final, , drop = FALSE]
frequency_lost <- as.data.frame(table(Trajectory = lost_paths$trajectory),
                                stringsAsFactors = FALSE)
frequency_lost <- frequency_lost[frequency_lost$Freq > 0, , drop = FALSE]
frequency_lost <- frequency_lost[order(-frequency_lost$Freq,
                                      frequency_lost$Trajectory), , drop = FALSE]
frequency_lost$Percent_of_Final_Loss <- 100 * frequency_lost$Freq / nrow(lost_paths)
write_out(frequency_lost, "loss_terminating_trajectory_frequencies.csv")
write_out(head(frequency_lost, 10), "table4_top10_loss_trajectories.csv")

# B) Conditional final-interval loss from the first TWO states.
# Never condition P(Lost) on the full trajectory: that would leak the outcome.
loss_probability <- function(x, by) {
  split_ids <- split(seq_len(nrow(x)),
                     do.call(interaction, c(unname(x[, by, drop = FALSE]),
                                               list(drop = TRUE))))
  if (!length(split_ids)) return(x[FALSE, by, drop = FALSE])
  res <- do.call(rbind, lapply(split_ids, function(ix) {
    z <- x[ix, , drop = FALSE]
    data.frame(z[1, by, drop = FALSE], n_pathways = nrow(z),
               n_lost = sum(z$lost_final),
               p_lost = mean(z$lost_final), row.names = NULL)
  }))
  rownames(res) <- NULL
  res[do.call(order, res[by]), , drop = FALSE]
}
loss_by_history <- loss_probability(d, c("s1", "s2"))
loss_by_history_active <- loss_by_history[loss_by_history$s2 != "Lost", , drop = FALSE]
write_out(loss_by_history, "final_loss_probability_by_prior_history.csv")
write_out(loss_by_history_active,
          "figure8_final_loss_probability_excluding_early_lost.csv")
loss_by_area <- loss_probability(d, c("urban_area_id", "s1", "s2"))
write_out(loss_by_area, "final_loss_probability_by_urban_area_history.csv")

# C) Consecutive transition counts and row-conditional probabilities.
transition_table <- function(df, source, destination, interval_name, by_area = FALSE) {
  src <- factor(df[[source]], levels = MF_STATES)
  dst <- factor(df[[destination]], levels = MF_STATES)
  if (!by_area) {
    tbl <- as.data.frame(table(From = src, To = dst), stringsAsFactors = FALSE)
    tbl <- tbl[tbl$Freq > 0, , drop = FALSE]
    totals <- as.numeric(table(src)[as.character(tbl$From)])
    return(data.frame(Interval_Transition = interval_name, tbl,
                      P_To_Given_From = tbl$Freq / totals))
  }
  out <- lapply(split(df, df$urban_area_id), function(sub) {
    z <- transition_table(sub, source, destination, interval_name, FALSE)
    z$City <- sub$urban_area_id[1]
    z
  })
  do.call(rbind, out)
}
tr12 <- transition_table(d, "s1", "s2", "1996-2007 to 2007-2015")
tr23 <- transition_table(d, "s2", "s3", "2007-2015 to 2015-2020")
write_out(rbind(tr12, tr23), "consecutive_state_transitions_pooled.csv")
write_out(rbind(transition_table(d, "s1", "s2", "1996-2007 to 2007-2015", TRUE),
                transition_table(d, "s2", "s3", "2007-2015 to 2015-2020", TRUE)),
          "consecutive_state_transitions_by_urban_area.csv")

area_pathways <- as.data.frame(table(City = d$urban_area_id, Trajectory = d$trajectory),
                               stringsAsFactors = FALSE)
area_pathways <- area_pathways[area_pathways$Freq > 0, , drop = FALSE]
area_pathways$Percent_Within_City <- 100 * area_pathways$Freq /
  as.numeric(table(d$urban_area_id)[area_pathways$City])
write_out(area_pathways, "trajectory_frequencies_by_urban_area.csv")

# D) Published trajectory counts. Verify, never edit observed data to make a match.
publication_table3 <- data.frame(
  Trajectory = c("Shattering -> Lost -> Lost", "Clearing -> Lost -> Lost",
                 "Displacing -> Stabilizing -> Displacing",
                 "Shattering -> Shattering -> Lost", "Displacing -> Lost -> Lost",
                 "Shattering -> Clearing -> Lost",
                 "Expanding -> Stabilizing -> Displacing",
                 "Shattering -> Expanding -> Lost",
                 "Shattering -> Expanding -> Expanding",
                 "Shattering -> Stabilizing -> Expanding"),
  Published_n = c(632, 101, 96, 72, 65, 57, 52, 46, 44, 44)
)
publication_table4 <- data.frame(
  Trajectory = c("Shattering -> Lost -> Lost", "Clearing -> Lost -> Lost",
                 "Shattering -> Shattering -> Lost", "Displacing -> Lost -> Lost",
                 "Shattering -> Clearing -> Lost", "Shattering -> Expanding -> Lost",
                 "Displacing -> Stabilizing -> Lost",
                 "Shattering -> Displacing -> Lost",
                 "Displacing -> Clearing -> Lost", "Expanding -> Lost -> Lost"),
  Published_n = c(632, 101, 72, 65, 57, 46, 43, 41, 27, 23)
)
verify_table <- function(ref, observed) {
  actual <- setNames(observed$Freq, observed$Trajectory)
  ref$Observed_n <- unname(actual[ref$Trajectory])
  ref$Matches <- !is.na(ref$Observed_n) & ref$Published_n == ref$Observed_n
  ref
}
check3 <- verify_table(publication_table3, frequency)
check4 <- verify_table(publication_table4, frequency_lost)
write_out(check3, "publication_table3_reproduction_check.csv")
write_out(check4, "publication_table4_reproduction_check.csv")
if (nrow(d) != 3186L || !all(check3$Matches) || !all(check4$Matches)) {
  warning("Some published trajectory benchmarks differ; inspect verification tables.")
}

# E) Figure 8: conditional loss probability for prior-state pairs.
grDevices::png(file.path(figdir, "figure8_conditional_loss_heatmap.png"),
              width = 1250, height = 1000, res = 155)
par(mar = c(9, 9, 4, 5))
mat <- matrix(NA_real_, nrow = length(MF_STATES), ncol = length(MF_STATES),
              dimnames = list(MF_STATES, MF_STATES))
for (i in seq_len(nrow(loss_by_history_active))) {
  z <- loss_by_history_active[i, ]
  mat[as.character(z$s1), as.character(z$s2)] <- z$p_lost
}
colormap <- colorRampPalette(c("#F3F4F7", "#F8CE85", "#B55242"))(80)
plot(0, 0, type = "n", xlim = c(0.5, 7.5), ylim = c(0.5, 7.5), xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     main = "Final-interval loss by prior fragmentation history")
for (i in seq_along(MF_STATES)) for (j in seq_along(MF_STATES)) {
  z <- mat[i, j]
  fill <- if (is.na(z)) "#EAEAEA" else colormap[1L + round(z * 79)]
  rect(i - 0.48, j - 0.48, i + 0.48, j + 0.48, col = fill, border = "white")
  if (!is.na(z)) text(i, j, sprintf("%.2f", z), cex = 0.74)
}
axis(1, at = seq_along(MF_STATES), labels = MF_STATES, las = 2)
axis(2, at = seq_along(MF_STATES), labels = MF_STATES, las = 2)
mtext("State in 1996-2007", side = 1, line = 6)
mtext("State in 2007-2015 (Lost excluded)", side = 2, line = 6)
mtext("Blank cells indicate unobserved history pairs", side = 3, line = 0.3, cex = 0.8)
grDevices::dev.off()

# F) Figure 9: base-R alluvial transition visualization (counts are exact).
# Each ribbon represents an adjacent-stage state transition, and the width is
# proportional to the number of descendant trajectories represented.
# Plotting adjacency flows does not imply every two-link ribbon is an individual
# distinct lineage; the underlying three-step pathways remain in the tables.
plot_alluvial <- function(d, path) {
  stages <- c("s1", "s2", "s3")
  xstage <- c(0.12, 0.5, 0.88)
  bar_width <- 0.043
  gap <- 0.007
  n <- nrow(d)
  state_bounds <- vector("list", 3)
  for (k in 1:3) {
    counts <- as.numeric(table(factor(d[[stages[k]]], levels = MF_STATES))) / n
    alloc <- counts * (1 - gap * (length(MF_STATES) - 1))
    bottoms <- c(0, head(cumsum(alloc + gap), -1))
    state_bounds[[k]] <- data.frame(state = MF_STATES, start = bottoms,
                                   end = bottoms + alloc, height = alloc)
  }
  grDevices::png(path, width = 1500, height = 1000, res = 155)
  par(mar = c(3, 2, 4, 2))
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "",
       main = "Reconstructed mangrove fragmentation state flows")
  for (k in 1:2) {
    tab <- as.data.frame(table(from = factor(d[[stages[k]]], levels = MF_STATES),
                               to = factor(d[[stages[k + 1]]], levels = MF_STATES)),
                         stringsAsFactors = FALSE)
    tab <- tab[tab$Freq > 0, , drop = FALSE]
    from_cursor <- setNames(state_bounds[[k]]$start, MF_STATES)
    to_cursor <- setNames(state_bounds[[k + 1]]$start, MF_STATES)
    scale <- (1 - gap * (length(MF_STATES) - 1)) / n
    for (i in seq_len(nrow(tab))) {
      r <- tab[i, ]; a <- as.character(r$from); b <- as.character(r$to)
      width <- r$Freq * scale
      # Reconcile tiny floating-point errors by using count-derived widths.
      y1 <- from_cursor[a]; y2 <- to_cursor[b]
      from_cursor[a] <- y1 + width
      to_cursor[b] <- y2 + width
      t <- seq(0, 1, length.out = 45)
      smooth <- t * t * (3 - 2 * t)
      xs <- (xstage[k] + bar_width / 2) +
        (xstage[k + 1] - xstage[k] - bar_width) * t
      low <- y1 + (y2 - y1) * smooth
      high <- (y1 + width) + (y2 - y1) * smooth
      polygon(c(xs, rev(xs)), c(low, rev(high)),
              col = grDevices::adjustcolor(MF_COLORS[a], alpha.f = 0.33),
              border = NA)
    }
  }
  for (k in 1:3) {
    x <- xstage[k]
    for (j in seq_along(MF_STATES)) {
      sb <- state_bounds[[k]][j, ]
      if (sb$height <= 0) next
      rect(x - bar_width / 2, sb$start, x + bar_width / 2, sb$end,
           col = MF_COLORS[MF_STATES[j]], border = "white", lwd = 0.65)
      if (sb$height >= 0.025) {
        shift <- if (k == 3) -0.035 else 0.033
        text(x + shift, (sb$start + sb$end) / 2, MF_STATES[j], cex = 0.65,
             adj = if (k == 3) 1 else 0, xpd = NA)
      }
    }
    text(x, 1.025, c("1996-2007", "2007-2015", "2015-2020")[k],
         font = 2, xpd = NA)
  }
  grDevices::dev.off()
}
plot_alluvial(d, file.path(figdir, "figure9_fragmentation_state_flows.png"))

write_out(data.frame(metric = c("pathway_rows", "urban_areas", "total_final_lost",
                                "number_distinct_trajectories", "table3_matches",
                                "table4_matches"),
                     value = c(nrow(d), length(unique(d$urban_area_id)),
                               sum(d$lost_final), nrow(frequency),
                               all(check3$Matches), all(check4$Matches))),
          "trajectory_analysis_summary.csv")
mf_session(file.path(outdir, "session_info.txt"))
message("06 complete: Tables 3 and 4 match: ",
        all(check3$Matches) && all(check4$Matches))
