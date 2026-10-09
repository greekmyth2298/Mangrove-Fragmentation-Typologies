# Historical input and path-dependence analysis provenance

This document identifies the actual files supplied by the study author on 8 October 2026 and distinguishes direct reproduction from methodological extensions.

## Exact original files

| Original supplied filename | Repository path | SHA-256 |
|---|---|---|
| `ALL CITIES COMPILED.csv` | `data/original/ALL CITIES COMPILED.csv` | `0f08c2c93b6c066bb24bf6a6a886e99c7322500c3cbd44b7c5345e52fe7f6516` |
| `PATH DEPENDENCE WITH URBAN AREA SUMMARY.R` | `reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R` | `62c3ba16773c8059e36506cae3b131a194ff7bda5e5660a8ddaf9371fc2a7a75` |

The **original input CSV is byte-for-byte identical** to `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` (SHA-256 `0f08c2c93b6c066bb24bf6a6a886e99c7322500c3cbd44b7c5345e52fe7f6516`). Both have 3,186 records and seven fields: `Country`, `City`, `City_ID_1996`, `S_1996_2007`, `S_2007_2015`, `S_2015_2020`, and `Boundary`. The archived R script is identical to `reference/historical_code/path_dependence_original_recovered.txt`; its `.R` filename is restored for clarity. These original files are **reference artifacts** and should not be run as the main reproducibility pipeline: the historical script uses a personal hard-coded working directory and requires additional tidyverse packages.

The executable analysis reads `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` and runs from the project root, so adding the archival files does not change any statistical result.

## What the historical code actually did

- `set.seed(42)` and `B <- 1000`.
- Pooled point estimates give equal weight to each descendant pathway.
- The historical script tries to detect a physical `Urban_Area` size column. This original seven-field file contains no such column. It therefore assigns each city an `urban_area_weight` of **1**, giving each city equal total contribution. **Equal urban-area weighting is not geographic-area-proportional weighting.** The pooled weighted conditional entropy is not the arithmetic mean of city-level entropy reductions.
- The `bootstrap_city_blocked` procedure samples urban areas with replacement and *then* resamples their constituent trajectories with replacement. The historically labeled "urban-area–blocked" bootstrap therefore implements **two-stage resampling**.
- The weighted two-stage bootstrap samples urban areas (with equal probabilities under the fallback) and then trajectories. Its `group_by(City)` weighting **merges repeated draws of the same city** within a replicate. Script 07 reproduces this historical convention, with a separate whole-area-only comparator that weights each drawn block.
- The published Methods describe sampling complete areas without the second trajectory-resampling step. Both procedures are kept and explicitly distinguished in the new R script and its outputs.

## Author-confirmed execution and publication comparison

On **8 October 2026**, the study author ran the full R pipeline with `MANGROVE_BOOTSTRAP_REPS=1000` and `MANGROVE_COMPARE_WHOLE_AREA=true` in RStudio. All seven stages completed, all 15 originally specified deterministic checks passed, and all four bootstrap computations finished (4,000 draws total). The original archived-file checks in this update should be validated on the author's machine after extracting this revised package. The tabulated 1,000-replicate results below were copied from the author's supplied RStudio output; they were **not rerun in the archive-building environment**.

| Method | Estimand | Bootstrap mean ΔH (bits) | 95% percentile CI (bits) |
|---|---|---:|---|
| Historical two-stage (publication) | Pooled | 0.1640771 | 0.1200140–0.2307880 |
| Historical two-stage (publication) | Equal city | 0.1919092 | 0.1420989–0.2594911 |
| Complete-area resampling (sensitivity) | Pooled | 0.1376426 | 0.1026148–0.1937501 |
| Complete-area resampling (sensitivity) | Equal drawn-area blocks | 0.1630857 | 0.1195581–0.2400979 |

The rounded historical two-stage bootstrap means and CIs match published Table 5. The **observed full-sample** ΔH values are 0.113223 bits (pooled) and 0.129515 bits (equal-city); bootstrap means are not the original point estimates.

## Remaining inference and reproducibility boundaries

The observed in-sample entropy reduction is conditional mutual information. Its empirical estimate is nonnegative by construction and can be upward-biased in sparse state tables. Percentile bootstrap intervals above zero are **not an independently calibrated conditional-independence significance test**; no such test is included. Positive ΔH provides a descriptive historical-information signal, without on its own proving causality or a non-random mechanism.

Upstream GIS delineation, lineage tracing, and manual `Lost`/`Stabilizing` assignment are not reconstructed from these seven-column tables. The historical fitted Random Forest object is unavailable, and newly fitted RF accuracies differ from the article. The uploaded external-validation CSV yields 88.8% overall accuracy and does not reproduce every published Table 2 figure. The pipeline flags the discrepancy without changing those data.

To verify the original artifacts in RStudio: `source("tests/test_original_source_provenance.R")`. This is also invoked by the full `source("run_all.R")` publication check. The optional independent Python audit is `python3 tests/reference_check.py`.
