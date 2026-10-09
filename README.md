# Mangrove fragmentation typologies, trajectories, and path dependence

**Computational reproducibility package for:** Gil, A. G. C., & Seto, K. C. (2026). *Fragmentation typologies, trajectories, and path dependence in urban mangrove landscapes of Southeast Asia*. **Landscape Ecology, 41**, Article 188. https://doi.org/10.1007/s10980-026-02425-9

The repository contains a cohesive seven-stage R pipeline, the supplied model-training and classification data, original reconstructed trajectory data, historical R code, and verification tests. It reproduces analyses **starting from author-supplied processed data**; original image processing and GIS lineage construction are upstream.

## Run the full pipeline

Open `Mangrove-Fragmentation-Typologies.Rproj` in RStudio and run from the repository root. Install `randomForest` once with `install.packages("randomForest")` (R 4.2+ recommended).

```r
# Faster smoke test
Sys.setenv(MANGROVE_BOOTSTRAP_REPS = "100",
           MANGROVE_COMPARE_WHOLE_AREA = "false")
source("run_all.R")

# Full analysis with original B=1000 and whole-area comparator
Sys.setenv(MANGROVE_BOOTSTRAP_REPS = "1000",
           MANGROVE_COMPARE_WHOLE_AREA = "true")
source("run_all.R")
```

The runner executes Stages 01–07 and `tests/test_pipeline.R`. To retest the existing outputs without refitting the model, run `source("tests/test_pipeline.R")`. Optional focused provenance verification is `source("tests/test_original_source_provenance.R")`. To run selected stages in a terminal, use `Rscript run_all.R 05 06 07` after the prerequisite files have been generated.

## Verified execution status (8 October 2026)

The author ran the complete pipeline in RStudio with 1,000 bootstrap draws for **each** of the four method/weighting combinations (4,000 draws). All seven scripts executed, and **all 15 originally specified deterministic publication checks passed**. The CSV and historical-code provenance tests added in this package have been checked independently and should also be run in RStudio after checkout.

| Reproduction item | Status |
|---|---|
| Tables 3–4 trajectory frequencies | Exact match |
| Table 5 pooled and equal-city information-theoretic point estimates | Match to published precision |
| Table 6 four cohort sizes and sensitivity estimates | Match to published precision |
| Table 5 published bootstrap means and percentile CIs | Exact match when rounded to three decimal places, using recovered two-stage method |
| Whole-urban-area bootstrap (alternative to original code) | Completed; produces distinct estimates |
| New RF model test accuracy | 85.44%; different from reported 82.42% |
| External validation overall accuracy from supplied CSV | 88.8%; published Table 2 not fully reproduced |
| Upstream spatial lineage construction | Not recreated from these CSVs |

The complete 1,000-draw results and provenance are documented in [`reference/ORIGINAL_ANALYSIS_PROVENANCE.md`](reference/ORIGINAL_ANALYSIS_PROVENANCE.md). Verification applies to the **analytical outputs and checks listed here**. A positive in-sample entropy reduction alone is insufficient for a formal conditional-independence or causal test.

## Original files and datasets

| Path | Rows | Description |
|---|---:|---|
| `data/original/ALL CITIES COMPILED.csv` | 3,186 | **Exact historical input** to original path-dependence script; byte-identical to processed trajectory CSV |
| `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` | 3,186 | Identical historical trajectories under the standardized pipeline filename |
| `data/raw/TRAINING_DATA.csv` | 1,212 | Labeled training observations and eight transformed RF predictors |
| `data/processed/EXTERNAL_VALIDATION.csv` | 500 | Author-supplied independent validation rows |
| `data/processed/PATCH_TRANSITIONS_1996_2020.csv` | 4,584 | Classification inputs from 21 urban areas |
| `data/interval_predictions/*.csv` | 8,909 | 30 archived classification tables across 10 urban areas and three intervals |

The first-interval 1996–2007 predictions exactly match the multiset of original ancestor IDs and states in the 3,186-row cohort. Later spatial lineage links and manual `Lost` and `Stabilizing` decisions cannot be uniquely recovered from the tables. Original repeated pathway rows and branching descendant identities are intentionally preserved.

**Original historical path-dependence script:** [`reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R`](reference/historical_code/PATH%20DEPENDENCE%20WITH%20URBAN%20AREA%20SUMMARY.R). The reference script contains a personal hard-coded working directory and uses tidyverse; it is archived for traceability and is **not** called by `run_all.R`. The operational code uses project-relative paths and only `randomForest` beyond base R.

## Pipeline map

| Stage | Work | Main output |
|---|---|---|
| 01 | Validate and prepare RF training inputs | `data/processed/TRAINING_DATA.csv` |
| 02 | Fit RF model and evaluate held-out data | `outputs/random_forest/` |
| 03 | Assess externally provided observed and predicted validation classes | `outputs/external_validation/` |
| 04 | Refit typologies and compare with original archived predictions | `outputs/typology_predictions/` |
| 05 | Load historical trajectories, retain descendant records, audit first-interval predictions | `outputs/trajectories/` |
| 06 | Summarize trajectory frequencies, patch loss, and transitions | `outputs/trajectory_analysis/` |
| 07 | Calculate entropy, empirical LR, sensitivity analyses, and both bootstrap procedures | `outputs/path_dependence/` |

Generated `outputs/` and `figures/` are excluded from Git by `.gitignore`. Running the pipeline creates them locally; the source files are intentionally retained in Git.

## Historical bootstrap and weighting choices

The original R code uses seed **42**, **1,000** draws, and two-stage resampling: sample the ten cities with replacement and resample trajectories within each selected city. This procedure reproduces the published bootstrap means and CIs. The article's Methods describe sampling complete cities with pathways retained; Stage 07 separately implements that literal whole-area alternative, so the distinction is visible.

The historical input has **no physical urban-area size field**. Accordingly, the original script's `Urban_Area` fallback assigned each city an equal total weight. **Equal-city weighting refers to each city's total contribution, not weighting by land area (km²).** For weighted two-stage draws, repeated selections of the same city are combined by city name in the original code. Both conventions are preserved in the reproduction; the alternative whole-area routine treats drawn blocks separately.

| Method, 1,000 draws | Pooled mean ΔH; 95% CI | Equal-city/block mean ΔH; 95% CI |
|---|---|---|
| Historical two-stage (published) | 0.164077; 0.120014–0.230788 | 0.191909; 0.142099–0.259491 |
| Complete-area-only alternative | 0.137643; 0.102615–0.193750 | 0.163086; 0.119558–0.240098 |

Full-data point estimates are **0.113223** pooled and **0.129515** equal-city-weighted. They are distinct from the corresponding bootstrap means. CIs are empirical bootstrap percentile intervals and do not constitute a calibrated test of conditional independence; positive in-sample conditional information is guaranteed for the plug-in estimator. See the archival provenance note for interpretation.

## Reproducibility boundaries

1. Original remote-sensing rasters, landscape geometry, spatial parent–child trace files, and manual adjudication notes are not included; the pipeline starts with supplied processed tables.
2. The original fitted Random Forest model is absent. A refit achieved **85.44%** held-out accuracy versus the reported **82.42%**, and **93.2%** agreement with archived interval labels. Historical archived labels are preserved.
3. The supplied 500-row validation file gives **88.8%** accuracy, with city-level discrepancies relative to published Table 2. Stage 03 warns and exports the comparison rather than silently editing data.
4. Time intervals have unequal lengths; the entropy analysis is a descriptive comparison of interval typology states and does not prove a mechanistic or causal ecological lock-in.
5. The manuscript discusses perimeter–area ratio, whereas the historical table contains the transformed `ED` (edge-density) predictor. Upstream predictor formula provenance remains uncertain.

## Repository files and citation

- `reference/DATA_PROVENANCE.md` — source inventory, original names, and lineage reconstruction boundary.
- `reference/ORIGINAL_ANALYSIS_PROVENANCE.md` — exact source hashes, historical bootstrap interpretation, and author-reported full-run results.
- `reference/original_scripts/` — original author-supplied Scripts 01–05.
- `reference/historical_code/` — unmodified original path-dependence code.
- `tests/` — publication benchmarks, provenance tests, and independent Python audit (`python3 tests/reference_check.py`).
- `CITATION.cff` and `LICENSE` — citation metadata and MIT software license.

Please cite the published article when using the analysis and document the remaining processing and inferential boundaries when interpreting the results.
