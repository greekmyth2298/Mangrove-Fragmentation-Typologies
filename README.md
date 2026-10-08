# Reproducible mangrove-fragmentation analyses (R scripts 01–07)

**Reference:** Gil, A. G. C., & Seto, K. C. (2026). *Fragmentation typologies, trajectories, and path dependence in urban mangrove landscapes of Southeast Asia*. **Landscape Ecology 41**, Article 188. https://doi.org/10.1007/s10980-026-02425-9

This reconstruction integrates the supplied five R scripts, the recovered historical path-dependence R code, the provided CSVs, and the **final published manuscript**. The published manuscript defines the intended estimands, classifications, and scope. When the historical implementation and the paper's methods prose differ, Script 07 implements and **labels both**. Results are never changed to force agreement with the paper.

## Quick start

1. Install **R >= 4.2** and the CRAN package `randomForest` (`install.packages("randomForest")`). Scripts 01, 03, and 05–07 use base R only.
2. Set the working directory to **this project's root** in RStudio or your terminal.
3. Run `Rscript run_all.R` to run all available stages. Script 04 is skipped when its GIS-derived full transition CSV is unavailable.
4. To test the independent trajectory and path-dependence branches without installing `randomForest`, run `Rscript run_all.R 05 06 07`.
5. To reduce bootstrap draws while testing the code, set `MANGROVE_BOOTSTRAP_REPS=100` before running 07; default is **1,000**. Set `MANGROVE_COMPARE_WHOLE_AREA=false` to disable the methodological comparator.

## Data flow

| Stage | Inputs | Outputs | Notes |
|---|---|---|---|
| 01 | `data/raw/TRAINING_DATA.csv` | `data/processed/TRAINING_DATA.csv` | Keeps eight transformed predictors and five typology codes |
| 02 | Prepared training CSV | `outputs/random_forest/` and `figures/random_forest/` | Seed 42, City × Typology 70/30 stratification, 500-tree RF |
| 03 | `data/processed/EXTERNAL_VALIDATION.csv` | `outputs/external_validation/` | Evaluates independently supplied manual and predicted codes; does not fabricate model-derived predictions |
| 04 | RF model bundle **plus** `data/processed/ALL_PATCH_TRANSITIONS.csv` | `outputs/typology_predictions/` | **Unavailable input**; GIS-derived patch-transition metrics must be supplied separately |
| 05 | `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` | `outputs/trajectories/FRAGMENTATION_TRAJECTORIES.csv` | Independently GIS-reconstructed, manually validated paths; not generated from 04 |
| 06 | Output of 05 | `outputs/trajectory_analysis/` and `figures/trajectory_analysis/` | Published Tables 3–4, transition probabilities, conditional loss risk, Figs. 8–9 approximations |
| 07 | Output of 05 | `outputs/path_dependence/` and `figures/path_dependence/` | Published Tables 5–6, effect sizes, bootstrap results, methodological comparator |

The shared definitions and formulas are in `scripts/00_shared.R`. The **runner** is `run_all.R`. The original supplied scripts, recovered historical path-dependence code, and final published paper are archived under `reference/` for provenance.

## Interpretive safeguards and known gaps

- **Branching lineages:** Each trajectory CSV row is retained as a descendant pathway. Repeated or completely identical rows may describe distinct descendants of the same 1996 patch. The CSV contains no independent descendant identity to verify this spatially. The synthetic `pathway_row_id` is an index into the CSV, **not** a GIS patch identifier.
- **Closed cohort:** Scripts 05–07 analyze 3,186 descendant pathway records from ten urban areas, traceable from 1996. They are not a sample of all patches or all 21 urban systems.
- **Absorbing state:** `Lost` is retained and cannot transition back into another class.
- **Metric naming:** The published prose describes a perimeter–area ratio (PAR), whereas the supplied transformed RF predictor is named `log_change_ED` (edge density). Their exact equivalence cannot be established without the upstream GIS transformation metadata; the script preserves the actual predictor name rather than silently renaming it.
- **Predictor provenance:** Eight transformed RF predictors and the GIS lineages were created upstream. The R pipeline validates and uses them; it cannot recreate them without the GIS data and workflow.
- **Typology families:** Scripts 02 and 04 model five trained codes (0–4). Stabilizing and Lost are added through the GIS/manual trajectory reconstruction; they are not predictions of the RF classifier.
- **Weighting:** Equal **urban-area** weighting gives each MUA study unit equal total weight. It is not weighting by its geographic size, nor by original ancestral patch.
- **Bootstrap discrepancy:** The historical R code uses a **two-stage bootstrap** (resample urban areas and then pathways within each drawn area). The published Methods (page 11) describe resampling **whole urban areas** and retaining all pathways. Script 07 reports the historical method and a separate literal whole-area comparator. The historical code's equal-city-weighted bootstrap treats duplicated sampled urban areas as a single city when assigning final weights. This is preserved under the label *legacy*.
- **Coverage limitation:** Without the original full GIS patch-transition and annual urban-area landscape-metric datasets, the pipeline cannot reconstruct the complete 21-urban-area Figure 4–5 summaries or independent spatial examples in Figure 6.
- **External validation discrepancy:** The 500-row external-validation file does not reproduce all Table 2 city accuracy values. Script 03 reports these mismatches; it does not relabel or replace observations.
- **Random Forest replication:** The new reproducible split follows the specified method and seed, but R randomForest version, sorting, and exact row sampling may differ from the unpublished original session. Reported internal validation metrics should be checked, not assumed reproduced.
- **Historical interpretation:** Positive in-sample entropy reduction indicates conditional predictive information in observed sequences. It does not establish feedback, causality, or ecological determinism.
- **Graphics:** Figures 8 and 9 are *independent visual reproductions* of the underlying reported analysis. They need not be pixel-identical to the published figures.

## Reproduction targets

**Tables 3–4:** The frequency files exported by Script 06 include exact target counts for the ten listed trajectories and emit a `Matches` indicator.

**Table 5:** Pooled LL (recent) -4010.9; LL (history) -3760.9; LR 500.08; conditional entropies 1.816 and 1.703 bits; pooled ΔH 0.113 bits; equal urban-area-weighted ΔH 0.130 bits. Bootstrap CIs are stochastic and are evaluated against the original recovered procedure, with limitations above.

**Table 6:** N = 3186, 2359, 2882, 2965; ΔH = 0.113, 0.153, 0.126, 0.119 bits for primary, early-loss exclusion, rare-history trimming within city (n ≥ 5), and boundary exclusion respectively.

## Testing status

The CSV input schemas and all published trajectory, entropy, and sensitivity point targets are independently checked by `tests/reference_check.py`. **Actual R execution has not been verified in the authoring environment because an R interpreter is unavailable**. A local RStudio/Rscript smoke test is required before citing the scripts as fully executed and reproducible.
