# Dataset provenance and upstream boundaries

All research inputs in this repository were supplied by the author. No new remote-sensing or geospatial observations were generated while rebuilding this R pipeline.

| Path | Origin | Scope |
|---|---|---|
| `data/raw/TRAINING_DATA.csv` | Labeled training table | 1,212 manually classified samples |
| `data/processed/EXTERNAL_VALIDATION.csv` | Supplied validation results | 500 records |
| `data/processed/PATCH_TRANSITIONS_1996_2020.csv` | Supplied long-interval table previously called `NORMALIZED 1996-2020 - Corrected Kota Kinabalu.csv` | 4,584 records from 21 areas |
| `data/interval_predictions/` | Original 30 interval-specific predicted CSVs | 8,909 records, 10 areas × 3 periods |
| `data/original/ALL CITIES COMPILED.csv` | **Exact historical CSV** supplied 8 October 2026 | 3,186 pathway rows, seven columns |
| `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` | Existing normalized-name pipeline input | Same bytes as historical `ALL CITIES COMPILED.csv` |

The 30 interval-specific predictions are renamed for portable paths, without modifying their contents. Their original filenames and SHA-256 checksums are in `data/interval_predictions/manifest.csv`. Identical ancestral patch IDs and repeated state sequences in the closed cohort are legitimate descendant records and are preserved without collapsing rows.

The first interval's archived classification entries match the full multiset of (urban area, 1996 ancestor ID, 1996–2007 class) in the cohort. Later parent–child mapping relies on upstream GIS lineage construction and manual adjudication. The R scripts cannot regenerate these GIS decisions solely from the included data.

The historical script is archived under both its original filename `reference/historical_code/PATH DEPENDENCE WITH URBAN AREA SUMMARY.R` and its earlier recovered `.txt` copy, with identical bytes. `reference/ORIGINAL_ANALYSIS_PROVENANCE.md` documents its two-stage bootstrap and actual equal-city fallback. The published article is linked in the README by DOI and is not included as a PDF.
