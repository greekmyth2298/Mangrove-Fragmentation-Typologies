# Upload this complete package to GitHub

Repository: https://github.com/greekmyth2298/Mangrove-Fragmentation-Typologies

1. Extract the ZIP. Its top level directly contains `README.md`, `run_all.R`, `scripts/`, `data/`, `reference/`, `tests/`, `LICENSE`, and the `.Rproj` file. There is **no enclosing directory inside the ZIP**.
2. Upload **all the extracted files and folders** to the repository root on GitHub using `Add file` → `Upload files`. Confirm the folder paths are preserved and the existing same-path files are overwritten by the new upload.
3. Commit with a message such as `Archive original path-dependence input and document full reproduction`.
4. **Note:** Uploading does not delete obsolete old root-level files automatically. After checking their contents, manually remove legacy colon-separated filenames and superseded root-level fix notes if present. The historical research code is preserved in `reference/`.
5. The package adds the unmodified historical file `data/original/ALL CITIES COMPILED.csv`. `data/processed/RECONSTRUCTED_TRAJECTORIES.csv` is the same file under the standardized name. Both are intentionally tracked for provenance.

Run `source("tests/test_original_source_provenance.R")` after extracting if you want to check archival-source identity. For the complete analysis, open the RStudio project and execute `source("run_all.R")` after setting the bootstrap environment variables in `START_HERE.md`.

The upstream GIS workflow and original RF fit are outside this package; the historical two-stage bootstrap differs from the Methods' description of whole-area resampling. These differences are documented rather than hidden.
