R code for reproducing the statistical analyses, figures and tables for the manuscript "Layered acoustic and semantic drivers of perceived pleasantness and eventfulness in transport-hub soundscapes".

## Requirements

- R 4.5+ (developed and verified on R 4.5.3)

- CRAN packages: `tidyverse`, `readxl`, `patchwork`, `lme4`, `lmerTest`, `performance`, `psych`, `emmeans`

- Install:

  ```r
  install.packages(c("tidyverse", "readxl", "patchwork", "lme4", "lmerTest",
                     "performance", "psych", "emmeans"))
  ```

- No non-standard hardware is required. A complete run takes about one to two minutes on a normal desktop; the exhaustive model grid accounts for most of it and uses `parallel::mclapply`, so it runs single-threaded on Windows and is correspondingly slower there.

## Data

The analysis reads a single input: the Mendeley Data workbook
`P34_transport_hub_soundscape_data.xlsx`.

1. Download it from Mendeley Data (CC BY 4.0) — see the manuscript's data-availability statement for the DOI.
2. Place the `.xlsx` file in the `data/` folder (see `data/README.md`).

The workbook holds the analysis-ready chain of the listening experiment: the 2,116 observation-level ratings of 36 transport-hub recordings by 59 listeners on the eight ISO/TS 12913-2 attributes with their ISO-P/ISO-E coordinates, the participant roster, the 36-recording table of 24 ArtemiS acoustic indicators across the energy, psychoacoustic and temporal layers, the one-second indicator series, and the sound-source pick counts and shares with their three-class aggregation.

The 36 audio excerpts themselves are not part of the deposit. Every indicator used by the models is deposited, so nothing here needs the audio.

## File structure

- `run_all.R` — master script: runs every analysis module, then builds every manuscript display item.
- `code/load_data.R` — single data entry: maps each analysis table to its workbook sheet and reads it with `col_types = "text"` plus a CSV round-trip, so column types match the working pipeline exactly.
- `code/helpers.R` — shared data entry, the four indicator-layer definitions, and the crossed random-intercept model helpers used by every module.
- `code/a*.R` — the analysis modules: descriptives (A01), attribute reliability (A02), one-way ANOVA and post-hoc contrasts (A03), indicator–perception correlations (A04), null-model variance decomposition (A05), the adopted specifications and everything reported about them (A11), the exhaustive one-indicator-per-layer model grid (A12), the dimension-level anatomy of the adopted driver sets (A13), the pre-specified moderation and curvature tests (A14), and leave-one-recording-out cross-validation (A15).
- `code/build_figures.R` — Figures 1–5 and Supplementary Figures S1–S2.
- `code/build_tables.R` — Tables 1–3 and Supplementary Tables S1–S11 (LaTeX fragments).

## Usage

From the repository root:

```bash
Rscript run_all.R
```

Outputs are written to:

- `output/figures/` — Figures 1–5 and Supplementary Figures S1–S2 (PNG, 600 dpi)
- `output/tables/` — Tables 1–3 and Supplementary Tables S1–S11 (LaTeX fragments; `tab*.tex` main, `si_*.tex` supplementary)
- `output/<analysis id>/` — the full regenerated result tables of each analysis module

To keep the console log alongside the outputs:

```bash
Rscript run_all.R 2>&1 | tee output/run_log.txt
```

`output/` is produced at run time and is safe to delete. It also holds `_workbook_cache/`, the extracted workbook sheets; delete it to force a fresh read.

## Verification

Run against the deposited workbook, this pipeline reproduces the manuscript's display items **byte-identically**: all 7 figures and all 14 LaTeX table fragments match the values in the paper exactly. Nothing in the pipeline is random, so no seed is needed.

Workbook cells are capped at 15 significant digits, so the unrounded full-precision columns of a few module result tables differ from the working pipeline in their last digits. The largest such difference is 2.2e-5 on a Satterthwaite degrees-of-freedom value of about 32 (relative 7e-7), which propagates from a ~1e-15 input perturbation through the numerical derivatives of that estimator; every reported quantity is printed to far fewer digits and none of them moves.

## Notes

- Every module is self-contained: it reads the deposited tables through `code/helpers.R` and writes only into its own `output/` folder, so modules can be run individually and in any order.
- The two adopted models are crossed random-intercept models with listener and recording intercepts, fitted by ML for model comparison (AIC, likelihood-ratio tests) and by REML for the reported coefficients and variance components, with Satterthwaite degrees of freedom.
- The main models are built by a single-pass ordered screen over the four layers. A12 re-derives the optimum without that path dependence by fitting all 1,512 one-indicator-per-layer combinations per outcome; the two grid optima are reported as rows of Supplementary Table S3.
- A02 computes the attribute reliabilities in both orientations. The manuscript reports Cronbach's alpha and ICC(2,1) from the listener-as-case layout and the ICC(2,57) figures from its transpose, which is the inter-rater reliability of a recording's mean.
- False-discovery-rate control (Benjamini–Hochberg) is applied within the declared families of A13 and A14 only; the correlation screen of Figure 3 is descriptive and carries no significance marks.

## License

Code in this repository is released under the MIT License (see `LICENSE`). The input data are archived separately under CC BY 4.0 at Mendeley Data.
