# Shape of the Troubles — Project Guide

## What this is
A Quarto manuscript: "The Shape of Conflict" — detecting impulse-decay mortality signatures in civil conflicts using all-cause mortality surfaces from the Human Mortality Database.

## Environment setup
- **R**: Use conda env `renv` — activate with `eval "$(conda shell.bash hook 2>/dev/null)" && conda activate renv`
- **Quarto**: `/usr/local/bin/quarto` (v1.7.29)
- **Render**: `quarto render manuscript.qmd --to html` (output: `_output/index.html`)
- **HMD credentials**: Stored in project memory. Set `HMD_USERNAME` and `HMD_PASSWORD` env vars before running `Rscript R/01-download-hmd.R`
- **No Xcode CLI tools** — use conda compilers only

## Key files
- `manuscript.qmd` — main article (includes sections via `{{< include >}}`)
- `_sections/01-05` — introduction, historical context, methods, results, discussion
- `R/config.R` — country codes, model parameters (k=0.097, half-life=6.76yr, onset=1972)
- `R/functions.R` — core model functions (calc_lmr, troubles_decay, fit_troubles_model, estimate_excess_deaths)
- `R/01-download-hmd.R` — downloads HMD data via cookie-based session auth
- `data/processed/hmd_counts.csv` — current download (11 Western European countries, 318K rows)

## Current branch: `quarto-refactor`
3 unpushed commits ahead of origin.

## Conventions
- ggplot2 + patchwork for all plots (not lattice)
- Tidyverse throughout
- No Algeria data available — removed as case study
- Vancouver citation style
