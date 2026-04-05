# The Shape of the Troubles

Visualising and quantifying conflict-attributable excess deaths in young adult males in Northern Ireland.

## Overview

This paper uses Human Mortality Database data to demonstrate that the Troubles in Northern Ireland left a distinctive demographic signature: an impulse-decay pattern of excess mortality in young adult males beginning in 1972, with a half-life of approximately 7 years.

## Reproduction

### Prerequisites

- R (>= 4.5)
- Quarto (>= 1.7)
- HMD account (register at https://www.mortality.org/)

### Steps

1. Set HMD credentials:
   ```bash
   export HMD_USERNAME="your@email.com"
   export HMD_PASSWORD="yourpassword"
   ```

2. Download data:
   ```bash
   Rscript R/01-download-hmd.R
   ```

3. Render manuscript:
   ```bash
   quarto render
   ```

## Project structure

```
├── manuscript.qmd          # Main manuscript (includes sections)
├── _sections/              # Manuscript sections
│   ├── 01-introduction.qmd
│   ├── 02-historical-context.qmd
│   ├── 03-methods.qmd
│   ├── 04-results.qmd
│   └── 05-discussion.qmd
├── R/                      # Analysis code
│   ├── packages.R          # Package loading
│   ├── functions.R         # Shared functions
│   ├── config.R            # Parameters and constants
│   └── 01-download-hmd.R   # Data acquisition
├── data/
│   ├── raw/                # Original data (gitignored)
│   └── processed/          # Derived data (gitignored)
├── references/
│   └── references.bib      # BibTeX bibliography
├── figures/                # Generated figures
├── archive/                # Original materials
│   ├── manuscripts/        # Previous docx submissions
│   └── scripts/            # Original R scripts
└── _quarto.yml             # Quarto project config
```

## History

This paper was originally developed in 2017 and submitted to Contemporary Social Science, JECH, and Health & Place. The analysis has been refactored into a reproducible Quarto workflow in 2026, with HMD data updated through 2023.

Original analysis repo: https://github.com/JonMinton/northern_ireland_troubles
