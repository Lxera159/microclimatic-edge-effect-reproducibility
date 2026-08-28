# Reproducibility package for the MEE manuscript

This package contains the city-level input data and R code required to
reproduce the regression, generalized additive model (GAM), partial least
squares regression (PLSR), and partial least squares structural equation
model (PLS-SEM) results reported in Figures 3-5.

## Directory structure

- `data/city_level_MEE_climate.csv`: analysis-ready data for 93 cities.
- `R/00_project_paths.R`: shared project paths.
- `R/01_figure3_linear_regression.R`: climate-specific linear regressions.
- `R/02_figure3_GAM.R`: joint GAM effects of WMEE and IMEE on CE.
- `R/03_figure4_PLSR.R`: climate-specific PLSR, VIP, and SRC analyses.
- `R/04_figure5_PLS_SEM.R`: PLS-SEM analysis and path diagram.
- `00_install_dependencies.R`: package installation.
- `run_all.R`: reproduces all analyses and figures.
- `results/`: generated figures, model summaries, and session information.

## Software requirements

- R 4.6.0 or a compatible recent R release.
- R packages: `ggplot2`, `mgcv`, `patchwork`, `pls`, `plspm`, `scales`,
  and `viridis`.
- PLSR reproduction uses `pls` version 2.8-5.

## Reproduction instructions

1. Download or clone the complete package.
2. Set the R working directory to the package root (the directory containing
   this README).
3. Install dependencies once:

```r
source("00_install_dependencies.R")
```

4. Run the complete workflow:

```r
source("run_all.R")
```

All generated files will be written to `results/`. The source dataset is
never modified.

Individual analyses can also be run from the package root, for example:

```r
source("R/04_figure5_PLS_SEM.R")
```

## Analysis details

The linear regression and GAM scripts analyze cooling efficiency (`CE`) as a
function of microclimatic edge-effect width (`WMEE`) and intensity (`IMEE`).
The PLSR script uses seven standardized predictors and fixed component counts
for the six final climate-specific models. Fixing the selected component
counts makes the published PLSR estimates deterministic. The PLS-SEM script
uses 2,000 bootstrap resamples with random seed 123 and displays paths whose
bootstrap 95% confidence intervals exclude zero.

`run_all.R` records the R and package versions in
`results/session_info.txt`.

## Data columns

The analysis-ready CSV contains only variables used in the supplied analyses
or descriptive data requested for reporting:

- `cityname`, `climate_zone`: city identifier and climate zone.
- `city_area_km2`, `UGS_area_km2`: GHS-UCDB city area and UGS area.
- `WMEE`, `IMEE`, `CE`: edge-effect width, edge-effect intensity, and cooling
  efficiency.
- `MPA`, `FRAC`, `SHAPE`, `MCH`: landscape metrics.
- `PRE`, `TMP`, `WS`, `NTL`: precipitation, summer temperature, summer wind
  speed, and nighttime light intensity.

## Expected principal outputs

- `Figure3_linear_regression.png`
- `Figure3_GAM_WMEE_IMEE.png`
- `Figure4_PLSR_WMEE.png`
- `Figure4_PLSR_IMEE.png`
- `Figure5_PLS_SEM.png`
- Corresponding CSV files containing model coefficients and summaries.

## Suggested Code Availability statement

> The data and R code required to reproduce the statistical analyses and
> Figures 3-5 are available in the accompanying repository at [repository
> DOI or URL]. The repository includes an analysis-ready dataset, documented
> scripts, dependency installation instructions, and a complete workflow for
> regenerating the reported outputs.

Replace `[repository DOI or URL]` after archiving the package in a repository
such as Zenodo.
