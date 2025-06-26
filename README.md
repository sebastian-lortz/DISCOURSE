
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DISCOURSE <a href="https://sebastian-lortz.github.io/discourse/"><img src="man/figures/logo-comp.png" align="right" height="150" alt-text="DISCOURSE logo"/></a>

<!-- badges:start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges:end -->

I introduce the DISCOURSE framework – Data-simulation via Iterative
Stochastic Combinatorial Optimization Using Reported Summary Estimates.
The primary scope of the algorithmic framework is to reconstruct
complete datasets using only summary statistics, giving researchers a
way - when raw data are unavailable - to inform replication study
decision‑making.

## Usage

The method is available as R package and comprehensive ShinyApp.

### Web App

You can use the app at
<https://sebastian-lortz.shinyapps.io/discourse/>. Expect longer
computation time compared to running the app locally.

### Installation

You can install the latest version of the R package `discourse` like so:

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("sebastian-lortz/discourse")
```

### Run

You can launch the ShinyApp locally by running:

``` r
discourse::run_app()
```

## Citation

Please cite `discourse` if you use it. To cite the software, use:

Lortz SAJ (2025). *discourse: Data-simulation via Iterative Stochastic
Combinatorial Optimization Using Reported Summary Estimates*. R package
version 0.0.1.000, <https://sebastian-lortz.github.io/discourse/>,
<https://github.com/sebastian-lortz/discourse>.

Or copy the reference information to your BibTeX file:

``` bibtex
@Manual{discourse,
  title        = {discourse: Data‐simulation via Iterative Stochastic Combinatorial Optimization Using Reported Summary Estimates},
  author       = {S. A. J. Lortz},
  year         = {2025},
  note         = {R package version 0.0.1.000},
  url          = {https://github.com/sebastian-lortz/discourse}
}
```

## Code of Conduct

I am open to feedback and new ideas. Please mind the Contributor Code of
Conduct.

## About

You are reading the doc about version: 0.0.1.000

This README has been compiled on 2025-06-26 17:16:06.
