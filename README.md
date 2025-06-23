
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{discourse}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/sebastian-lortz/discourse/graph/badge.svg)](https://app.codecov.io/gh/sebastian-lortz/discourse)
<!-- badges: end -->

## Installation

You can install the development version of `{discourse}` like so:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("sebastian-lortz/discourse", quiet = TRUE)
```

## Run

You can launch the application by running:

``` r
discourse::run_app()
```

## About

You are reading the doc about version : 0.0.1.000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-06-23 10:53:25 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading discourse
#> ── R CMD check results ──────────────────────────────── discourse 0.0.1.000 ────
#> Duration: 21.3s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> discourse Coverage: 26.25%
#> R/fct_check_grim.R: 0.00%
#> R/fct_get_rmse_parallel.R: 0.00%
#> R/fct_get_rmse.R: 0.00%
#> R/fct_get_stats_parallel.R: 0.00%
#> R/fct_get_stats.R: 0.00%
#> R/fct_hill_climb.R: 0.00%
#> R/fct_optim_aov.R: 0.00%
#> R/fct_optim_lm.R: 0.00%
#> R/fct_optim_lme.R: 0.00%
#> R/fct_optim_vec.R: 0.00%
#> R/fct_parallel_aov.R: 0.00%
#> R/fct_parallel_lm.R: 0.00%
#> R/fct_parallel_lme.R: 0.00%
#> R/fct_plot_cooling.R: 0.00%
#> R/fct_plot_error_ratio.R: 0.00%
#> R/fct_plot_error.R: 0.00%
#> R/fct_plot_rmse.R: 0.00%
#> R/fct_plot_summary.R: 0.00%
#> R/fct_print.R: 0.00%
#> R/fct_summary.R: 0.00%
#> R/fct_weights_est.R: 0.00%
#> R/fct_weights_vec.R: 0.00%
#> R/run_app.R: 0.00%
#> R/utils_helpers.R: 0.00%
#> src/helpers.cpp: 0.00%
#> R/mod_optim_lme.R: 40.99%
#> R/mod_optim_lm.R: 42.35%
#> R/mod_optim_vec.R: 48.48%
#> R/mod_optim_aov.R: 49.77%
#> R/app_server.R: 86.96%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/utils_server.R: 100.00%
```
