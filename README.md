
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
# install devtools if you don’t have it yet
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# install the development version from GitHub
devtools::install_github("sebastian-lortz/discourse")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo sebastian-lortz/discourse@HEAD
#> utf8         (1.2.4    -> 1.2.6   ) [CRAN]
#> pillar       (1.10.1   -> 1.10.2  ) [CRAN]
#> tibble       (3.2.1    -> 3.3.0   ) [CRAN]
#> scales       (1.3.0    -> 1.4.0   ) [CRAN]
#> rlang        (1.1.5    -> 1.1.6   ) [CRAN]
#> Rdpack       (2.6.2    -> 2.6.4   ) [CRAN]
#> reformulas   (0.4.0    -> 0.4.1   ) [CRAN]
#> nloptr       (2.1.1    -> 2.2.1   ) [CRAN]
#> stringi      (1.8.4    -> 1.8.7   ) [CRAN]
#> cpp11        (0.5.1    -> 0.5.2   ) [CRAN]
#> ggplot2      (3.5.1    -> 3.5.2   ) [CRAN]
#> Deriv        (4.1.6    -> 4.2.0   ) [CRAN]
#> purrr        (1.0.2    -> 1.0.4   ) [CRAN]
#> generics     (0.1.3    -> 0.1.4   ) [CRAN]
#> MatrixModels (0.5-3    -> 0.5-4   ) [CRAN]
#> doBy         (4.6.24   -> 4.6.27  ) [CRAN]
#> broom        (1.0.7    -> 1.0.8   ) [CRAN]
#> quantreg     (5.99.1   -> 6.1     ) [CRAN]
#> pbkrtest     (0.5.3    -> 0.5.4   ) [CRAN]
#> fs           (1.6.5    -> 1.6.6   ) [CRAN]
#> sass         (0.4.9    -> 0.4.10  ) [CRAN]
#> bslib        (0.8.0    -> 0.9.0   ) [CRAN]
#> promises     (1.3.2    -> 1.3.3   ) [CRAN]
#> later        (1.4.1    -> 1.4.2   ) [CRAN]
#> jsonlite     (1.8.9    -> 2.0.0   ) [CRAN]
#> mime         (0.12     -> 0.13    ) [CRAN]
#> httpuv       (1.6.15   -> 1.6.16  ) [CRAN]
#> tinytex      (0.54     -> 0.57    ) [CRAN]
#> evaluate     (1.0.1    -> 1.0.4   ) [CRAN]
#> knitr        (1.49     -> 1.50    ) [CRAN]
#> mvtnorm      (1.3-2    -> 1.3-3   ) [CRAN]
#> RcppArmad... (14.2.3-1 -> 14.4.3-1) [CRAN]
#> emmeans      (1.10.7   -> 1.11.1  ) [CRAN]
#> Installing 33 packages: utf8, pillar, tibble, scales, rlang, Rdpack, reformulas, nloptr, stringi, cpp11, ggplot2, Deriv, purrr, generics, MatrixModels, doBy, broom, quantreg, pbkrtest, fs, sass, bslib, promises, later, jsonlite, mime, httpuv, tinytex, evaluate, knitr, mvtnorm, RcppArmadillo, emmeans
#> Installing packages into '/private/var/folders/tt/dhl_zln57k19qn1d4t5xvhw40000gn/T/Rtmp1c0vmV/temp_libpath107374dd5928'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/tt/dhl_zln57k19qn1d4t5xvhw40000gn/T//Rtmp1JcbrK/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/tt/dhl_zln57k19qn1d4t5xvhw40000gn/T/Rtmp1JcbrK/remotes1175d4699199a/sebastian-lortz-discourse-35022cf/DESCRIPTION’ ... OK
#> * preparing ‘discourse’:
#> * checking DESCRIPTION meta-information ... OK
#> * cleaning src
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Omitted ‘LazyData’ from DESCRIPTION
#> * building ‘discourse_0.0.0.9000.tar.gz’
#> Installing package into '/private/var/folders/tt/dhl_zln57k19qn1d4t5xvhw40000gn/T/Rtmp1c0vmV/temp_libpath107374dd5928'
#> (as 'lib' is unspecified)
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
#> [1] "2025-06-23 10:47:50 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading discourse
#> ── R CMD check results ──────────────────────────────── discourse 0.0.1.000 ────
#> Duration: 21.6s
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
