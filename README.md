
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
#> [1] "2025-06-23 12:39:13 CEST"
```

Here are the tests results and Namespace:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading discourse
#> ── R CMD check results ──────────────────────────────── discourse 0.0.1.000 ────
#> Duration: 20.8s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```
