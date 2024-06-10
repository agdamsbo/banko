
<!-- README.md is generated from README.Rmd. Please edit that file -->

# banko

<!-- badges: start -->

[![R-CMD-check](https://github.com/agdamsbo/banko/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agdamsbo/banko/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of banko is to ease the creation of banko plates for playing.
The functionality will be implemented for different fun games.

The initial algorithm was inspired from the python package
[`banko`](https://github.com/skipperkongen/banko/).

Ideas are welcome, but please mind the code of conduct (see below).

## To do

- Print plates and export as PDF

- Host as Shiny app

- Different game implementations (travebanko, others)

Banko is the Danish version of bingo, which is [nicely implemented in R
here](https://github.com/jennybc/bingo).

## Installation

You can install the development version of banko from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agdamsbo/banko")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(banko)
plates(2)
#> [[1]]
#> # A tibble: 3 × 9
#>    ...1  ...2  ...3  ...4  ...5  ...6  ...7  ...8  ...9
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     3    12    21    NA    40    53    NA    NA    NA
#> 2     4    16    24    NA    NA    56    NA    78    NA
#> 3    NA    18    26    39    NA    NA    69    NA    83
#> 
#> [[2]]
#> # A tibble: 3 × 9
#>    ...1  ...2  ...3  ...4  ...5  ...6  ...7  ...8  ...9
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    NA    NA    NA    32    41    NA    63    70    82
#> 2    NA    17    24    NA    47    52    NA    72    NA
#> 3     6    19    NA    NA    48    NA    69    74    NA
```

## Code of Conduct

Please note that the banko project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
