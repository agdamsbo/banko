
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

- ~~Print plates and export as PDF~~ DONE

- Host as Shiny app

- Different game implementations (~~travebanko~~ DONE, others)

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

Create numbers for 5 plates:

``` r
library(banko)
plates(5)
```

Export 20 plates with 5 on each page as pdf:

``` r
plates(20) |> purrr::map(gg_plate) |> multiplate_pdf()
```

## Code of Conduct

Please note that the banko project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
