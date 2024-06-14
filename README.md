
# banko R package

<!-- badges: start -->

[![R-CMD-check](https://github.com/agdamsbo/banko/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agdamsbo/banko/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Go and create your own banko cards or play travebanko with your friends in
[this hosted version of the package](https://agdamsbo.shinyapps.io/banko/).

The goal of banko is to ease the creation of banko cards and share ideas for
for different fun games such as travebanko.

The initial algorithm was inspired from the python package
[`banko`](https://github.com/skipperkongen/banko/).

Ideas are welcome, but please mind the code of conduct (see below).

## To do

- ~~Print cards and export as PDF~~ DONE

- ~~Build Shiny app~~ DONE

- ~~Host shiny app~~ DONE

- Streamline documentation and code

- Add some guards and rails for common functions. It is a little rough at the moment.

- Different game implementations (~~travebanko~~ DONE, others)

Banko is a Danish, special version of bingo, which is [nicely implemented in R
here](https://github.com/jennybc/bingo).

## Installation

You can install the development version of banko from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agdamsbo/banko")
```

## Example

Create numbers for 5 cards:

``` r
library(banko)
cards(5)
```

Export 20 cards with 5 on each page as pdf, with seed specified at 3425:

``` r
cards(20,seed=3425) |> 
purrr::map(gg_card) |> 
cards_grob() |> 
export_pdf(path = "banko.pdf")
```

To get all necessary materials for a game of travebanko, for 30 
participants/groups with 8 stops, run this:

``` r
cards(30, 5) |> travebanko(stops = 8) |> export_pdf()
```

Or even easier, [launch the app directly in your browser](https://agdamsbo.shinyapps.io/banko/).


## Code of Conduct

Please note that the banko project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
