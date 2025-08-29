#' Generate complete columns
#'
#' @param col column index
#'
#' @return numeric vector
#' @export
#'
full_col <- function(col) {
  min <- ifelse(col == 0, 1, 0)
  max <- ifelse(col == 8, 10, 9)

  sort(sample(seq(min, max), size = 3)) + col * 10
}


#' Eliminate elements in columns to apply to cards criteria
#'
#' @param cols tibble of complete columns
#'
#' @return tibble
#' @export
eliminate <- function(cols) {
  # first two rows
  for (i in 1:2) {
    cols[i, sample(1:9, size = 4)] <- NA
  }
  # third row
  cols[3, sample(which(!apply(is.na(cols[1:2, ]), 2, all)), size = 4)] <- NA
  # output modified data
  cols
}

#' Generate one card
#'
#' @return tibble
#' @export
#'
#' @examples
#' generate()
generate <- function() {
  out <- seq(0, 8) |>
    purrr::map(full_col) |>
    dplyr::bind_cols(.name_repair = "unique_quiet") |>
    eliminate()
  structure(out, class = c("banko", class(out)))
}


#' Creates n unique cards
#'
#' @param n number of cards
#' @param seed integer seed. Default is NULL. The used seed is saved as attribute.
#'
#' @return list
#' @export
#'
#' @examples
#' cards(5) |>
#'   purrr::map(\(.x) attr(.x, which = "banko_seed")) |>
#'   unique()
#' attr(cards(5), which = "banko_seed")
cards <- function(n, seed = NULL) {
  if (is.null(seed)) seed <- abs(sample(.Random.seed, 1))

  set.seed(seed)

  l <- list()

  # Repeats until n unique in list
  repeat{
    # Generates new card
    p <- structure(generate(), banko_seed = seed)

    # Tests if unique compared to rest in list before appending
    if (is_unique_card(p, l)) {
      l[[length(l) + 1]] <- p
    }

    # Breaks when l has length n
    if ((length(l) == n)) {
      break
    }
  }
  # outputs unique cards
  structure(l,
    banko_seed = seed,
    class = c("banko_list", class(l))
  )
}


#' Test if card is unique compared to list of cards
#'
#' @param p new card
#' @param l list of cards
#'
#' @return logical
#' @export
#'
#' @examples
#' is_unique_card(c(1, 2, NA), list(c(2, 4, 6), c(1, NA, 2)))
is_unique_card <- function(p, l) {
  ## Tests only full sequence
  l |>
    purrr::map(get_sequence) |>
    purrr::map_lgl(\(.x){
      identical(get_sequence(p), .x)
    }) |>
    (\(.x) !any(.x))()
}

#' Convert df/tibble to vector. Option to remove NAs
#'
#' @param data data frame, tibble or vector.
#' @param no_nas remove NAs. Default is TRUE
#'
#' @return vector
#' @export
#'
#' @examples
#' data <- c(1, 2, NA)
#' get_sequence(c(1, NA, 2))
#' get_sequence(c(1, 2, NA), FALSE)
get_sequence <- function(data, no_nas = TRUE) {
  # To test completely unique, compare sequence without omitting
  out <- data |>
    as.matrix() |>
    as.vector()

  if (no_nas) {
    out[which(!is.na(out))]
    # Not using na.omit(), as this appends attributes
  } else {
    out
  }
}


#' Extract unique numbers from cards and mix
#'
#' @param cards banko cards
#'
#' @return vector
#' @export
#'
#' @examples
#' cards(20) |> unique_numbers()
unique_numbers <- function(cards) {
  ns <- cards |>
    purrr::map(\(.x) get_sequence(.x, no_nas = TRUE)) |>
    purrr::list_c() |>
    unique()

  shuffle_seq(ns)
}

#' Randomly shuffles the order of a vector
#'
#' @param data vector
#'
#' @returns vector
#' @export
#'
#' @examples
#' 1:10 |> shuffle_seq()
shuffle_seq <- function(data) {
  sample(data, size = length(data), replace = FALSE)
}

generate_bingo <- function(dim = c(2, 3), base = 1:90, sort = FALSE) {
  out <- sample(base, prod(dim))
  if (isTRUE(sort)) {
    out <- sort(out)
  }
  structure(matrix(out, nrow = dim[1]), class = c("banko", class(out)))
}

#' Create unique bingo cards with specified dimensions and custom base
#'
#' @param n
#' @param dim
#' @param base
#' @param sort
#' @param seed
#'
#' @returns
#' @export
#'
#' @examples
#' bingo(2, base = 1:15, sort = TRUE)
#' bingo(2, base = list.files("/Users/au301842/Library/CloudStorage/OneDrive-Personal/Research/PhD/Formidling/Pictograms", full.names = TRUE))
bingo <- function(n, dim = c(2, 3), base = 1:90, sort = FALSE, seed = NULL) {
  if (is.null(seed)) seed <- abs(sample(.Random.seed, 1))

  set.seed(seed)
  # browser()

  l <- list()

  stopifnot(length(base) / prod(dim) > 2)

  # Repeats until n unique in list
  repeat{
    # Generates new card
    p <- structure(generate_bingo(dim = dim, base = base, sort = sort), banko_seed = seed)

    # Tests if unique compared to rest in list before appending
    if (is_unique_card(p, l)) {
      l[[length(l) + 1]] <- p
    }

    # Breaks when l has length n
    if ((length(l) == n)) {
      break
    }
  }
  # outputs unique cards
  structure(l,
    banko_seed = seed,
    class = c("banko_list", class(l))
  )
}

#' Title
#'
#' @param n
#' @param dim
#' @param base
#' @param sort
#' @param seed
#'
#' @returns
#' @export
#'
#' @examples
#' memory(n = 2)
#' memory(base = list.files("/Users/au301842/Library/CloudStorage/OneDrive-Personal/Research/PhD/Formidling/Pictograms", full.names = TRUE))
memory <- function(n = NULL, dim = c(2, 3), base = 1:90, sort = FALSE, seed = NULL) {
  if (is.null(seed)) seed <- abs(sample(.Random.seed, 1))

  set.seed(seed)
  ## Splitting in groups
  out <- chunks_of_n(shuffle_seq(base), n = prod(dim), even = FALSE)
  ## Removing last if not to size
  out <- out[lapply(out, length) == prod(dim)]

  ## Applying dims and class
  out <- lapply(out, \(.x){
    structure(matrix(.x, nrow = dim[1]), class = c("banko", class(.x)))
  })

  if (isTRUE(sort)) {
    out <- lapply(out, sort)
  }

  if (!is.null(n)) {
    out <- out[seq_len(n)]
  }

  structure(out,
    banko_seed = seed,
    class = c("banko_list", class(out))
  )
}
