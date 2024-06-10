#' Genereate complete columns
#'
#' @param col column index
#'
#' @return numeric vector
#'
#' @examples
#' generate_full_col(col = 3)
generate_full_col <- function(col) {
  min_num <- ifelse(col == 0, 1, 0)
  max_num <- ifelse(col == 8, 10, 9)

  sort(sample(seq(min_num, max_num), size = 3)) + col * 10
}


#' Eliminate elements in columns to fullfill criteria for plates
#'
#' @param cols tibble of complete columns
#'
#' @return
#' @export
#'
#' @examples
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

#' Generate one plate
#'
#' @return tibble
#' @export
#'
#' @examples
generate <- function() {
  seq(0, 8) |>
    purrr::map(generate_full_col) |>
    dplyr::bind_cols(.name_repair = "unique_quiet") |>
    eliminate()
}


#' Creates n random plates. No checking of uniqueness
#'
#' @param n number of plates
#'
#' @return list
#' @export
#'
#' @examples
#' plates(5)
plates <- function(n) {
  l <- list()

  # Repeats until n unique in list
  repeat{
    if (length(l) == 0) {
      # Just creates first
      # This is easier on reading, but function actually handles empty list
      # in comparison step
      l[[1]] <- generate()
    }

    # Generates new plate
    p <- generate()

    # Tests if unique compared to rest in list before appending
    if (is_unique_plate(p, l)) {
      l[[length(l) + 1]] <- p
    }

    # Breaks when l has length n
    if ((length(l) == n)) {
      break
    }
  }
  # outputs unique plates
  l
}


#' Test if plate is unique compared to list of plates
#'
#' @param p new plate
#' @param l list of plates
#'
#' @return logical
#' @export
#'
#' @examples
#' is_unique_plate(c(1,2,NA),list(c(2,4,6),c(1,NA,2)))
is_unique_plate <- function(p, l) {
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
#' @return
#' @export
#'
#' @examples
#' data <- c(1,2,NA)
#' get_sequence(c(1,NA,2))
#' get_sequence(c(1,2,NA),FALSE)
get_sequence <- function(data,no_nas=TRUE) {
  # To test completely unique, compare sequence without omitting
  out <- data |>
    as.matrix() |>
    as.vector()

  if (no_nas){
    out[which(!is.na(out))]
    # Not using na.omit(), as this appends attributes
  } else {
    out
  }
}

# Source: https://github.com/skipperkongen/banko/blob/main/src/banko/algorithms.py
