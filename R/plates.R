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


#' Eliminate elements in columns to apply to plates criteria
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

#' Generate one plate
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


#' Creates n random plates. No checking of uniqueness
#'
#' @param n number of plates
#' @param seed integer seed. Default is NULL. The used seed is saved as attribute.
#'
#' @return list
#' @export
#'
#' @examples
#' plates(5) |> purrr::map(\(.x) attr(.x,which = "banko_seed")) |> unique()
#' attr(plates(5),which = "banko_seed" )
plates <- function(n,seed=NULL) {
  if (is.null(seed)) seed <- abs(sample(.Random.seed,1))

  set.seed(seed)

  l <- list()

  # Repeats until n unique in list
  repeat{
    # Generates new plate
    p <- structure(generate(),banko_seed=seed)

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
  structure(l,banko_seed=seed,
            class=c("banko_list",class(l)))
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
#' is_unique_plate(c(1, 2, NA), list(c(2, 4, 6), c(1, NA, 2)))
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


#' Extract unique numbers from plates and mix
#'
#' @param plates banko plates
#'
#' @return vector
#' @export
#'
#' @examples
#' plates(20) |> unique_numbers()
unique_numbers <- function(plates) {
  ns <- plates |>
    purrr::map(\(.x) get_sequence(.x, no_nas = TRUE)) |>
    purrr::list_c() |>
    unique()

  sample(ns, size = length(ns), replace = FALSE)
}
