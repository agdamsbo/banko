#' Extracts shortest sequence of numbers to get at least one complete row on
#' each plate
#'
#' @description
#' This can be used to draw a sequence before the game to ensure all will have
#' at least one row.
#'
#' Could be used for travebanko with FDF, the scouts or others.
#'
#' @param data list of plates
#' @param g number of subsets to test
#'
#' @return list with list and numeric vector
#' @export
#'
#' @examples
#' plates(20) |>
#'   sequence4one() |>
#'   length()
sequence4one <- function(data, g = 100) {
  # In the case of small number of plates, just use all possible combinations to test
  if ((3^length(data)) < g) {
    g <- 3^length(data)
  }

  # All combination would be in expand.grid(), but this is quickly very heavy.
  # Instead, g random samples of row subsets are drawn to find the shortest
  # sequence of numbers.

  l <- list()
  # Repeats until g unique in list
  repeat{
    # Generates new rows index vector
    p <- sample(seq_len(nrow(data[[1]])),
      size = length(data),
      replace = TRUE
    )

    # Tests if unique compared to rest in list before appending
    if (is_unique_plate(p, l)) {
      l[[length(l) + 1]] <- p
    }

    # Breaks when l has length g
    if ((length(l) == g)) {
      break
    }
  }

  seq.test <- l |>
    purrr::map(\(.x){
      .x |>
        purrr::imap(\(.y, .i){
          data[[.i]][.y, ] |>
            get_sequence()
        }) |>
        purrr::list_c() |>
        unique()
    })

  min.index <- seq.test |>
    lengths() |>
    which.min()

  list(plates=data,sequence=seq.test[[min.index]])
}


#' Number of complete rows in each plate from sequence
#'
#' @param plates banko plates
#' @param sequence number sequence
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' plates(10) |>
#' sequence4one() |>
#' (\(.x) n_complete_rows(.x[[1]], .x[[2]]))()
#'
#' n_complete_rows(plates=plates(40)) |> factor() |> summary()
n_complete_rows <- function(plates, sequence=NULL) {
  if (is.null(sequence)) sequence <- sequence4one(plates)
  plates |> purrr::map(\(.x){
    apply(.x, 1, get_sequence) |>
      apply(2, \(.y) {
        .y %in% sequence |>
          all()
      }) |>
      sum()
  }) |>
    purrr::list_c()
}
