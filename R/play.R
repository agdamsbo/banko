#' Extracts shortest sequence of numbers to get at least one complete row on
#' each card
#'
#' @description
#' This can be used to draw a sequence before the game to ensure all will have
#' at least one row.
#'
#' Could be used for travebanko with FDF, the scouts or others.
#'
#' @param data list of cards
#' @param g number of subsets to test
#' @param selection selection strategy for sequence. Can be "min" or "random".
#'
#' @return list with list and numeric vector
#' @export
#'
#' @examples
#' cards(20) |>
#'   sequence4one() |>
#'   length()
sequence4one <- function(data, g = 100, selection="min") {
  # In the case of small number of cards, just use all possible combinations to test
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
    if (is_unique_card(p, l)) {
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

  seq.lengths <- seq.test |>
    lengths()

  if (selection=="min"){
    index <- seq.lengths |>
      which.min()
  } else if (selection=="random"){
    index <- 1
  } else {
    stop("Selection strategy has to be either 'min' or 'random'.")
  }

  list(cards=data,sequence=seq.test[[index]])
}


#' Number of complete rows in each card from sequence
#'
#' @param cards banko cards
#' @param sequence number sequence
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' cards(10) |>
#' sequence4one() |>
#' (\(.x) n_complete_rows(.x[[1]], .x[[2]]))()
#'
#' n_complete_rows(cards=cards(40)) |> factor() |> summary()
n_complete_rows <- function(cards, sequence=NULL) {
  if (is.null(sequence)) {
    sequence <- sequence4one(cards) |>
      purrr::pluck("sequence")
    }
  cards |> purrr::map(\(.x){
    apply(.x, 1, get_sequence) |>
      apply(2, \(.y) {
        .y %in% sequence |>
          all()
      }) |>
      sum()
  }) |>
    purrr::list_c()
}


#' Gives number of correct fields on each card from sequence
#'
#' @param cards banko cards
#' @param sequence number sequence
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' cards <- cards(5)
n_each_card <- function(cards, sequence=NULL) {
  if (is.null(sequence)) {
    sequence <- sequence4one(cards) |>
      purrr::pluck("sequence")
  }
  cards |> purrr::map(\(.x){
    get_sequence(.x) |>
      (\(.y) {
        .y %in% sequence
      })() |>
      sum()
  }) |>
    purrr::list_c()
}
