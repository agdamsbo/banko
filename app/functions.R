

########

#### Current file: R//cards.R 

########

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
#' cards(5) |> purrr::map(\(.x) attr(.x,which = "banko_seed")) |> unique()
#' attr(cards(5),which = "banko_seed" )
cards <- function(n,seed=NULL) {
  if (is.null(seed)) seed <- abs(sample(.Random.seed,1))

  set.seed(seed)

  l <- list()

  # Repeats until n unique in list
  repeat{
    # Generates new card
    p <- structure(generate(),banko_seed=seed)

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
  structure(l,banko_seed=seed,
            class=c("banko_list",class(l)))
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

  sample(ns, size = length(ns), replace = FALSE)
}


########

#### Current file: R//export.R 

########

utils::globalVariables(c("text", "x1", "x2", "y1", "y2"))
#' Base R style plotting
#'
#' @param x "banko" class object
#'
#' @return plot
#' @export
#'
#' @examples
#' cards(5) |> purrr::map(plot)
#'
#' cards(5)[[1]] |> plot()
plot.banko <- function(x, ...) {
  old <- graphics::par(pty = "s", mar = c(0, 0, 0, 0))
  ncol <- dim(x)[2]
  nrow <- dim(x)[1]
  on.exit(graphics::par(old))

  graphics::plot(
    c(0, 9),
    c(0, -9),
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  graphics::rect(
    col(x) - 1,
    -row(x) + 1,
    col(x),
    -row(x),
    col = NULL,
    border = NULL
  )
  graphics::text(col(x) - 0.5,
    -row(x) + 0.5,
    get_sequence(x, no_nas = FALSE),
    cex = 2,
    col = "black",
  )
}

#' Print banko cards with ggplot2
#'
#' @param data "banko" class object
#' @param text.size size for number. Default is 14.
#' @param title optional title for all cards
#' @param note optional note for all cards
#' @param id.hash add md5 hash to notes section
#'
#' @return ggplot2 list object
#' @export
#'
#' @examples
#' data <- cards(5)[[1]]
#' data |> gg_card()
#' cards(5) |>
#'   purrr::map(gg_card) |>
#'   patchwork::wrap_plots(ncol = 1)
gg_card <- function(data, text.size = 14, title = NULL, note = NULL, id.hash = FALSE) {
  assertthat::assert_that("banko" %in% class(data))

  d <- tibble::tibble(
    text = get_sequence(data, no_nas = FALSE),
    x1 = rep(0:8, each = 3),
    x2 = rep(1:9, each = 3),
    y1 = rep(2:0, times = 9),
    y2 = rep(3:1, times = 9)
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = d,
      ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
      color = "black",
      alpha = 0
    ) +
    ggplot2::geom_text(
      data = d,
      ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = text),
      size = text.size, na.rm = TRUE
    ) +
    ggplot2::theme_void()

  if (!is.null(title)) {
    t <- tibble::tibble(
      text = title,
      x1 = 0,
      x2 = 9,
      y1 = 3,
      y2 = 3.7
    )
    p <- p +
      ggplot2::geom_rect(
        data = t,
        ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
        color = "black", alpha = 0
      ) +
      ggplot2::geom_text(
        data = t,
        ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = text),
        size = text.size
      )
  }

  if (id.hash) {
    note <- paste0(note, ". id: ", digest::digest(d, algo = "md5"))
  }


  if (!is.null(title)) {
    t <- tibble::tibble(
      text = note,
      x1 = 0,
      x2 = 9,
      y1 = -.3,
      y2 = 0
    )
    p <- p +
      ggplot2::geom_rect(
        data = t,
        ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
        color = "black", alpha = 0
      ) +
      ggplot2::geom_text(
        data = t,
        ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = text),
        size = text.size / 2
      )
  }

  structure(p,
    class = c("gg_card", class(p)),
    banko_seed = attr(data, which = "banko_seed")
  )
}

#' Iterative sequence
#'
#' @param i iteration
#' @param n sequence size
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' for (i in 1:2) print(seq_iter(i, 5))
seq_iter <- function(i, n) {
  seq((i - 1) * n + 1, i * n)
}

#' Multipage multicard PDF export
#'
#' @param data list of gg_card
#' @param n number of cards per page
#' @param note note to print on
#'
#' @return list
#' @export
#'
#' @examples
#' # Not evaluated
#' # cards(20) |>
#' # purrr::map(gg_card) |>
#' # cards_grob() |>
#' # export_pdf(path = "banko.pdf")
cards_grob <- function(data,
                        n = 5,
                        note = "agdamsbo/banko") {
  assertthat::assert_that(
    "gg_card" %in% (purrr::map(data, class) |> purrr::list_c())
  )

  pl <- list()

  for (i in seq_len(ceiling(length(data) / n))) {
    pl[[i]] <- gridExtra::arrangeGrob(
      grobs = data[seq_iter(i, n)],
      ncol = 1, nrow = n,
      left = grid::textGrob(glue::glue(note), gp = grid::gpar(fontsize = 10), rot = 90, vjust = 2)
    )
  }

  structure(pl, class = c("arrangelist", class(data)))
}

#' Travebanko grob merge
#'
#' @param data banko cards
#' @param stops stops to make posters for
#' @param post.footer note for footer
#'
#' @return list
#' @export
#'
#' @examples
#' l <- cards(30, 5) |> travebanko(stops = 8)
#' l |> export_pdf()
travebanko <- function(data,
                       stops,
                       post.footer = "Please make a note when they were put up and taken down, if in public.") {
  cards_list <- data |>
    sequence4one()


  front <- cards_list |>
    (\(.x){
      stats_walk(.x[[1]], .x[[2]], stops = stops)
    })()

  signs <- cards_list[[2]] |> stops_walk(stops = stops)

  signs_grob <- signs |>
    purrr::map(\(.x){
      l <- purrr::map2(.x, c(60, 50), \(.y, .i){
        grid::textGrob(.y, gp = grid::gpar(fontsize = .i))
      })
      gridExtra::arrangeGrob(
        grobs = l,
        ncol = 1,
        bottom = grid::textGrob(glue::glue(post.footer), gp = grid::gpar(fontsize = 10), vjust = -4)
      )
    }) |>
    (\(.x) structure(.x, class = c("arrangelist", class(.x))))()

  structure(
    list(
      front |> grid::textGrob(),
      signs_grob,
      data |> purrr::map(gg_card) |> cards_grob()
    ),
    class = c("arrangelist", class(data)),
    banko_seed = attr(data, which = "banko_seed")
  )
}



#' Travebanko stats
#'
#' @param stops n stops
#' @param cards banko cards list.
#' @param sequence numeric vector of sequence drawn.
#'
#' @return list
#' @export
#'
#' @examples
#' cards(10) |> sequence4one() |>
#'  (\(.x){
#'    stats_walk(.x[[1]], .x[[2]], stops = 2)
#'  })()
stats_walk <- function(cards, sequence, stops) {
  seed <- cards |> attr(which = "banko_seed")

  summary_str <- n_complete_rows(cards, sequence) |>
    factor(levels = 1:3, labels = c("One row", "Two rows", "Full card")) |>
    summary() |>
    (\(.x){
      paste(paste(names(.x), .x, sep = ": "), collapse = ", ")
    })()

  glue::glue(
    "God tur med travebanko!\n\n
Her er {length(cards)} plader og {stops} poster.\n
Spillet er designet til at alle har mindst en hel raekke.\n
Gendan med koden her (seed): {seed}\n\n
Vindere: {summary_str}\n\n
Tal paa poster: \n {split_seq(sequence,l=15) |> purrr::map(paste,collapse=' ') |> purrr::list_c() |> paste(collapse='\n')}
"
  )
}

#' Travebanko stops
#'
#' @param sequence numeric vector of sequence drawn.
#' @param stops n stops
#'
#' @return list
#' @export
#'
#' @examples
#' data <- cards_list
stops_walk <- function(sequence, stops) {
  split_seq(sequence, n = stops) |>
    purrr::imap(\(.x, .i){
      list(
        header = glue::glue("Post nr {.i}\n\n"),
        numbers = split_seq(.x, l = 5) |> purrr::map(\(.y)paste(.y, collapse = "   ")) |>
          glue::glue_collapse(sep = "\n")
      )
    })
}

#' Splits sequence in n groups of equal size or groups of max l
#'
#' @param sequence vector
#' @param n number of groups. Ignored if l is specified.
#' @param l max length of groups
#' @param split.labels optional label for groups
#'
#' @return list
#' @export
#'
#' @examples
#' split_seq(1:8, l = 3)
split_seq <- function(sequence, n = NULL, l = NULL, split.labels = NULL) {
  if (!is.null(l)) n <- ceiling(length(sequence) / l)

  if (is.null(split.labels)) split.labels <- seq_len(n)

  split(
    sequence,
    cut(seq_along(sequence),
      breaks = n,
      labels = split.labels
    )
  )
}


#' Print list of grobs as PDF
#'
#' @param list list of grobs to print
#' @param path output path. glue string
#' @param paper paper format. Passed to "pdf" engine.
#'
#' @return NULL
#' @export
export_pdf <- function(list,
                       path = "banko_{attr(list, which = 'banko_seed')}.pdf",
                       paper="a4") {

  ggplot2::ggsave(glue::glue(path),
    list,
    device = "pdf",
    title = "agdamsbo/banko",
    paper=paper
  )
}


########

#### Current file: R//play.R 

########

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
