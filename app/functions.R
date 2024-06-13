########
#### Current file: R//cards.R
########








full_col <- function(col) {
  min <- ifelse(col == 0, 1, 0)
  max <- ifelse(col == 8, 10, 9)

  sort(sample(seq(min, max), size = 3)) + col * 10
}








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








generate <- function() {
  out <- seq(0, 8) |>
    purrr::map(full_col) |>
    dplyr::bind_cols(.name_repair = "unique_quiet") |>
    eliminate()
  structure(out, class = c("banko", class(out)))
}













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












is_unique_card <- function(p, l) {
  ## Tests only full sequence
  l |>
    purrr::map(get_sequence) |>
    purrr::map_lgl(\(.x){
      identical(get_sequence(p), .x)
    }) |>
    (\(.x) !any(.x))()
}













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











seq_iter <- function(i, n) {
  seq((i - 1) * n + 1, i * n)
}
















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









export_pdf <- function(list,
                       path = "banko_{attr(list, which = 'banko_seed')}.pdf") {
  ggplot2::ggsave(glue::glue(path),
    list,
    device = "pdf",
    title = "agdamsbo/banko",
    paper = "a4",
    create.dir = TRUE,
    width = 210,
    height = 297,
    units = "mm"
  )
}


########
#### Current file: R//play.R
########





















sequence4one <- function(data, g = 100, selection = "min") {
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

  if (selection == "min") {
    index <- seq.lengths |>
      which.min()
  } else if (selection == "random") {
    index <- 1
  } else {
    stop("Selection strategy has to be either 'min' or 'random'.")
  }

  list(cards = data, sequence = seq.test[[index]])
}
















n_complete_rows <- function(cards, sequence = NULL) {
  if (is.null(sequence)) {
    sequence <- sequence4one(cards) |>
      purrr::pluck("sequence")
  }
  cards |>
    purrr::map(\(.x){
      apply(.x, 1, get_sequence) |>
        apply(2, \(.y) {
          .y %in% sequence |>
            all()
        }) |>
        sum()
    }) |>
    purrr::list_c()
}












n_each_card <- function(cards, sequence = NULL) {
  if (is.null(sequence)) {
    sequence <- sequence4one(cards) |>
      purrr::pluck("sequence")
  }
  cards |>
    purrr::map(\(.x){
      get_sequence(.x) |>
        (\(.y) {
          .y %in% sequence
        })() |>
        sum()
    }) |>
    purrr::list_c()
}
