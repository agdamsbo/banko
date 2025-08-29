utils::globalVariables(c("text", "x1", "x2", "y1", "y2"))
#' Base R style plotting
#'
#' @param x "banko" class object
#' @param ... not used. Conforms to standard docs for plot extensions
#'
#' @return plot
#' @export
#'
#' @examples
#' cards(5) |> purrr::map(plot)
#'
#' cards(1)[[1]] |> plot()
plot.banko <- function(x, ...) {
  old <- graphics::par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(graphics::par(old))
  # browser()
  graphics::plot(
    c(0, col(x)),
    c(0, -col(x)),
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
  graphics::text(
    col(x) - 0.5,
    -row(x) + 0.5,
    get_sequence(x, no_nas = FALSE),
    cex = 2,
    col = "black",
  )
}

#' Prepare matrix for gg_card
#'
#' @param data any object with two dimensions (data.frame or matrix)
#'
#' @returns tibble
#' @export
#'
#' @examples
#' matrix(1:10, ncol = 5) |> prepare_gg_card()
#' matrix(seq(2 * 3), ncol = 2) |> prepare_gg_card()
prepare_gg_card <- function(data) {
  dim <- dim(data)
  stopifnot(length(dim) == 2)

  height <- dim[1]
  width <- dim[2]

  tibble::tibble(
    key = as.character(get_sequence(data, no_nas = FALSE)),
    x1 = rep(seq(width) - 1, each = height),
    x2 = rep(seq(width), each = height),
    y1 = rep(rev(seq(height)) - 1, times = width),
    y2 = rep(rev(seq(height)), times = width)
  )
}

base_gg_card <- function(data,text.size = 14){
  stopifnot(all(c("key","x1","x2","y1","y2")%in%names(data)))

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2,xmin = x1, xmax = x2, ymin = y1, ymax = y2)
  )

  if ("presence" %in% names(data)){
    p <- p +
      ggplot2::geom_rect(
        ggplot2::aes(fill = presence),
        alpha = .5, color = "black"
      )+
      ggplot2::guides(fill = "none")
  }

  p <- p +
    ggplot2::geom_rect(
      # ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
      color = "black",
      alpha = 0
    )

  if (any(grepl("*[\\.svg|\\.png]$", data$key))) {
    p <- p +
      geom_image(ggplot2::aes(image = key), size = text.size / 50)
  } else {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = key),
        size = text.size,
        na.rm = TRUE
      )
  }

  p +
    ggplot2::theme_void()
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
#'   purrr::map(gg_card)
#' cards(5) |>
#'   lapply(gg_card)
#' bingo(5, base = LETTERS)[[1]] |> gg_card()
#' bingo(2, base = list.files("/Users/au301842/Library/CloudStorage/OneDrive-Personal/Research/PhD/Formidling/Pictograms", full.names = TRUE))[[1]] |> gg_card()
#' bingo(2, base = LETTERS, dim = c(2, 5))[[1]] |> gg_card()
#' memory()[[1]] |> gg_card()
gg_card <- function(data, text.size = 14, title = NULL, note = NULL, id.hash = FALSE) {
  assertthat::assert_that("banko" %in% class(data))
  # browser()
  d <- prepare_gg_card(data)

  p <- base_gg_card(d,text.size=text.size)

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
        ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = key),
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
        ggplot2::aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2, label = key),
        size = text.size / 2
      )
  }
  # browser()
  structure(p,
    class = c("gg_card", class(p)),
    banko_seed = attr(data, which = "banko_seed")
  )
}

#' Extend sequence with specified fill to match target length
#'
#' @param data vector
#' @param target numeric
#' @param fill vector
#'
#' @returns vector
#' @export
#'
#' @examples
#' extend_seq(1:3, 8)
extend_seq <- function(data, target, fill = NA) {
  # browser()
  if (length(data) < target) {
    c(data, rep(fill, target - length(data)))[seq(target)]
  } else {
    data
  }
}

#' Plot sample of base sequence as binary heatmap
#'
#' @param data numeric vector
#' @param text.size text size. Default 14.
#' @param ncol number of columns to plot. Defaults to ceiling(sqrt(length(base.seq)))
#' @param base.seq numeric vector to plot. Default is 1:90
#' @param colors colors for heatmap. Defaults is c("white","grey").
#'
#' @return ggplot2 list object
#' @export
#'
#' @examples
#' cards(5)[[1]] |>
#'   get_sequence() |>
#'   gg_seq_heat(colors = c("white", "grey"))
#' c() |> gg_seq_heat(colors = c("white", "grey"), base.seq = sample(1:100, 25))
#' c() |> gg_seq_heat(colors = c("white", "grey"), base.seq = LETTERS)
gg_seq_heat <- function(data,
                        text.size = 14,
                        ncol = NULL,
                        base.seq = 1:90,
                        colors = c("white", "grey")) {
  if (is.null(ncol)) {
    ncol <- ceiling(sqrt(length(base.seq)))
  }

  nrow <- ceiling(length(base.seq) / ncol)

  d <- prepare_gg_card(matrix(extend_seq(base.seq, prod(ncol, nrow)),ncol=ncol)) |>
    dplyr::mutate(presence = key %in% data)

  p <- base_gg_card(d)

  if (!is.null(colors)) {
    p <- p +
      ggplot2::scale_fill_manual(values = colors)
  }

  structure(p,
    class = c("gg_seq_heat", class(p))
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
#' cards(5) |>
#'   purrr::map(gg_card) |>
#'   cards_grob() |>
#'   export_pdf(path = "banko.pdf")
#'
#' bingo(5,
#'   base = list.files("/Users/au301842/Library/CloudStorage/OneDrive-Personal/Research/PhD/Formidling/Pictograms",
#'     full.names = TRUE
#'   )
#' ) |>
#'   purrr::map(gg_card) |>
#'   cards_grob() |>
#'   export_pdf(path = "bingo.pdf")
#'
#' memory(
#'   n = 10,
#'   base = list.files("/Users/au301842/Library/CloudStorage/OneDrive-Personal/Research/PhD/Formidling/Pictograms",
#'     full.names = TRUE
#'   )
#' ) |>
#'   purrr::map(gg_card) |>
#'   cards_grob() |>
#'   export_pdf(path = "memory.pdf")
#'
#' memory(
#'   n = 10,
#'   base = list.files("/Users/au301842/Downloads/twemoji-color-font-main/assets/twemoji-svg",
#'     full.names = TRUE
#'   )
#' ) |>
#'   purrr::map(gg_card) |>
#'   cards_grob() |>
#'   export_pdf(path = "memory_emoji.pdf")
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

# gg2grob <- function(data, ...) {
#   structure(ggplot2::ggplotGrob(data, ...), class = c("arrangelist", class(data)))
# }

#' Travebanko grob merge
#'
#' @param data banko cards
#' @param stops stops to make posters for
#' @param post.footer note for footer. Available values for the string are:
#' sign.index (current sign/stop), stops (number of stops) and the supplied
#' cards list (data).
#' @param post.header header glue string for signs/posts. Available values for
#' the string are: sign.index (current sign/stop) and stops (number of stops).
#'
#' @return list
#' @export
#'
#' @examples
#' # data <- cards(30, 5)
#' # l <- cards(30, 5) |> travebanko(stops = 8)
#' # l |> export_pdf()
travebanko <- function(data,
                       stops,
                       post.footer = "Post {sign.index} of {stops}. Put up on {format(Sys.Date(),'%d-%m-%Y')}.",
                       post.header = "Post {sign.index}") {
  # browser()
  cards_list <- data |>
    sequence4one()

  heat <- cards_list[["sequence"]] |>
    gg_seq_heat() |>
    ggplot2::ggplotGrob()

  front <- cards_list |>
    (\(.x){
      stats_walk(.x[["cards"]], .x[["sequence"]], stops = stops)
    })()

  signs <- cards_list[[2]] |> stops_walk(stops = stops, header = post.header)

  signs_grob <- signs |>
    purrr::imap(\(.x, sign.index){
      l <- purrr::map2(
        .x,
        list(c(70, "bold"), c(50, "plain")) |>
          purrr::map(\(.y)stats::setNames(.y, c("size", "weight"))),
        \(.y, .i){
          grid::textGrob(.y,
            gp = grid::gpar(
              fontsize = .i[["size"]],
              fontface = .i[["weight"]]
            )
          )
        }
      )
      gridExtra::arrangeGrob(
        grobs = l,
        ncol = 1,
        bottom = grid::textGrob(glue::glue(post.footer), gp = grid::gpar(fontsize = 10), vjust = -4)
      )
    }) |>
    (\(.x) structure(.x, class = c("arrangelist", class(.x))))()

  structure(
    list(
      front |> grid::textGrob() |> list(),
      heat |> list(),
      signs_grob,
      data |> purrr::map(gg_card) |> cards_grob()
    ) |> unlist(recursive = FALSE),
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
#' cards(10) |>
#'   sequence4one() |>
#'   (\(.x){
#'     stats_walk(.x[[1]], .x[[2]], stops = 2)
#'   })()
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
#' @param header header glue string
#'
#' @return list
#' @export
#'
#' @examples
#' cards(10) |>
#'   sequence4one() |>
#'   (\(.x){
#'     stops_walk(.x[[2]], stops = 2)
#'   })()
#' cards(2) |>
#'   sequence4one() |>
#'   (\(.x){
#'     stops_walk(.x[[2]], stops = 10)
#'   })()
stops_walk <- function(sequence, stops, header = "Post {sign.index}") {
  ls <- split_seq(sequence, n = stops)

  ## Adds 99 if the leement has length 0. Edge case for few cards and many stops.
  ls[lengths(ls) == 0] <- 99

  ls |>
    purrr::imap(\(.x, sign.index){
      list(
        header = glue::glue(header),
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
#' split_seq(sequence = 1:5, l = 12)
split_seq <- function(sequence, n = NULL, l = NULL, split.labels = NULL) {
  if (!is.null(l)) n <- ceiling(length(sequence) / l)

  if (is.null(split.labels)) split.labels <- seq_len(n)

  # cut() fails on n=1, this avoids that.
  if (n == 1) {
    splitter.f <- factor(rep(1, length(sequence)))
  } else {
    splitter.f <- cut(seq_along(sequence),
      breaks = n,
      labels = split.labels
    )
  }

  split(
    sequence,
    splitter.f
  )
}


#' Print list of grobs as PDF
#'
#' @param list list of grobs to print
#' @param path output path. glue string
#'
#' @return NULL
#' @export
export_pdf <- function(list,
                       path = "banko_{attr(list, which = 'banko_seed')}.pdf") {
  grDevices::pdf(file = NULL)
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
