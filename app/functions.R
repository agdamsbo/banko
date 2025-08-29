

########
#### Current file: R/cards.R 
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

  shuffle_seq(ns)
}










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





















bingo <- function(n, dim = c(2, 3), base = 1:90, sort = TRUE, seed = NULL) {
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


########
#### Current file: R/export.R 
########

utils::globalVariables(c("text", "x1", "x2", "y1", "y2"))












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












extend_seq <- function(data, target, fill = NA) {
  # browser()
  if (length(data) < target) {
    c(data, rep(fill, target - length(data)))[seq(target)]
  } else {
    data
  }
}


















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

# gg2grob <- function(data, ...) {
#   structure(ggplot2::ggplotGrob(data, ...), class = c("arrangelist", class(data)))
# }


















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


########
#### Current file: R/geom_image.R 
########

library(ggplot2)
library(magick)
library(grid)
library(grDevices)



##' key drawing function
##'
##'
##' @name draw_key
##' @param data A single row data frame containing the scaled aesthetics to display in this key
##' @param params A list of additional parameters supplied to the geom.
##' @param size Width and height of key in mm
##' @return A grid grob
NULL


ggname <- getFromNamespace("ggname", "ggplot2")

##' @rdname draw_key
##' @importFrom grid rectGrob
##' @importFrom grid pointsGrob
##' @importFrom grid gpar
##' @importFrom scales alpha
##' @export
draw_key_image <- function(data, params, size) {
  kt <- getOption("ggimage.keytype")
  if (is.null(kt)) {
    kt <- "point"
  }

  if (kt == "point") {
    keyGrob <- pointsGrob(
      0.5, 0.5,
      pch = 19,
      gp = gpar (
        col = alpha(data$colour, data$alpha),
        fill = alpha(data$colour, data$alpha),
        fontsize = 3 * ggplot2::.pt,
        lwd = 0.94
      )
    )
  } else if (kt == "rect") {
    keyGrob <- rectGrob(gp = gpar(
      col = NA,
      fill = alpha(data$colour, data$alpha)
    ))
  } else if (kt == "image") {
    img <- image_read(system.file("extdata/Rlogo.png", package="ggimage"))
    grobs <- lapply(seq_along(data$colour), function(i) {
      img <- color_image(img, data$colour[i], data$alpha[i])

      rasterGrob(
        0.5, 0.5,
        image = img,
        width = 1,
        height = 1
      )
    })
    class(grobs) <- "gList"

    keyGrob <- ggname("image_key",
                      gTree(children = grobs))
  }
  return(keyGrob)
}


##' geom layer for visualizing image files
##'
##'
##' @title geom_image
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param nudge_x horizontal adjustment to nudge image
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' \dontrun{
##' library("ggplot2")
##' library("ggimage")
##' set.seed(2017-02-21)
##' d <- data.frame(x = rnorm(10),
##'                 y = rnorm(10),
##'                 image = sample(c("https://www.r-project.org/logo/Rlogo.png",
##'                                 "https://jeroenooms.github.io/images/frink.png"),
##'                               size=10, replace = TRUE)
##'                )
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image))
##' }
##' @author Guangchuang Yu
geom_image <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", inherit.aes=TRUE,
                       na.rm=FALSE, by="width", nudge_x = 0, ...) {

  by <- match.arg(by, c("width", "height"))

  layer(
    data=data,
    mapping=mapping,
    geom=GeomImage,
    stat=stat,
    position=position,
    show.legend=NA,
    inherit.aes=inherit.aes,
    params = list(
      na.rm = na.rm,
      by = by,
      nudge_x = nudge_x,
      ##angle = angle,
      ...),
    check.aes = FALSE
  )
}


##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 draw_key_blank
##' @importFrom grid gTree
##' @importFrom grid gList
GeomImage <- ggproto("GeomImage", Geom,
                     setup_data = function(data, params) {
                       if (is.null(data$subset))
                         return(data)
                       data[which(data$subset),]
                     },

                     default_aes = aes(image=system.file("extdata/Rlogo.png", package="ggimage"),
                                       size=0.05, colour = NULL, angle = 0, alpha=1),

                     draw_panel = function(data, panel_params, coord, by, na.rm=FALSE,
                                           .fun = NULL, image_fun = NULL,
                                           hjust=0.5, nudge_x = 0, nudge_y = 0, asp=1) {
                       data <- GeomImage$make_image_data(data, panel_params, coord, .fun, nudge_x, nudge_y)

                       adjs <- GeomImage$build_adjust(data, panel_params, by)

                       grobs <- lapply(seq_len(nrow(data)), function(i){
                         imageGrob(x = data$x[i],
                                   y = data$y[i],
                                   size = data$size[i],
                                   img = data$image[i],
                                   colour = data$colour[i],
                                   alpha = data$alpha[i],
                                   angle = data$angle[i],
                                   adj = adjs[i],
                                   image_fun = image_fun,
                                   hjust = hjust,
                                   by = by,
                                   asp = asp
                         )
                       })
                       ggname("geom_image", gTree(children = do.call(gList, grobs)))
                     },
                     make_image_data = function(data, panel_params, coord, .fun, nudge_x = 0, nudge_y = 0,...){
                       data$x <- data$x + nudge_x
                       data$y <- data$y + nudge_y
                       data <- coord$transform(data, panel_params)

                       if (!is.null(.fun) && is.function(.fun)) {
                         data$image <- .fun(data$image)
                       }
                       if (is.null(data$image)){
                         return(NULL)
                       }else{
                         return(data)
                       }
                     },
                     build_adjust = function(data, panel_params, by){
                       if (by=='height' && "y.range" %in% names(panel_params)) {
                         adjs <- data$size / diff(panel_params$y.range)
                       } else if (by == 'width' && "x.range" %in% names(panel_params)){
                         adjs <- data$size / diff(panel_params$x.range)
                       } else if ("r.range" %in% names(panel_params)) {
                         adjs <- data$size / diff(panel_params$r.range)
                       } else {
                         adjs <- data$size
                       }
                       adjs[is.infinite(adjs)] <- 1
                       return(adjs)
                     },
                     non_missing_aes = c("size", "image"),
                     required_aes = c("x", "y"),
                     draw_key = draw_key_image ## draw_key_blank ## need to write the `draw_key_image` function.
)


##' @importFrom magick image_read
##' @importFrom magick image_read_svg
##' @importFrom magick image_read_pdf
##' @importFrom magick image_transparent
##' @importFrom magick image_rotate
##' @importFrom grid rasterGrob
##' @importFrom grid viewport
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom methods is
##' @importFrom tools file_ext
imageGrob <- function(x, y, size, img, colour, alpha, angle, adj, image_fun, hjust, by, asp=1, default.units='native'){
  if (is.na(img)){
    return(zeroGrob())
  }
  if (!is(img, "magick-image")) {
    if (tools::file_ext(img) == "svg") {
      img <- image_read_svg(img, width=1000)
    } else if (tools::file_ext(img) == "pdf") {
      img <- image_read_pdf(img)
    } else {
      img <- image_read(img, density=300)
    }
    asp <- getAR2(img)/asp
  }

  if (size == Inf) {
    x <- 0.5
    y <- 0.5
    width <- 1
    height <- 1
  } else if (by == "width") {
    width <- size * adj
    height <- size / asp
  } else {
    width <- size * asp * adj
    height <- size
  }

  if (hjust == 0 || hjust == "left") {
    x <- x + width/2
  } else if (hjust == 1 || hjust == "right") {
    x <- x - width/2
  }

  if (!is.null(image_fun)) {
    img <- image_fun(img)
  }

  if (angle != 0) {
    img <- image_rotate(img, angle)
  }

  if (!is.null(colour)){
    img <- color_image(img, colour, alpha)
  }

  if (size == Inf){
    grob <- rasterGrob(x = x,
                       y = y,
                       image = img,
                       default.units = default.units,
                       height = height,
                       width = width
    )
  }else{
    grob <- rasterGrob(
      x = x,
      y = y,
      image = img,
      default.units = default.units,
      height = height
    )
  }
  return(grob)
}

# ##' @importFrom grid makeContent
# ##' @importFrom grid convertHeight
# ##' @importFrom grid convertWidth
# ##' @importFrom grid unit
# ##' @method makeContent fixasp_raster
# ##' @export
# makeContent.fixasp_raster <- function(x) {
#     ## reference https://stackoverflow.com/questions/58165226/is-it-possible-to-plot-images-in-a-ggplot2-plot-that-dont-get-distorted-when-y?noredirect=1#comment102713437_58165226
#     ## and https://github.com/GuangchuangYu/ggimage/issues/19#issuecomment-572523516
#     ## Convert from relative units to absolute units
#     children <- x$children
#     for (i in seq_along(children)) {
#         y <- children[[i]]
#         h <- convertHeight(y$height, "cm", valueOnly = TRUE)
#         w <- convertWidth(y$width, "cm", valueOnly = TRUE)
#         ## Decide how the units should be equal
#         ## y$width <- y$height <- unit(sqrt(h*w), "cm")
#
#         y$width <- unit(w, "cm")
#         y$height <- unit(h, "cm")
#         x$children[[i]] <- y
#     }
#     x
# }

##' @importFrom magick image_info
getAR2 <- function(magick_image) {
  info <- image_info(magick_image)
  info$width/info$height
}


compute_just <- getFromNamespace("compute_just", "ggplot2")


## @importFrom EBImage readImage
## @importFrom EBImage channel
## imageGrob2 <- function(x, y, size, img, by, colour, alpha) {
##     if (!is(img, "Image")) {
##         img <- readImage(img)
##         asp <- getAR(img)
##     }

##     unit <- "native"
##     if (any(size == Inf)) {
##         x <- 0.5
##         y <- 0.5
##         width <- 1
##         height <- 1
##         unit <- "npc"
##     } else if (by == "width") {
##         width <- size
##         height <- size/asp
##     } else {
##         width <- size * asp
##         height <- size
##     }

##     if (!is.null(colour)) {
##         color <- col2rgb(colour) / 255

##         img <- channel(img, 'rgb')
##         img[,,1] <- colour[1]
##         img[,,2] <- colour[2]
##         img[,,3] <- colour[3]
##     }

##     if (dim(img)[3] >= 4) {
##         img[,,4] <- img[,,4]*alpha
##     }

##     rasterGrob(x = x,
##                y = y,
##                image = img,
##                default.units = unit,
##                height = height,
##                width = width,
##                interpolate = FALSE)
## }


## getAR <- function(img) {
##     dims <- dim(img)[1:2]
##     dims[1]/dims[2]
## }


##################################################
##                                              ##
## another solution, but the speed is too slow  ##
##                                              ##
##################################################

## draw_key_image <- function(data, params, size) {
##     imageGrob(0.5, 0.5, image=data$image, size=data$size)
## }


## ##' @importFrom ggplot2 ggproto
## ##' @importFrom ggplot2 Geom
## ##' @importFrom ggplot2 aes
## ##' @importFrom ggplot2 draw_key_blank
## GeomImage <- ggproto("GeomImage", Geom,
##                      non_missing_aes = c("size", "image"),
##                      required_aes = c("x", "y"),
##                      default_aes = aes(size=0.05, image="https://www.r-project.org/logo/Rlogo.png"),
##                      draw_panel = function(data, panel_scales, coord, by, na.rm=FALSE) {
##                          data$image <- as.character(data$image)
##                          data <- coord$transform(data, panel_scales)
##                          imageGrob(data$x, data$y, data$image, data$size, by)
##                      },
##                      draw_key = draw_key_image
##                      )


## ##' @importFrom grid grob
## imageGrob <- function(x, y, image, size=0.05, by="width") {
##     grob(x=x, y=y, image=image, size=size, by=by, cl="image")
## }

## ##' @importFrom grid drawDetails
## ##' @importFrom grid grid.raster
## ##' @importFrom EBImage readImage
## ##' @method drawDetails image
## ##' @export
## drawDetails.image <- function(x, recording=FALSE) {
##     image_object <- lapply(x$image, readImage)
##     names(image_object) <- x$image
##     for (i in seq_along(x$image)) {
##         img <- image_object[[x$image[i]]]
##         size <- x$size[i]
##         by <- x$by
##         asp <- getAR(img)
##         if (is.na(size)) {
##             width <- NULL
##             height <- NULL
##         } else if (by == "width") {
##             width <- size
##             height <- size/asp
##         } else {
##             width <- size * asp
##             height <- size
##         }

##         grid.raster(x$x[i], x$y[i],
##                     width = width,
##                     height = height,
##                     image = img,
##                     interpolate=FALSE)
##     }
## }

## ##' @importFrom ggplot2 discrete_scale
## ##' @importFrom scales identity_pal
## ##' @importFrom ggplot2 ScaleDiscreteIdentity
## ##' @export
## scale_image <- function(..., guide = "legend") {
##   sc <- discrete_scale("image", "identity", identity_pal(), ..., guide = guide,
##                        super = ScaleDiscreteIdentity)

##   sc
## }




########
#### Current file: R/play.R 
########


























sequence4one <- function(data, g = 100, selection = "min") {
  # browser()
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

    if (selection == "random") {
      if ((length(l) == 1)) {
        break
      }
    } else if (selection == "min") {
      # Breaks when l has length g
      if ((length(l) == g)) {
        break
      }
    } else {
      stop("Selection strategy has to be either 'min' or 'random'.")
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

  ## If "random", seq.test has length 1.
  index <- seq.test |>
    lengths() |>
    which.min()

  ## Shuffle sequence order
  sequence <- seq.test[[index]] |> shuffle_seq()

  list(cards = data, sequence = sequence)
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


########
#### Current file: https://raw.githubusercontent.com/agdamsbo/project.aid/refs/heads/main/R/chunks_of_n.R 
########
















chunks_of_n <- function(d, n, label = NULL, even = FALSE, pattern = NULL) {
  if (!(is.vector(d) |
    is.data.frame(d)) |
    inherits(d, "list")) {
    stop("Provided data is not vector or data.frame.")
  }

  if (is.data.frame(d)) ns <- nrow(d) else ns <- length(d)

  if (even) {
    g <- sort(rep_len(seq_len(ceiling(ns / n)), ns))
  } else {
    g <- ceiling(seq_len(ns) / n)
  }

  ls <- split(d, g)

  if (!is.null(pattern)) {
    if (is.data.frame(d)) {
      ns <- str_extract(d = d[[1]], pattern = pattern)
    } else {
      ns <- str_extract(d = d, pattern = pattern)
    }


    suffix <- do.call(c, lapply(split(ns, g), function(i) {
      paste0(i[[1]], "-", i[[length(i)]])
    }))
  } else {
    suffix <- names(ls)
  }

  if (is.character(label)) {
    names(ls) <- paste0(label, "-", suffix)
  } else {
    names(ls) <- suffix
  }

  ls
}













n_chunks <- function(d, n, ...) {
  if (!(is.vector(d) |
    is.data.frame(d)) |
    inherits(d, "list")) {
    stop("Provided data is not vector or data.frame.")
  }

  if (is.data.frame(d)) ns <- nrow(d) else ns <- length(d)

  nn <- ceiling(ns / n)

  chunks_of_n(d = d, n = nn, ...)
}
