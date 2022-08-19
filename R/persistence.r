#' @title Persistence diagrams
#'
#' @description Visualize persistence data in a (flat, diagonal, or landscape)
#'   persistence diagram.
#'   

#' @details
#'
#' *Persistence diagrams* are
#' [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html) of
#' persistence data. *Persistence landscapes* can be understood as rotated
#' diagonal persistence diagrams.
#' 

#' @template persistence-data
#'

#' @section Persistence diagrams:

#'   Persistence diagrams recognize extended persistence data, with negative
#'   birth/death values arising from the relative part of the filtration.
#'
#'   The original persistence diagrams plotted persistence against birth in what
#'   we call "flat" diagrams, but most plot death against birth in "diagonal"
#'   diagrams, often with a diagonal line indicating zero persistence.
#'
#'   The convenience layer `geom_diagonal()` is appropriate only for the
#'   "diagonal" layout and accepts no aesthetics. It pairs well with the
#'   `geom_fundamental_box()` layer that renders fundamental boxes at specified
#'   time points (Chung & Lawson, 2020).
#'   

#' @section Persistence landscapes:

#'
#'   Persistence landscapes, anticipated by some alternative coordinatizations
#'   of persistence diagrams, were proposed as Lipschitz functions that
#'   demarcate the Pareto frontiers of persistence diagrams. They can be
#'   averaged over the diagrams obtained from multiple data sets designed or
#'   hypothesized to have been generated from the same underlying topological
#'   structure.
#'   
#'   Persistence landscapes do not currently recognize extended persistence data.
#'   

#' @template ref-edelsbrunner2000
#' @template ref-edelsbrunner2012
#' @template ref-bubenik2015
#' @template ref-chazal2017
#' @template ref-chung2020
#'   

#' @name persistence
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical: if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category, plotted in grey
#'   (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data; defaults to
#'   `segment` in `geom_vietoris1()` and to `polygon` in `geom_vietoris2`. Pass
#'   a string to override the default.
#' @param diagram One of `"flat"`, `"diagonal"`, or `"landscape"`; the
#'   orientation for the diagram should take.
#' @param t A numeric vector of time points at which to place fundamental boxes.
#' @example inst/examples/ex-persistence.R

#' @rdname persistence
#' @export
stat_persistence <- function(mapping = NULL,
                             data = NULL,
                             geom = "point",
                             position = "identity",
                             diagram = "diagonal",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  layer(
    stat = StatPersistence,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPersistence <- ggproto(
  "StatPersistence", Stat,
  
  required_aes = c("start", "end"),
  
  compute_group = function(data, scales,
                           diagram = "diagonal") {
    save(data, scales, diagram,
         file = here::here("stat-persistence-compute.rda"))
    load(here::here("stat-persistence-compute.rda"))
    
    # <<<<<< compute persistence from list column of data objects >>>>>>
    
    # points in cartesian coordinates (un-negated from opposite filtration)
    data$x <- abs(data$start)
    data$y <- abs(data$end)
    # endpoints of frontier segments
    data$x0 <- data$y0 <- data$x
    data$xend <- data$yend <- data$y
    # TODO: Is this overkill? Could just use `x0` and `xend`, though at some
    # risk of confusion.
    
    # computed variable: `part`
    data$part <- with(data, {
      part <- NA_character_
      part[start >= 0 & end >= 0] <- "ordinary"
      part[start <  0 & end <  0] <- "relative"
      part[start >= 0 & end <  0] <- "extended"
      factor(part, levels = c("ordinary", "relative", "extended"))
    })
    # computed variable: `persistence` (infinite for extended points)
    data$persistence <- data$end - data$start
    data$persistence <- ifelse(data$persistence < 0, Inf, data$persistence)
    # computed variable: `feature_id` (sort by dimension, birth, death)
    data$feature_id <- interaction(
      if (is.null(data$group)) NA_character_ else data$group,
      data$start, data$end,
      drop = TRUE, lex.order = TRUE
    )
    # re-distinguish duplicates
    data$feature_id <- order(order(data$feature_id))
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    
    # return point data
    data
  }
)

#' @rdname persistence
#' @export
stat_frontier <- function(mapping = NULL,
                          data = NULL,
                          geom = "line",
                          position = "identity",
                          diagram = "diagonal",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatFrontier,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatFrontier <- ggproto(
  "StatFrontier", Stat,
  
  required_aes = c("start", "end"),
  
  compute_group = function(data, scales,
                           diagram = "diagonal") {
    
    # first row (for aesthetics)
    first_row <- data[1, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # Pareto frontier
    data <- pareto_persistence(data)
    data <- data[order(data$start), ]
    if (! all(data$end == cummax(data$end))) {
      warning("`start` and `end` are not anti-sorted.")
    }
    
    # points in cartesian coordinates
    data$x <- data$start
    data$y <- data$end
    
    # computed variables
    data$persistence <- data$end - data$start
    
    # data frame of segments
    data <- data.frame(
      x = c(rep(data$start, each = 2), data$end[nrow(data)]),
      y = c(data$start[1], rep(data$end, each = 2))
    )
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    
    # return frontier data
    cbind(data, first_row)
  }
)

#' @rdname persistence
#' @export
geom_frontier <- function(mapping = NULL,
                          data = NULL,
                          stat = "persistence",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFrontier,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomFrontier <- ggproto(
  "GeomFrontier", GeomSegment,
  
  required_aes = c("x", "y", "x0", "y0", "xend", "yend"),
  
  draw_panel = function(data, panel_params, coord,
                        lineend = "butt", linejoin = "round",
                        na.rm = FALSE) {
    
    data <- remove_missing(
      data, na.rm = na.rm,
      vars = c("x", "y", "x0", "y0", "xend", "yend",
               "linetype", "size", "shape"),
      name = "geom_frontier"
    )
    
    # expand two-segment rows to one-segment rows
    data <- data[rep(seq(nrow(data)), each = 2L), , drop = FALSE]
    repl_rows <- seq(nrow(data)) %% 2L == 0L
    data$xend[repl_rows] <- data$x0[repl_rows]
    data$yend[repl_rows] <- data$y0[repl_rows]
    data$x0 <- data$y0 <- NULL
    
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0 ||
        inherits(data, "waiver")) 
      return(zeroGrob())
    if (coord$is_linear()) {
      coord <- coord$transform(data, panel_params)
      return(grid::segmentsGrob(
        coord$x, coord$y, coord$xend, coord$yend,
        default.units = "native",
        gp = grid::gpar(
          col = alpha(coord$colour, coord$alpha),
          fill = alpha(coord$colour, coord$alpha),
          lwd = coord$size * .pt,
          lty = coord$linetype,
          lineend = lineend, linejoin = linejoin
        ),
        arrow = NULL
      ))
    }
    
    data$group <- seq(nrow(data))
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c(xend = "x", yend = "y"))
    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group), ]
    GeomPath$draw_panel(pieces, panel_params, coord,
                        arrow = NULL, lineend = lineend)
  }
)

#' @rdname persistence
#' @export
geom_fundamental_box <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 diagram = "diagonal",
                                 t = NULL,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 ...) {
  layer(
    geom = GeomFundamentalBox,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      t = t,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFundamentalBox <- ggproto(
  "GeomFundamentalBox", GeomPolygon,
  
  required_aes = c(),
  default_aes = aes(colour = "black", fill = "grey",
                    size = 0.5, linetype = 1, alpha = .25),
  
  setup_data = function(data, params) {
    
    # keep only columns that are constant throughout the data
    data <- dplyr::select_if(
      data,
      function(x) length(unique(x)) == 1L
    )[1L, , drop = FALSE]
    rownames(data) <- NULL
    
    data
  },
  
  draw_panel = function(data, panel_params, coord,
                        diagram = "diagonal", t = NULL) {
    
    # expand ranges if appropriate
    # adapted from `GeomAbline$draw_panel`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      #ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
      ranges$y <- ranges$y + c(-1, 1) * diff(ranges$y)
    }
    
    # define segments and interior in default (diagonal) layout
    # use `group` to separate boxes for different times
    data$x <- NULL; data$xend <- NULL
    data$y <- NULL; data$yend <- NULL
    data$group <- NULL
    ray_data <- data.frame(
      x = as.vector(rbind(0, t)), xend = rep(t, each = 2L),
      y = rep(t, each = 2L), yend = as.vector(rbind(t, ranges$y[[2L]])),
      group = rep(seq_along(t), each = 2L)
    )
    ray_data <- merge(ray_data, data)
    ray_data$group <- -1L
    int_data <- data.frame(
      x = as.vector(rbind(t, t, 0, 0)),
      y = as.vector(rbind(t, ranges$y[[2L]], ranges$y[[2L]], t)),
      group = rep(seq_along(t), each = 4L)
    )
    int_data <- merge(int_data, data)
    int_data$colour <- "transparent"
    
    # diagram transformations
    ray_data <- diagram_transform(ray_data, diagram)
    int_data <- diagram_transform(int_data, diagram)
    
    # combine grobs for rays and interior
    ray_grob <- GeomSegment$draw_panel(ray_data, panel_params, coord)
    int_grob <- GeomPolygon$draw_panel(int_data, panel_params, coord)
    grob <- do.call(grid::grobTree, list(ray_grob, int_grob))
    grob$name <- grid::grobName(grob, "geom_fundamental_box")
    grob
  },
  
  draw_key = draw_key_blank
)

diagram_transform <- function(data, diagram) {
  switch(
    match.arg(diagram, c("flat", "diagonal", "landscape")),
    flat = transform(
      data,
      y = data$y - data$x,
      y0 = data$y0 - data$x0,
      yend = data$yend - data$xend
    ),
    diagonal = data,
    landscape = transform(
      data,
      x = (data$x + data$y) / 2,
      y = (data$y - data$x) / 2,
      x0 = (data$x0 + data$y0) / 2,
      y0 = (data$y0 - data$x0) / 2,
      xend = (data$xend + data$yend) / 2,
      yend = (data$yend - data$xend) / 2
    )
  )
}

pareto_persistence <- function(data) {
  if ("rPref" %in% rownames(utils::installed.packages())) {
    pareto_persistence_rPref(data)
  } else {
    pareto_persistence_base(data)
  }
}

pareto_persistence_base <- function(data) {
  pd <- data[order(data$start, -data$end), ]
  pd[! duplicated(cummax(pd$end)), ]
}

pareto_persistence_rPref <- function(data) {
  rPref::psel(data, rPref::low("start") * rPref::high("end"))
}

#' @rdname persistence
#' @export
geom_diagonal <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomDiagonal,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDiagonal <- ggproto(
  "GeomDiagonal", GeomAbline,
  
  required_aes = c(),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = .75),
  
  setup_data = function(data, params) {
    
    # keep only columns that are constant throughout the data
    data <- aggregate(
      data[, setdiff(names(data), "PANEL"), drop = FALSE],
      by = data[, "PANEL", drop = FALSE],
      function(x) if (length(unique(x)) == 1L) unique(x) else NA
    )
    data[] <- lapply(data, function(x) if (all(is.na(x))) NULL else x)
    rownames(data) <- NULL
    
    data
  },
  
  draw_panel = function(data, panel_params, coord) {
    
    # adapted from `GeomAbline$draw_panel`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      ranges$x[[2L]] <- ranges$x[[2L]] + diff(ranges$x)
    }
    
    # define two points: the origin, and the largest projection to the diagonal
    data$x <- 0
    data$y <- 0
    data$xend <- ranges$x[[2L]]
    data$yend <- ranges$x[[2L]]
    
    GeomSegment$draw_panel(data, panel_params, coord)
  },
  
  draw_key = draw_key_blank
)
