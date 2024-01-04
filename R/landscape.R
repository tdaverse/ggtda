#' @title Persistence landscapes
#'
#' @description Visualize persistence data as a persistence landscape.
#'   

#' @details
#'
#' {*Persistence landscapes*}, anticipated by some alternative coordinatizations
#' of persistence diagrams, were proposed as Lipschitz functions that demarcate
#' the Pareto frontiers of persistence diagrams. They can be averaged over the
#' diagrams obtained from multiple data sets designed or hypothesized to have
#' been generated from the same underlying topological structure.
#'
#' Persistence landscapes do not currently recognize extended persistence data.
#' 

#' @template ref-bubenik2015
#' @template ref-chazal2017

#' @eval rd_sec_aesthetics(
#'   stat_landscape = StatLandscape,
#'   geom_landscape = GeomLandscape,
#'   extra_note = paste0(
#'     "`start` and `end` are dropped during the statistical transformation."
#'   )
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "landscape",
#'   "x,y" = "coordinates of segment endpoints of each frontier.",
#'   "level" = "position of each frontier, starting from the outermost.",
#'   extra_note = paste0(
#'     "Note that ",
#'     "`start` and `end` are dropped during the statistical transformation."
#'   )
#' )

#' @name landscape
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @inheritParams persistence
#' @param geom The geometric object to use display the data; defaults to `line`.
#'   Pass a string to override the default.
#' @example inst/examples/ex-landscape.R
# file.edit("inst/examples/ex-landscape.R")
NULL

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatLandscape <- ggproto(
  "StatLandscape", Stat,
  
  required_aes = c("start", "end"),
  
  default_aes = aes(group = interaction(after_stat(level), group)),
  
  compute_group = function(data, scales,
                           diagram = "landscape") {
    
    # first row (for aesthetics)
    first_row <- data[1L, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # iteratively peel and stack frontiers
    # NB: points along slopes are not discarded
    pd <- as.matrix(data[, c("start", "end"), drop = FALSE])
    pl <- list()
    k <- 0L
    while (nrow(pd) > 0L) {
      k <- k + 1L
      
      # identify frontier points
      pd <- pd[order(pd[, 1L], -pd[, 2L]), , drop = FALSE]
      peak_ids <- which(! duplicated(cummax(pd[, 2L])))
      peaks <- pd[peak_ids, , drop = FALSE]
      
      # identify col points
      cols <- cbind(
        start = c(peaks[, 1L], peaks[nrow(peaks), 2L]),
        end = c(peaks[1L, 1L], peaks[, 2L])
      )
      
      # flatten valleys
      valley_ids <- which(cols[, 1L] > cols[, 2L])
      for (i in rev(valley_ids)) {
        cols <- rbind(
          cols[seq(i - 1L), , drop = FALSE],
          cols[i, c(2L, 2L), drop = FALSE],
          cols[i, c(1L, 1L), drop = FALSE],
          cols[seq(i + 1L, nrow(cols)), , drop = FALSE]
        )
      }
      
      # extract frontier
      frontier <- rbind(peaks, cols)
      frontier <- frontier[order(frontier[, 1L], frontier[, 2L]), ]
      # frontier <- rbind(
      #   frontier[1L, ] - Inf,
      #   frontier,
      #   frontier[nrow(frontier), ] + Inf
      # )
      pl[[k]] <- frontier
      
      # reset persistence diagram
      pd <- pd[-peak_ids, , drop = FALSE]
      pd <- rbind(pd, cols[-c(1L, nrow(cols)), , drop = FALSE])
      pd <- pd[pd[, 1L] < pd[, 2L], , drop = FALSE]
    }
    
    # data frame
    data <- do.call(rbind, pl)
    data <- as.data.frame(data)
    names(data) <- c("x", "y")
    # diagram transformation
    data <- diagram_transform(data, diagram)
    # compute level of each frontier
    data$level <- rep(seq(length(pl)), sapply(pl, nrow))
    
    # return landscape data
    cbind(data, first_row)
  }
)

#' @rdname landscape
#' @export
stat_landscape <- function(mapping = NULL,
                          data = NULL,
                          geom = "landscape",
                          position = "identity",
                          diagram = "landscape",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatLandscape,
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
GeomLandscape <- ggproto(
  "GeomLandscape", GeomPath,
  
  draw_panel = function(data, panel_params, coord,
                        diagram = "diagonal",
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    
    # # adapted from `ggplot2::GeomPath`
    # # (data should already be ordered; or, order by slope)
    # data <- data[order(data$group), , drop = FALSE]
    
    # adapted from `ggplot2::GeomAbline`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }
    
    # extend each level to the extended range
    horizon <- diagram_horizon(ranges, diagram)
    
    # make room for horizons
    group_start <- c(TRUE, data$group[-1L] != data$group[-nrow(data)])
    group_end <- c(data$group[-1L] != data$group[-nrow(data)], TRUE)
    data <- data[rep(seq(nrow(data)), (group_start | group_end) + 1L), ]
    # insert horizons
    group_start <- c(TRUE, data$group[-1L] != data$group[-nrow(data)])
    group_end <- c(data$group[-1L] != data$group[-nrow(data)], TRUE)
    data$x[group_start] <- horizon$x[1L]
    data$y[group_start] <- horizon$y[1L]
    data$x[group_end] <- horizon$x[2L]
    data$y[group_end] <- horizon$y[2L]
    
    # adapted from `ggplot2::GeomPath`
    munched <- coord_munch(coord, data, panel_params)
    group_diff <- munched$group[-1L] != munched$group[-nrow(munched)]
    start <- c(TRUE, group_diff)
    end   <- c(group_diff, TRUE)
    grob <- grid::segmentsGrob(
      x0 = munched$x[!end],   y0 = munched$y[!end],
      x1 = munched$x[!start], y1 = munched$y[!start],
      default.units = "native", arrow = NULL,
      gp = grid::gpar(
        col =  alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$colour, munched$alpha)[!end],
        lwd = (munched$linewidth[!end] %||% munched$size[!end]) * .pt,
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
    grob$name <- grid::grobName(grob, "geom_landscape")
    grob
  }
)

#' @rdname landscape
#' @export
geom_landscape <- function(mapping = NULL,
                           data = NULL,
                           stat = "landscape",
                           position = "identity",
                           diagram = "diagonal",
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    geom = GeomLandscape,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}

# pareto_ids <- function(x) {
#   if ("rPref" %in% rownames(utils::installed.packages())) {
#     pareto_ids_rPref(x)
#   } else {
#     pareto_ids_base(x)
#   }
# }
# pareto_ids_base <- function(x) {
#   ord <- order(x[, 1L], -x[, 2L])
#   x <- x[ord, , drop = FALSE]
#   order(ord)[which(! duplicated(cummax(x[, 2L])))]
# }
# pareto_ids_rPref <- function(x) {
#   x <- as.data.frame(x)
#   names(x) <- c("start", "end")
#   rPref::psel.indices(x, rPref::low("start") * rPref::high("end"))
# }

diagram_horizon <- function(ranges, diagram) {
  switch(
    match.arg(diagram, c("flat", "diagonal", "landscape")),
    flat = list(x = ranges$x, y = c(0, 0)),
    diagonal = {
      lim <- c(
        max(ranges$x[1L], ranges$y[1L]),
        min(ranges$x[2L], ranges$y[2L])
      )
      list(x = lim, y = lim)
    },
    landscape = list(x = ranges$x, y = c(0, 0))
  )
}
