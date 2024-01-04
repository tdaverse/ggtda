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
#'   extra_note = paste0(
#'     "`start` and `end` are dropped during the statistical transformation."
#'   )
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "landscape",
#'   "x,y" = "coordinates of segment endpoints of each frontier.",
#'   "level" = "position of each frontier, starting from the outermost."
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
NULL

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatLandscape <- ggproto(
  "StatLandscape", Stat,
  
  required_aes = c("start", "end"),
  
  default_aes = aes(group = interaction(group, after_stat(level))),
  
  compute_group = function(data, scales,
                           diagram = "diagonal") {
    
    # first row (for aesthetics)
    first_row <- data[1L, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # iteratively peel and stack frontiers
    # NB: points along slopes are not discarded
    # TODO: extend landscapes to plot boundaries
    pd <- as.matrix(data[, c("start", "end"), drop = FALSE])
    pl <- list()
    k <- 0L
    while (nrow(pd) > 0L) {
      k <- k + 1L
      peak_ids <- pareto_ids(pd)
      peaks <- pd[peak_ids, , drop = FALSE]
      cols <- cbind(
        start = c(peaks[, 1L], peaks[nrow(peaks), 2L]),
        end = c(peaks[1L, 1L], peaks[, 2L])
      )
      valley_ids <- which(cols[, 1L] > cols[, 2L])
      for (i in rev(valley_ids)) {
        cols <- rbind(
          cols[seq(i - 1L), , drop = FALSE],
          cols[i, c(2L, 2L), drop = FALSE],
          cols[i, c(1L, 1L), drop = FALSE],
          cols[seq(i + 1L, nrow(cols)), , drop = FALSE]
        )
      }
      frontier <- rbind(peaks, cols)
      frontier <- frontier[order(frontier[, 1L], frontier[, 2L]), ]
      pl[[k]] <- frontier
      pd <- pd[-peak_ids, , drop = FALSE]
      pd <- rbind(pd, cols[-c(1L, nrow(cols)), , drop = FALSE])
      pd <- pd[pd[, 1L] < pd[, 2L], , drop = FALSE]
    }
    
    # data frame with level column
    data <- do.call(rbind, pl)
    data <- as.data.frame(data)
    names(data) <- c("x", "y")
    data$level <- rep(seq(length(pl)), sapply(pl, nrow))
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    
    # return landscape data
    cbind(data, first_row)
  }
)

#' @rdname landscape
#' @export
stat_landscape <- function(mapping = NULL,
                          data = NULL,
                          geom = "line",
                          position = "identity",
                          diagram = "diagonal",
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

pareto_ids <- function(x) {
  if ("rPref" %in% rownames(utils::installed.packages())) {
    pareto_ids_rPref(x)
  } else {
    pareto_ids_base(x)
  }
}

pareto_ids_base <- function(x) {
  ord <- order(x[, 1L], -x[, 2L])
  x <- data[ord, , drop = FALSE]
  order(ord)[which(! duplicated(cummax(x[, 2L])))]
}

pareto_ids_rPref <- function(x) {
  x <- as.data.frame(x)
  names(x) <- c("start", "end")
  rPref::psel.indices(x, rPref::low("start") * rPref::high("end"))
}
