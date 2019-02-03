#' @title Persistence diagrams
#'
#' @description Visualize persistence data in a (flat or diagonal) persistence
#'   diagram.
#'

#' @details
#'
#' *Persistence diagrams* are
#' [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html) of
#' persistence data.
#' 

#' @template persistence-data
#'

#' @section Persistence diagrams:
#'
#'   The original persistence diagrams plotted persistence against birth in what
#'   we call "flat" diagrams, but most plot death against birth in "diagonal"
#'   diagrams, often with a diagonal line indicating zero persistence.
#'   

#' @template ref-edelsbrunner2000
#' @template ref-chazal2017
#'   

#' @name persist
#' @import ggplot2
#' @family plot layers for persistence data
#' @inheritParams ggplot2::layer
#' @param na.rm Logical:
#'   if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category,
#'   plotted in grey (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param stat The statistical transformation to use on the data.
#'   Defaults to `identity`; pass a string to override the default.
#' @param diag default `FALSE` to plot flat persistence diagram; `TRUE` will
#'   plot a diagonal persistence diagram.
#' @example inst/examples/ex-persist.R
#' @rdname persist
#' @export
geom_persist <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         diag = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    geom = GeomPersist,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diag = diag,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPersist <- ggproto(
  "GeomPersist", GeomPoint,
  
  required_aes = c("start", "end"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    if (! is.null(data$x) & ! is.null(data$xend) &
        is.null(data$start) & is.null(data$end)) {
      
      warning(
        "Substituting `x` and `xend` for missing persistence aesthetics ",
        "`start` and `end`."
      )
      
      # change `x` and `xend` to `start` and `end`
      data$start <- data$x
      data$end <- data$xend
    }
    
    # warn of any nonsense data
    if (any(data$end - data$start < 0)) {
      wh <- which(data$end - data$start < 0)
      if (length(wh) > 6) wh <- c(wh[1:3], "...", wh[length(wh)])
      warning(
        "Some persistence data have `start` before `end`: ",
        paste(wh, collapse = ", ")
      )
    }
    
    # switch to flat orientation
    if (! params$diag) {
      data$end <- data$end - data$start
    }
    
    # convert to persistence coordinates
    data$x <- data$start
    data$y <- data$end
    
    # return the pre-processed data set
    data
  },
  
  # generate graphical objects for each panel
  draw_panel = function(data, panel_params, coord,
                        diag = TRUE) {
    
    # point graphical object with new name
    grob <- GeomPoint$draw_panel(data, panel_params, coord)
    grob$name <- grid::grobName(grob, "geom_persist")
    grob
  }
)
