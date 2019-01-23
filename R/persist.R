#' Plots Persistent Homology as a Persistence Diagram
#' 
#' Plots a persistence diagram, including flat and diagonal
#' persistence diagrams.
#' 
#' @name persist
#' @import ggplot2
#' @family TDA plot layers
#' @inheritParams ggplot2::layer
#' @param na.rm Logical:
#'   if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category,
#'   plotted in grey (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param stat The statistical transformation to use on the data.
#'   Defaults to `identity`; pass a string to override the default.
#' @param flat default `TRUE` to plot flat persistence diagram; `FALSE` will
#'   plot a diagonal persistence diagram.
#' @example inst/examples/ex-persist.R
#' @rdname persist
#' @export
geom_persist <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         flat = TRUE,
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
      flat = flat,
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
    if (params$flat) {
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
                        flat = FALSE) {
    
    # point graphical object with new name
    grob <- GeomPoint$draw_panel(data, panel_params, coord)
    grob$name <- grid::grobName(grob, "geom_persist")
    grob
  }
)
