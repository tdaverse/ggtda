#' Plot Persistent Homology as Topological Barcode
#'
#' Plots a topological barcode.
#'

#' @name barcode
#' @import ggplot2
#' @family TDA plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical:
#'   if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category,
#'   plotted in grey (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data.
#'   Defaults to `segment`; pass a string to override the default.
#' @param stat The The statistical transformation to use on the data.
#'   Defaults to `identity`; pass a string to override the default.
#' @example inst/examples/ex-barcode.r

#' @rdname barcode
#' @export
stat_barcode <- function(mapping = NULL,
                         data = NULL,
                         geom = "segment",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    stat = StatBarcode,
    data = data,
    mapping = mapping,
    geom = geom,
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
StatBarcode <- ggproto(
  "StatBarcode", Stat,
  
  required_aes = c("xmin", "xmax", "group"),
  
  # pre-process of the parameters
  setup_params = function(data, params) {
    
    # return the pre-processed parameters
    params
  },
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # return the pre-processed data set
    data
  },
  
  # statistical transformation into plot-ready data
  compute_panel = function(data, scales) {
    
    # change `xmin` and `xmax` to `x` and `xend` (for `geom_segment()`)
    data$x <- data$xmin
    data$xend <- data$xmax
    data$xmin <- NULL
    data$xmax <- NULL
    
    # introduce categorical y-values in order of `group`, `x`, and `xend`
    data$y <- as.integer(interaction(
      data$group, data$x, data$xend,
      drop = TRUE, lex.order = TRUE
    ))
    data$yend <- data$y
    
    # return the transformed data frame
    data
  }
)

#' @rdname barcode
#' @export
geom_barcode <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    geom = GeomBarcode,
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
#' @usage NULL
#' @export
GeomBarcode <- ggproto(
  "GeomBarcode", GeomSegment,
  
  required_aes = c("x", "xend", "group"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # introduce categorical y-values in order of `group`, `x`, and `xend`
    data$y <- as.integer(interaction(
      data$group, data$x, data$xend,
      drop = TRUE, lex.order = TRUE
    ))
    data$yend <- data$y
    
    # return the pre-processed data set
    data
  }
)
