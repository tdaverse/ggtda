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
#' @example inst/examples/ex-barcode.R

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
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # introduce categorical y-values in order of `group`, `x`, and `xend`
    grp <- if (is.null(data$group)) NA_character_ else data$group
    data$y <- as.integer(interaction(
      grp, data$start, data$end,
      drop = TRUE, lex.order = TRUE
    ))
    
    # return the pre-processed data set
    data
  },
  
  # generate graphical objects for each panel
  draw_panel = function(data, panel_params, coord) {
    
    # fill out necessary parameters for `GeomSegment`
    data$yend <- data$y
    
    # segment graphical object with new name
    grob <- GeomSegment$draw_panel(data, panel_params, coord)
    grob$name <- grid::grobName(grob, "geom_barcode")
    grob
  }
)
