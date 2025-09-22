#' @name persistence
#' @import ggplot2
#' @family plot layers for persistence data
#' @inheritParams ggplot2::layer
NULL

# file.edit("tests/testthat/test-barcode.R")
# file.edit("inst/examples/ex-barcode.R")

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomBarcode <- ggproto(
  "GeomBarcode", GeomSegment,
  
  required_aes = c("dataset|start", "dataset|end"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # compute vertical position if absent (see `StatPersistence$compute_*()`)
    if (is.null(data$id)) {
      interaction_args <- c(
        # always sort first by `group`
        if (! is.null(data$group)) list(data$group),
        # sort by available properties
        list(data$start, data$end),
        # drop unused levels and use lexicographic order
        list(drop = TRUE, lex.order = TRUE)
      )
      data$y <- do.call(interaction, args = interaction_args)
      # re-distinguish duplicates
      data$y <- order(order(data$y))
    } else {
      # use `id` for vertical position
      data$y <- data$id
    }
    
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

#' @rdname persistence
#' @order 4
#' @export
geom_barcode <- function(mapping = NULL,
                         data = NULL,
                         stat = "persistence",
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
