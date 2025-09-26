#' @include persistence.R
NULL

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
StatBarcode <- ggproto(
  "StatBarcode", StatPersistence,
  
  positional_aes = c("x", "xend", "y", "yend"),
  
  derive_positional_aes = function(data, params) {
    
    # `x` and `xend` are simply `birth` and `death`
    data$x <- data$birth
    data$xend <- data$death
    
    # compute vertical position (sort by `group`, then `order_by`)
    interaction_args <- c(
      # first sort by group
      if (! is.null(data$group)) list(data$group),
      # next sort by specified properties in order
      lapply(params$order_by, \(f) data[[f]]),
      # additional parameters to `interaction`,
      # drop unused levels and use lexicographic order
      list(drop = TRUE, lex.order = TRUE)
    )
    
    data$y <- do.call(interaction, args = interaction_args)
    
    # re-distinguish duplicates
    data$y <- order(order(data$y))
    
    # horizontal segments, y == yend
    data$yend <- data$y
    
    data
  }
)

#' @rdname persistence
#' @order 4
#' @export
stat_barcode <- function(mapping = NULL,
                         data = NULL,
                         geom = "barcode",
                         position = "identity",
                         filtration = "Rips",
                         diameter_max = NULL,
                         radius_max = NULL,
                         max_hom_degree = 1L,
                         field_order = 2L,
                         engine = NULL,                                                  
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    geom = geom,
    data = data,
    mapping = mapping,
    stat = StatBarcode,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      filtration = filtration,
      diameter_max = diameter_max,
      radius_max = radius_max,
      max_hom_degree = max_hom_degree,
      field_order = field_order,
      engine = engine,      
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomBarcode <- ggproto(
  "GeomBarcode", GeomSegment
)

#' @rdname persistence
#' @order 4
#' @export
geom_barcode <- function(mapping = NULL,
                         data = NULL,
                         stat = "barcode",
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
