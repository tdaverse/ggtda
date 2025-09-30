#' @include persistence.R
NULL

#' @name barcode
#' @title Barcode Diagrams
#' 
#' @description 
#'   Visualize persistence homologies with barcode diagrams.
#' 
#' @details
#'   *Barcode diagrams* are [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html)
#'    of persistence data.
#'   
#' @template persistence-data  
#' @template persistence-computed-vars  
#' 
#' @template ref-carlsson2004
#' @template ref-carlsson2014
#' @template ref-chazal2017
#' 
#' @import ggplot2
#' @family plot layers for persistence data
#' @param order_by A character vector comprised of (`"persistence"`, `"birth"`, and/or `"end"`)
#'  by which the features should be ordered (within `group`);
#'  defaults to `c("persistence", "birth")`.
#' @inheritParams persistence
#' @inheritParams ggplot2::geom_segment
#' 
#' @eval rd_sec_aesthetics(
#'   stat_barcode = StatBarcode,
#'   geom_barcode = GeomBarcode
#' )
#' 
#' @example inst/examples/ex-barcode.R
NULL

# file.edit("tests/testthat/test-barcode.R")
# file.edit("inst/examples/ex-barcode.R")

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatBarcode <- ggproto(
  "StatBarcode", StatPersistence,
  
  positional_aes = c("x", "xend", "y", "yend"),
  
  extra_params = c(StatPersistence$extra_params, "order_by"),
  
  setup_params = function(self, data, params) {
    
    # Assign default value for `order_by`
    params$order_by <- params$order_by %||% c("persistence", "birth")
    
    # Check validity of `order_by`, warning if specified incorrectly
    order_by_options <- c("persistence", "birth", "death")
    if (! all(params$order_by %in% order_by_options)) {
      ignore_by <- setdiff(params$order_by, order_by_options)
      warning(
        "`order_by` recognizes only: `",
        paste0(order_by_options, collapse = "`, `"),
        "`; `",
        paste0(ignore_by, collapse = "`, `"),
        "` will be ignored."
      )
      params$order_by <- intersect(params$order_by, order_by_options)
    }
    
    # Continue with `StatPersistence$setup_params()`
    StatPersistence$setup_params(data, params)
  },
  
  derive_positional_aes = function(data, params) {
    
    # `x` and `xend` are simply `birth` and `death`
    data$x <- data$birth
    data$xend <- data$death
    
    # compute vertical position (sort by `group`, then `order_by`)
    interaction_args <- c(
      # first sort by group
      if (! is.null(data$group)) list(data$group),
      # next sort by specified properties in order
      lapply(params$order_by, function(col) data[[col]]),
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

#' @rdname barcode
#' @order 1
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
                         order_by = c("persistence", "start"),
                         engine = NULL,                                                  
                         infinity_break = Inf,
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
      order_by = order_by,
      engine = engine,      
      infinity_break = infinity_break,
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

#' @rdname barcode
#' @order 2
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
