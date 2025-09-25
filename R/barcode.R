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
  
  
  setup_params = function(self, data, params) {
    
    # TODO: removed "part" as an option, for now
    #   -- once we finalize how `part` is to be computed we can implement it
    #      and re-include it here.
    #   -- will need to compute in `GeomBarcode$set_data` just like `persistence`
    
    # order_by_options <- c("start", "end", "part", "persistence")
    order_by_options <- c("start", "end", "persistence")

    # discard unrecognized feature properties with a warning
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
    
    params
  },
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # If `stat = "identity"` this computed variable will be missing,
    # we have to compute it here if it's needed for sorting:
    if (is.null(data$persistence) & "persistence" %in% params$order_by) {
      data$persistence <- data$end - data$start
    }
    
    # TODO: Calculate `part` as in `persistence` above,
    #       to allow for sorting with stat = "identity"
    
    # compute vertical position (sort by `group`, then `order_by`)
    interaction_args <- c(
      # always sort first by `group`
      if (! is.null(data$group)) list(data$group),
      # sort by specified properties in order
      lapply(params$order_by, \(f) if (params$decreasing) -xtfrm(data[[f]]) else data[[f]]),
      # drop unused levels and use lexicographic order
      list(drop = TRUE, lex.order = TRUE)
    )
    
    data$y <- do.call(interaction, args = interaction_args)
    
    # re-distinguish duplicates
    data$y <- order(order(data$y))
    
    # horizontal segments, y == yend
    data$yend <- data$y
    
    # return the pre-processed data set
    data
  },
  
  # `order_by` and `decreasing` parameters are only used in `$setup_data()`,
  draw_panel = function(data, panel_params, coord, order_by = c("persistence", "start"), decreasing = FALSE) {
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
                         order_by = c("persistence", "start"),
                         decreasing = FALSE,
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
      order_by = order_by,
      decreasing = decreasing,
      na.rm = na.rm,
      ...
    )
  )
}
