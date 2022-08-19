#'   The convenience layer `geom_diagonal()` is appropriate only for the
#'   "diagonal" layout and accepts no aesthetics.
#'   

#' @rdname persistence
#' @export
geom_diagonal <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    geom = GeomDiagonal,
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
#' @format NULL
#' @usage NULL
#' @export
GeomDiagonal <- ggproto(
  "GeomDiagonal", GeomAbline,
  
  required_aes = c(),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = .75),
  
  setup_data = function(data, params) {
    
    # keep only columns that are constant throughout the data
    data <- aggregate(
      data[, setdiff(names(data), "PANEL"), drop = FALSE],
      by = data[, "PANEL", drop = FALSE],
      function(x) if (length(unique(x)) == 1L) unique(x) else NA
    )
    data[] <- lapply(data, function(x) if (all(is.na(x))) NULL else x)
    rownames(data) <- NULL
    
    data
  },
  
  draw_panel = function(data, panel_params, coord) {
    
    # adapted from `GeomAbline$draw_panel`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      ranges$x[[2L]] <- ranges$x[[2L]] + diff(ranges$x)
    }
    
    # define two points: the origin, and the largest projection to the diagonal
    data$x <- 0
    data$y <- 0
    data$xend <- ranges$x[[2L]]
    data$yend <- ranges$x[[2L]]
    
    GeomSegment$draw_panel(data, panel_params, coord)
  },
  
  draw_key = draw_key_blank
)
