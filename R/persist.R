#' Plot Persistent Homology as a Persistence Diagram
#' 
#' Plots a flat persistence diagram.
#' 
#' @name persistence
#' @import ggplot2
#' @family TDA plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical:
#'   if `FALSE`, the default, `NA` values are not included
#'   if `TRUE`, `NA` values constitute a separate category,
#'   plotted in grey (regardless of the color scheme)
#' @param ... additional arguments passed to [ggplot2::layer()]
#' @param geom The geometric object used to display the data.
#'   Defaults to `point`; pass a string to override the default.
#' @param stat The statistical transformation used to display the data.
#'   Defaults to `identity`; another useful option is `flat`.
#'   Pass a string to override the default.
#' @example inst/examples/ex-persist.R
#' @export

#' @rdname persistence
#' @export
stat_flat <- function(mapping = NULL,
                      data = NULL,
                      geom = "persistence",
                      position = "identity",
                      flat = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    stat = StatFlat,
    data = data,
    mapping = mapping,
    geom = geom,
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
#' @usage NULL
#' @export
StatFlat <- ggproto(
  "StatFlat", Stat,
  
  required_aes = c("start", "end"),
  
  setup_data = function(data, params) {
    
    # warn of any nonsense data
    if (any(data$end - data$start < 0)) {
      wh <- which(data$end - data$start < 0)
      if (length(wh) > 6) wh <- c(wh[1:3], "...", wh[length(wh)])
      warning(
        "Some persistence data have `start` before `end`: ",
        paste(wh, collapse = ", ")
      )
    }
    
    data
  },
  
  # statistical transformation into plot-ready data
  compute_panel = function(data, scales,
                           flat = TRUE) {
    
    # switch to flat orientation
    data$end <- data$end - data$start
    
    # return the transformed data frame
    data
  }
)

#' @rdname persistence
#' @export
geom_persistence <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             flat = FALSE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  layer(
    geom = GeomPersistence,
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
GeomPersistence <- ggproto(
  "GeomPersistence", GeomPoint,
  
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
    grob$name <- grid::grobName(grob, "geom_persistence")
    grob
  }
)
