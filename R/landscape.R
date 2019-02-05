#' @title Persistent landscapes
#' 
#' @description Visualize persistence data in a persistence landscape.
#' 

#' @details
#'
#' *Persistence landscapes* can be understood as rotated diagonal [persistence
#' diagrams](persist).
#' 

#' @template ref-edelsbrunner2012
#' @template ref-bubenik2015
#'   

#' @name landscape
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data; defaults to
#'   `segment` in `geom_vietoris1()` and to `polygon` in `geom_vietoris2`. Pass
#'   a string to override the default.
#' @example inst/examples/ex-landscape.R

#' @rdname landscape
#' @export
stat_landscape <- function(mapping = NULL,
                           data = NULL,
                           geom = "segment",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    stat = StatLandscape,
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
StatLandscape <- ggproto(
  "StatLandscape", Stat,
  
  required_aes = c("start", "end"),
  
  compute_group = function(data, scales) {
    
    # data frame of segments
    data$id <- 1:nrow(data)
    segments <- rbind(
      data.frame(
        id = data$id,
        x = data$start, xend = (data$start + data$end) / 2,
        y = 0, yend = (data$end - data$start) / 2
      ),
      data.frame(
        id = data$id,
        x = (data$start + data$end) / 2, xend = data$end,
        y = (data$end - data$start) / 2, yend = 0
      )
    )
    
    # merge segment coordinates with data attributes
    data$x <- NULL
    data$xend <- NULL
    data$y <- NULL
    data$yend <- NULL
    data <- merge(data, segments, by = "id")
    data$id <- NULL
    
    # return landscape data
    data
  }
)

#' @rdname landscape
#' @export
stat_frontier <- function(mapping = NULL,
                          data = NULL,
                          geom = "line",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatFrontier,
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
StatFrontier <- ggproto(
  "StatFrontier", Stat,
  
  required_aes = c("start", "end"),
  
  compute_group = function(data, scales) {
    
    # first row (for aesthetics)
    first_row <- data[1, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # Pareto frontier
    data <- pareto_persistence(data)
    
    # data frame of segments
    data <- data[order(data$start), ]
    if (! all(data$end == cummax(data$end))) {
      warning("`start` and `end` are not anti-sorted.")
    }
    data <- data.frame(
      x = c(rep(data$start, each = 2), data$end[nrow(data)]),
      y = c(data$start[1], rep(data$end, each = 2))
    )
    
    # rotation transform
    data <- transform(
      data,
      x = (data$x + data$y) / 2,
      y = (data$y - data$x) / 2
    )
    
    # return frontier data
    cbind(data, first_row)
  }
)

pareto_persistence <- function(data) {
  if ("rPref" %in% rownames(utils::installed.packages())) {
    rPref::psel(data, rPref::low("start") * rPref::high("end"))
  } else {
    pd <- data[order(data$start, -data$end), ]
    pd[! duplicated(cummax(pd$end)), ]
  }
}
