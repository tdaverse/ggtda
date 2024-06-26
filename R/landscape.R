#' @title Persistence landscapes
#'
#' @description Visualize persistence data as a persistence landscape.
#'   

#' @details
#'
#' {*Persistence landscapes*}, anticipated by some alternative coordinatizations
#' of persistence diagrams, were proposed as Lipschitz functions that demarcate
#' the Pareto frontiers of persistence diagrams. They can be averaged over the
#' diagrams obtained from multiple data sets designed or hypothesized to have
#' been generated from the same underlying topological structure.
#'
#' Persistence landscapes do not currently recognize extended persistence data.
#' 

#' @template ref-bubenik2015
#' @template ref-chazal2017

#' @eval rd_sec_aesthetics(
#'   stat_landscape = StatLandscape,
#'   geom_landscape = GeomLandscape
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "landscape",
#'   "x,y" = "coordinates of segment endpoints of each frontier.",
#'   dimension = "feature dimension (with 'dataset' aesthetic only).",
#'   group = "interaction of existing 'group', dataset ID, and 'dimension'.",
#'   "level" = "position of each frontier, starting from the outermost.",
#'   "slope" = "slope of the landscape abscissa.",
#'   extra_note = paste0(
#'     "Note that ",
#'     "`start` and `end` are dropped during the statistical transformation."
#'   )
#' )

#' @name landscape
#' @include persistence.R
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @inheritParams persistence
#' @param n_levels The number of levels to compute and plot. If `Inf` (the
#'   default), determined to be all levels.
#' @example inst/examples/ex-landscape.R
#' @example inst/examples/ex-persistence-dataset.R
NULL

# file.edit("inst/examples/ex-landscape.R")

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatLandscape <- ggproto(
  "StatLandscape", Stat,
  
  # required_aes = c("start", "end"),
  required_aes = StatPersistence$required_aes,
  
  # default_aes = aes(group = interaction(after_stat(level), group)),
  
  dropped_aes = c("start", "end"),
  
  setup_data = StatPersistence$setup_data,
  
  setup_params = StatPersistence$setup_params,
  
  # compute_panel = Stat$compute_panel,
  
  compute_group = function(
    data, scales,
    diagram = "landscape",
    # 'dataset' aesthetic
    filtration = "Rips",
    diameter_max = NULL, radius_max = NULL, dimension_max = 1L,
    field_order = 2L,
    engine = NULL,
    n_levels = Inf
  ) {
    
    # empty case
    if (nrow(data) == 0L) {
      names(data)[match(c("start", "end"), names(data))] <- c("x", "y")
      return(data)
    }
    
    # first row (for aesthetics)
    first_row <- data[1L, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # iteratively peel and stack frontiers
    # NB: points along slopes are not discarded
    pd <- as.matrix(data[, c("start", "end"), drop = FALSE])
    pl <- list()
    k <- 0L
    while (k < n_levels && nrow(pd) > 0L) {
      k <- k + 1L
      
      # identify frontier points
      pd <- pd[order(pd[, 1L], -pd[, 2L]), , drop = FALSE]
      peak_ids <- which(! duplicated(cummax(pd[, 2L])))
      peaks <- pd[peak_ids, , drop = FALSE]
      
      # identify col points
      cols <- cbind(
        start = c(peaks[, 1L], peaks[nrow(peaks), 2L]),
        end = c(peaks[1L, 1L], peaks[, 2L])
      )
      
      # flatten valleys
      valley_ids <- which(cols[, 1L] > cols[, 2L])
      for (i in rev(valley_ids)) {
        cols <- rbind(
          cols[seq(i - 1L), , drop = FALSE],
          cols[i, c(2L, 2L), drop = FALSE],
          cols[i, c(1L, 1L), drop = FALSE],
          cols[seq(i + 1L, nrow(cols)), , drop = FALSE]
        )
      }
      
      # extract frontier
      frontier <- rbind(peaks, cols)
      frontier <- frontier[order(frontier[, 1L], frontier[, 2L]), ]
      frontier <- rbind(
        # frontier[1L, ] - Inf,
        c(-Inf, -Inf),
        frontier,
        # frontier[nrow(frontier), ] + Inf
        c(Inf, Inf)
      )
      pl[[k]] <- frontier
      
      # reset persistence diagram
      pd <- pd[-peak_ids, , drop = FALSE]
      pd <- rbind(pd, cols[-c(1L, nrow(cols)), , drop = FALSE])
      pd <- pd[pd[, 1L] < pd[, 2L], , drop = FALSE]
    }
    
    # data frame
    data <- do.call(rbind, pl)
    data <- as.data.frame(data)
    names(data) <- c("x", "y")
    # compute level of each frontier
    data$level <- rep(seq(length(pl)), sapply(pl, nrow))
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    data$slope <- diagram_slope(diagram)
    
    # return landscape data
    cbind(data, first_row)
  }
)

#' @rdname landscape
#' @export
stat_landscape <- function(mapping = NULL,
                          data = NULL,
                          geom = "landscape",
                          position = "identity",
                          filtration = "Rips",
                          diameter_max = NULL, radius_max = NULL,
                          dimension_max = 1L,
                          field_order = 2L,
                          engine = NULL,
                          diagram = "landscape",
                          n_levels = Inf,
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
      filtration = filtration,
      diameter_max = diameter_max, radius_max = radius_max,
      dimension_max = dimension_max,
      field_order = field_order,
      engine = engine,
      diagram = diagram,
      n_levels = n_levels,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLandscape <- ggproto(
  "GeomLandscape", GeomPath,
  
  draw_panel = function(data, panel_params, coord,
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    
    # # adapted from `ggplot2::GeomPath`
    # # (data should already be ordered; or, order by slope)
    # data <- data[order(data$group), , drop = FALSE]
    
    # adapted from `ggplot2::GeomAbline`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }
    
    # extend each level to the extended range
    data <- diagram_horizon(data, ranges)
    
    # adapted from `ggplot2::GeomPath`
    munched <- coord_munch(coord, data, panel_params)
    group_diff <- munched$group[-1L] != munched$group[-nrow(munched)]
    start <- c(TRUE, group_diff)
    end   <- c(group_diff, TRUE)
    grob <- grid::segmentsGrob(
      x0 = munched$x[!end],   y0 = munched$y[!end],
      x1 = munched$x[!start], y1 = munched$y[!start],
      default.units = "native", arrow = NULL,
      gp = grid::gpar(
        col =  alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$colour, munched$alpha)[!end],
        lwd = (munched$linewidth[!end] %||% munched$size[!end]) * .pt,
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
    grob$name <- grid::grobName(grob, "geom_landscape")
    grob
  }
)

#' @rdname landscape
#' @export
geom_landscape <- function(mapping = NULL,
                           data = NULL,
                           stat = "landscape",
                           position = "identity",
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    geom = GeomLandscape,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      ...
    )
  )
}

# pareto_ids <- function(x) {
#   if ("rPref" %in% rownames(utils::installed.packages())) {
#     pareto_ids_rPref(x)
#   } else {
#     pareto_ids_base(x)
#   }
# }
# pareto_ids_base <- function(x) {
#   ord <- order(x[, 1L], -x[, 2L])
#   x <- x[ord, , drop = FALSE]
#   order(ord)[which(! duplicated(cummax(x[, 2L])))]
# }
# pareto_ids_rPref <- function(x) {
#   x <- as.data.frame(x)
#   names(x) <- c("start", "end")
#   rPref::psel.indices(x, rPref::low("start") * rPref::high("end"))
# }

diagram_slope <- function(diagram) {
  switch(
    match.arg(diagram, c("flat", "diagonal", "landscape")),
    flat = 0,
    diagonal = 1,
    landscape = 0
  )
}

# WARNING: cannot handle infinite slope
diagram_horizon <- function(data, ranges) {
  # rows designating horizons (-1 & 1) versus peaks & cols (0)
  data$sign <- ifelse(
    (is.infinite(data$x) | is.infinite(data$y)),
    (-1) ^ c(data$level[-nrow(data)] == data$level[-1L], FALSE),
    0
  )
  # rows designating horizons
  ends <- data$sign != 0
  # minimum x coordinates to leave range
  rans <- c(
    min(ranges$x[1L],
        ifelse(data$slope == 0, ranges$y[1L], ranges$y[1L] / data$slope)),
    max(ranges$x[2L],
        ifelse(data$slope == 0, ranges$y[2L], ranges$y[2L] / data$slope))
  )
  # row indices of `rans` (only for horizons)
  inds <- (data$sign[ends] + 3) / 2
  # finitize horizons
  data$x[ends] <- rans[inds]
  data$y[ends] <- rans[inds] * data$slope[ends]
  # drop extraneous columns
  data$sign <- NULL
  data
}
