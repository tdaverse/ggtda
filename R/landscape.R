#' @include persistence.R
NULL

#' @name persistence
#' @include persistence.R
#' @import ggplot2
#' @family plot layers for persistence data
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param n_levels The number of levels to compute and plot. If `Inf` (the
#'   default), determined to be all levels.
NULL

# file.edit("inst/examples/ex-landscape.R")


#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export    
StatLandscape <- ggproto(
  "StatLandscape", StatPersistence,
  
  positional_aes = c("x", "y"),
  extra_params = c(StatPersistence$extra_params, "n_levels"),
  
  setup_params = function(self, data, params) {
    
    # Different default `diagram` compared to StatPersistence
    params$diagram <- params$diagram %||% "landscape"
    
    # StatPersistence doesn't have `n_levels`
    params$n_levels <- params$n_levels %||% Inf
    
    # Continue with `StatPersistence$setup_params()`
    StatPersistence$setup_params(data, params)
  },
  
  derive_positional_aes = function(data, params) {
    
    # persistence homology -> path representation of landscape diagram
    data <- landscape_path(data, params$n_levels %||% Inf)
    
    # diagram transformation
    data <- diagram_transform(data, params$diagram %||% "landscape")
    data$slope <- diagram_slope(params$diagram %||% "landscape")
    
    # TODO: If birth + death is specified they're dropped!
    #       Need to keep them around as they're "required" aesthetics!
    
    data
  }
)

#' @rdname persistence
#' @order 3
#' @export
stat_landscape <- function(mapping = NULL,
                           data = NULL,
                           geom = "landscape",
                           position = "identity",
                           filtration = "Rips",
                           diameter_max = NULL,
                           radius_max = NULL,
                           max_hom_degree = 1L,
                           field_order = 2L,
                           engine = NULL,                          
                           diagram = "landscape",
                           n_levels = Inf,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    geom = geom,
    data = data,
    mapping = mapping,
    stat = StatLandscape,
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
  "GeomLandscape", Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = GeomPath$default_aes,
  
  draw_key = GeomPath$draw_key,
  
  draw_group = function(
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10
  ) {
    
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
    end <- c(group_diff, TRUE)
    grob <- grid::segmentsGrob(
      x0 = munched$x[!end],
      y0 = munched$y[!end],
      x1 = munched$x[!start],
      y1 = munched$y[!start],
      default.units = "native",
      arrow = NULL,
      gp = grid::gpar(
        col = alpha(munched$colour, munched$alpha)[!end],
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

#' @rdname persistence
#' @order 3
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

# data argument is a data.frame with columns "start" and "end",
# as returned by engines
landscape_path <- function(data, n_levels = Inf) {
  
  # empty case
  if (nrow(data) == 0L) {
    data$x <- data$birth
    data$y <- data$death
    
    return(data)
  }
  
  # `data$group` encodes both PANEL and group (dimension)
  data_split <- split(data, data$group)
  data_split <- lapply(data_split, landscape_path_group, n_levels = n_levels)
  data <- do.call(rbind, data_split)
  
  data
}

landscape_path_group <- function(data, n_levels = Inf) {
  
  # Keep group-level aesthetics to attach to landscape representation
  first_row <- data[1L, setdiff(names(data), c("birth", "death", "part", "persistence")), drop = FALSE]
  rownames(first_row) <- NULL
  
  # iteratively peel and stack frontiers
  # NB: points along slopes are not discarded
  # Including additional columns to preserve as computed variables,
  # this allows mapping to, for example, aes(linewidth = after_stat(persistence))
  pd <- as.data.frame(data[, c("birth", "death", "birth", "death", "part", "persistence"), drop = FALSE])
  colnames(pd) <- c("x", "y", "birth", "death", "part", "persistence")
  pl <- list()
  k <- 0L
  
  while (k < n_levels && nrow(pd) > 0L) {
    k <- k + 1L
    
    # identify frontier points
    pd <- pd[order(pd[, 1L], -pd[, 2L]), , drop = FALSE]
    peak_ids <- which(! duplicated(cummax(pd[, 2L])))
    peaks <- pd[peak_ids, , drop = FALSE]
    
    # information about feature
    feature_info <- peaks[1 , c("birth", "death", "part", "persistence")]
    
    # identify col points
    cols <- cbind(
      x = c(peaks[, 1L], peaks[nrow(peaks), 2L]),
      y = c(peaks[1L, 1L], peaks[, 2L])
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
    frontier <- rbind(peaks[, c("x", "y")], cols)
    frontier <- frontier[order(frontier[, 1L], frontier[, 2L]), ]
    frontier <- rbind(
      # frontier[1L, ] - Inf,
      c(-Inf, -Inf),
      frontier,
      # frontier[nrow(frontier), ] + Inf
      c(Inf, Inf)
    )
    
    # Include birth and death in output?
    frontier <- cbind(frontier, feature_info)
      
    pl[[k]] <- frontier
    
    # reset persistence diagram
    pd <- pd[-peak_ids, , drop = FALSE]
    pd <- rbind(pd, cols[-c(1L, nrow(cols)), , drop = FALSE])
    pd <- pd[pd[, 1L] < pd[, 2L], , drop = FALSE]
  }
  
  # data frame
  data <- do.call(rbind, pl)
  data <- as.data.frame(data)
  data$level <- rep(seq(length(pl)), sapply(pl, nrow))
  
  data <- cbind(data, first_row)
  rownames(data) <- NULL
  
  data
}


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
