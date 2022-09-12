#' Simplicial complexes from 2-d point clouds
#'
#' Desc here
#'
#'
#' @section Aesthetics: geom_hdr understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - **dim**
#'   - **type**
#'   - **simplex_id**
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - size
#'
#' @section Computed variables:
#'
#'   \describe{ \item{dim}{The dimension of the corresponding simplex, an ordered factor}}
#'   \describe{ \item{type}{The type of simplex represented by each row, one of:
#'   `"simplex"`, `"simplex_one"`, `"zero_skeleton"`, `"one_skeleton"`.}}
#'   \describe{ \item{simplex_id}{A set of unique ids for each simplex, for each `type`}}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::stat_identity
#' @param radius,diameter The radius or diameter used to construct the simplicial complex.
#' @param max_dimension Compute simplexes of dimension up to `max_dimension`
#' (only relevant for the Vietoris-Rips complex computed with the `simplextree` engine).
#' @param complex The type of complex to compute (either `"Vietoris"`, `"Rips"`, `"Cech"`, or `"alpha"`).
#' @param engine What computational engine to use. Reasonable defaults are chosen based on `complex`.
#' @param zero_skeleton Should the 0-skeleton be plotted?
#' @param one_skeleton Should the 1-skeleton be plotted?
#' @param simplexes Should the >0-simplexes corresponding to the maximal complex
#' be plotted?
#' @param outlines Should the outlines of polygons representing the simplexes be drawn?
#' (Only availabe for `engine = "simplextree"`).
#' 
#'
#' @name geom_simplicial_complex
#' @rdname geom_simplicial_complex
#'
#' @import ggplot2
#' @importFrom simplextree maximal
#'
#' @examples
#'
#' set.seed(1)
#'
#' s <- seq(0, 2*pi, length.out = 40)
#' df <- data.frame(
#'   x = cos(s) + rnorm(40, 0, .1),
#'   y = sin(s) + rnorm(40, 0, .1)
#' )
#'
#' # default, visualizing dim w/ alpha:
#' ggplot(df, aes(x, y)) +
#'   geom_simplicial_complex(radius = .4)
#'
#' # visualizing dim w/ fill:
#' ggplot(df, aes(x, y)) +
#'   geom_simplicial_complex(
#'     mapping = aes(fill = after_stat(dim)),
#'     alpha = .5, radius = .4
#'   )
#'
#'
#' # Visualizing multiple groups together
#' s <- c(s, s)
#' df_mix <- data.frame(
#'   x = cos(s) + rnorm(80, 0, .1),
#'   y = sin(s) + rnorm(80, 0, .1)
#' )
#'
#' df_mix$x <- df_mix$x + rep(c(-2, 2), length.out = 80)
#' df_mix$lab <- rep(c("a", "b"), length.out = 80)
#'
#' ggplot(df_mix, aes(x, y, fill = lab)) +
#'   geom_simplicial_complex(radius = .4)
NULL


#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatSimplicialComplex <-  ggproto(
  "StatSimplicialComplex", Stat,
  
  required_aes = c("x", "y"),
  
  # Alternatively, could assign fill = after_stat(dim)
  default_aes = aes(alpha = after_stat(dim)),
  
  compute_group = function(
    data, scales,
    radius = NULL, diameter = NULL, 
    max_dimension = 10, complex = "Rips", engine = NULL 
  ) {
    
    # handle disk size
    if (is.null(radius) && is.null(diameter)) {
      return(data[NULL, , drop = FALSE])
    }
    if (!is.null(radius)) {
      if (!is.null(diameter)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`diameter` value will be used.")
      } else {
        diameter <- radius * 2
      }
    }
    
    # logic to deduce reasonable values of engine
    # + issue warnings when choices are incompatible
    engine <- assign_complex_engine(complex, engine, max_dimension)
    
    res <- 
      switch(engine,
        "simplextree" = simplicial_complex_simplextree(data, diameter, max_dimension, complex),
        "base" = simplicial_complex_base(data, diameter, max_dimension, complex),
        "RTriangle" = simplicial_complex_RTriangle(data, diameter, max_dimension, complex)
      )
    
    # Remove simplexes w/ dim > max_dimension
    #  -- might be good to do this before computations happen 
    #     (at least for max_dimension < 2)
    if (max_dimension < 2) {
      res <- res[res$type != "simplex", ]
    }
    
    if (max_dimension < 1) {
      res <- res[! res$type %in% c("simplex_one", "one_skeleton"), ]
    }
    
    if (max_dimension < 0) {
      res <- res[NULL, ]
    }
    
    res
    
  }
  
)

#' @rdname geom_simplicial_complex
#' @export
stat_simplicial_complex <- function(mapping = NULL, data = NULL, geom = "SimplicialComplex",
                                    position = "identity", na.rm = FALSE, show.legend = NA,
                                    radius = NULL, diameter = NULL, 
                                    max_dimension = 10, complex = "Rips", engine = "simplextree", 
                                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatSimplicialComplex, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, radius = radius, diameter = diameter, engine = engine, max_dimension = max_dimension, complex = complex, ...)
  )
}


#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomSimplicialComplex <- ggproto("GeomSimplicialComplex", Geom,
                                 
  draw_group = function(data, panel_params, coord,
                        zero_skeleton = TRUE, one_skeleton = FALSE, simplexes = TRUE, outlines = TRUE,
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    
    n <- nrow(data)
    
    if (n == 0) return(zeroGrob())
    
    # Munching happens at the group level,
    # need to reconsider how to deal w/ transforms
    if (FALSE) {
      
      munched <- coord_munch(coord, data, panel_params)
      
      # Sort by simplex_id to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$simplex_id),]
      
      zero_skeleton_data <- munched[munched$type == "zero_skeleton",]
      one_skeleton_data <- munched[munched$type == "one_skeleton",]
      simplex_data <- munched[munched$type == "simplex",]
      simplex_one_data <- munched[munched$type == "simplex_one",]
      
    } else {
      
      data <- coord$transform(data, panel_params)
      
      data <- data[order(data$simplex_id),]
      
      zero_skeleton_data <- data[data$type == "zero_skeleton",]
      one_skeleton_data <- data[data$type == "one_skeleton",]
      simplex_data <- data[data$type == "simplex",]
      simplex_one_data <- data[data$type == "simplex_one",]
      
    }
    
    
    # List to hold various grobs (polygons, linesegments, points)
    grobs <- list()
    
    # Drawing the simplexes w/ dim = 1 -----
    if (simplexes & nrow(simplex_one_data) > 0) {
      
      # First, need to collapse pairs of rows corresponding
      # to line segments (edges)
      simplex_one_data <- split(simplex_one_data, simplex_one_data$simplex_id)
      simplex_one_data <- lapply(simplex_one_data, collapse_one_simplex_pairs)
      simplex_one_data <- do.call(rbind, simplex_one_data)
      
      # Currently, can't adjust alpha of zero- and one-skeleton
      # If overplotting is an issue, set one_skeleton = FALSE
      # (Similar to geom_density)
      grobs$simplex_one <- grid::segmentsGrob(
        simplex_one_data$x, simplex_one_data$y,
        simplex_one_data$xend, simplex_one_data$yend,
        default.units = "native",
        gp = grid::gpar(
          # col = alpha(one_skeleton_data$colour, one_skeleton_data$alpha),
          # fill = alpha(one_skeleton_data$fill, one_skeleton_data$alpha),
          col = simplex_one_data$colour,
          fill = simplex_one_data$fill,
          lwd = simplex_one_data$linewidth * .pt,
          lty = simplex_one_data$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = NULL # not directed
      )
      
    }
    
    # Drawing the simplexes w/ dim > 1 -----
    if (simplexes & nrow(simplex_data) > 0) {
      
      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each (simplex_id)_group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(simplex_data$simplex_id)
      first_rows <- simplex_data[first_idx, ]
      
      grobs$simplexes <- grid::polygonGrob(
        simplex_data$x, simplex_data$y, default.units = "native",
        id = simplex_data$simplex_id,
        gp = grid::gpar(
          col = if (outlines & !one_skeleton) first_rows$colour else NA,
          fill = alpha(first_rows$fill, first_rows$alpha),
          lwd = first_rows$linewidth * .pt,
          lty = first_rows$linetype,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
      
    }
    
    # Drawing the 1-skeleton -----
    if (one_skeleton & nrow(one_skeleton_data) > 0) {
      
      # First, need to collapse pairs of rows corresponding
      # to line segments (edges)
      one_skeleton_data <- split(one_skeleton_data, one_skeleton_data$simplex_id)
      one_skeleton_data <- lapply(one_skeleton_data, collapse_one_simplex_pairs)
      one_skeleton_data <- do.call(rbind, one_skeleton_data)
      
      # Currently, can't adjust alpha of zero- and one-skeleton
      # If overplotting is an issue, set one_skeleton = FALSE
      # (Similar to geom_density)
      grobs$one_skeleton <- grid::segmentsGrob(
        one_skeleton_data$x, one_skeleton_data$y,
        one_skeleton_data$xend, one_skeleton_data$yend,
        default.units = "native",
        gp = grid::gpar(
          # col = alpha(one_skeleton_data$colour, one_skeleton_data$alpha),
          # fill = alpha(one_skeleton_data$fill, one_skeleton_data$alpha),
          col = one_skeleton_data$colour,
          fill = one_skeleton_data$fill,
          lwd = one_skeleton_data$linewidth * .pt,
          lty = one_skeleton_data$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = NULL # not directed
      )
      
    }
    
    
    # Drawing the 0-skeleton -----
    if (zero_skeleton) {
      
      stroke_size <- zero_skeleton_data$stroke
      stroke_size[is.na(stroke_size)] <- 0
      
      # Currently, can't adjust alpha of zero- and one-skeleton
      # If overplotting is an issue, set zero_skeleton = FALSE
      # (Similar to geom_density)
      grobs$zero_skeleton <- grid::pointsGrob(
        zero_skeleton_data$x, zero_skeleton_data$y,
        pch = zero_skeleton_data$shape,
        gp = grid::gpar(
          col = zero_skeleton_data$colour,
          fill = zero_skeleton_data$fill,
          # col = alpha(zero_skeleton_data$colour, zero_skeleton_data$alpha),
          # fill = alpha(zero_skeleton_data$fill, zero_skeleton_data$alpha),
          # Stroke is added around the outside of the point
          fontsize = zero_skeleton_data$size * .pt + stroke_size * .stroke / 2,
          lwd = zero_skeleton_data$stroke * .stroke / 2
        )
      )
      
    }
    
    grid::gTree(children = do.call(grid::gList, grobs))
    
  },
 
  default_aes = aes(colour = "black", fill = "grey30", alpha = NA,
                    linewidth = 0.5, linetype = 1,
                    shape = 21, size = 1.5, stroke = .5),
  
  required_aes = c("x", "y", "simplex_id", "dim", "type"),
  
  draw_key = draw_key_simplex,
  
  rename_size = TRUE
)


#' @rdname geom_simplicial_complex
#' @export
geom_simplicial_complex <- function(mapping = NULL, data = NULL,
                                    stat = "SimplicialComplex", position = "identity",
                                    zero_skeleton = TRUE, one_skeleton = FALSE,
                                    simplexes = TRUE, outlines = TRUE,
                                    ...,
                                    na.rm = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSimplicialComplex,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      zero_skeleton = zero_skeleton, one_skeleton = one_skeleton,
      simplexes = simplexes, outlines = outlines,
      ...
    )
  )
}


# Helper functions ---------------------------------------------------------

collapse_one_simplex_pairs <- function(df) {
  res <- df[1,]
  res$xend <- df[2, "x"]
  res$yend <- df[2, "y"]
  
  res
}

# Helper function to find return rows of df
# representing convex hull of (x, y) coords
simplex_chull <- function(df) {
  df[grDevices::chull(df$x, df$y), , drop = FALSE]
}


