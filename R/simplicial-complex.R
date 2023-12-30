#' @title Simplicial complexes from 2-D point clouds
#'
#' @description Construct and plot simplicial complexes that equal or
#' approximate the topology of a ball cover of a set of points.
#' 

#' @details Persistent homology is ultimately based on the topological
#'   properties of regions containing a set of points. When the region is the
#'   union of balls of a common radius, its homology is equal to or approximated
#'   by that of several families of **simplicial complexes** constructed on the
#'   point set. The simplicial complex stat constructs these simplicial
#'   complexes for a set of points in \eqn{xy}-space while the geom plots them
#'   on the same coordinates as the points.
#'

#' @section Aesthetics: `geom_simplicial_complex()` understands the following
#'   aesthetics (required aesthetics are in bold):
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

#' @section Computed variables: These are calculated by the 'stat' part of
#'   layers and can be accessed with [delayed evaluation][ggplot2::aes_eval].
#' 
#'   - `after_stat(dim)`
#'     The dimension of the corresponding simplex, an ordered factor.
#'   - `after_stat(type)`
#'     The type of simplex represented by each row, one of:
#'     `"zero_simplex"`, `"one_simplex"`, `"k_simplex"`.
#'     The type determines the graphical element used to represent the simplex
#'     (point/marker, segment, polygon).
#'   - `after_stat(simplex_id)`
#'     A set of unique simplex identifiers within each `type`.
#'

#' @name simplicial_complex
#' @import ggplot2
#' @family plot layers for constructions on 2-D point clouds
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::stat_identity
#' @param radius,diameter The radius or diameter used to construct the
#'   simplicial complex.
#' @param max_dimension Compute simplexes of dimension up to `max_dimension`
#'   (only relevant for the Vietoris-Rips complex computed with the
#'   `simplextree` engine).
#' @param complex The type of complex to compute (either `"Vietoris"`, `"Rips"`,
#'   `"Cech"`, or `"alpha"`).
#' @param engine What computational engine to use. Reasonable defaults are
#'   chosen based on `complex`.
#' @param zero_simplexes One of `"none"`, `"maximal"`, and `"all"` (default).
#' @param one_simplexes One of `"none"`, `"maximal"` (default), and `"all"`.
#' @param outlines Should the outlines of polygons representing the simplexes be
#'   drawn?
#' @example inst/examples/ex-simplicial-complex.R
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
    zero_simplexes = "all", one_simplexes = "maximal",
    max_dimension = 10, complex = "Rips", engine = NULL 
  ) {
    
    # TODO:
    # Add check for validitiy of zero_ and one_simplexes arguments
    # move to setup params method
    # handle disk size
    if (is.null(radius) && is.null(diameter)) {
      return(data[NULL, , drop = FALSE])
    }
    if (! is.null(radius)) {
      if (! is.null(diameter)) {
        warning("Pass a value to only one of `radius` and `diameter`; ",
                "`diameter` value will be used.")
      } else {
        diameter <- radius * 2
      }
    }
    
    # logic to deduce reasonable values of engine
    # + issue warnings when choices are incompatible
    engine <- assign_complex_engine(complex, engine, max_dimension)
    
    res <- switch(
      engine,
      base = simplicial_complex_base(
        data, diameter, max_dimension, complex, zero_simplexes, one_simplexes
      ),
      RTriangle = simplicial_complex_RTriangle(
        data, diameter, max_dimension, complex, zero_simplexes, one_simplexes
      ),
      simplextree = simplicial_complex_simplextree(
        data, diameter, max_dimension, complex, zero_simplexes, one_simplexes
      )
    )
    
    # TODO:
    # Take care of zero_ or one_simplexes == "none"
    # and remove simplexes w/ dim > max_dimension
    if (max_dimension < 2) {
      res <- res[res$type != "k_simplex", ]
    }
    if (max_dimension < 1 | one_simplexes == "none") {
      res <- res[res$type != "one_simplex", ]
    }
    # QUESTION: Require upstream that `max_dimension >= 0`?
    if (max_dimension < 0 | zero_simplexes == "none") {
      res <- res[res$type != "zero_simplex", ]
    }
    
    res
  }
  
)

#' @rdname simplicial_complex
#' @export
stat_simplicial_complex <- function(mapping = NULL,
                                    data = NULL,
                                    geom = "SimplicialComplex",
                                    position = "identity",
                                    radius = NULL,
                                    diameter = NULL, 
                                    zero_simplexes = "all",
                                    one_simplexes = "maximal",
                                    max_dimension = 10,
                                    complex = "Rips",
                                    engine = "simplextree", 
                                    na.rm = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE,
                                    ...) {
  layer(
    stat = StatSimplicialComplex, 
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      diameter = diameter,
      zero_simplexes = zero_simplexes,
      one_simplexes = one_simplexes,
      max_dimension = max_dimension,
      engine = engine,
      complex = complex,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomSimplicialComplex <- ggproto(
  "GeomSimplicialComplex", Geom,
  
  draw_group = function(data, panel_params, coord,
                        outlines = TRUE,
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    
    n <- nrow(data)
    
    if (n == 0) return(zeroGrob())
    
    # TODO:
    # Munching happens at the group level,
    # need to reconsider how to deal w/ transforms
    if (FALSE) {
      
      munched <- coord_munch(coord, data, panel_params)
      
      # Sort by simplex_id to make sure that colors, fill, etc. come in same
      # order
      munched <- munched[order(munched$simplex_id),]
      
      zero_simplex_data <- data[data$type == "zero_simplex",]
      one_simplex_data <- data[data$type == "one_simplex",]
      k_simplex_data <- data[data$type == "k_simplex",]
      
    } else {
      
      data <- coord$transform(data, panel_params)
      
      data <- data[order(data$simplex_id),]
      
      zero_simplex_data <- data[data$type == "zero_simplex",]
      one_simplex_data <- data[data$type == "one_simplex",]
      k_simplex_data <- data[data$type == "k_simplex",]
      
    }
    
    # List to hold various grobs (polygons, linesegments, points)
    grobs <- list()
    
    # Drawing the simplexes w/ dim > 1 -----
    if (nrow(k_simplex_data) > 0) {
      
      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each (simplex_id)_group, and assume all
      # these values are the same within each group.
      first_idx <- !duplicated(k_simplex_data$simplex_id)
      first_rows <- k_simplex_data[first_idx, ]
      
      grobs$simplexes <- grid::polygonGrob(
        k_simplex_data$x, k_simplex_data$y, default.units = "native",
        id = k_simplex_data$simplex_id,
        gp = grid::gpar(
          col = if (outlines) first_rows$colour else NA,
          fill = alpha(first_rows$fill, first_rows$alpha),
          lwd = first_rows$linewidth * .pt,
          lty = first_rows$linetype,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
      
    }
    
    # Drawing the one_simplexes -----
    if (nrow(one_simplex_data) > 0) {
      
      # First, need to collapse pairs of rows corresponding
      # to line segments (edges)
      one_simplex_data <- split(one_simplex_data, one_simplex_data$simplex_id)
      one_simplex_data <- lapply(one_simplex_data, collapse_one_simplex_pairs)
      one_simplex_data <- do.call(rbind, one_simplex_data)
      
      # Currently, can't adjust alpha of zero- and one-simplexes
      # If overplotting is an issue, set one_simplexes = "none"
      # (Similar to geom_density)
      grobs$one_simplex <- grid::segmentsGrob(
        one_simplex_data$x, one_simplex_data$y,
        one_simplex_data$xend, one_simplex_data$yend,
        default.units = "native",
        gp = grid::gpar(
          # col = alpha(one_simplex_data$colour, one_simplex_data$alpha),
          # fill = alpha(one_simplex_data$fill, one_simplex_data$alpha),
          col = one_simplex_data$colour,
          fill = one_simplex_data$fill,
          lwd = one_simplex_data$linewidth * .pt,
          lty = one_simplex_data$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = NULL # not directed
      )
      
    }
    
    # Drawing the 0-simplexes -----
    if (nrow(zero_simplex_data) > 0) {
      
      stroke_size <- zero_simplex_data$stroke
      stroke_size[is.na(stroke_size)] <- 0
      
      # Currently, can't adjust alpha of zero- and one-simplexes
      # If overplotting is an issue, set zero_simplexes = FALSE
      # (Similar to geom_density)
      grobs$zero_simplex_data <- grid::pointsGrob(
        zero_simplex_data$x, zero_simplex_data$y,
        pch = zero_simplex_data$shape,
        gp = grid::gpar(
          col = zero_simplex_data$colour,
          fill = zero_simplex_data$fill,
          # col = alpha(zero_simplex_data$colour, zero_simplex_data$alpha),
          # fill = alpha(zero_simplex_data$fill, zero_simplex_data$alpha),
          # Stroke is added around the outside of the point
          fontsize = zero_simplex_data$size * .pt + stroke_size * .stroke / 2,
          lwd = zero_simplex_data$stroke * .stroke / 2
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

#' @rdname simplicial_complex
#' @export
geom_simplicial_complex <- function(mapping = NULL, data = NULL,
                                    stat = "SimplicialComplex",
                                    position = "identity",
                                    outlines = TRUE,
                                    na.rm = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE,
                                    ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSimplicialComplex,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlines = outlines,
      na.rm = na.rm,
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
