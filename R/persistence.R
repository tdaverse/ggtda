#' @title Persistence homologies
#'
#' @description 
#'   Compute persistence homologies with `stat_persistence()` and
#'   plot different visual representations with
#'   persistence diagrams, persistence landscapes, and barcode diagrams
#'   using `geom_persistence()`, `geom_landscape()`, and `geom_barcode()`, respectively.
#'   
#'   Briefly, these representations can be understood as follows:
#'   \itemize{
#'   
#'   \item{**Persistence diagrams**}{
#'   are [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html) of
#'   persistence data.
#'   }
#'   
#'   \item{**Persistence landscapes**}{
#'   TODO
#'   }
#'   
#'   \item{**Barcode diagrams**}{
#'   are [vertical interval
#'   plots](https://ggplot2.tidyverse.org/reference/geom_linerange.html) of
#'   persistence data.
#'   }
#'   
#'   }
#'   
#'   For a more thorough treatment of these representations,
#'   refer to the **Details** section below and
#'   \code{vignette("visualize-persistence", package = "ggtda")}.
#'   
#' @eval rd_sec_aesthetics(
#'   stat_persistence = StatPersistence,
#'   geom_persistence = GeomPersistence,
#'   geom_landscape = GeomLandscape,
#'   geom_barcode = GeomBarcode,
#'   geom_fundamental_box = GeomFundamentalBox
#' )
#' 
#' @details
#' 
#' Here we provide clarification on the two ways these functions accept data 
#' (via the `dataset` aesthetic with `stat_persistence()` and the 
#' `start` and `end` aesthetics in with `stat_identity()`).
#' We also provide basic theoretical treatments of the methods for visualizing 
#' persistent homologies.
#' 
#' ## Point cloud data:
#' 
#'   TODO - Speak to types of data `dataset` aesthetic accepts, preempting 
#'   discussion of "persistence data".
#'   
#'   TODO - Include minor details on how **ggtda** accepts point cloud data
#'   (list column with matrix, dataframe, etc).
#'   Direct to \code{vignette("grouped-list-data", package = "ggtda")}.
#' 
#' ## Persistence data:
#' 
#'   *Persistence data* encode the values of an underlying parameter
#'   \eqn{\epsilon} at which topological features appear ("birth") and disappear
#'   ("death"). The difference between the birth and the death of a feature is
#'   called its *persistence*. As topological features may be of different
#'   dimensions, persistence data sets usually also include the dimension of
#'   each feature.
#'
#'   Persistence data can be provided to `geom_persistence()`, `geom_landscape()`,
#'   and `geom_landscape()` by specifying `stat = "identity"` and mapping the
#'   `start` and `end` aesthetics to the "birth" and "death" values in the layer data.
#'   It is standard to also map aesthetics such as `color` or `linetype` to
#'   features' "dimension" values in the layer data.
#'
#' ## Persistence diagrams:
#' 
#'   Persistence diagrams recognize extended persistence data, with negative
#'   birth/death values arising from the relative part of the filtration.
#'
#'   The original persistence diagrams plotted persistence against birth in what
#'   we call "flat" diagrams, but most plot death against birth in "diagonal"
#'   diagrams, often with a diagonal line indicating zero persistence.
#'   In `geom_persistence()`, these alternatives can be specified with 
#'   the `diagram` parameter.
#'
#'   The `geom_fundamental_box()` layer renders fundamental boxes at specified
#'   time points (Chung & Lawson, 2020).
#'   
#' ## Persistence landscapes:
#' 
#' Persistence landscapes, anticipated by some alternative coordinatizations
#' of persistence diagrams, were proposed as Lipschitz functions that demarcate
#' the Pareto frontiers of persistence diagrams. They can be averaged over the
#' diagrams obtained from multiple data sets designed or hypothesized to have
#' been generated from the same underlying topological structure.
#' 
#' Note: `geom_persistence()` does not currently recognize extended persistence data.
#'   
#' ## Barcode diagrams:
#'
#'   Barcode diagrams traditionally extend along the horizontal axis and are arranged
#'   vertically in order of group (e.g. dimension) and birth. They may also be
#'   transposed and juxtaposed with persistence diagrams. While
#'   topological features of different dimensions are usually plotted together
#'   in persistence diagrams, barcode diagrams often separate segments corresponding to
#'   features of different dimension, by vertical grouping or by faceting.
#'
#'   
#' @template ref-edelsbrunner2000
#' @template ref-edelsbrunner2012
#' @template ref-chung2020
#' 
#' @template ref-bubenik2015
#' @template ref-chazal2017
#' 
#' @template ref-carlsson2004
#' @template ref-carlsson2014
#' @template ref-chazal2017
#'
#' @eval rd_sec_computed_vars(
#'   stat = "persistence",
#'   start = "birth value of each feature (from 'dataset' aesthetic).",
#'   end = "death value of each feature (from 'dataset' aesthetic).",
#'   dimension = "integer feature dimension (from 'dataset' aesthetic).",
#'   group = "interaction of existing 'group', dataset ID, and 'dimension'.",
#'   part =
#'   "whether features belong to ordinary, relative, or extended homology.",
#'   persistence =
#'   "differences between birth and death values of features."
#' )
#' 
#' @name persistence
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical: if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category, plotted in grey
#'   (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param order_by A character vector comprised of (`"start"`, `"end"`,
#'  `"part"`, and/or `"persistence"`) by which the features should be ordered 
#'  (within `group`); defaults to `c("persistence", "start")`.
#' @param decreasing Logical; whether to sort features by decreasing values of
#'   `order_by` (again, within `group`).
#' @param diagram One of `"flat"`, `"diagonal"`, or `"landscape"`; the
#'   orientation for the diagram should take.
#' @param t A numeric vector of time points at which to place fundamental boxes.
#' @param filtration The type of filtration from which to compute persistent
#'   homology; one of `"Rips"`, `"Vietoris"` (equivalent) or `"alpha"`.
#' @param diameter_max,radius_max Maximum diameter or radius for the simplicial
#'   filtration. Both default to `NULL`, in which case the complete filtration
#'   is constructed.
#' @param max_hom_degree Maximum dimension of the simplicial filtration,
#'  the highest-dimensional features to be calculated.
#' @param field_order (Prime) order of the field over which to compute
#'   persistent homology.
#' @param engine The computational engine to use (see 'Details'). Reasonable
#'   defaults are chosen based on `filtration`.
#' @example inst/examples/ex-persistence.R
#' @example inst/examples/ex-landscape.R
#' @example inst/examples/ex-barcode.R
#' @example inst/examples/ex-barcode.R
#' @example inst/examples/ex-persistence-extended.R
#' @example inst/examples/ex-persistence-dataset.R
NULL

# file.edit("tests/testthat/test-persistence.R")
# file.edit("inst/examples/ex-persistence.R")
# file.edit("inst/examples/ex-persistence-extended.R")
# file.edit("inst/examples/ex-persistence-dataset.R")

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPersistence <- ggproto(
  "StatPersistence", Stat,
  
  required_aes = "dataset",
  
  dropped_aes = "dataset",
  
  # only explicitly passed params
  setup_params = function(self, data, params) {
    
    # Check that `start` and `end` aesthetics haven't been incorrectly supplied
    if (!is.null(data$start) | !is.null(data$end)) {
      stop(paste0(
        "`start` and `end` aesthetics have been supplied.\n",
        class(self)[1],
        " only accepts the `dataset` aesthetic.\nDid you mean to use `stat = \"identity\"`?"
      ))
    }
    
    # pre-process filtration parameters
    
    # logic to deduce reasonable values of engine
    # + issue warnings when choices are incompatible
    params$filtration <-
      match.arg(params$filtration, c("Vietoris", "Rips", "alpha"))
    
    if (! is.null(params$engine)) params$engine <- 
        match.arg(params$engine, c("TDA", "GUDHI", "Dionysus", "ripserr"))
    
    params$engine <-
      assign_filtration_engine(params$filtration, params$engine)
      
    
    # reconcile thresholds
    if (is.null(params$radius_max) && is.null(params$diameter_max)) {
      params$diameter_max <- Inf
    }
    if (! is.null(params$radius_max)) {
      if (! is.null(params$diameter_max)) {
        warning("Both `radius_max` and `diameter_max` were passed; ",
                "only `diameter_max` value will be used.")
      } else {
        params$diameter_max <- params$radius_max * 2
      }
    }
    
    params

  },
  
  setup_data = function(data, params) {
    
    # compute PH listwise
    ph_list <- switch(
      params$engine,
      "TDA" = simplicial_filtration_TDA(
        data$dataset, params$filtration,
        params$diameter_max, params$max_hom_degree, params$field_order,
        library = "GUDHI"
      ),
      "GUDHI" = simplicial_filtration_TDA(
        data$dataset, params$filtration,
        params$diameter_max, params$max_hom_degree, params$field_order,
        library = "GUDHI"
      ),
      "Dionysus" = simplicial_filtration_TDA(
        data$dataset, params$filtration,
        params$diameter_max, params$max_hom_degree, params$field_order,
        library = "Dionysus"
      ),
      "ripserr" = simplicial_filtration_ripserr(
        data$dataset,
        params$diameter_max, params$max_hom_degree, params$field_order
      )
    )
    
    # introduce identifier (and overwrite `dataset` column)
    data$dataset <- seq(nrow(data))
    for (i in seq_along(ph_list)) ph_list[[i]]$dataset <- i
    # bind the list of output data frames
    ph_data <- do.call(rbind, ph_list)
    
    # merge persistent homology data back into original data
    data <- merge(data, ph_data, by = "dataset")
    
    # introduce or interact with 'group' aesthetic
    data$group <- if (is.null(data$group)) {
      interaction(as.character(data$dataset), data$dimension)
    } else {
      interaction(data$group, as.character(data$dataset), data$dimension)
    }
    # data$dataset <- NULL
      
    
    data
  },
  
  compute_panel = function(
    data, 
    scales,
    filtration = "Rips",
    diameter_max = NULL, 
    radius_max = NULL, 
    max_hom_degree = 1L,
    field_order = 2L,
    engine = NULL
  ) {
    
    data$x <- data$start
    data$y <- data$end
    
    # Cast dimension as ordered factor, with levels ranging from 0 to specified max dim
    data$dimension <- ordered(data$dimension, c(0, seq_len(max_hom_degree)))
    
    # TODO -- Cory thinks these should likely be removed?
    # compute 'part'
    data$part <- with(data, {
      part <- NA_character_
      part[start >= 0 & end >= 0] <- "ordinary"
      part[start <  0 & end <  0] <- "relative"
      part[start >= 0 & end <  0] <- "extended"
      factor(part, levels = c("ordinary", "relative", "extended"))
    })
    
    # compute 'persistence'
    data$persistence <- data$end - data$start
    # (negative or infinite for extended points?)
    # data$persistence <- ifelse(data$persistence < 0, Inf, data$persistence)
    
    data
  }
)

#' @rdname persistence
#' @order 1
#' @export
stat_persistence <- function(mapping = NULL,
                             data = NULL,
                             geom = "persistence",
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
    stat = StatPersistence,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      filtration = filtration,
      diameter_max = diameter_max, radius_max = radius_max,
      max_hom_degree = max_hom_degree,
      field_order = field_order,
      engine = engine,
      order_by = order_by,
      decreasing = decreasing,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomPersistence <- ggproto(
  "GeomPersistence", GeomPoint,
  
  # Can use our StatPersistence
  # OW, can pre-process and use StatIdentity
  required_aes = c("dataset|start", "dataset|end"),
  
  setup_data = function(data, params) {
    
    if (is.null(data$dataset)) {
      data$x <- data$start
      data$y <- data$end
    }
    
    data
    
  },
  
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE, diagram = "diagonal") {
    
    # First transform (start, end) x-y pairs to desired parameterization
    data <- diagram_transform(data, diagram)
    
    # Use GeomPoint's draw method with transformed data
    grob <- GeomPoint$draw_panel(data, panel_params, coord, na.rm)
    grob$name <- grid::grobName(grob, "geom_persistence")
    grob
    
  }
  
)


#' @rdname persistence
#' @order 2
#' @export
geom_persistence <- function(mapping = NULL,
                             data = NULL,
                             stat = "persistence",
                             position = "identity",
                             diagram = "diagonal",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  
  c(
    layer(
      stat = stat,
      data = data,
      mapping = mapping,
      geom = GeomPersistence,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        diagram = diagram,
        na.rm = na.rm,
        ...
      )
    ),
    # TODO: While this ensures equal aspect ratio, it doesn't ensure equal limits.
    #       Is there a way to force `xlim = ylim`?
    coord_fixed()
  )
}


# Helper functions ---------------------------------------------------------

# Transform given (x, y) columns in `data` which represent (birth, death) values
# into different parameterizations/coordinate systems
diagram_transform <- function(data, diagram) {
  switch(
    match.arg(diagram, c("flat", "diagonal", "landscape")),
    flat = transform(
      data,
      y = data$y - data$x
    ),
    diagonal = data,
    landscape = transform(
      data,
      x = (data$x + data$y) / 2,
      y = ifelse(
        is.infinite(data$x) & is.infinite(data$y),
        # accommodate landscape horizons
        0,
        (data$y - data$x) / 2
      )
    )
  )
}
