#' @name persistence
#' @title Persistence Diagrams
#'
#' @description 
#'   Visualize persistence homologies with persistence diagrams.
#'   Also, render fundamental boxes at specified time points with
#'   `geom_fundamental_box()`. 
#' 
#' @details
#'   *Persistence diagrams* are 
#'   [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html) of
#'   persistence data.
#'   
#' @template persistence-data  
#' @template persistence-computed-vars  
#' 
#' @template ref-edelsbrunner2000
#' @template ref-edelsbrunner2012
#' @template ref-chung2020
#' 
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical: if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category, plotted in grey
#'   (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
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
#' @param infinity_break Where to plot the death of features with infinite
#'  persistence (i.e. `death == Inf`). Defaults to `Inf`, corresponding to the edge
#'  of the plotting window.
#'   
#' @eval rd_sec_aesthetics(
#'   stat_persistence = StatPersistence,
#'   geom_persistence = GeomPersistence
#' )#'   
#'   
#' @example inst/examples/ex-persistence.R
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
  
  # "StatPersistence", StatIdentity, 
  # Inhereit from StatIdentity because it allows points at infinity?
  # But it skips compute_group()...
  
  required_aes = c("dataset|birth", "dataset|death"),
  
  dropped_aes = "dataset",
  
  extra_params = c(
    "filtration", 
    "diameter_max",
    "radius_max",
    "max_hom_degree",
    "field_order",
    "engine",
    "diagram",
    "na.rm"
  ),
  
  setup_params = function(self, data, params) {
    
    # Assign default values, in case they're not specified in `geom_*()` call
    # (`self$setup_data()` doesn't get default parameter values!)
    params$filtration <- params$filtration %||% "Rips"
    params$diameter_max <- params$diameter_max %||% NULL
    params$radius_max <- params$radius_max %||% NULL
    params$max_hom_degree <- params$max_hom_degree %||% 1L
    params$field_order <- params$field_order %||% 2L
    params$engine <- params$engine %||% NULL
    params$diagram <- params$diagram %||% "diagonal"
    params$infinity_break <- params$infinity_break %||% Inf
    
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
  
  # calculations are all rowwise, no danger to do them here vs `$compute_*()`
  setup_data = function(self, data, params) {
    
    # If `dataset` aesthetic is supplied first calculate persistence homology
    if (! is.null(data$dataset)) {
    
      # Compute PH listwise
      # `switch()` will be replaced by {pheng} exports
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
      
      # Cast dimension as ordered factor, with levels ranging from 0 to specified max dim
      # TODO -- should this instead be just a factor? Avoid Viridis w/ 2 levels?
      data$dimension <- ordered(data$dimension, c(0, seq_len(params$max_hom_degree)))
    }
    
    # compute `persistence`
    data$persistence <- data$death - data$birth
    # (negative or infinite for extended points?)
    # data$persistence <- ifelse(data$persistence < 0, Inf, data$persistence)
   
    # Issue warning if any finite `death` values above supplied infinity break
    if (any(params$infinity_break < setdiff(data$death, Inf))) {
      warning(
        "Persistence homology `death` values exceed specified `infinity_break`.\n",
        "This can result in misleading visuals, consider choosing a larger value.",
        call. = FALSE
      )
    }
      
    # Computed variable, what features have death at Inf
    data$infinite <- is.infinite(data$death)
    # TODO: similar computed variable for censored death once {ripserr} PR is finished
    
    # Temporarily set death as `params$infinity_break`,
    # this allows plotting death at finite value in {ggplot2} --
    # we make this change for the positional aesthetics and then reverse it
    # for the computed aesthetic `death`
    data$death[data$infinite] <- params$infinity_break
    # TODO: similar trick for censored death once {ripserr} PR is finished
     
    # Different Stats will derive these in other ways, custom method to handle
    # Note: these must be rowwise, calculated on entire `data`, not group-level
    data <- self$derive_positional_aes(data, params)
    
    # Temporarily set infinite death values to be their features' birth values
    # to avoid issues with deafult filtering by `Stat$compute_layer()`
    #   -- This is reverse in `compute_group()`.   
    data$death[data$infinite] <- data$birth[data$infinite]
    # TODO: similar trick for censored death once {ripserr} PR is finished
    
    data
  },
  
  # Stat-specific positional aesthetics,
  # must manually handle there scale transforms in $compute_group()
  positional_aes = c("x", "y"),
  
  derive_positional_aes = function(data, params) {
    data$x <- data$birth
    data$y <- data$death
    
    data <- diagram_transform(data, params$diagram)
    
    data
  },
  
  # Stat$compute_layer is removing points at infinity!
  # Can't access `scales` in compute_layer... how to fix?
  
  compute_group = function(self, data, scales, infinity_break = Inf) {
    
    # Reintroduce infinite values of `data$death
    data$death[data$infinite] <- infinity_break
    # TODO: Will need similar re-coding for censored deaths
    
    # Make sure positional aesthetics get back transformed from scales
    fix_positional_aes_scales(data, scales, self$positional_aes)
  }
  
)


# Apply scale transformations (if specified) to `positional_aes`
fix_positional_aes_scales <- function(data, scales, positional_aes) {

  # axis each positional aesthetic belongs to
  axes <- regmatches(positional_aes, regexpr("^(x|y)", positional_aes))
  
  for (i in seq_along(positional_aes)) {
    # current positional aesthetic 
    var <- positional_aes[i]
    var_axis <- axes[i]
    
    # If there is a scale transformation overwrite data$var (positional_aes[i])
    if (! is.null(scales[[var_axis]])) {
      # fix the column in `data` corresponding to var, per axis transformation
      data[[var]] <- scales[[var_axis]]$get_transformation()$transform(data[[var]])
    }
  }
  
  data
}

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
                             diagram = "diagonal",
                             infinity_break = Inf,
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
      diameter_max = diameter_max,
      radius_max = radius_max,
      max_hom_degree = max_hom_degree,
      field_order = field_order,
      engine = engine,
      infinity_break = infinity_break,
      diagram = diagram,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomPersistence <- ggproto(
  "GeomPersistence", GeomPoint
)


#' @rdname persistence
#' @order 2
#' @export
geom_persistence <- function(mapping = NULL,
                             data = NULL,
                             stat = "persistence",
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  
  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomPersistence,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
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
