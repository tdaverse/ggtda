#' @title Persistence diagrams
#'
#' @description Visualize persistence data in a (flat, diagonal, or landscape)
#'   persistence diagram.
#'   

#' @details
#'
#' {*Persistence diagrams*} are
#' [scatterplots](https://ggplot2.tidyverse.org/reference/geom_point.html) of
#' persistence data.
#' 

#' @template persistence-data
#'

#' @section Persistence diagrams:

#'   Persistence diagrams recognize extended persistence data, with negative
#'   birth/death values arising from the relative part of the filtration.
#'
#'   The original persistence diagrams plotted persistence against birth in what
#'   we call "flat" diagrams, but most plot death against birth in "diagonal"
#'   diagrams, often with a diagonal line indicating zero persistence.
#'
#'   The `geom_fundamental_box()` layer renders fundamental boxes at specified
#'   time points (Chung & Lawson, 2020).
#'   

#' @template ref-edelsbrunner2000
#' @template ref-edelsbrunner2012
#' @template ref-chung2020
#'   

#' @eval rd_sec_aesthetics(
#'   stat_persistence = StatPersistence,
#'   geom_fundamental_box = GeomFundamentalBox
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "persistence",
#'   start = "birth value of each feature (from 'dataset' aesthetic).",
#'   end = "death value of each feature (from 'dataset' aesthetic).",
#'   dimension = "integer feature dimension (from 'dataset' aesthetic).",
#'   group = "interaction of existing 'group', dataset ID, and 'dimension'.",
#'   id = "character feature identifier (across 'group').",
#'   part =
#'   "whether features belong to ordinary, relative, or extended homology.",
#'   persistence =
#'   "differences between birth and death values of features."
#' )

#' @name persistence
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical: if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category, plotted in grey
#'   (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param order_by A character vector of required or computed variables
#'   (`"start"`, `"end"`, `"part"`, and/or `"persistence"`) by which the
#'   features should be ordered (within `group`); defaults to `c("persistence",
#'   "start")`. This will most notably impact the appearance of [barcode]s.
#' @param decreasing Logical; whether to sort features by decreasing values of
#'   `order_by` (again, within `group`).
#' @param diagram One of `"flat"`, `"diagonal"`, or `"landscape"`; the
#'   orientation for the diagram should take.
#' @param t A numeric vector of time points at which to place fundamental boxes.

# REVIEW: Single data set param?
# @param point_cloud Optional; a single data set for which methods exist to
#   compute persistent homology. Alternatively, a list column of data sets can
#   be passed to the `dataset` aesthetic.

#' @param filtration The type of filtration from which to compute persistent
#'   homology; one of `"Rips"`, `"Vietoris"` (equivalent) or `"alpha"`.
#' @param diameter_max,radius_max Maximum diameter or radius for the simplicial
#'   filtration. Both default to `NULL`, in which case the complete filtration
#'   is constructed.
#' @param dimension_max Maximum dimension of the simplicial filtration.
#' @param field_order (Prime) order of the field over which to compute
#'   persistent homology.
#' @param engine The computational engine to use (see 'Details'). Reasonable
#'   defaults are chosen based on `filtration`.
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
  
  required_aes = c("start|dataset", "end|dataset"),
  
  # optional_aes = c("dataset"),
  
  # only explicitly passed params
  setup_params = function(data, params) {
    
    if (is.null(data$dataset)) {
      # discard unused parameters
      
      dataset_params <- intersect(
        names(data),
        c("filtration", "diameter_max", "radius_max", "dimension_max",
          "field_order")
      )
      if (length(dataset_params) > 0L) {
        params_vec <- paste0("`", dataset_params, "`", collapse = ", ")
        warning("Parameters ", params_vec,
                " are only used with the `dataset` aesthetic.")
        params <- params[setdiff(names(params), params_vec)]
      }
      
    } else {
      # pre-process filtration parameters
      
      # logic to deduce reasonable values of engine
      # + issue warnings when choices are incompatible
      params$filtration <-
        match.arg(params$filtration, c("Vietoris", "Rips", "alpha"))
      if (! is.null(params$engine)) params$engine <- 
          match.arg(params$engine, c("TDA", "GUDHI", "Dionysus", "ripserr"))
      params$engine <-
        assign_filtration_engine(params$filtration, params$engine)
      
    }
    
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
  
  setup_data = function(data, params) {
    
    # REVIEW: Single data set param?
    # # check if data was provided via 'point_cloud' argument
    # if (! is.null(params$point_cloud)) {
    #   
    #   if (is.null(data$dataset)) {
    #     
    #     if (nrow(data) > 1) {
    #       # TODO: fix this message
    #       stop("The 'point_cloud' argument requires at most 1 row of `data`.")
    #     }
    #     data$dataset <- I(list(point_cloud))
    #     
    #   } else {
    #     
    #     warning(
    #       "An argument was passed to the 'dataset' aesthetic,",
    #       " so the 'point_cloud' argument will be ignored."
    #     )
    #     params$point_cloud <- NULL
    #     
    #   }
    #   
    # }
    
    if (! is.null(data$dataset)) {
      
      if (! is.null(data$start) || ! is.null(data$end)) {
        warning("Map to either `start` and `end` or to `dataset` aesthetic, ",
                "not both. `dataset` will be used.")
        data$end <- data$start <- NULL
      }
      
      # compute PH listwise
      ph_list <- switch(
        params$engine,
        "TDA" = simplicial_filtration_TDA(
          data$dataset, params$filtration,
          params$diameter_max, params$dimension_max, params$field_order,
          library = "GUDHI"
        ),
        "GUDHI" = simplicial_filtration_TDA(
          data$dataset, params$filtration,
          params$diameter_max, params$dimension_max, params$field_order,
          library = "GUDHI"
        ),
        "Dionysus" = simplicial_filtration_TDA(
          data$dataset, params$filtration,
          params$diameter_max, params$dimension_max, params$field_order,
          library = "Dionysus"
        ),
        "ripserr" = simplicial_filtration_ripserr(
          data$dataset,
          params$diameter_max, params$dimension_max, params$field_order
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
      data$dataset <- NULL
      
    }
    
    data
  },
  
  compute_panel = function(
    data, scales,
    order_by = c("persistence", "start"),
    decreasing = FALSE,
    diagram = "diagonal",
    # REVIEW: Single data set param?
    # point_cloud = NULL,
    # 'dataset' aesthetic
    filtration = "Rips",
    diameter_max = NULL, radius_max = NULL, dimension_max = 1L,
    field_order = 2L,
    engine = NULL
  ) {
    
    # points in cartesian coordinates (un-negated from opposite filtration)
    data$x <- abs(data$start)
    data$y <- abs(data$end)
    
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
    
    # computed variable: `id` (sort by `group`, then `order_by`)
    interaction_args <- c(
      # always sort first by `group`
      if (! is.null(data$group)) list(data$group),
      # sort by specified properties in order
      lapply(order_by, \(f) if (decreasing) -xtfrm(data[[f]]) else data[[f]]),
      # drop unused levels and use lexicographic order
      list(drop = TRUE, lex.order = TRUE)
    )
    data$id <- do.call(interaction, args = interaction_args)
    # re-distinguish duplicates
    data$id <- order(order(data$id))
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    
    # return point data
    data
  }
)

#' @rdname persistence
#' @export
stat_persistence <- function(mapping = NULL,
                             data = NULL,
                             geom = "point",
                             position = "identity",
                             # REVIEW: Single data set param?
                             # point_cloud = NULL,
                             filtration = "Rips",
                             diameter_max = NULL, radius_max = NULL,
                             dimension_max = 1L,
                             field_order = 2L,
                             engine = NULL,
                             order_by = c("persistence", "start"),
                             decreasing = FALSE,
                             diagram = "diagonal",
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
      # REVIEW: Single data set param?
      # point_cloud = point_cloud,
      filtration = filtration,
      diameter_max = diameter_max, radius_max = radius_max,
      dimension_max = dimension_max,
      field_order = field_order,
      engine = engine,
      order_by = order_by,
      decreasing = decreasing,
      diagram = diagram,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFundamentalBox <- ggproto(
  "GeomFundamentalBox", GeomPolygon,
  
  required_aes = c(),
  default_aes = aes(colour = "black", fill = "grey",
                    linewidth = 0.5, linetype = 1, alpha = .25),
  
  setup_data = function(data, params) {
    
    # keep only columns that are constant throughout the data
    data <- dplyr::select_if(
      data,
      function(x) length(unique(x)) == 1L
    )[1L, , drop = FALSE]
    rownames(data) <- NULL
    
    data
  },
  
  draw_panel = function(data, panel_params, coord,
                        order_by = c("persistence", "start"),
                        decreasing = FALSE,
                        diagram = "diagonal", t = NULL) {
    
    # expand ranges if appropriate
    # adapted from `GeomAbline$draw_panel`
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      #ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
      ranges$y <- ranges$y + c(-1, 1) * diff(ranges$y)
    }
    
    # define segments and interior in default (diagonal) layout
    # use `group` to separate boxes for different times
    data$x <- NULL; data$xend <- NULL
    data$y <- NULL; data$yend <- NULL
    data$group <- NULL
    ray_data <- data.frame(
      x = as.vector(rbind(0, t)), xend = rep(t, each = 2L),
      y = rep(t, each = 2L), yend = as.vector(rbind(t, ranges$y[[2L]])),
      group = rep(seq_along(t), each = 2L)
    )
    ray_data <- merge(ray_data, data)
    ray_data$group <- -1L
    int_data <- data.frame(
      x = as.vector(rbind(t, t, 0, 0)),
      y = as.vector(rbind(t, ranges$y[[2L]], ranges$y[[2L]], t)),
      group = rep(seq_along(t), each = 4L)
    )
    int_data <- merge(int_data, data)
    int_data$colour <- "transparent"
    
    # diagram transformations
    ray_data <- diagram_transform(ray_data, diagram)
    int_data <- diagram_transform(int_data, diagram)
    
    # combine grobs for rays and interior
    ray_grob <- GeomSegment$draw_panel(ray_data, panel_params, coord)
    int_grob <- GeomPolygon$draw_panel(int_data, panel_params, coord)
    grob <- do.call(grid::grobTree, list(ray_grob, int_grob))
    grob$name <- grid::grobName(grob, "geom_fundamental_box")
    grob
  },
  
  draw_key = draw_key_blank,
  
  # https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @rdname persistence
#' @export
geom_fundamental_box <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 diagram = "diagonal",
                                 t = NULL,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 ...) {
  layer(
    geom = GeomFundamentalBox,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      t = t,
      na.rm = na.rm,
      ...
    )
  )
}

# Helper functions ---------------------------------------------------------

order_by_options <- c("start", "end", "part", "persistence")

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
        0,
        (data$y - data$x) / 2
      )
    )
  )
}

diagram_slope <- function(diagram) {
  switch(
    match.arg(diagram, c("flat", "diagonal", "landscape")),
    flat = 0,
    diagonal = 1,
    landscape = 0
  )
}
