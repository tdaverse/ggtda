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
#'   dimension = "feature dimension (from 'dataset' aesthetic).",
#'   group = "interaction of existing 'group', dataset ID, and 'dimension'.",
#'   id = "feature identifier (across 'group').",
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
#' @param diagram One of `"flat"`, `"diagonal"`, or `"landscape"`; the
#'   orientation for the diagram should take.
#' @param t A numeric vector of time points at which to place fundamental boxes.
# @param point_cloud Optional; a single data set for which methods exist to
#   compute persistent homology. Alternatively, a list column of data sets can
#   be passed to the `dataset` aesthetic.
#' @param diameter_max,radius_max (Document.)
#' @param dimension_max (Document.)
#' @param field_order (Document.)
#' @param complex The type of filtration from which to compute persistent
#'   homology; currently only `"Rips"` and `"Vietoris"` (equivalent) are
#'   accepted.
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
  
  setup_data = function(data, params) {
    
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
      
      # REVIEW: move this to `stat_persistence()`?
      params$complex <- match.arg(params$complex, c("Vietoris", "Rips"))
      
      # ensure that engine can handle data
      ph_classes <- if (is.na(.ripserr_version)) {
        stop("Package {ripserr} is required but could not be found.")
      } else if (.ripserr_version == "0.1.1") {
        # https://github.com/cran/ripserr/blob/
        # 8cadc3a86009149418d6f9a61124af9d6372d34e/R/calculate.R#L68
        c(
          "dist", "matrix",
          gsub("as\\.matrix\\.", "",
               as.character(methods(base::as.matrix)))
        )
      } else if (.ripserr_version >= "0.2.0") {
        gsub("vietoris_rips\\.", "",
             as.character(methods(ripserr::vietoris_rips)))
      }
      ph_classes <- setdiff(ph_classes, "default")
      if (! all(vapply(data$dataset, inherits, FALSE, what = ph_classes))) {
        stop("`dataset` only accepts data of the following classes: ",
             paste(paste("'", ph_classes, "'", sep = ""), collapse = ", "))
      }
      
      # ensure data is in transformation-ready form here
      
      # handle disk size
      if (is.null(params$radius_max) && is.null(params$diameter_max)) {
        params$diameter_max <- Inf
      }
      if (! is.null(params$radius_max)) {
        if (! is.null(params$diameter_max)) {
          warning("Pass a value to only one of ",
                  "`radius_max` and `diameter_max`; ",
                  "`diameter_max` value will be used.")
        } else {
          params$diameter_max <- params$radius_max * 2
        }
      }
      
      # separate out dataset list column
      dataset_list <- data$dataset
      # introduce identifier
      data$dataset <- seq(nrow(data))
      # compute persistent homology from dataset list
      if (params$diameter_max == Inf) params$diameter_max <- -1L
      dataset_list <- if (is.na(.ripserr_version)) {
        stop("Package {ripserr} is required but could not be found.")
      } else if (.ripserr_version == "0.1.1") {
        lapply(
          dataset_list,
          ripserr::vietoris_rips,
          threshold = params$diameter_max,
          dim = params$dimension_max %||% 1L,
          p = params$field_order %||% 2L,
          return_format = "df"
        )
      } else if (.ripserr_version >= "0.2.0") {
        lapply(
          dataset_list,
          ripserr::vietoris_rips,
          threshold = params$diameter_max,
          max_dim = params$dimension_max %||% 1L,
          p = params$field_order %||% 2L
        )
      }
      # introduce identifier
      dataset_list <- mapply(
        \(d, i) { d <- as.data.frame(d); d$dataset <- i; d },
        d = dataset_list, i = seq(nrow(data)),
        SIMPLIFY = FALSE
      )
      # bind the list of output data frames
      ph_data <- do.call(rbind, dataset_list)
      # rename to required aesthetics
      names(ph_data)[match(c("birth", "death"), names(ph_data))] <- 
        c("start", "end")
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
  
  setup_params = function(data, params) {
    
    # discard unused parameters
    if (is.null(data$dataset)) {
      
      dataset_params <- intersect(
        names(data),
        c("diameter_max", "radius_max", "dimension_max", "field_order",
          "complex")
      )
      if (length(dataset_params) > 0L) {
        params_vec <- paste0("`", dataset_params, "`", collapse = ", ")
        warning("Parameters ", params_vec,
                " are only used with the `dataset` aesthetic.")
        params <- params[setdiff(names(params), params_vec)]
      }
      
    } else {
      
      # REVIEW: move this to `stat_persistence()`?
      params$complex <- match.arg(params$complex, c("Vietoris", "Rips"))
      
    }
    
    params
  },
  
  compute_panel = function(
    data, scales,
    diagram = "diagonal",
    # # 'point_cloud' parameter
    # point_cloud = NULL,
    # 'dataset' aesthetic
    diameter_max = Inf, radius_max = NULL, dimension_max = 1L,
    field_order = 2L, complex = "Rips"
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
    
    # computed variable: `id` (sort by dimension, birth, death)
    data$id <- interaction(
      if (is.null(data$dimension)) NA_character_ else data$dimension,
      data$start, data$end,
      drop = TRUE, lex.order = TRUE
    )
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
                             # point_cloud = NULL,
                             diameter_max = Inf, radius_max = NULL,
                             dimension_max = 1L,
                             field_order = 2L,
                             complex = "Rips",
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
      # point_cloud = point_cloud,
      diameter_max = diameter_max, radius_max = radius_max,
      dimension_max = dimension_max,
      field_order = field_order,
      complex = complex,
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
