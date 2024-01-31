# Implementation of different engines for `StatPersistence`

## {TDA}

simplicial_filtration_TDA <- function(
    dataset_list, filtration, diameter_max, dimension_max, field_order,
    library
) {
  
  # ensure that engine can handle data
  if (any(! vapply(dataset_list, is.numeric, FALSE) &
          ! vapply(dataset_list, is.data.frame, FALSE))) {
    stop("'TDA' engines only accept `dataset`s",
         " of classes 'numeric' and 'data.frame'.")
  }
  
  # note
  if (! is.null(field_order) && field_order != 2L)
    warning("'TDA' engines cannot specify a field with order > 2.")
  
  # compute PH
  if (filtration %in% c("Rips", "Vietoris")) {
    ph_list <- lapply(
      dataset_list,
      TDA::ripsDiag,
      maxdimension = dimension_max %||% 1L,
      maxscale = diameter_max,
      library = library
    )
  } else if (filtration == "alpha") {
    ph_list <- lapply(
      dataset_list,
      TDA::alphaComplexDiag,
      maxdimension = dimension_max %||% 1L,
      library = library
    )
  }
  
  # format as data frames
  ph_list <- lapply(
    ph_list, \(d) as.data.frame(unclass(d$diagram))
  )
  # rename to required aesthetics
  for (i in seq_along(ph_list))
    names(ph_list[[i]]) <- c("dimension", "start", "end")
  
  ph_list
}

## {ripserr}

simplicial_filtration_ripserr <- function(
    dataset_list, diameter_max, dimension_max, field_order
) {
  
  # ensure that engine can handle data
  ph_classes <- if (.ripserr_version == "0.1.1") {
    # https://github.com/cran/ripserr/blob/
    # 8cadc3a86009149418d6f9a61124af9d6372d34e/R/calculate.R#L68
    c(
      "dist", "matrix",
      gsub("as\\.matrix\\.", "",
           as.character(utils::methods(base::as.matrix)))
    )
  } else if (.ripserr_version >= "0.2.0") {
    gsub("vietoris_rips\\.", "",
         as.character(utils::methods(ripserr::vietoris_rips)))
  }
  ph_classes <- setdiff(ph_classes, "default")
  if (! all(vapply(dataset_list, inherits, FALSE, what = ph_classes))) {
    stop("'ripserr' engine only accepts `dataset`s of the following classes: ",
         paste(paste("'", ph_classes, "'", sep = ""), collapse = ", "))
  }
  
  # compute PH
  if (diameter_max == Inf) diameter_max <- -1L
  ph_list <- if (.ripserr_version == "0.1.1") {
    lapply(
      dataset_list,
      ripserr::vietoris_rips,
      threshold = diameter_max,
      dim = dimension_max %||% 1L,
      p = field_order %||% 2L,
      return_format = "df"
    )
  } else if (.ripserr_version >= "0.2.0") {
    lapply(
      dataset_list,
      ripserr::vietoris_rips,
      threshold = diameter_max,
      max_dim = dimension_max %||% 1L,
      p = field_order %||% 2L
    )
  }
  
  # format as data frames
  ph_list <- lapply(ph_list, as.data.frame)
  # rename to required aesthetics
  for (i in seq_along(ph_list))
    names(ph_list[[i]]) <- c("dimension", "start", "end")
  
  ph_list
}

## assignment rules

# Logic for matching params w/ optimal engine
assign_filtration_engine <- function(filtration, engine) {
  
  if (filtration %in% c("Rips", "Vietoris")) {
    
    return(filtration_engine_rules("Vietoris-Rips", engine, c(
      if (! is.na(.ripserr_version)) "ripserr",
      if ("TDA" %in% rownames(utils::installed.packages()))
        c("TDA", "GUDHI", "Dionysus")
    )))
    
  }
  
  if (filtration == "alpha") {
    
    return(filtration_engine_rules("alpha", engine, c(
      if ("TDA" %in% rownames(utils::installed.packages())) c("TDA", "GUDHI")
    )))
    
  }
  
}

filtration_engine_rules <- function(
    filt_name, engine,
    engine_options, engine_default = engine_options[[1L]]
) {
  
  if (is.null(engine) || length(engine_options == 0L)) {
    
    if (is.null(engine_default)) {
      stop("No available engine can compute ", filt_name, " filtrations.")
    }
    engine <- engine_default
    
  } else if (! engine %in% engine_options) {
    
    msg <- paste0(
      "The ", engine, " engine cannot compute ", filt_name, " filtrations;",
      "\n", "switching to `engine = '", engine_default, "'`."
    )
    warning(msg, call. = FALSE)
    
    engine <- engine_default
    
  }
  
  engine
  
}
