# Implementation of different engines for `StatPersistence`

persistence <- function(
    data, diameter_max, dimension_max, field_order, engine, complex
) {
  
  # ensure that engine can handle data
  engine_classes <- list(
    ripserr = if (is.na(.ripserr_version)) {
      stop("Package {ripserr} is required but could not be found.")
    } else if (.ripserr_version == as.package_version("0.1.1")) {
      # https://github.com/cran/ripserr/blob/
      # 8cadc3a86009149418d6f9a61124af9d6372d34e/R/calculate.R#L68
      c(
        "dist", "matrix",
        gsub("as\\.matrix\\.", "",
             as.character(methods(base::as.matrix)))
      )
    } else if (.ripserr_version >= as.package_version("0.2.0")) {
      gsub("vietoris_rips\\.", "",
           as.character(methods(ripserr::vietoris_rips)))
    },
    TDA = c("matrix", "data.frame", "dist")
  )
  check_engine_class <- function(engine, data) {
    if (! all(vapply(data, inherits, FALSE, what = engine_classes[[engine]]))) {
      stop(
        "{", engine, "} accepts only classes \"",
        paste0(engine_classes[[engine]], collapse = "\", \""), "\"; ",
        "not \"", class(data)[[1L]], "\".",
        call. = FALSE
      )
    }
  }
  
  # ensure that engine can use complex
  engine_complexes <- list(
    ripserr = c("Vietoris", "Rips", "cubical"),
    TDA = c("Vietoris", "Rips", "alpha")
  )
  check_engine_complex <- function(engine, complex) {
    if (! complex %in% engine_complexes[[engine]]) {
      stop(
        "{", engine, "} computes only complexes \"",
        paste0(engine_complexes[[engine]], collapse = "\", \""), "\"; ",
        "not \"", complex, "\".",
        call. = FALSE
      )
    }
  }
  
  switch(
    engine,
    ripserr = persistence_ripserr(
      data, diameter_max, dimension_max, field_order, complex
    ),
    TDA = persistence_TDA(
      data, diameter_max, dimension_max, field_order, complex
    )
  )
}

## {ripserr}
persistence_ripserr <- function(
    data, diameter_max, dimension_max, field_order, complex
) {
  
  # TODO: Move these outside `persistence()` call
  check_engine_class("ripserr", data)
  check_engine_complex("ripserr", complex)
  if (complex == "Rips") complex <- "Vietoris"
  
  if (diameter_max == Inf) diameter_max <- -1L
  
  pd <- switch(
    complex,
    Vietoris = {
      if (.ripserr_version == as.package_version("0.1.1")) {
        ripserr::vietoris_rips(
          data,
          threshold = diameter_max,
          # deprecated to `max_dim` in v0.2.0
          dim = dimension_max %||% 1L,
          p = field_order %||% 2L,
          # ignored in v0.2.0
          return_format = "df"
        )
      } else if (.ripserr_version >= as.package_version("0.2.0")) {
        ripserr::vietoris_rips(
          data,
          threshold = diameter_max,
          dimension_max = dimension_max %||% 1L,
          p = field_order %||% 2L
        )
      }
    },
    cubical = {
      if (! inherits(data, "array")) data <- as.matrix(data)
      ripserr::cubical(
        dataset = data,
        threshold = diameter_max,
        method = "lj"
      )
    }
  )
  
  pd <- as.data.frame(pd)
  names(pd)[match(c("birth", "death"), names(pd))] <- c("start", "end")
  pd
}

## {TDA}
persistence_TDA <- function(
    data, diameter_max, dimension_max, field_order, complex
) {
  
  # TODO: Move these outside `persistence()` call
  check_engine_class("TDA", data)
  check_engine_complex("TDA", complex)
  if (complex == "Rips") complex <- "Vietoris"
  
  if (dimension_max == Inf) {
    dimension_max <- if (inherits(data, "dist")) {
      attr(data, "Size") - 1L
    } else {
      ncol(data) - 1L
    }
  }
  
  pd <- switch(
    complex,
    Vietoris = {
      TDA::ripsDiag(
        X = data,
        maxdimension = dimension_max,
        maxscale = diameter_max,
        dist = if (inherits(data, "dist")) "arbitrary" else "euclidean",
        library = "GUDHI", location = FALSE
      )
    },
    alpha = {
      TDA::alphaComplexDiag(
        X = data,
        maxdimension = dimension_max,
        library = "GUDHI", location = FALSE
      )
    }
  )
  
  pd <- as.data.frame(unclass(pd$diagram))
  names(pd)[match(c("Birth", "Death"), names(pd))] <- c("start", "end")
  pd
}
