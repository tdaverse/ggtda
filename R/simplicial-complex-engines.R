# Implementation of different engines for `StatSimplicialComplex`

## base 

# These should probably be factored out more cleanly
# Currently, an if statement for each complex
simplicial_complex_base <- function(
    data, diameter, max_dimension, complex, zero_simplices, one_simplices
) {
  
  # df_zero_simplices <- indices_to_data(data)
  df_zero_simplices <- data
  df_zero_simplices$dim <- 0L
  df_zero_simplices$id <- seq(nrow(data))
  
  # Compute other data.frame objects based on complex
  
  if (complex %in% c("Rips", "Vietoris")) {
    
    edges <- proximate_pairs(data, diameter)
    df_one_simplices <- indices_to_data(data, edges)
    
    # Now do simplices (only of dim 1)
    # (need edges as a nice data.frame again)
    edges <- as.data.frame(proximate_pairs(data, diameter))
    
    # indices of triples of data points having diameter less than `diameter`
    # edges2 <- edges
    # names(edges2)[match(c("a", "b"), names(edges2))] <- c("b", "c")
    faces <- merge(
      edges,
      transform(edges, b = edges$a, c = edges$b, a = NULL),
      # edges2,
      by = "b", all = FALSE,
      sort = FALSE
    )
    
    # edges3 <- edges
    # names(edges3)[match(c("a", "b"), names(edges3))] <- c("a", "c")
    faces <- merge(
      faces,
      transform(edges, c = edges$b, b = NULL),
      # edges3,
      by = c("a", "c"), all = FALSE,
      sort = FALSE
    )
    
    df_high_simplices <- indices_to_data(data, faces)
    
  }
  
  if (complex == "Cech") {
    
    edges <- proximate_pairs(data, diameter)
    df_one_simplices <- indices_to_data(data, edges)
    
    # Now do simplices (only of dim 1)
    faces <- proximate_triples(data, diameter)
    df_high_simplices <- indices_to_data(data, faces)
    
  }
  
  # Pair down to maximal simplices if necessary
  # necessary <=> only want maximal 0/1-simplices AND no higher-dim simplices
  # being plotted
  if (one_simplices == "maximal" & max_dimension > 1L) {
    df_one_simplices <- 
      get_maximal_one_simplices(edges, faces, df_one_simplices)
  }
  
  if (zero_simplices == "maximal" & max_dimension > 0L) {
    df_zero_simplices <- 
      get_maximal_zero_simplices(edges, df_zero_simplices)
  } 
  
  rbind(df_high_simplices, df_one_simplices, df_zero_simplices)
  
} 

## RTriangle

simplicial_complex_RTriangle <- function(
    data, diameter, max_dimension, complex, zero_simplices, one_simplices
) {
  
  # 0-simplices always just the point cloud
  df_zero_simplices <- indices_to_data(data)
  
  # Compute other data.frame objects based on complex
  
  if (complex == "alpha") {
    
    dt <- RTriangle::triangulate(
      RTriangle::pslg(as.matrix(data[, c("x", "y")])), Y = TRUE
    )[["E"]]
    
    # indices of Delaunay edges within `diameter` of each other
    edge_dists <- apply(
      data[dt[, 2L], c("x", "y"), drop = FALSE] -
        data[dt[, 1L], c("x", "y"), drop = FALSE],
      1L, norm, "2"
    )
    
    edges <- dt[edge_dists < diameter, , drop = FALSE]
    
    df_one_simplices <- indices_to_data(data, edges)
    
    # Now do simplices (only of dim 2)
    
    # Delaunay triangulation, keeping only indices of edges and of triangles
    dt <- RTriangle::triangulate(
      RTriangle::pslg(as.matrix(data[, c("x", "y")])),
      Y = TRUE
    )[["T"]]
    
    # indices of Delaunay faces within `diameter` of each other
    face_vertices <- unique(as.vector(dt))
    
    if (nrow(dt) * 3L > choose(length(face_vertices), 2L)) {
      
      # calculate all distances among face vertices
      face_vertices <- sort(face_vertices)
      faces <- proximate_triples(data[face_vertices, , drop = FALSE], diameter)
      faces[] <- face_vertices[as.matrix(faces)]
      dt_faces <- matrix(t(apply(dt, 1L, sort)), ncol = 3L)
      faces <- merge(faces, as.data.frame(dt_faces), by = seq(3L))
      
    } else {
      
      # calculate pairwise distances between face edges
      face_pairs <- rbind(dt[, c(1L, 2L)], dt[, c(1L, 3L)], dt[, c(2L, 3L)])
      
      face_dists <- apply(
        data[face_pairs[, 2L], c("x", "y"), drop = FALSE] -
          data[face_pairs[, 1L], c("x", "y"), drop = FALSE],
        1L, norm, "2"
      )
      
      face_dists <- matrix(face_dists, ncol = 3L, byrow = FALSE)
      
      # shed too-distant pairs
      face_wh <- apply(face_dists < diameter, 1L, all)
      faces <- dt[face_wh, , drop = FALSE]
      face_dists <- face_dists[face_wh, , drop = FALSE]
      
      # semiperimeters
      face_sp <- .5 * apply(face_dists, 1L, sum)
      
      # circumdiameters
      face_cd <- .5 * apply(face_dists, 1L, prod) / sqrt(
        face_sp * (face_sp - face_dists[, 1L]) *
          (face_sp - face_dists[, 2L]) * (face_sp - face_dists[, 3L])
      )
      
      # squares of longest sides
      face_m <- apply(face_dists, 1L, max)^2
      
      # sum of squares of remaining sides
      face_n <- apply(face_dists^2, 1L, sum) - face_m
      
      # when largest angles obtuse, longest side lengths;
      # otherwise, circumdiameters
      face_diam <- ifelse(
        face_m > face_n,
        apply(face_dists, 1L, max),
        face_cd
      )
      
      faces <- faces[face_diam < diameter, , drop = FALSE]
      
    }
    
    df_high_simplices <- indices_to_data(data, faces)
    
  }
  
  # Pair down to maximal simplices if necessary
  # necessary <=> only want maximal 0/1-simplices AND no higher-dim simplices
  # being plotted
  if (one_simplices == "maximal" & max_dimension > 1L) {
    df_one_simplices <- 
      get_maximal_one_simplices(edges, faces, df_one_simplices)
  }
  
  if (zero_simplices == "maximal" & max_dimension > 0L) {
    df_zero_simplices <- 
      get_maximal_zero_simplices(edges, df_zero_simplices)
  } 
  
  rbind(df_high_simplices, df_one_simplices, df_zero_simplices)
  
}

# Helper functions ---------------------------------------------------------

# Converts a matrix of indices corresponding to simplices of equal 
# dimensions (0, 1, or 2) to correct representation for GeomSimplicialComplex
# (indices arg. is missing when we want the 0-simplices)
# m = dim + 1, no. of points in each simplex
indices_to_data <- function(
    data, indices = matrix(seq(nrow(data)), ncol = 1L), m = NULL
) {
  
  if (is.null(m)) m <- ncol(indices)
  
  # want "flat" vector of indices and for each index to be a row in res
  indices <- as.vector(t(indices))
  res <- data[indices, , drop = FALSE]
  
  if (nrow(res) > 0L) {
    
    res$id <- rep(seq(length(indices) / m), each = m)
    
    # Always needs to be 2 -- GeomSimplicialComplex only wants different values
    # if simplices of dim > 2 being plotted (i.e. engine = "simplextree")
    # res$dim <- ordered(2)
    res$dim <- m - 1L
    
    # Type of object for GeomSimplicialComplex to plot
    # res$type <- switch(
    #   m,
    #   "zero_simplex",
    #   "one_simplex", 
    #   "high_simplex"
    # )
    
  } else {
    
    # If res is empty, still need columns
    res$id <- vector("integer")
    # res$dim <- ordered(x = character(), levels = 2)
    res$dim <- vector("integer")
    # res$type <- vector("character")
    
  }
  
  res
  
}

# Get maximal one_simplices from edges + faces (subset of df_one_simplices)
# Pretty computationally expensive: O(|1-simplices| x |2-simplices|)?
get_maximal_one_simplices <- function(edges, faces, df_one_simplices) {
  
  edges_unique <- apply(edges, 1L, is_maximal, faces)
  
  are_maximal <- rep(edges_unique, each = 2L)
  df_one_simplices[are_maximal, , drop = FALSE]
  
}

# Determine if edge is contained in faces (is edge a maximal simplex)
is_maximal <- function(edge, faces) {
  
  res <- apply(faces, 1L, function(face) all(edge %in% face))
  
  ! any(res)
  
}

# Get maximal 0-simplices from edges + vertices (rows of df_zero_simplices)
# This is simple computationally as we can collapse edges into vector of unqiue
# indices
get_maximal_zero_simplices <- function(edges, df_zero_simplices) {
  
  edges_unique <- as.matrix(edges)
  edges_unique <- unique(edges_unique)
  
  are_maximal <- setdiff(seq(nrow(df_zero_simplices)), edges_unique)
  df_zero_simplices[are_maximal, , drop = FALSE]
  
}

proximate_pairs <- function(data, diameter) {
  
  distances <- as.matrix(stats::dist(data[, c("x", "y")]))
  
  pairs <- which(distances < diameter & upper.tri(distances), arr.ind = TRUE)
  
  dimnames(pairs) <- list(NULL, c("a", "b"))
  pairs
  
}

proximate_triples <- function(data, diameter) {
  
  # distances between pairs
  dists <- data.frame(
    a = rep(seq((nrow(data) - 1L)), rep(seq(nrow(data) - 1L, 1L))),
    b = unlist(lapply(seq(2L, nrow(data)), function(k) seq(k, nrow(data)))),
    d = as.vector(stats::dist(data))
  )
  
  # shed too-distant pairs
  dists <- dists[dists$d < diameter, , drop = FALSE]
  
  # distances among triples
  triples <- merge(
    dists,
    transform(dists, c = dists$b, b = dists$a, a = NULL),
    by = "b", suffixes = c("_ab", "_bc")
  )
  
  triples <- merge(
    triples,
    transform(dists, c = dists$b, b = NULL, d_ac = dists$d, d = NULL),
    by = c("a", "c")
  )
  
  # semiperimeters
  triples$s <- .5 * (triples$d_ab + triples$d_bc + triples$d_ac)
  
  # circumdiameters
  triples$cd <- .5 * triples$d_ab * triples$d_bc * triples$d_ac / sqrt(
    triples$s * (triples$s - triples$d_ab) *
      (triples$s - triples$d_bc) * (triples$s - triples$d_ac)
  )
  
  # squares of longest sides
  triples$m <- pmax(triples$d_ab, triples$d_bc, triples$d_ac)^2
  
  # sum of squares of remaining sides
  triples$n <- triples$d_ab^2 + triples$d_bc^2 + triples$d_ac^2 - triples$m
  
  # when largest angles obtuse, longest side lengths; otherwise, circumdiameters
  triples$diam <- ifelse(
    triples$m > triples$n,
    pmax(triples$d_ab, triples$d_bc, triples$d_ac),
    triples$cd
  )
  
  triples <- triples[triples$diam < diameter, c("a", "b", "c"), drop = FALSE]
  rownames(triples) <- NULL
  
  as.matrix(triples)
  
}

## {simplextree}

simplicial_complex_simplextree <- function(
    data, diameter, max_dimension, complex, zero_simplices, one_simplices
) {
  
  # The entire set of 0-simplices needs to come from data
  df_zero_simplices <- data
  df_zero_simplices$id <- seq(nrow(data))
  df_zero_simplices$dim <- 0L
  
  # Compute simplicial complex up to `max_dimension`, encoded as a 'simplextree'
  # (all further computed values derive from st)
  st <- data_to_simplextree(data, diameter, max_dimension, complex)
  
  # Convert simplex tree into a list
  if (is.na(.simplextree_version)) {
    stop("Package {simplextree} is required but could not be found.")
  } else if (.simplextree_version >= "1.0.1") {
    simplices <- as.list(simplextree::maximal(st))
  } else if (.simplextree_version == "0.9.1") {
    simplices <- st$serialize()
  } else {
    stop("No method available for {simplextree} v", .simplextree_version)
  }
  
  # If there are no edges, just return zero_simplices
  # Otherwise, go on to find one_simplices + high_simplices
  if (length(simplices) == 0) return(df_zero_simplices)
  
  # Convert simplices into dataframe -----
  
  # no of points in each simplex
  sizes <- vapply(simplices, length, numeric(1))
  
  # unique identifier for each simplex
  ids <- rep(1:length(simplices), times = sizes)
  
  # dimension of each simplex as ordered factor
  dims <- rep(sizes, times = sizes) - 1L
  # dims <- ordered(dims, levels = seq(min(dims), max(dims)))
  
  # combine ordered pairs for each simplex into dataframe
  indices <- unlist(simplices)
  df_simplices <- data[indices,]
  
  # fix row names
  rownames(df_simplices) <- NULL
  
  # include relevant computed values
  df_simplices$id <- ids
  df_simplices$dim <- dims
  
  # reorder rows so that higher dim simplices are plotted last
  # note: order among simplices of equal dim doesn't matter
  #       b/c we split() on id
  df_simplices <- df_simplices[order(df_simplices$dim), , drop = FALSE]
  
  # take convex hull of each simplex for polygon grob:
  # (df_simplices is brielfly a list of data.frames, each corresponding to a
  # k-simplex)
  df_simplices <- split(df_simplices, df_simplices$id)
  df_simplices <- lapply(df_simplices, simplex_chull)
  df_simplices <- do.call(rbind, df_simplices)
  
  # Store 1- and >1-dim simplices seperately
  # (1-dim need to be drawn as line segments)
  df_one_simplices <- df_simplices[df_simplices$dim == 1L, , drop = FALSE]
  df_high_simplices <- df_simplices[df_simplices$dim > 1L, , drop = FALSE]
  
  # Overwrite df_one_simplices to include non-maximal edges
  # if user wants all one-simplices or if higher-dim simplices aren't plotted,
  if (one_simplices == "all" | max_dimension == 1L) {
    
    # convert matrix of edges into a list
    edges <- apply(st$edges, 1L, identity, simplify = FALSE)
    
    # unique identifier for each edge
    edge_id <- rep(seq(length(edges)), each = 2L)
    
    # get indices of edge coords
    edges <- unlist(edges)
    
    # combine ordered pairs for each edge into dataframe
    df_one_simplices <- data[edges, , drop = FALSE]
    df_one_simplices$id <- edge_id
    df_one_simplices$dim <- 1L
    
  }
  
  # Similar operation for zero_simplices argument:
  if (zero_simplices == "maximal") {
    
    maximal_vertices <- setdiff(seq(nrow(data)), st$vertices)
    
    df_zero_simplices <- data[maximal_vertices, , drop = FALSE]
    df_zero_simplices$dim <- 0L
    df_zero_simplices$id <- seq(nrow(data))
    
  } 
  
  rbind(df_high_simplices, df_one_simplices, df_zero_simplices)
  
}

# Helper function, returns simplextree to be manipulated into data.frame
# f: data -> simplextree encoding simplicial complex for given `complex`
data_to_simplextree <- function(df, diameter, max_dimension, complex) {
  
  if (complex %in% c("Rips", "Vietoris")) {
    # For the Vietoris-Rips complex, just return the flag complex:
    # w/ simplices with dim up to max_dimension
    
    # Find edges given diameter:
    edges <- t(proximate_pairs(df, diameter))
    edges <- as.data.frame(edges)
    edges <- as.list(edges)
    
    # Construct the flag complex as a simplex tree
    if (is.na(.simplextree_version)) {
      stop("Package {simplextree} is required but could not be found.")
    } else if (.simplextree_version >= "1.0.1") {
      st <- simplextree::simplex_tree(edges)
      st <- simplextree::expand(st, k = max_dimension)
    } else if (.simplextree_version == "0.9.1") {
      st <- simplextree::simplex_tree()
      st$insert(edges)
      # strange behavior in this archived version must be worked around
      if (! is.null(st$dimension)) st$expand(k = max_dimension)
    } else {
      stop("No method available for {simplextree} v", .simplextree_version)
    }
    
    # Return flag complex
    st
    
  } else {
    
    # Error out if engine is used incorrectly
    stop("Only Vietoris-Rips complexes are implemented in {simplextree}.")
    
  }
  
}

## {TDA}

simplicial_complex_TDA <- function(
    data, diameter, max_dimension, complex, zero_simplices, one_simplices,
    library
) {
  
  # The entire set of 0-simplices needs to come from data
  df_zero_simplices <- data
  df_zero_simplices$id <- seq(nrow(data))
  df_zero_simplices$dim <- 0L
  
  # Compute simplicial complex up to `max_dimension`, encoded as a 'Diagram'
  # (all further computed values derive from `pd`)
  pd <- data_to_Filtration(data[, c("x", "y")],
                           diameter, max_dimension,
                           complex, library)
  
  # data frame of coordinates with linking index
  df_coords <- as.data.frame(pd$coordinates)
  df_coords$row <- seq(nrow(df_coords))
  
  # data frame of combinatorics with linking index
  pd_dim <- vapply(pd$cmplx, length, 0L) - 1L
  df_combin <- data.frame(
    row = unlist(pd$cmplx),
    id = rep(seq_along(pd_dim), pd_dim + 1L),
    dim = rep(pd_dim, pd_dim + 1L)
  )
  df_combin <-
    df_combin[order(df_combin$dim, df_combin$id), , drop = FALSE]
  
  # merge coordinates with combinatorics & return
  df_simplices <- merge(df_coords, df_combin, by = "row")
  df_simplices$row <- NULL
  df_simplices
}

# Helper function, returns 'Diag' to be manipulated into 'data.frame'
# f: data -> simplextree encoding simplicial complex for given `complex`
data_to_Filtration <- function(df, diameter, max_dimension, complex, library) {
  
  if (complex %in% c("Rips", "Vietoris")) {
    
    pd <- TDA::ripsFiltration(X = df, maxdimension = max_dimension,
                              maxscale = diameter, library = library)
    
  } else if (complex == "alpha") {
    
    pd <- TDA::alphaComplexFiltration(X = df, library = library)
    
  }
  
  pd
}

## assignment rules

# Logic for matching params w/ optimal engine
assign_complex_engine <- function(complex, engine, max_dimension) {
  
  if (! is.null(engine) && engine == "base" && max_dimension > 2L) {
    
    warning(
      "The base engine cannot construct complexes of `max_dimension` > 2.",
      call. = FALSE
    )
    
  }
  
  if (complex %in% c("Rips", "Vietoris")) {
    
    # Default to "base" engine if not plotting high dim simplices
    # if (max_dimension < 3L & is.null(engine)) return("base")
    
    return(complex_engine_rules("Vietoris-Rips", engine, c(
      if (! is.na(.simplextree_version)) "simplextree",
      if ("TDA" %in% rownames(utils::installed.packages()))
        c("TDA", "GUDHI", "Dionysus"),
      "base"
    )))
    
  }
  
  if (complex == "Cech") {
    
    return(complex_engine_rules("Cech", engine, c(
      "base"
    )))
    
  }
  
  if (complex == "alpha") {
    
    return(complex_engine_rules("alpha", engine, c(
      if ("TDA" %in% rownames(utils::installed.packages())) c("TDA", "GUDHI"),
      if ("RTriangle" %in% rownames(utils::installed.packages())) "RTriangle"
    )))
    
  }
  
}

complex_engine_rules <- function(
    complex_name, engine, engine_options, engine_default = engine_options[[1L]]
) {
  
  if (is.null(engine)) {
    
    if (is.null(engine_default)) {
      stop("No engine is available to construct ", complex_name, " complexes.")
    }
    engine <- engine_default
    
  } else if (! engine %in% engine_options) {
    
    msg <- paste0(
      'The ', engine, ' engine cannot construct ', complex_name, ' complexes;',
      '\n', 'switching to `engine = "', engine_default, '"`.'
    )
    warning(msg, call. = FALSE)
    
    engine <- engine_default
    
  }
  
  engine
  
}
