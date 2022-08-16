# Implentation of different engines for StatSimplicialComplex


## simplextree
simplicial_complex_simplextree <- function(data, diameter, max_dimension, complex) {
  
  # zero_skeleton needs to come from data, not simplextree
  zero_skeleton <- indeces_to_data(data)
  
  
  # Compute simplicial complex up to max_dimension, encoded as a simplextree
  # (all further computed values derive from st)
  st <- data_to_simplextree(data, diameter, max_dimension, complex)
  
  # Convert simplextree into a list
  simplexes <- as.list(simplextree::maximal(st))
  
  # If there are no edges, just return zero-skeleton
  # Otherwise, go on to find one_skeleton + simplexes
  if (length(simplexes) == 0)  return(zero_skeleton)
  
  
  
  # Convert simplexes into dataframe -----
  
  # no of points in each simplex
  sizes <- vapply(simplexes, length, numeric(1))
  
  # unique identifier for each simplex
  simplex_id <- rep(1:length(simplexes), times = sizes)
  
  # dimension of each simplex as ordered factor
  dims <- rep(sizes, times = sizes) - 1
  dims <- ordered(dims, levels = min(dims):max(dims))
  
  # combine ordered pairs for each simplex into dataframe
  indeces <- unlist(simplexes)
  simplexes <- data[indeces,]
  
  # fix row names
  rownames(simplexes) <- NULL
  
  # include relevant computed values
  simplexes$simplex_id <- simplex_id
  simplexes$dim <- dims
  
  # reorder rows so that higher dim simplexes are plotted last
  # note: order among simplexes of equal dim doesn't matter
  #       b/c we split() on simplex_id
  simplexes <- simplexes[order(simplexes$dim), ]
  
  # take convex hull of each simplex for polygon grob:
  simplexes <- split(simplexes, simplexes$simplex_id)
  simplexes <- lapply(simplexes, simplex_chull)
  
  simplexes <- do.call(rbind, simplexes)
  
  
  # Store 1- and >1-dim simplexes seperately
  # (1-dim need to be drawn as line segments)
  simplexes_one <- simplexes[simplexes$dim == 1,]
  simplexes <- simplexes[simplexes$dim != 1,]
  
  if (nrow(simplexes) > 0) {
    simplexes$type <- "simplex"
  }
  simplexes$dim <- droplevels(simplexes$dim)
  
  if (nrow(simplexes_one) > 0) {
    simplexes_one$type <- "simplex_one"
    simplexes_one$dim <- levels(simplexes$dim)[1]
  }
  
  
  
  # Convert 1-skeleton into dataframe -----
  
  # convert matrix of edges into a list
  edges <- apply(st$edges, 1, function(x) x, simplify = FALSE)
  
  # unique identifier for each edge
  edge_id <- rep(1:length(edges), each = 2)
  
  # get indeces of edge coords
  edges <- unlist(edges)
  
  # combine ordered pairs for each edge into dataframe
  one_skeleton <- data[edges,]
  one_skeleton$simplex_id <- edge_id
  
  # fill in irrelevant computed values
  # dim not actually used, but can't be NA b/c of scale
  zero_skeleton$dim <- levels(simplexes$dim)[1]
  one_skeleton$dim <- levels(simplexes$dim)[1]
  one_skeleton$type <- "one_skeleton"
  
  
  rbind(simplexes, simplexes_one, zero_skeleton, one_skeleton)
  
}


# Helper function, returns simplextree to be manipulated into data.frame
# f: data -> simplextree encoding simplicial complex for given `complex`
data_to_simplextree <- function(df, diameter, max_dimension, complex) {
  
  if (complex %in% c("Rips", "Vietoris")) {
    # For the Vietoris-Rips complex, just return the flag complex:
    # w/ simplexes with dim up to max_dimension
    
    # Find edges given diameter:
    edges <- t(proximate_pairs(df, diameter))
    edges <- as.data.frame(edges)
    edges <- as.list(edges)
    
    # Construct the 1-skeleton of the complex as a simplextree
    st <- simplextree::simplex_tree(edges)
    
    # Return flag complex
    simplextree::expand(st, k = max_dimension)
    
  } else {
    
    # Error out if engine is used incorrectly
    stop("Only Vietoris-Rips complex is currently implemented for simplextree engine")
    
  }
  
}

## Base 
# These should probably be factored out more cleanly
# Currently, an if statement for each complex,
# computes data.frame's `simplexes` and `one_skeleton` w/ relevant calc. fields
simplicial_complex_base <- function(data, diameter, max_dimension, complex) {
  
  # zero_skeleton always just the point cloud
  zero_skeleton <- indeces_to_data(data)
  
  
  # Compute other data.frame objects based on complex
  
  if (complex %in% c("Rips", "Vietoris")) {
    
    edges <- proximate_pairs(data, diameter)
    one_skeleton <- indeces_to_data(data, edges)
    
    # Now do simplexes (only of dim 1)
    # (need edges as a nice data.frame again)
    edges <- as.data.frame(proximate_pairs(data, diameter))
    
    # indices of triples of data points having diameter less than `diameter`
    faces <- merge(
      edges,
      transform(edges, b = a, c = b, a = NULL),
      by = "b", all = FALSE,
      sort = FALSE
    )
    
    faces <- merge(
      faces,
      transform(edges, c = b, b = NULL),
      by = c("a", "c"), all = FALSE,
      sort = FALSE
    )
    
    simplexes <- indeces_to_data(data, faces)

  }
  
  
  
  if (complex == "Cech") {
    
    edges <- proximate_pairs(data, diameter)
    one_skeleton <- indeces_to_data(data, edges)
    
    # Now do simplexes (only of dim 1)
    faces <- proximate_triples(data, diameter)
    simplexes <- indeces_to_data(data, faces)
    
    
  }
  
 
  # Not determining maximal 1-simplexes currently,
  # very computationally expensive
  if (TRUE) {
    
    if (nrow(one_skeleton) > 1) one_skeleton$type <- "simplex_one"
  
    rbind(simplexes, zero_skeleton, one_skeleton)
  
  } else {
    
    # determine maximal 1-simplexes
    simplexes_one <- get_simplexes_one(edges, faces, one_skeleton)
    
    rbind(simplexes, simplexes_one, zero_skeleton, one_skeleton)
    
    
  }
  
} 
    



## RTriangle
simplicial_complex_RTriangle <- function(data, diameter, max_dimension, complex) {
  
  # zero_skeleton always just the point cloud
  zero_skeleton <- indeces_to_data(data)
  
  
  # Compute other data.frame objects based on complex
  
  if (complex == "alpha") {
    
    dt <- RTriangle::triangulate(RTriangle::pslg(as.matrix(data[, c("x", "y")])), Y = TRUE)[["E"]]
    
    # indices of Delaunay edges within `diameter` of each other
    edge_dists <- apply(
      data[dt[, 2L], c("x", "y"), drop = FALSE] -
        data[dt[, 1L], c("x", "y"), drop = FALSE],
      1L, norm, "2"
    )
    
    edges <- dt[edge_dists < diameter, , drop = FALSE]
    
    one_skeleton <- indeces_to_data(data, edges)
    
    
    # Now do simplexes (only of dim 1)
    
    # Delaunay triangulation, keeping only indices of edges and of triangles
    dt <- RTriangle::triangulate(RTriangle::pslg(as.matrix(data[, c("x", "y")])), Y = TRUE)[["T"]]
    
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
    
    simplexes <- indeces_to_data(data, faces)
    
  }
  
 
  # Not determining maximal 1-simplexes currently,
  # very computationally expensive
  if (TRUE) {
    
    if (nrow(one_skeleton) > 1) one_skeleton$type <- "simplex_one"
  
    rbind(simplexes, zero_skeleton, one_skeleton)
  
  } else {
    
    # determine maximal 1-simplexes
    simplexes_one <- get_simplexes_one(edges, faces, one_skeleton)
    
    rbind(simplexes, simplexes_one, zero_skeleton, one_skeleton)
    
    
  }
  
}







 
## Shared helper functions:

# Converts a matrix of indeces corresponding to simplexes of equal 
# dimensions (0, 1, or 2) to correct representation for GeomSimplicialComplex
# (indices arg. is missing when we want the 0-skeleton)
indeces_to_data <- function(data, indices = matrix(1:nrow(data), ncol = 1)) {
  
  # m = dim + 1, no. of points in each simplex
  m <- ncol(indices)
  
  # want "flat" vector of indices and for each index to be a column in res
  indices <- as.vector(t(indices))
  res <- data[indices,]
  
  if (nrow(res) > 0) {
    
    res$simplex_id <- rep(1:(length(indices)/m), each = m)
    
    # Always needs to be 2 -- GeomSimplicialComplex only wants different values if
    # simplexes of dim > 2 being plotted (i.e. engine = "simplextree")
    res$dim <- ordered(2)
    
    # Type of object for GeomSimplicialComplex to plot
    res$type <- 
      switch(m,
        "zero_skeleton",
        "one_skeleton", 
        "simplex"
      )
    
  } else {
    
    # If res is empty, still need columns
    res$simplex_id <- vector("numeric")
    res$dim <- ordered(x = character(), levels = 2)
    res$type <- vector("character")
    
  }
  
  res
  
}


# Get maximal one_simplexes from edges + faces (subset of one_skeleton)
# Pretty computationally expensive: O(|one_skeleton| x |two_skeleton|)
# Could avoid this step if we just treated one_skeleton as simplexes_one
# -- this is reasonable if we're only plotting up to dim = 2,
#    all edges are either maximal or an edge of a 2-dim simplex (boundary!)
#    The only benefit is this allows for simplex_boundary = FALSE
get_simplexes_one <- function(edges, faces, one_skeleton) {
  
  simplexes_one <- one_skeleton[apply(edges, 1, is_maximal, faces),]
  
  if (nrow(simplexes_one) > 0) simplexes_one$type <- "simplex_one"
  
  simplexes_one
    
}

# Determine if edge is contained in faces (is edge a maximal simplex)
is_maximal <- function(edge, faces) {
  
  res <- apply(faces, 1, function(face) all(edge %in% face))
  
  !any(res)
  
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
    a = rep(1:(nrow(data) - 1), rep((nrow(data) - 1):1)),
    b = unlist(lapply(2:nrow(data), function(k) k:nrow(data))),
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



# Logic for matching params w/ optimal engine

assign_complex_engine <- function(complex, engine, max_dimension) {
  
  if (complex %in% c("Rips", "Vietoris")) {
    
    # Default to "base" engine if not plotting high dim simplexes
    if (max_dimension < 3 & is.null(engine)) return("base")
    
    return(complex_engine_rules("Vietoris-Rips", engine, c("simplextree", "base")))
    
  }
  
  if (complex == "Cech") {
    
    return(complex_engine_rules("Cech", engine, "base"))
    
  }
  
  
  if (complex == "alpha") {

    return(complex_engine_rules("alpha", engine, "RTriangle"))
    
  }
  
  stop("Invalid choice of `complex`, see `?geom_simplicial_complex` for details", call. = FALSE)
  
}

complex_engine_rules <- function(complex_name, engine, engine_options, engine_default = engine_options[1]) {
  
  if (is.null(engine)) {
    
    engine <- engine_default
    
  } else {
    
    if (! engine %in% engine_options) {
      
      msg <- paste0(
        'Specified engine incompatible with ', complex_name, ' complex,', '\n',
        'defaulting to `engine = "', engine_default, '"`.'
      )
      
      warning(msg, call. = FALSE)
      
      engine <- engine_default
      
    }
    
  }
  
  engine
  
}
