
# equilateral triangle
equilateral_triangle <- 
  data.frame(x = cos(2*pi*c(0,1/3,2/3)), y = sin(2*pi*c(0,1/3,2/3)))
# small perturbations from key values
eps <- .00000001

test_that("Vietoris-Rips complexes immediately close triangles", {
  
  # {base}
  
  # diameter just below edge length
  p0 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(diameter = sqrt(3) - eps)
  b0 <- ggplot_build(p0)
  expect_equal(b0$data[[1L]]$dimension, rep(0L, 3L))
  
  # diameter just above edge length
  p1 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(diameter = sqrt(3) + eps)
  b1 <- ggplot_build(p1)
  expect_equal(sort(b1$data[[1L]]$dimension), rep(c(0L, 2L), each = 3L))
  
  # {TDA}
  
  # diameter just below edge length
  p0 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(engine = "TDA", diameter = sqrt(3) - eps)
  b0 <- ggplot_build(p0)
  expect_equal(b0$data[[1L]]$dimension, rep(0L, 3L))
  
  # diameter just above edge length
  p1 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(engine = "TDA", diameter = sqrt(3) + eps)
  b1 <- ggplot_build(p1)
  # FIXME: when to include edges?
  expect_equal(sort(b1$data[[1L]]$dimension), rep(c(0L, 2L), each = 3L))
  
  # {simplextree}
  
  # diameter just below edge length
  p0 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(engine = "simplextree", diameter = sqrt(3) - eps)
  b0 <- ggplot_build(p0)
  expect_equal(b0$data[[1L]]$dimension, rep(0L, 3L))
  
  # diameter just above edge length
  p1 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(engine = "simplextree", diameter = sqrt(3) + eps)
  b1 <- ggplot_build(p1)
  expect_equal(sort(b1$data[[1L]]$dimension), rep(c(0L, 2L), each = 3L))
  
})

test_that("Čech complexes mediately close triangles", {
  
  # diameter just below edge length
  p0 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) - eps)
  b0 <- ggplot_build(p0)
  expect_equal(b0$data[[1L]]$dimension, rep(0L, 3L))
  
  # diameter just above edge length
  p1 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(complex = "Cech", diameter = sqrt(3) + eps)
  b1 <- ggplot_build(p1)
  expect_equal(sort(b1$data[[1L]]$dimension), rep(c(0L, 1L), times = c(3L, 6L)))
  
  # radius just below triangle radius
  p2 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(complex = "Cech", radius = 1 - eps)
  b2 <- ggplot_build(p2)
  expect_equal(sort(b2$data[[1L]]$dimension), rep(c(0L, 1L), times = c(3L, 6L)))
  
  # radius just above triangle radius
  p3 <- ggplot(equilateral_triangle, aes(x, y)) +
    geom_simplicial_complex(complex = "Cech", radius = 1 + eps)
  b3 <- ggplot_build(p3)
  expect_equal(sort(b3$data[[1L]]$dimension), rep(c(0L, 2L), each = 3L))
  
})
