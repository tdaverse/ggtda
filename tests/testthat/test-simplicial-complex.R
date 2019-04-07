context("Test visualization of Vietoris-Rips diagrams")
library("ggtda")

test_that("Proximate functions work as expected on equilateral triangle", {
  
  # equilateral triangle
  et <- data.frame(x = cos(2*pi*c(0,1/3,2/3)), y = sin(2*pi*c(0,1/3,2/3)))
  # small perturbations from key values
  eps <- .01
  
  # proximate pairs at diameter ~ sqrt(3)
  expect_equal(nrow(ggtda:::proximate_pairs(et, diameter = sqrt(3) - eps)), 0)
  expect_equal(nrow(ggtda:::proximate_pairs(et, diameter = sqrt(3) + eps)), 3)
  
  # proximate triples at diameter ~ sqrt(3)
  expect_equal(nrow(ggtda:::proximate_triples(et, diameter = sqrt(3) - eps)), 0)
  expect_equal(nrow(ggtda:::proximate_triples(et, diameter = sqrt(3) + eps)), 1)
})

test_that("Proximate functions work as expected on noisy circle data", {
  
  # pseudorandom dataset (points from noisy circle) w/ seed for reproducibility
  set.seed(42)
  angles <- runif(10, 0, 2 * pi)
  annulus <- cbind(x = cos(angles) + rnorm(10, 0, 0.1),
                   y = sin(angles) + rnorm(10, 0, 0.1))
  
  # proximate pairs (row permutation irrelevant so sort first)
  output <- proximate_pairs(annulus, diameter = 0.3)
  output <- output[order(output[, 1]), ]
  
  # expected output
  expected <- matrix(c(1, 2, 5, 9, 9, 10), ncol = 2, byrow = TRUE)
  colnames(expected) <- c("a", "b")
  
  # ensure equal
  expect_equal(expected, output)
})

# sample dataset
data("annulus2d")
d <- as.data.frame(annulus2d)

test_that("Ball layer works as expected", {
  
  # balls of specified radius and resolution (segments)
  p <- ggplot(d, aes(x = x, y = y)) +
    stat_disk(radius = 0.35, segments = 60, fill = "aquamarine3", alpha = 0.15)
  expect_is(p, "ggplot")
  expect_is(p$layer[[1]], "ggproto")
  expect_equal(c(p$labels$x, p$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p)), nrow(d) * (60 + 1))
})

test_that("Čech layers work as expected", {
  
  # Čech 0-skeleton stat
  p0 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech0()
  expect_is(p0, "ggplot")
  expect_is(p0$layer[[1]], "ggproto")
  expect_equal(c(p0$labels$x, p0$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p0)), nrow(d))
  
  # Čech 1-skeleton stat
  p1 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech1(diameter = 0.7)
  expect_is(p1, "ggplot")
  expect_is(p1$layer[[1]], "ggproto")
  expect_equal(c(p1$labels$x, p1$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p1)), 1097)
  
  # Čech 2-skeleton stat
  p2 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech2(diameter = 0.7)
  expect_is(p2, "ggplot")
  expect_is(p2$layer[[1]], "ggproto")
  expect_equal(c(p2$labels$x, p2$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p2)), 12651)
})

test_that("Vietoris layers work as expected", {
  
  # Vietoris 0-skeleton stat
  p0 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris0()
  expect_is(p0, "ggplot")
  expect_is(p0$layer[[1]], "ggproto")
  expect_equal(c(p0$labels$x, p0$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p0)), nrow(d))
  
  # Vietoris 1-skeleton stat
  p1 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris1(diameter = 0.7)
  expect_is(p1, "ggplot")
  expect_is(p1$layer[[1]], "ggproto")
  expect_equal(c(p1$labels$x, p1$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p1)), 1097)
  
  # Vietoris 2-skeleton stat
  p2 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris2(diameter = 0.7)
  expect_is(p2, "ggplot")
  expect_is(p2$layer[[1]], "ggproto")
  expect_equal(c(p2$labels$x, p2$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p2)), 16977)
})
