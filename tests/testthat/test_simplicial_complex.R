context("Test visualization of Vietoris-Rips diagrams")
library("ggtda")

test_that("Vietoris-Rips distance calculations run fine", {
  # set seed for reproducibility
  set.seed(42)
  
  # generate pseudorandom dataset (points from noisy circle)
  angles <- runif(10, 0, 2 * pi)
  annulus <- cbind(x = cos(angles) + rnorm(10, 0, 0.1),
                   y = sin(angles) + rnorm(10, 0, 0.1))
  
  # correct output and check for match (row permutation irrelevant so sort first)
  expected <- matrix(c(1, 2, 5, 9, 9, 10), ncol = 2, byrow = TRUE)
  colnames(expected) <- c("a", "b")
  output <- proximate_pairs(annulus, diameter = 0.3)
  output <- output[order(output[, 1]), ]
  
  expect_equal(expected, output)
})

test_that("Vietoris-Rips geom(s) and stat(s) work correctly", {
  # sample dataset - noisy circle
  angles <- runif(100, 0, 2 * pi)
  annulus <- cbind(x = cos(angles) + rnorm(100, 0, 0.2),
                   y = sin(angles) + rnorm(100, 0, 0.2))
  
  # build ggplot2 objects without plotting
  annulus_df <- as.data.frame(annulus)
  disk.gg <- ggplot(annulus_df, aes(x = x, y = y)) +
    stat_disk(radius = 0.35, fill = "aquamarine3", alpha = 0.15) +
    geom_point()
  vietoris1.gg <- ggplot(annulus_df, aes(x = x, y = y)) +
    stat_vietoris1(diameter = 0.7, alpha = 0.25) +
    geom_point()
  vietoris2.gg <- ggplot(annulus_df, aes(x = x, y = y)) +
    stat_vietoris2(diameter = 0.7, alpha = 0.1) +
    geom_point()
  
  # run tests - not doing much, just making sure there's no errors
  #   before this point
  expect_true(is.ggplot(disk.gg))
  expect_true(is.ggplot(vietoris1.gg))
  expect_true(is.ggplot(vietoris2.gg))
})
