context("Test visualization via persistence diagrams")
library("ggplot2")
library("ggtda")

test_that("Persistence diagram geom(s) and stat(s) work correctly", {
  # sample dataset
  test.df <- data.frame(start = c(0, 1, 2, 3),
                        end = c(1, 3, 6, 10))
  flat.df <- data.frame(start = c(0, 1, 2, 3),
                        end = c(1, 2, 4, 7))
  
  # build ggplot2 objects without plotting
  flat.gg <- ggplot(data = test.df, aes(start = start, end = end)) +
    geom_persist()
  diag.gg <- ggplot(data = test.df, aes(start = start, end = end)) +
    geom_persist(diag = TRUE)
  
  # run tests - not doing much, just making sure there's no errors
  #   before this point
  expect_true(is.ggplot(diag.gg))
  expect_true(is.ggplot(flat.gg))
})
