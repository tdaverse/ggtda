context("Test visualization via persistence diagrams")
library("ggplot2")
library("ggtda")

test_that("Persistence geom(s) and stat(s) work correctly", {
  # sample dataset
  test.df <- data.frame(start = c(0, 1, 2, 3),
                        end = c(1, 3, 6, 10))
  flat.df <- data.frame(start = c(0, 1, 2, 3),
                        end = c(1, 2, 4, 7))
  
  # build ggplot2 objects without plotting
  test.gg <- ggplot(data = test.df, aes(start = start, end = end)) +
    geom_persist()
  flat.gg <- ggplot(data = test.df, aes(start = start, end = end)) +
    geom_persist(stat = "flat")
  flat2.gg<- ggplot(data = test.df, aes(start = start, end = end)) +
    stat_flat()
  
  # run tests - not doing much, just making sure there's no errors
  #   before this point
  expect_true(is.ggplot(test.gg))
  expect_true(is.ggplot(flat.gg))
  expect_true(is.ggplot(flat2.gg))
})
