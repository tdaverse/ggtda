context("Test visualization theme(s)")
library("ggtda")

test_that("Minimalist TDA theme works correctly", {
  # sample dataset
  test.df <- data.frame(start = c(0, 1, 2, 3),
                        end = c(1, 3, 6, 10))
  
  # build ggplot2 objects without plotting
  test.gg <- ggplot(data = test.df, aes(start = start, end = end)) +
    geom_persist()
  test.theme1 <- test.gg + theme_persist()
  test.theme2 <- test.gg + theme_barcode()
  
  # run tests - not doing much, just making sure there's no errors
  #   before this point
  expect_true(is.ggplot(test.gg))
  expect_true(is.ggplot(test.theme1))
  expect_true(is.ggplot(test.theme2))
})
