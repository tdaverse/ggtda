context("Test visualization via persistence barcodes")
library("ggtda")

test_that("Persistence barcode geom(s) and stat(s) work correctly", {
  # toy example
  test.df <- data.frame(
    birth = c(0, 0, 1, 2),
    death = c(5, 3, 5, 3),
    dim = c("0", "0", "2", "1")
  )
  
  # build ggplot2 objects without plotting
  test.geom <- ggplot(data = test.df,
                      aes(start = birth, end = death,
                          group = dim, color = dim)) +
    geom_barcode()
  expect_equal(class(ggplot_build(test.geom)), "ggplot_built")
  
  # use alternative positional aesthetics
  test.geom <- ggplot(data = test.df,
                      aes(xmin = birth, xmax = death,
                          group = dim, color = dim)) +
    geom_barcode()
  expect_equal(class(ggplot_build(test.geom)), "ggplot_built")
  
  # warn to use correct aesthetics
  test.geom <- ggplot(data = test.df,
                      aes(x = birth, xend = death,
                          group = dim, color = dim)) +
    geom_barcode()
  expect_error(ggplot_build(test.geom), "start and end")
})
