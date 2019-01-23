context("Test visualization via persistence barcodes")
library("ggplot2")
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
                      aes(xmin = birth, xmax = death, group = dim, color = dim)) +
    geom_barcode()
  
  # run tests - not doing much, just making sure there's no errors
  #   before this point
  expect_true(is.ggplot(test.geom))
})
