context("Themes")

# Tests adapted from ggplot2 theme tests ---------------------------------------

test_that("provided themes explicitly define only (void) theme elements", {
  t0 <- theme_void()
  
  t <- theme_persist()
  expect_is(t, "theme")
  expect_true(all(names(t) %in% names(t0)))
  
  t <- theme_barcode()
  expect_is(t, "theme")
  expect_true(all(names(t) %in% names(t0)))
})

test_that("themes don't change without acknowledgement", {
  # sample dataset
  df <- data.frame(birth = c(0, 1, 2, 3),
                   death = c(1, 3, 6, 10),
                   dimension = factor(c(1L, 1L, 2L, 3L)),
                   cloud = "A")
  # element-rich persistence plot
  p <- ggplot(df, aes(start = birth, end = death, color = dimension)) +
    stat_persistence() +
    facet_wrap(~ cloud)
  # comparisons
  skip_on_cran()
  vdiffr::expect_doppelganger("theme_persist, persistence", p + theme_persist())
  # element-rich barcode plot
  p <- ggplot(df, aes(start = birth, end = death, color = dimension)) +
    geom_barcode() +
    facet_wrap(~ cloud)
  # comparisons
  skip_on_cran()
  vdiffr::expect_doppelganger("theme_barcode, barcode", p + theme_barcode())
  vdiffr::expect_doppelganger("theme_barcode, barcode, vertical",
                              p + theme_barcode(vertical = TRUE))
})
