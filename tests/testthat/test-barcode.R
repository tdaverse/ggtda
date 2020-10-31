context("Barcode plots")

# ggplot object tests ----------------------------------------------------------

# toy example
d <- data.frame(
  birth = c(0, 0, 1, 2),
  death = c(5, 3, 5, 3),
  dim = c("0", "0", "2", "1")
)

test_that("barcode layers work correctly", {
  
  # build ggplot objects without plotting
  b_start_end <- ggplot(data = d,
                        aes(start = birth, end = death,
                            group = dim, color = dim)) +
    geom_barcode()
  expect_is(b_start_end, "ggplot")
  expect_is(b_start_end$layer[[1L]], "ggproto")
  expect_equal(class(ggplot_build(b_start_end)), "ggplot_built")
  expect_equal(c(b_start_end$labels$start, b_start_end$labels$end),
               c("birth", "death"))
  
  # use alternative positional aesthetics
  b_xmin_xmax <- ggplot(data = d,
                        aes(xmin = birth, xmax = death,
                            group = dim, color = dim)) +
    geom_barcode()
  expect_is(b_xmin_xmax, "ggplot")
  expect_is(b_xmin_xmax$layer[[1L]], "ggproto")
  expect_equal(class(ggplot_build(b_xmin_xmax)), "ggplot_built")
  expect_equal(c(b_xmin_xmax$labels$xmin, b_xmin_xmax$labels$xmax),
               c("birth", "death"))
  
  # warn to use correct aesthetics
  b_x_xend <- ggplot(data = d,
                     aes(x = birth, xend = death, group = dim, color = dim)) +
    geom_barcode()
  expect_is(b_x_xend, "ggplot")
  expect_is(b_x_xend$layer[[1L]], "ggproto")
  expect_error(ggplot_build(b_x_xend), "start and end")
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger("geom_barcode, start, end", b_start_end)
  vdiffr::expect_doppelganger("geom_barcode, xmin, xmax", b_xmin_xmax)
})
