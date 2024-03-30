
test_that("`diagram_transform()` correctly transforms key points", {
  d <- data.frame(
    x = c(-Inf, 0, 0, exp(1), exp(1), pi,  Inf),
    y = c(-Inf, 0, 1, exp(1), pi,     Inf, Inf)
  )
  
  # diagonal transform (do nothing)
  d_diag <- diagram_transform(d, "diagonal")
  expect_identical(d, d_diag)
  
  # flat transform
  d2 <- d
  d2$y <- d$y - d$x
  d_flat <- diagram_transform(d, "flat")
  expect_identical(d2, d_flat)
  
  # landscape transform
  d3 <- transform(
    d,
    x = (x + y) / 2,
    y = (y - x) / 2
  )
  # accommodate landscape horizons
  d3$y[c(1L, nrow(d3))] <- 0
  d_land <- diagram_transform(d, "landscape")
  expect_identical(d3, d_land)
  
})

# ggplot object tests ----------------------------------------------------------

# sample data set
d <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

test_that("persistence layers work correctly", {
  
  # build ggplot objects without plotting:
  # flat persistence diagram
  p_pf <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence(diagram = "flat")
  expect_is(p_pf, "ggplot")
  expect_is(p_pf$layer[[1L]], "ggproto")
  expect_equal(c(p_pf$labels$start, p_pf$labels$end), c("birth", "death"))
  # diagonal persistence diagram
  p_pd <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence(diagram = "diagonal")
  expect_is(p_pd, "ggplot")
  expect_is(p_pd$layer[[1L]], "ggproto")
  expect_equal(c(p_pd$labels$start, p_pd$labels$end), c("birth", "death"))
  # landscape persistence diagram
  p_pl <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence(diagram = "landscape")
  expect_is(p_pl, "ggplot")
  expect_is(p_pl$layer[[1L]], "ggproto")
  expect_equal(c(p_pl$labels$start, p_pl$labels$end), c("birth", "death"))
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger("stat_persistence, flat", p_pf)
  vdiffr::expect_doppelganger("stat_persistence, diagonal", p_pd)
  vdiffr::expect_doppelganger("stat_persistence, landscape", p_pl)
})
