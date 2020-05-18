context("Test visualization via persistence diagrams")
library("ggtda")

# sample data set
d <- data.frame(
  birth = c(0, 0, 1, 2, 1.5),
  death = c(5, 3, 5, 3, 6),
  dim = c("0", "0", "2", "1", "1")
)

test_that("Persistence layers work correctly", {
  
  # build ggplot2 objects without plotting:
  # flat persistence diagram
  p_pf <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence(diagram = "flat")
  expect_is(p_pf, "ggplot")
  expect_is(p_pf$layer[[1]], "ggproto")
  expect_equal(c(p_pf$labels$start, p_pf$labels$end), c("birth", "death"))
  # diagonal persistence diagram
  p_pd <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence()
  expect_is(p_pd, "ggplot")
  expect_is(p_pd$layer[[1]], "ggproto")
  expect_equal(c(p_pd$labels$start, p_pd$labels$end), c("birth", "death"))
  # landscape persistence diagram
  p_pl <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_persistence(diagram = "landscape")
  expect_is(p_pl, "ggplot")
  expect_is(p_pl$layer[[1]], "ggproto")
  expect_equal(c(p_pl$labels$start, p_pl$labels$end), c("birth", "death"))
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger(
    "Flat persistence diagram", p_pf, "persistence"
  )
  vdiffr::expect_doppelganger(
    "Diagonal persistence diagram", p_pd, "persistence"
  )
  vdiffr::expect_doppelganger(
    "Landscape persistence diagram", p_pl, "persistence"
  )
})

test_that("Frontier layers work correctly", {
  
  # build ggplot2 objects without plotting:
  # flat frontier diagram
  p_ff <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier(diagram = "flat")
  expect_is(p_ff, "ggplot")
  expect_is(p_ff$layer[[1]], "ggproto")
  expect_equal(c(p_ff$labels$start, p_ff$labels$end), c("birth", "death"))
  # diagonal frontier diagram
  p_fd <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier()
  expect_is(p_fd, "ggplot")
  expect_is(p_fd$layer[[1]], "ggproto")
  expect_equal(c(p_fd$labels$start, p_fd$labels$end), c("birth", "death"))
  # landscape frontier diagram
  p_fl <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier(diagram = "landscape")
  expect_is(p_fl, "ggplot")
  expect_is(p_fl$layer[[1]], "ggproto")
  expect_equal(c(p_fl$labels$start, p_fl$labels$end), c("birth", "death"))
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger(
    "Flat frontier diagram", p_ff, "persistence"
  )
  vdiffr::expect_doppelganger(
    "Diagonal frontier diagram", p_fd, "persistence"
  )
  vdiffr::expect_doppelganger(
    "Landscape frontier diagram", p_fl, "persistence"
  )
})
