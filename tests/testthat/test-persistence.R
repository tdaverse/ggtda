context("Persistence diagrams")

# Tests adapted from rPref psel tests ------------------------------------------

# 'mtcars' data, subsetted and renamed for persistence interpretation
d <- subset(mtcars, select = c(mpg, hp))
names(d) <- c("start", "end")

test_that("pareto calculations work on 'mtcars' data not using rPref", {
  expect_equal(rownames(pareto_persistence_base(d)),
               rownames(d)[c(16L, 24L, 31L)])
})

test_that("pareto calculations work on 'mtcars' data using rPref", {
  skip_if_not_installed("rPref")
  expect_equal(rownames(pareto_persistence_rPref(d)),
               rownames(d)[c(16L, 24L, 31L)])
})

test_that("pareto calculations are obtained via conditional function", {
  expect_equal(rownames(pareto_persistence(d)),
               rownames(d)[c(16L, 24L, 31L)])
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

test_that("Frontier layers work correctly", {
  
  # build ggplot objects without plotting:
  # flat frontier diagram
  p_ff <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier(diagram = "flat")
  expect_is(p_ff, "ggplot")
  expect_is(p_ff$layer[[1L]], "ggproto")
  expect_equal(c(p_ff$labels$start, p_ff$labels$end), c("birth", "death"))
  # diagonal frontier diagram
  p_fd <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier(diagram = "diagonal")
  expect_is(p_fd, "ggplot")
  expect_is(p_fd$layer[[1L]], "ggproto")
  expect_equal(c(p_fd$labels$start, p_fd$labels$end), c("birth", "death"))
  # landscape frontier diagram
  p_fl <- ggplot(data = d, aes(start = birth, end = death, color = dim)) +
    stat_frontier(diagram = "landscape")
  expect_is(p_fl, "ggplot")
  expect_is(p_fl$layer[[1L]], "ggproto")
  expect_equal(c(p_fl$labels$start, p_fl$labels$end), c("birth", "death"))
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger("stat_frontier, flat", p_ff)
  vdiffr::expect_doppelganger("stat_frontier, diagonal", p_fd)
  vdiffr::expect_doppelganger("stat_frontier, landscape", p_fl)
})
