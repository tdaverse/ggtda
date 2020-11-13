context("Simplicial complexes")

test_that("proximate functions threshold correctly on equilateral triangle", {
  
  # equilateral triangle
  et <- data.frame(x = cos(2*pi*c(0,1/3,2/3)), y = sin(2*pi*c(0,1/3,2/3)))
  # small perturbations from key values
  eps <- .00000001
  
  # proximate pairs at diameter ~ sqrt(3)
  expect_equal(nrow(proximate_pairs(et, diameter = sqrt(3) - eps)), 0L)
  expect_equal(nrow(proximate_pairs(et, diameter = sqrt(3) + eps)), 3L)
  
  # proximate triples at diameter ~ 2
  expect_equal(nrow(proximate_triples(et, diameter = 2 - eps)), 0L)
  expect_equal(nrow(proximate_triples(et, diameter = 2 + eps)), 1L)
})

# sample data set
n <- 100L; sd <- 0.1
set.seed(7)
t <- stats::runif(n = n, min = 0, max = 2*pi)
d <- data.frame(
  x = cos(t) + stats::rnorm(n = n, mean = 0, sd = sd),
  y = sin(t) + stats::rnorm(n = n, mean = 0, sd = sd)
)

# ggplot object tests ----------------------------------------------------------

test_that("disk layer works as expected", {
  
  # disks of specified radius and resolution (segments)
  p <- ggplot(d, aes(x = x, y = y)) +
    geom_point() +
    stat_disk(radius = 0.35, segments = 60L, fill = "aquamarine3")
  expect_is(p, "ggplot")
  expect_is(p$layer[[1]], "ggproto")
  expect_equal(c(p$labels$x, p$labels$y), c("x", "y"))
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # throws error when tested by CI tools
  expect_equal(nrow(layer_data(p, i = 2L)), nrow(d) * (60L + 1L))
  
  # visual regression test
  vdiffr::expect_doppelganger("stat_disk, annulus", p)
})

test_that("Cech layers work as expected", {
  
  # Čech 0-simplices stat
  p0 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech0()
  expect_is(p0, "ggplot")
  expect_is(p0$layer[[1]], "ggproto")
  expect_equal(c(p0$labels$x, p0$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p0)), nrow(d))
  
  # Čech 1-simplices stat
  p1 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech1(diameter = 0.7)
  expect_is(p1, "ggplot")
  expect_is(p1$layer[[1]], "ggproto")
  expect_equal(c(p1$labels$x, p1$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p1)), 1090L)
  
  # Čech 2-simplices stat
  p2 <- ggplot(d, aes(x = x, y = y)) +
    stat_cech2(diameter = 0.7)
  expect_is(p2, "ggplot")
  expect_is(p2$layer[[1]], "ggproto")
  expect_equal(c(p2$labels$x, p2$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p2)), 17205L)
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger("stat_cech0, annulus", p0)
  vdiffr::expect_doppelganger("stat_cech1, annulus", p1)
  vdiffr::expect_doppelganger("stat_cech2, annulus", p2)
})

test_that("Vietoris layers work as expected", {
  
  # Vietoris 0-simplices stat
  p0 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris0()
  expect_is(p0, "ggplot")
  expect_is(p0$layer[[1]], "ggproto")
  expect_equal(c(p0$labels$x, p0$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p0)), nrow(d))
  
  # Vietoris 1-simplices stat
  p1 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris1(diameter = 0.7)
  expect_is(p1, "ggplot")
  expect_is(p1$layer[[1]], "ggproto")
  expect_equal(c(p1$labels$x, p1$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p1)), 1090L)
  
  # Vietoris 2-simplices stat
  p2 <- ggplot(d, aes(x = x, y = y)) +
    stat_vietoris2(diameter = 0.7)
  expect_is(p2, "ggplot")
  expect_is(p2$layer[[1]], "ggproto")
  expect_equal(c(p2$labels$x, p2$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p2)), 17214L)
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression tests
  vdiffr::expect_doppelganger("stat_vietoris0, annulus", p0)
  vdiffr::expect_doppelganger("stat_vietoris1, annulus", p1)
  vdiffr::expect_doppelganger("stat_vietoris2, annulus", p2)
})

test_that("face layer works as expected", {
  
  # disks of specified radius and resolution (segments)
  p1 <- ggplot(d, aes(x = x, y = y)) +
    geom_face(stat = "disk", radius = 0.35) +
    geom_point()
  expect_is(p1, "ggplot")
  expect_is(p1$layer[[1]], "ggproto")
  expect_equal(c(p1$labels$x, p1$labels$y), c("x", "y"))
  
  # Vietoris-Rips face geom
  p2 <- ggplot(d, aes(x = x, y = y)) +
    geom_face(stat = "vietoris2", diameter = 0.7, alpha = .05)
  expect_is(p2, "ggplot")
  expect_is(p2$layer[[1]], "ggproto")
  expect_equal(c(p2$labels$x, p2$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p2)), 17214L)
  
  # Čech face geom
  p3 <- ggplot(d, aes(x = x, y = y)) +
    geom_face(stat = "cech2", diameter = 0.7, alpha = .05)
  expect_is(p3, "ggplot")
  expect_is(p3$layer[[1]], "ggproto")
  expect_equal(c(p3$labels$x, p3$labels$y), c("x", "y"))
  expect_equal(nrow(layer_data(p3)), 17205L)
  
  # skip on continuous integration services
  skip_on_travis()
  skip_on_appveyor()
  
  # visual regression test
  vdiffr::expect_doppelganger("geom_face, stat_disk, annulus", p1)
  vdiffr::expect_doppelganger("geom_face, stat_vietoris, annulus", p2)
  vdiffr::expect_doppelganger("geom_face, stat_cech, annulus", p3)
})
