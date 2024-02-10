
test_that("`draw_key_simplex()` produces a viable grob", {
  # key can be created
  expect_no_error(
    k <- draw_key_simplex(
      data.frame(x = 1, y = 1, alpha = .5),
      params = list(),
      size = 1
    )
  )
  # key has expected classes
  expect_s3_class(k, c("polygon", "grob"))
  # vertices are at expected distances
  expect_equal(as.vector(dist(cbind(k$x, k$y))), rep(sqrt(3) / 2, 3L))
})
