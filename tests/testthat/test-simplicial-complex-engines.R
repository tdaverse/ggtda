
df <- data.frame(
  x = c(1, -0.5, -0.5),
  y = c(0, 0.866025403784439, -0.866025403784438),
  PANEL = structure(c(1L, 1L, 1L), levels = "1", class = "factor"), 
  group = c(-1L, -1L, -1L)
)
es <- cbind(a = c(1, 2, 3), b = c(2, 3, 3))
fs <- cbind(a = 1, c = 3, b = 2)

test_that("`indices_to_data()` performs for vertices", {
  expect_equal(nrow(indices_to_data(df)), 3L)
  expect_equal(nrow(indices_to_data(df, es)), 6L)
  expect_equal(nrow(indices_to_data(df, fs)), 3L)
})
