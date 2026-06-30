
df <- data.frame(
  x = c(1, -1/2, -1/2),
  y = c(0, sqrt(3)/2, - sqrt(3)/2),
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

# proper detection of edges in faces - see #68
# rm {4,7} & {6,7}
edges_test <- data.frame(
  a = c(1L, 2L, 1L, 2L, 2L, 3L, 4L, 5L), 
  b = c(2L, 3L, 4L, 4L, 5L, 5L, 6L, 7L)
)
faces_test1 <- data.frame(
  a = c(1L, 2L),
  c = c(4L, 5L),
  b = c(2L, 3L)
)
# also rm {2,3,5}
faces_test2 <- data.frame(
  a = c(1L),
  c = c(4L),
  b = c(2L)
)

test_that("`are_edges_maximal()` correctly tests edge-face incidence", {
  expect_equal(
    are_edges_maximal(edges_test, faces_test1),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    are_edges_maximal(edges_test, faces_test2),
    c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})
