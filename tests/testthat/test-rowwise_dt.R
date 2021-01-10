test_that("rowwise_dt works", {
  dt <- rowwise_dt(
    A = , B = ,
    1, "a"
  )
  expect_equal(dt, data.table(A = 1, B = "a"))
  # error if named argument is not empty
  expect_error(rowwise_dt(A = , B = 2, 1, 2))
  # error if no header
  expect_error(rowwise_dt(1, "a"), "Must provide at least one column", fixed = TRUE)
  # error if body is not multiple length of the header
  expect_error(
    rowwise_dt(A = , B = , C = , 1, "a", 2, "b", 3),
    "There're 3 columns but the number of cells is 5, which is not an integer multiple of the columns",
    fixed = TRUE
  )
  # create list element automatically
  expect_equal(rowwise_dt(A = , list(1)), data.table(A = list(1)))
  expect_equal(rowwise_dt(A = , 1:2), data.table(A = list(1:2)))
  expect_equal(rowwise_dt(A = , double()), data.table(A = list(double())))
  # error if named argument is in the middle
  expect_error(rowwise_dt(A = , B = , 1, 2, C = , 4), "Header must be the first N arguments", fixed = TRUE)
  # evaluate arguments in the correct frame
  ncols <- 1e6
  expect_equal(rowwise_dt(A = , ncols), data.table(A = ncols))
})
