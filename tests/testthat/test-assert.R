test_that("assertive function works", {
  expect_error(
    assert_dt(iris),
    "x (data.frame) is not a data.table", fixed = TRUE
  )
  dt <- data.table(A = c(1, 1, 2))
  expect_silent(assert_dt(dt))
  expect_error(assert_has_key(dt))
  expect_error(assert_unique(dt))
  expect_equal(get_duplicates(dt), dt[1:2])
  dt <- data.table(A = c(1, 2, NA, 1), B = 1:4, key = "A")
  expect_error(assert_no_na_in_key(dt))
  expect_error(assert_unique(dt))
  expect_error(assert_pk(dt))
  expect_equal(get_duplicates(dt), dt[J(1)])
  expect_equal(nrow(get_duplicates(dt, by = NULL)), 0L)
  expect_equal(get_duplicates(dt, only.key = TRUE), dt[J(1), .(A)])
  setkey(dt, B)
  expect_silent(assert_pk(dt))
})
