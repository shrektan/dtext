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
})
