test_that("time_taken() works", {
  expect_is(time_taken(), "proc_time") # first call is reset proc.time
  expect_is(time_taken(), "character") # 2nd call is time taken
  expect_is(time_taken(), "character") # 3rd call is time taken
  expect_is(time_taken(TRUE), "proc_time") # param TRUE means reset
})

test_that("NA_date_ is a Date", {
  expect_is(NA_date_, "Date")
})

test_that("NA_datetime_ is a POSIXct", {
  expect_is(NA_datetime_, "POSIXct")
})

test_that("r_date works", {
  expect_identical(r_date(18262), as.Date("2020-01-01"))
  # convert logical values to numeric values
  expect_equal(storage.mode(r_date(NA)), "double")
  # do not change integer type
  expect_equal(storage.mode(r_date(1L)), "integer")
})

test_that("r_datetime works", {
  expect_identical(r_datetime(1610801231), as.POSIXct("2021-01-16 12:47:11"))
  # convert logical values to numeric values
  expect_equal(storage.mode(r_date(NA)), "double")
  # do not change integer type
  expect_equal(storage.mode(r_date(1L)), "integer")
})
