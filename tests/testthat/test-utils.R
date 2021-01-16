test_that("time_taken() works", {
  expect_is(time_taken(), "proc_time") # first call is reset proc.time
  expect_is(time_taken(), "character") # 2nd call is time taken
  expect_is(time_taken(), "character") # 3rd call is time taken
  expect_is(time_taken(TRUE), "proc_time") # param TRUE means reset
})

test_that("NA_date_ is a Date", {
  expect_is(NA_date_, "Date")
})
