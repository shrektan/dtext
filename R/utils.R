#' Convinient Time Taken
#'
#' Compared to [data.table::timetaken()], this version doesn't require the user
#' to store the value of [base::proc.time()] explicitly. Instead, the first call in a
#' new session will store the value of proc.time automatically. Or you can call the
#' function with param `TRUE` to reset the stored proc.time value, afterwards.
#'
#' @param reset When `TRUE`, it resets the clock.
#' @examples
#'  time_taken()
#'  Sys.sleep(1)
#'  cat("Finished in",time_taken(),"\n")
#' @export
time_taken <- local({
  proc_time <- NULL
  function(reset = FALSE) {
    last_proc_time <- proc_time
    proc_time <<- base::proc.time()
    if (isFALSE(reset) && !is.null(last_proc_time)) {
      timetaken(last_proc_time)
    } else {
      invisible(proc_time)
    }
  }
})

#' The NA value with the Date / Date-Time Class
#' @export
NA_date_ <- structure(NA_real_, class = "Date")

#' @rdname NA_date_
#' @export
NA_datetime_ <- structure(NA_real_, class = c("POSIXct", "POSIXt"))

#' Restore back to R Date
#'
#' Calling R functions like `ifelse()` may accidentally convert a Date vector
#' to a numeric vector, due to losing the class attributes. This helper function
#' enables you to restore the numeric vector back to R Date.
#'
#' @param x the numeric values behind the Date value, which is the number of days
#'   since 1970-01-01, with negative values for earlier dates.
#'
#' @examples
#'  r_date(18262)
#'
#' @export
r_date <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  structure(x, class = "Date")
}

r_datetime <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  structure(x, class = c("POSIXct", "POSIXt"))
}
