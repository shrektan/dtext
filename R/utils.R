#' Convinient Time Taken
#'
#' Compared to [data.table::timetake()], this version doesn't require the user
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
  last_proc_time <- NULL
  function(reset = FALSE) {
    if (reset || is.null(last_proc_time)) {
      last_proc_time <<- base::proc.time()
    } else {
      timetaken(last_proc_time)
    }
  }
})
