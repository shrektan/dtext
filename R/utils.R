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
