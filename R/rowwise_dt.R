#' @import data.table
NULL

#' @title Create a data.table rowwisely
#' @description `rowwise_dt` allows the user to create a `data.table` object by
#'  specifying a row-by-row layout. It's convenient and highly readable when
#'  the table is small.
#' @usage rowwise_dt(...)
#' @param ... Arguments that defines the structure of a `data.table`.
#'  The column names must be specified as `col=` and may only appear before the data.
#'  See the examples section for details.
#' @return A `data.table` object. The default is for each column to return as a vector.
#'  However, if any column has a length that is not one (e.g., `list(1, 2)`),
#'  the whole column will be converted to a list column.
#' @examples
#'  rowwise_dt(
#'    A=, B=, C=,
#'    1, "a", 2:3,
#'    2, "b", list(5)
#'  )
#' @note This function has been filed to data.table in
#'  [PR#4291](https://github.com/Rdatatable/data.table/pull/4291), where the function
#'  name is `rowwiseDT`.
#' @export
rowwise_dt <- function(...) {
  x <- substitute(list(...))[-1L]
  if (is.null(nms <- names(x)))
    stop("Must provide at least one column (use `name=`). See ?rowwise_dt for details")
  header_pos <- which(nzchar(nms))
  if (any(nzchar(x[header_pos])))
    stop("Named arguments must be empty")
  if (!identical(header_pos, seq_along(header_pos)))
    stop("Header must be the first N arguments")
  header <- nms[header_pos]
  ncols <- length(header)
  body <- lapply(x[-header_pos], eval, envir = parent.frame())
  if (length(body) %% ncols != 0L)
    stop(sprintf("There're %d columns but the number of cells is %d, which is not an integer multiple of the columns", ncols, length(body)))
  # make all the non-scalar elements to a list
  body <- lapply(body, function(x) if (length(x) != 1L) list(x) else x)
  body <- split(body, rep(seq_len(length(body) / ncols), each = ncols))
  ans <- rbindlist(body)
  setnames(ans, header)
  ans
}
