#' @title Assert the data.table has a primary key
#' @description The primary key here is similar to the concept in relational DB.
#'  It means the key uniquely identifies each record in the table. It's an important
#'  attribute when `x` is an information table, which is later quried / merged against
#'  by other tables. A primary-keyed table can avoid duplicates in such cases.
#' @inheritParams assert_dt
#' @details Throws error unless `x` has a key, contains no `NA` values in key columns,
#'  and is unique for key values.
#' @export
assert_pk <- function(x) {
  # as assert_no_na_in_key() would call assert_dt() and assert_has_key()
  # we don't need to call them explicitly
  assert_no_na_in_key(x)
  assert_unique(x)
}

#' @title Assert a data.table object
#' @param x a `data.table` object
#' @export
assert_dt <- function(x) {
  if (!is.data.table(x))
    stop(sprintf("x (%s) is not a data.table", toString(class(x))))
  invisible(x)
}

#' @title Assert the data.table has key
#' @inheritParams assert_dt
#' @export
assert_has_key <- function(x) {
  assert_dt(x)
  if (!haskey(x))
    stop("x doesn't have a key")
  invisible(x)
}

#' @title Assert the key contains no NA
#' @inheritParams assert_dt
#' @export
assert_no_na_in_key <- function(x) {
  assert_dt(x)
  assert_has_key(x)
  has_na <- vapply(key(x), function(col) anyNA(x[[col]]), logical(1L))
  na_cols <- sprintf("%s(%d)", colnames(x)[has_na], which(has_na))
  if (length(na_cols))
    stop(sprintf("columns %s contain NA", toString(na_cols)))
  invisible(x)
}

#' @title Assert uniqueness of the data.table
#' @description Throws error when duplicates are found in the table
#' @details When `x` has a key, it only check the key columns.
#' @inheritParams assert_dt
#' @export
assert_unique <- function(x) {
  assert_dt(x)
  if (!identical(anyDuplicated(x, by = key(x)), 0L))
    stop("x contains duplicates, run `get_duplicates()` to see the values")
  invisible(x)
}

#' @title Find the duplicates
#' @inheritParams assert_dt
#' @param by The columns used for uniqueness checks. By default, it only relies on
#'  the key columns of `x` to decide duplication. See [data.table::duplicated()]
#'  for more info.
#' @param only.key When `TRUE` and `x` has key, only the key columns are returned.
#' @param ... Other arguments be passed to [data.table::duplicated()].
#' @note When `x` has keys, the duplicateness is dependes on the key columns only.
#' @return The duplicated records of `x`.
#' @export
get_duplicates <- function(x, only.key = FALSE, by = key(x), ...) {
  cols <- if (haskey(x) && only.key) key(x) else colnames(x)
  x[duplicated(x, by = by), cols, with = FALSE]
}
