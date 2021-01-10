#' @title Is it a unique data.table?
#' @details When `x` has a key, it only check the key columns.
#' @return `TRUE` when there's no duplicate found in the table.
#'  Otherwise, `FALSE` is returned.
#' @inheritParams is_pk
#' @export
is_unique <- function(x) {
  stopifnot(is.data.table(x))
  identical(anyDuplicated(x, by = key(x)), 0L)
}

#' @title Is there any NA in the key columns of the data.table?
#' @inheritParams is_pk
#' @details It throws error when `x` doesn't have a key
#' @export
any_na_in_key <- function(x) {
  stopifnot(is.data.table(x), haskey(x))
  vapply(key(x), function(col) anyNA(x[[col]]), logical(1L))
}

#' @title Does the data.table have a primary key?
#' @describe The primary key here is similar to the concept in relational DB.
#'  It means the key uniquely identifies each record in the table.
#' @param x a `data.table` object
#' @return `TRUE` when the `data.table` object has a key,
#'  has no `NA` in key columns and doesn't have duplicate key values.
#'  Otherwise, it returns `FALSE`.
#' @export
is_pk <- function(x) {
  stopifnot(is.data.table(x))
  haskey(x) && !any_na_in_key(x) && is_unique(x)
}
