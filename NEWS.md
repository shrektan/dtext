# CHANGES IN dtkit VERSION 0.1

- Add `rowwise_dt()` to create `data.table` object rowwisely. Note that this function has been filed to `data.table` in [PR#4291](https://github.com/Rdatatable/data.table/pull/4291) (not merged yet), with the function name `rowwiseDT()`.
- Add a set of assertive functions (e.g., `assert_pk()`).
- `time_taken()` is a wrapper version of `data.table::timetaken()`, which is useful for me to use interactively.
