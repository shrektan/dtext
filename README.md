# Kit for data.table

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/shrektan/dtkit/branch/main/graph/badge.svg)](https://codecov.io/gh/shrektan/dtkit?branch=master)
[![R build status](https://github.com/shrektan/dtkit/workflows/R-CMD-check/badge.svg)](https://github.com/shrektan/dtkit/actions)
<!-- badges: end -->

This package provides some useful helper functions to work with [data.table](http://r-datatable.com).

Since these are helper functions, the function names are usually concise and may cause conflict with other R packages. Thus, I suggest people use the package without explicitly *attaching* it. In other words, please call the function with `dtkit::fun()`.
