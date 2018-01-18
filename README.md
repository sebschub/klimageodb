[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/sebschub/klimageodb.svg?branch=master)](https://travis-ci.org/sebschub/klimageodb) [![codecov](https://codecov.io/gh/sebschub/klimageodb/branch/master/graph/badge.svg)](https://codecov.io/gh/sebschub/klimageodb)

# klimageodb

This package simplifies the access to our measurement database. The
database fields are described in `travis/create_tables.sql`.

## Installation

You can install klimageodb from github with:


``` r
# install.packages("devtools")
devtools::install_github("sebschub/klimageodb")
```
If you receive the error `Installation failed: error in running
command`, try setting `options(unzip = "unzip")` before running
`install_github`.

The package is based on the
[DBI](https://cran.r-project.org/web/packages/DBI/) and the
[RPostgres](https://cran.r-project.org/web/packages/RPostgres/) package.
