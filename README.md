pivottabler
================

[![Build Status](https://travis-ci.org/cbailiss/pivottabler.svg?branch=master)](https://travis-ci.org/cbailiss/pivottabler)

The `pivottabler` package enables pivot tables to be created with just a few lines of R.

The objectives of the `pivottabler` package are to:

-   Provide an easy way of creating pivot tables, without requiring the user to specify low-level layout logic.
-   Provide multiple ways of specifying calculation logic to cover both simple and more sophisticated requirements.
-   Provide styling options so the pivot tables can be themed/branded as needed.

All calculations for the pivot tables take place inside R, enabling the use of a wide-range of R functions in the calculation logic.

Pivot tables are rendered as htmlwidgets. The HTML can be exported for use outside of R.

### Installation

You can install:

-   **(CRAN release is pending)** the latest released version from CRAN with

``` r
install.packages("pivottabler")
```

-   the latest development version from github with

``` r
devtools::install_github("cbailiss/pivottabler")
```

### Example

A simple example of creating a pivot table:

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
```

More complex pivot tables can also be created, e.g. with irregular layouts, using multiple data frames, using multiple calculations and/or custom R calculation functions. See the package vignettes for more details:

``` r
# to see a list of available package vignettes:
vignette(package="pivottabler")
# to open a specific vignette
vignette(topic="abc", package="pivottabler")
```
