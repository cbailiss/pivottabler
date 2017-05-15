pivottabler
================

[![Build Status](https://travis-ci.org/cbailiss/pivottabler.svg?branch=master)](https://travis-ci.org/cbailiss/pivottabler) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pivottabler)](https://cran.r-project.org/package=pivottabler)

The `pivottabler` package enables pivot tables to be created with just a few lines of R.

The `pivottabler` package aims to:

-   Provide an easy way of creating pivot tables, without requiring the user to specify low-level layout logic.
-   Provide multiple ways of specifying calculation logic to cover both simple and more sophisticated requirements.
-   Provide styling options so the pivot tables can be themed/branded as needed.

All calculations for the pivot tables take place inside R, enabling the use of a wide-range of R functions in the calculation logic.

Pivot tables are rendered as htmlwidgets or as Latex. The HTML/Latex can be exported for use outside of R.

Pivot tables can also be converted to a standard R matrix or data frame.

### Installation

You can install:

-   the latest released version from CRAN with

``` r
install.packages("pivottabler")
```

-   the latest development version from github with

``` r
devtools::install_github("cbailiss/pivottabler", build_vignettes = TRUE)
```

### Example

A simple example of creating a pivot table - summarising the types of trains run by different train companies:

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains) # bhmtrains is a data frame with columns TrainCategory, TOC, etc.
pt$addColumnDataGroups("TrainCategory") # e.g. Express Passenger
pt$addRowDataGroups("TOC") # TOC = Train Operating Company e.g. Arriva Trains Wales
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
```

### More Information

More complex pivot tables can also be created, e.g. with irregular layouts, using multiple data frames, using multiple calculations and/or custom R calculation functions. See the package vignettes for more details:

``` r
# to see a list of available package vignettes:
vignette(package="pivottabler")
# to open a specific vignette
vignette(topic="v01-introduction", package="pivottabler")
```

The vignettes can also be read on CRAN at: <https://cran.r-project.org/package=pivottabler>

The Introduction vignette includes an examples gallery.
