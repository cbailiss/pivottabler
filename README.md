pivottabler
================

[![Build Status](https://travis-ci.org/cbailiss/pivottabler.svg?branch=master)](https://travis-ci.org/cbailiss/pivottabler) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pivottabler)](https://cran.r-project.org/package=pivottabler)

The `pivottabler` package enables pivot tables to be created with just a few lines of R.

The `pivottabler` package aims to:

-   Provide an easy way of creating pivot tables, without requiring the user to specify low-level layout logic.
-   Provide multiple ways of specifying calculation logic to cover both simple and more sophisticated requirements.
-   Provide styling options so the pivot tables can be themed/branded as needed.

All calculations for the pivot tables take place inside R, enabling the use of a wide-range of R functions in the calculation logic.

Pivot tables are rendered as htmlwidgets, Latex or plain text. The HTML/Latex/text can be exported for use outside of R.

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

`pivottabler` has many styling and formatting capabilities when rendering pivot tables in HTML / as htmlwidgets using `pt$renderPivot()`, however the most basic output is simply as plain text.

#### Plain Text Output

A simple example of creating a pivot table - summarising the types of trains run by different train companies:

``` r
library(pivottabler)
# arguments:  qpvt(dataFrame, rows, columns, calculations)
qpvt(bhmtrains, "TOC", "TrainCategory", "n()") # TOC = Train Operating Company 
```

                         Express Passenger  Ordinary Passenger  Total  
    Arriva Trains Wales               3079                 830   3909  
    CrossCountry                     22865                  63  22928  
    London Midland                   14487               33792  48279  
    Virgin Trains                     8594                       8594  
    Total                            49025               34685  83710  

`pivottabler` also offers a more verbose syntax that is more self-describing and offers additional options that aren't available with the quick-pivot functions such as control over totals/sub-totals. The equivalent verbose commands to output the same pivot table as above are:

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains) # bhmtrains is a data frame with columns TrainCategory, TOC, etc.
pt$addColumnDataGroups("TrainCategory") # e.g. Express Passenger
pt$addRowDataGroups("TOC") # TOC = Train Operating Company e.g. Arriva Trains Wales
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$evaluatePivot()
pt
```

Multiple levels can be added to the pivot table row or column headings, e.g. looking at combinations of TOC and PowerType:

``` r
library(pivottabler)
qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()")
```

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains)
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$addRowDataGroups("PowerType") # D/EMU = Diesel/Electric Multiple Unit, HST=High Speed Train
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$evaluatePivot()
pt
```

                                Express Passenger  Ordinary Passenger  Total  
    Arriva Trains Wales  DMU                 3079                 830   3909  
                         Total               3079                 830   3909  
    CrossCountry         DMU                22133                  63  22196  
                         HST                  732                        732  
                         Total              22865                  63  22928  
    London Midland       DMU                 5638                5591  11229  
                         EMU                 8849               28201  37050  
                         Total              14487               33792  48279  
    Virgin Trains        DMU                 2137                       2137  
                         EMU                 6457                       6457  
                         Total               8594                       8594  
    Total                                   49025               34685  83710  

#### HTML Output

The HTML rendering of the same two pivot tables shown above (each constructed using both a quick-pivot function and verbose syntax) is:

``` r
library(pivottabler)
qhpvt(bhmtrains, "TOC", "TrainCategory", "n()") 
```

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains) 
pt$addColumnDataGroups("TrainCategory")
pt$addRowDataGroups("TOC")
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$renderPivot()
```

![<http://cbailiss.me.uk/pivottablerreadmeimgs/example1.png>](http://cbailiss.me.uk/pivottablerreadmeimgs/example1.png)

``` r
library(pivottabler)
qhpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()")  
```

``` r
library(pivottabler)
pt <- PivotTable$new()
pt$addData(bhmtrains) # bhmtrains is a data frame with columns TrainCategory, TOC, etc.
pt$addColumnDataGroups("TrainCategory") # e.g. Express Passenger
pt$addRowDataGroups("TOC") # TOC = Train Operating Company e.g. Arriva Trains Wales
pt$addRowDataGroups("PowerType") # D/EMU = Diesel/Electric Multiple Unit, HST=High Speed Train
pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
pt$evaluatePivot()
pt
```

![<http://cbailiss.me.uk/pivottablerreadmeimgs/example2.png>](http://cbailiss.me.uk/pivottablerreadmeimgs/example2.png)

### More Information

More complex pivot tables can also be created, e.g. with irregular layouts, using multiple data frames, using multiple calculations and/or custom R calculation functions. See the package vignettes for more details:

``` r
# to see a list of available package vignettes:
vignette(package="pivottabler")
# to open a specific vignette
vignette(topic="v01-introduction", package="pivottabler")
```

The vignettes can also be read on CRAN at: <https://cran.r-project.org/package=pivottabler>
