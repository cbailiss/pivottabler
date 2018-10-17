## Background

This is the 4th update to the pivottabler package.
This package is written entirely in R with no external dependencies/components other than the packages listed in the DESCRIPTION file.
Development approach is aligned with practices described in:
http://r-pkgs.had.co.nz/

## Why does the package title contain "in R"?

The package author recognises that normally it is superfluous to include "in R" in a package title.  However, in this case...

The title of the package includes "in R" as a contrast clause to the other popular pivot table package (rpivotTable).  rpivotTable constructs the pivot table using JavaScript.  This package uses only R, meaning the pivot table can be used with a much wider range of R functions and output formats.

## Test environments

* local OS (windows) install, R 3.5.1
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.5.1.
* win-builder, R-devel = R 3.6.0.

## R CMD check results

### Local R CMD check results

0 errors | 0 warnings | 0 notes

### Travis-CI R CMD check results

0 errors | 0 warnings | 1 note

NOTE:  
  installed size is  6.7Mb
  sub-directories of 1Mb or more:
    data   3.8Mb
    doc    2.4Mb

Unsure of reason for discrepancy between Local R CMD and Travis-CI CMD check results. The sample data has been compressed into three rda files, total size 1 MB. Different compression options were explored and the option chosen that resulted in the smallest files.

### win-builder check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None.
