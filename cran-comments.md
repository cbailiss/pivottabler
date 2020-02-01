## Background

This package is written entirely in R with no external dependencies/components other than the packages listed in the DESCRIPTION file.
This update has not made any significant changes to the structure of the package or dependencies.
Development approach is aligned with practices described in:
http://r-pkgs.had.co.nz/

## Why does the package title contain "in R"?

The package author recognises that normally it is superfluous to include "in R" in a package title.  However, in this case...

The title of the package includes "in R" as a contrast clause to the other popular pivot table package (rpivotTable).  rpivotTable constructs the pivot table using JavaScript.  This package uses only R, meaning the pivot table can be used with a much wider range of R functions and output formats.

## Test environments

* local OS (windows) install, R 3.6.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2.
* R-hub, R-devel

## R CMD check results

### Local R CMD check results

0 errors | 0 warnings | 1 note

NOTE:  
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    data   2.0Mb
    doc    3.5Mb
    R      1.1Mb

### Travis-CI R CMD check results

0 errors | 0 warnings | 1 note

NOTE:  
  installed size is  6.9Mb
  sub-directories of 1Mb or more:
    data   2.0Mb
    doc    3.6Mb
    R      1.1Mb

### R-hub check results

0 errors | 0 warnings | 1 note

NOTE:  
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    R      1.1Mb
    data   2.0Mb
    doc    3.5Mb

### Comments on check results

The sample data has been compressed into three rda files, total size 1 MB. Different compression options were explored and the option chosen that resulted in the smallest files.

## Downstream dependencies

None.
