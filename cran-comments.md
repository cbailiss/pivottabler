## Background

This is the 1st update to a relatively new package.
This package is written entirely in R with no external dependencies/components other than the packages listed in the DESCRIPTION file.
Development approach is aligned with practices described in:
http://r-pkgs.had.co.nz/

## Test environments

* local OS (windows) install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0.
* win-builder, R-devel = R 3.5.0.

## R CMD check results

### Local R CMD check results

0 errors | 0 warnings | 0 notes

### Travis-CI R CMD check results

0 errors | 0 warnings | 1 note

NOTE:  
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    data   3.9Mb
    doc    1.7Mb

Unsure of reason for discrepancy between Local R CMD and Travis-CI CMD check results. The sample data has been compressed into three rda files, total size 1 MB. Different compression options were explored and the option chosen that resulted in the smallest files.

### win-builder check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None.
