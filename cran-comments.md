## Background

This package is written entirely in R with no external dependencies/components other than the packages listed in the DESCRIPTION file.
This update has not made any significant changes to the structure of the package or dependencies.
Development approach is aligned with practices described in:
http://r-pkgs.had.co.nz/

## Resubmission

* Removed "in R" from package title as requested.
* Reduced vignette build time by removing vignettes from package - they are now hosted on a separate website, with a small vignette left inside the package as a pointer to the new external location.
* Reduced automated test time by significantly reducing the number of tests that will be run on CRAN.

## Test environments

* local OS (windows) install, R 3.6.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2.
* R-hub, three tests:
  * Windows Server 2008 R2 SP1, R-devel 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

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
