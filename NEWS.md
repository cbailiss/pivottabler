pivottabler 0.2.0.9000:  Performance Improvements
================

Current development version and 0.3.0-rc (CRAN submission May 30th).

Breaking Changes
----------------

* Pivot table initialiser parameters renamed from messages and messageFile to traceEnabled and traceFile respectively. 

Improvements
----------------

* pivottabler now calculates cell values in batches in order to reduce the calculation time required for larger data frames.  For large pivot tables based on large data frames this typically results in a big performance improvement, e.g. for a pivot table of 1000 cells based on a data frame with 10 million rows the rendering time is around 7 seconds in version 0.3.0 compared to over 480 seconds in version 0.2.0.  See the new Performance vignette for more details.
* pivottabler now also supports the data.table package for performing pivot table summary/aggregation calculations.  dplyr remains the default however data.table offers a moderate performance improvement for large data frames (10 million rows and above).  See the Calculations vignette for more details.
* addRowDataGroups and addColumnDataGroups functions pre-group the data to reduce the time required for larger data frames.
* New argumentCheckMode parameter added to pivot table initialiser to provide an additional option to reduce the time required to create larger pivot tables.
* print() method added to PivotTable class.  Can now print a simple plain text view of the pivot table to the console using just `pt` or retrieve the plain text as a character value using `pt$asCharacter`.
* Quick-pivot functions added that construct a basic pivot table with one line of R: `qpvt()`, `qhpvt()` and `qlpvt()`.  See the Introduction vignette for more details.
* Internal pivot filters class differentiates more clearly between 'all', 'some' and 'none' match cases for more robust filtering and early elimination of some cell calculations.
* The getCells() function has been made more intuitive to use when getting specific cells by using a new cellCoordinates argument.  See the Finding and Formatting vignette for details.
* Stricter name checking for calculation names to avoid later unclear dplyr/data.table errors caused by syntax errors arising from illegal names.

Bug Fixes
----------------

* Various small bug fixes.

Upcoming Changes
----------------

* The previous usage of the arguments for the getCells() function is still supported (and is still the default) however the new argument usage will be made the default in a future version.  For now, a message is displayed noting the upcoming change.

pivottabler 0.2.0:  New Output/Conversion Options, New Find Options
================

Breaking Changes
----------------

(none)

Improvements
----------------

* Added the ability to output a pivot table in Latex.
* Added the asMatrix() function to allow the pivot table contents to be retrieved as a matrix.
* Added the asDataFrame() and asTidyDataFrame() functions to allow the pivot table contents to be retrieved as a data frame.
* Added findRowDataGroups() and findColumnDataGroups() functions to find data groups (i.e. headings) that match specified criteria to simplify scenarios such as changing the styling of specific headings.
* Added the getCells() function to retrieve cells by row number and/or column number.
* Added the findCells() function to find cells in the body of a pivot table that match specified criteria to simplify scenarios such as conditional formatting.
* Five new vignettes added.  Many changes to the existing vignettes.
* Modified the sample data by specifying a time zone (UTC) for all POSIXct data to remove inconsistencies when using the data in different time zones. 
* Modified the automated tests to no longer use the digest package.
* Updated object documentation to wrap lines longer than 80 characters.

Bug Fixes
----------------

* Various small bug fixes.
* Shiny vignette examples now working.

pivottabler 0.1.0
================

Initial version.

Earlier versions
================

No versions prior to 0.1.0 were released.
