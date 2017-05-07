pivottabler 0.2.0.9000:  Performance Improvements
================

Current development version.

Breaking Changes
----------------

* Pivot table initialiser parameters renamed from messages and messageFile to traceEnabled and traceFile respectively.  

Improvements
----------------

* Pivot table now calculates cell values in batches in order to reduce the calculation time required for larger data frames.
* addRowDataGroups and addColumnDataGroups functions pre-group the data to reduce the time required for larger data frames.
* New argumentCheckMode parameter added to pivot table initialiser to provide options to reduce time required to create larger pivot tables.
* Internal pivot filters class differentiates between all, some and none match cases for more robust filtering and early elimination of some calculations.

Bug Fixes
----------------

* Various small bug fixes.

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
