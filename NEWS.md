pivottabler 0.2.0
================

Breaking Changes
----------------

(none)

Improvements
----------------

* Added the ability to output a pivot table in Latex.
* Added findRowDataGroups() and findColumnDataGroups() functions to find data groups (i.e. headings) that match specified criteria to simplify scenarios such as changing the styling of specific headings.
* Added the getCells() function to retrieve cells by row number and/or column number.
* Added the findCells() function to find cells in the body of a pivot table that match specified criteria to simplify scenarios such as conditional formatting.
* Added the asMatrix() function to allow the pivot table contents to be retrieved as a matrix.
* Added the asDataFrame() and asTidyDataFrame() functions to allow the pivot table contents to be retrieved as a data frame.
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
