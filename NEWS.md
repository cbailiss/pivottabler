pivottabler 1.0.0.9000
================

Breaking Changes
----------------

TBC

Improvements
----------------

* Convert a pivot table to a `basictabler` table - enabling flexible/arbitrary changes to be made to pivot tables after they have been created, e.g. inserting or deleting rows/columns/cells. 

Bug Fixes
----------------

TBC

Upcoming Changes
----------------

TBC

pivottabler 1.0.0
================

Breaking Changes
----------------

* The default value of the `specifyCellsAsList` argument in the `pt$getCells()` function has been changed to `TRUE`.
The previous usage of the `pt$getCells()` function is still supported (now you must explicitly specify `specifyCellsAsList=FALSE`).  This change has been planned since v0.3.0 (June 2017) and a warning message has been displayed since then.  See the Finding and Formatting vignette for more details on the `specifyCellsAsList` argument. 
* Additional checks are now made to prevent calculations being moved/added after the calculation row/column groups have been generated.   There is a small chance this will cause errors in existing user code - though this will only occur where user code is trying to move/add calculations after the calculations have already been set in the pivot table by a call to `pt$addColumnCalculationGroups()` or `pt$addRowCalculationGroups()` (previously this would silently fail).

Improvements
----------------

* pivottabler now supports using any of the following data types in both row/column headings and cell values: integer, numeric, character, logical, Date, and POSIXct.
* Improved support for illegal data frame column names and illegal calculation names (e.g. including spaces or symbols such as dash, plus, dollar, etc).  See  the Details Appendix (A1) vignette for details.
* The new `PivotFiltersOverrides` class provides many new options for overriding the data used to calculate cell values.  It is now possible to add to, remove from or entirely replace filter criteria as part of calculation definitions.  This makes calculations such as "% of row/column/grand total", ratios/multiples, rolling averages and cumulative sums easier.  See the Calculations Appendix (A2) vignette for examples.
* Row/column heading style settings for data groups can now be declared up-front using the `baseStyleName` and `styleDeclarations` arguments in `pt$addColumnDataGroups(...)` and `pt$addRowDataGroups(...)`.  See the Styling vignette for an example.
* Row/column heading style settings for calculations can now be declared up-front using the `headingBaseStyleName` and `headingStyleDeclarations` arguments in `pt$defineCalculation(...)`.  See the Styling vignette for an example.
* Cell style settings for calculations can now be declared up-front using the `cellBaseStyleName` and `cellStyleDeclarations` arguments in `pt$defineCalculation(...)`.  See the Styling vignette for an example.
* Additional `exportOptions` parameter when exporting to HTML, Latex and Excel for controlling how NA, NaN, -Inf and Inf are exported.  See the Details Appendix (A1) for more information.
* Additional parameter `outputHeadingsAs` in `pt$writeToExcelWorksheet(...)` to control how row/column headings are formatted when exporting to Excel.  See the Excel Export vignette for more details.
* `pt$asDataFrame(...)` and `pt$asTidyDataFrame(...)` now support additional parameter `stringsAsFactors` with default value `default.stringsAsFactors()`.

Bug Fixes
----------------

* Bug fixed that would cause corrupt Excel files to be generated when exporting pivottables with no row/column groups to Excel.

Upcoming Changes
----------------

No breaking changes currently planned.

pivottabler 0.4.0
================

Breaking Changes
----------------

* Removed support for R 3.2.x.  Minimum supported version of base R now R 3.3.0. 

Improvements
----------------

* It is now possible to output a pivot table to an Excel file with one line of R, including with styling that closely matches the HTML output.  See the Excel Export vignette for more details. 
* Quick-pivot functions now support showing/hiding totals and renaming the captions of totals, which was previously only possible using the verbose syntax.  See the Introduction vignette for more details.

Bug Fixes
----------------

* Corrections to ordering of code in Styling vignette.
* A couple of other small bug fixes.

Upcoming Changes
----------------

* The previous usage of the arguments for the getCells() function is still supported (and is still the default) however the new argument usage will be made the default in a future version.  For now, a message is displayed noting the upcoming change.  See the Finding and Formatting vignette for more details.

pivottabler 0.3.0:  Performance Improvements and Quick-Pivot Functions
================

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

* The previous usage of the arguments for the getCells() function is still supported (and is still the default) however the new argument usage will be made the default in a future version.  For now, a message is displayed noting the upcoming change.  See the Finding and Formatting vignette for more details.

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
