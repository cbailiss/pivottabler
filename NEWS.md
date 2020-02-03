pivottabler 1.3.0
================

Overview
--------

This release introduces a new layout type - outline layout - that can make larger pivot tables with multiple levels of row groups more readable and more visually appealing.  

Several small improvements mean that irregular pivot tables (e.g. two pivot tables in one) are now easier to construct.

The package vignettes have grown too large be hosted on CRAN.  They have moved to:  http://www.pivottabler.org.uk/articles/ 

Improvements
----------------

* A new "outlined" layout is now supported (in "beta" in this release).  This generates additional rows for row data groups which appear as headings within the pivot table.  See the new "Regular Layout" vignette for details.
* Several small improvements make building irregular layouts much easier, including: 
   * adding empty rows and columns, e.g. using `pt$addRowGroup(caption="By Size", isEmpty=TRUE)` or `pt$addColumnGroup(...)`,
   * adding individual data groups, e.g. using `pt$addRowGroup(variableName="Size", values="Small")` or `pt$addColumnGroup(...)`,
   * adding total data groups, e.g. using `pt$addRowGroup(variableName="Size", isTotal=TRUE)` or `pt$addColumnGroup(...)`.
   * Several other options that are described in the "Irregular Layout" vignette.
* Headings for the row data groups (i.e. headings for the first column / first few columns) in a pivot table can now be specified.  See the "Pivot tables as standard tables (row group headings)" section in the "Data Groups" vignette for details.  The new `showRowGroupHeaders` argument can be used with `pt$renderPivot()`, `pt$getHtml()`, `pt$saveHtml()`, `pt$writeToExcelWorksheet()` and `pt$asBasicTable()`.
* New function `pt$asDataMatrix()` provides a cleaner way to convert a pivot table to a matrix, where the row/column headings in the pivot table become the row/column headings in the matrix.  See the "Outputs" vignette for details.
* New function `setStyling()` provides an alternative method to set style declarations on data group headers and cells.  See the "Irregular Layout" vignette for an example.
* Additional arguments can now be passed to custom functions used to format calculation values.  See the `fmtFuncArgs` parameter in the "Calculations" vignette for details.
* Additional arguments can now be passed to custom functions used to format data group values.  See the `fmtFuncArgs` parameter in the "Data Groups" vignette for details.

Bug Fixes
----------------

The `atLevel` argument of `pt$addColumnDataGroups()` and `pt$addRowDataGroups()` now behaves correctly / more intuitively.  Previously it would often add the data groups at the level below the level expected.

Deprecated
----------------

The following can still be used but now emit a deprecation warning:

* `pt$getTopColumnGroups()` has been deprecated and replaced with `pt$topColumnGroups`.
* `pt$getLeafColumnGroups()` has been deprecated and replaced with `pt$leafColumnGroups`.
* `pt$getTopRowGroups()` has been deprecated and replaced with `pt$topRowGroups`.
* `pt$getLeafRowGroups()` has been deprecated and replaced with `pt$leafRowGroups`.


pivottabler 1.2.3
=================

This release includes one small bug fix only:
Adding more than nine calculations causes the calculation columns to appear in the wrong order (issue #25).


pivottabler 1.2.2
=================

This release includes one small bug fix only:
Calling pt$setStyling(cells=...) on an empty list of cells now succeeds without an error (issue #23).


pivottabler 1.2.1
=================

This release includes one small bug fix only:
Calling pt$asDataFrame() on a pivot table containing blank/NA cells now succeeds without an error (issue #20).


pivottabler 1.2.0
================

Overview
--------

This release includes one small potentially breaking change and one bug fix.

Breaking Changes
----------------

**Changes to rowspan and colspan attributes in HTML**

When generating HTML, previous versions of the package would always generate rowspan and colspan attributes for merged table cells, even if the number of rows or columns being spanned was only one.  Starting with v1.2.0, rowspan and colspan attributes are only generated where the number of rows or columns being spanned is greater than one.  This should make no difference to the visual appearance of the table, however it may cause issues for users who require the previous behaviour.  The previous behaviour is still available by specifying `compatibility=list(explicitHeaderSpansOfOne=TRUE)` as an argument when creating the pivot table, either in `PivotTable$new()` or one of the quick pivot functions such as `qpvt()`.

Improvements
----------------

Small improvements have been made to the conversion of pivot tables to basic tables (in the `basictabler` package).  Starting from this version of `pivottabler` and v0.3.0 of `basictabler` the HTML that is generated from the two packages should be more consistent.  Previously, `basictabler` would render row/column header cells using the HTML **td** element instead of the more correct **th** element.  Thanks to @rickwargo for reporting this difference in HTML output between the two packages.

Bug Fixes
----------------

Calling `pt$setStyling()` on the same cell multiple times now succeeds (previously failed with error).


pivottabler 1.1.0
================

Overview
--------

This release includes:

* Ability to convert a pivot table to a basic table (from the basictabler package)
* Many small styling improvements
* More styling information and examples in the vignettes

Breaking Changes
----------------

This version of `pivottabler` generates slightly different CSS/HTML for the built-in themes/styling compared to previous versions.  The visual appearance is unchanged.  This may be a breaking change for users who require the generated CSS/HTML code to be identical to previous versions.

More details:

In version 1.0.0 and earlier versions of `pivottabler`, the built-in themes used a shared set of style declarations for both calculation value cells and total cells.  From `pivottabler` version 1.1.0 onwards, total cells use a separate set of style declarations.  The visual appearance of pivot tables using the built-in themes has not changed, only the HTML/CSS that is generated is slightly different - so the great majority of users will not be affected.  

This change reduces the risk of styling changes to totals accidentally affecting all calculation value cells and vice-versa.  

The output of earlier versions, where total cells and calculation value cells use a shared set of style declarations, can be generated by specifying `compatibility=list(totalStyleIsCellStyle=TRUE)` as an argument when creating the pivot table, either in `PivotTable$new()` or one of the quick pivot functions such as `qpvt()`.

Improvements
----------------

* Convert a pivot table to a `basictabler` table - enabling flexible/arbitrary changes to be made to pivot tables after they have been created, e.g. inserting or deleting rows/columns/cells.  See the "Outputs" vignette for more details.
* New function `pt$setStyling()` simplifies the setting of formatting and styling on data groups and table cells.  See the "Styling" vignette for details.
* Specifying styling/formatting when creating pivot tables using the `qpvt()` and `qhpvt()` functions is now possible.  See the "Introduction" vignette for a list of parameters for these functions.  See the "Styling" vignette for more examples.
* Specifying styling/formatting when creating pivot tables using the verbose syntax is also now possible - styling can be specified when adding data groups and when adding calculations to pivot tables.  Again, see the "Styling" vignette for more details and examples.
* A more detailed explanation of styling rules has been added to the "Styling" vignette.


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

* Bug fixed that would cause corrupt Excel files to be generated when exporting pivot tables with no row/column groups to Excel.

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
