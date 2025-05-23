% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PivotOpenXlsxRenderer.R
\docType{class}
\name{PivotOpenXlsxRenderer}
\alias{PivotOpenXlsxRenderer}
\title{R6 class that renders a pivot table into an Excel worksheet.}
\format{
\code{\link[R6]{R6Class}} object.
}
\description{
The `PivotOpenXlsxRenderer` class creates a representation of a
pivot table in an Excel file using the `openxlsx` package.
See the "Excel Export" vignette for details and examples.
}
\examples{
# This class should only be created by the pivot table.
# It is not intended to be created outside of the pivot table.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PivotOpenXlsxRenderer-new}{\code{PivotOpenXlsxRenderer$new()}}
\item \href{#method-PivotOpenXlsxRenderer-clearIsRenderedFlags}{\code{PivotOpenXlsxRenderer$clearIsRenderedFlags()}}
\item \href{#method-PivotOpenXlsxRenderer-writeToCell}{\code{PivotOpenXlsxRenderer$writeToCell()}}
\item \href{#method-PivotOpenXlsxRenderer-writeToWorksheet}{\code{PivotOpenXlsxRenderer$writeToWorksheet()}}
\item \href{#method-PivotOpenXlsxRenderer-clone}{\code{PivotOpenXlsxRenderer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PivotOpenXlsxRenderer-new"></a>}}
\if{latex}{\out{\hypertarget{method-PivotOpenXlsxRenderer-new}{}}}
\subsection{Method \code{new()}}{
Create a new `PivotOpenXlsxRenderer` object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PivotOpenXlsxRenderer$new(parentPivot)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parentPivot}}{The pivot table that this `PivotOpenXlsxRenderer`
instance belongs to.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `PivotOpenXlsxRenderer` object.
An internal method used when rendering a pivot table to HTML.
Clear the IsRendered flags that exist on the `PivotDataGroup` class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PivotOpenXlsxRenderer-clearIsRenderedFlags"></a>}}
\if{latex}{\out{\hypertarget{method-PivotOpenXlsxRenderer-clearIsRenderedFlags}{}}}
\subsection{Method \code{clearIsRenderedFlags()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PivotOpenXlsxRenderer$clearIsRenderedFlags()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
No return value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PivotOpenXlsxRenderer-writeToCell"></a>}}
\if{latex}{\out{\hypertarget{method-PivotOpenXlsxRenderer-writeToCell}{}}}
\subsection{Method \code{writeToCell()}}{
Writes a value to a cell and applies styling as needed.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PivotOpenXlsxRenderer$writeToCell(
  wb = NULL,
  wsName = NULL,
  rowNumber = NULL,
  columnNumber = NULL,
  value = NULL,
  applyStyles = TRUE,
  baseStyleName = NULL,
  style = NULL,
  mapFromCss = TRUE,
  mergeRows = NULL,
  mergeColumns = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{wb}}{A `Workbook` object representing the Excel file being written
to.}

\item{\code{wsName}}{A character value specifying the name of the worksheet to
write to.}

\item{\code{rowNumber}}{An integer value specifying the row number of the cell
to write to.}

\item{\code{columnNumber}}{An integer value specifying the column number of the
cell to write to.}

\item{\code{value}}{The value to write into the cell.}

\item{\code{applyStyles}}{Default `TRUE` to write styling information to the cell.}

\item{\code{baseStyleName}}{A character value specifying a named style defined
in the pivot table.}

\item{\code{style}}{A `PivotStyle` object containing CSS style declarations to
override the base style.}

\item{\code{mapFromCss}}{Default `TRUE` to automatically convert CSS style
declarations to their Excel equivalents.}

\item{\code{mergeRows}}{An integer vector specifying the row extent of a merged
cell.}

\item{\code{mergeColumns}}{An integer vector specifying the column extent of a
merged cell.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No return value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PivotOpenXlsxRenderer-writeToWorksheet"></a>}}
\if{latex}{\out{\hypertarget{method-PivotOpenXlsxRenderer-writeToWorksheet}{}}}
\subsection{Method \code{writeToWorksheet()}}{
Write the pivot table into the specified workbook and worksheet at
the specified row-column location.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PivotOpenXlsxRenderer$writeToWorksheet(
  wb = NULL,
  wsName = NULL,
  topRowNumber = NULL,
  leftMostColumnNumber = NULL,
  outputHeadingsAs = "formattedValueAsText",
  outputValuesAs = "rawValue",
  applyStyles = TRUE,
  mapStylesFromCSS = TRUE,
  exportOptions = NULL,
  showRowGroupHeaders = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{wb}}{A `Workbook` object representing the Excel file being written
to.}

\item{\code{wsName}}{A character value specifying the name of the worksheet to
write to.}

\item{\code{topRowNumber}}{An integer value specifying the row number in the
Excel worksheet  to write the pivot table.}

\item{\code{leftMostColumnNumber}}{An integer value specifying the column number
in the Excel worksheet to write the pivot table.}

\item{\code{outputHeadingsAs}}{Must be one of "rawValue",
"formattedValueAsText" (default) or "formattedValueAsNumber" to specify
how data groups are written into the Excel sheet.}

\item{\code{outputValuesAs}}{Must be one of "rawValue" (default),
"formattedValueAsText" or "formattedValueAsNumber" to specify
how cell values are written into the Excel sheet.}

\item{\code{applyStyles}}{Default `TRUE` to write styling information to the cell.}

\item{\code{mapStylesFromCSS}}{Default `TRUE` to automatically convert CSS style
declarations to their Excel equivalents.}

\item{\code{exportOptions}}{A list of additional export options - see the
"A1. Appendix" for details.}

\item{\code{showRowGroupHeaders}}{Default `FALSE`, specify `TRUE` to write row
group headers.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No return value.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PivotOpenXlsxRenderer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PivotOpenXlsxRenderer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PivotOpenXlsxRenderer$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
