
# This is a quick function intended to assist with rendering basic tables in the
# vignettes.
# It doesn't emit any styles, it is intended to pick up the styles of one of the
# pivot tables rendered into the same page. this isn't intended as a general
# purpose function and so skips some checks.

#' Output a table into a package vignette.
#'
#' \code{renderBasicTable} is utility function that renders a basic table into a
#' package vignette.  This function is primarily intended for internal use by
#' the pivottabler package.
#'
#' @import htmltools
#' @import htmlwidgets
#' @export
#' @param matrix Tabular data to render.
#' @param stylePrefix Text prefix for CSS style declarations.
#' @param columnNamesAsHeader Include column names in output (if FALSE, the
#' first row from the matrix is used as the column headings).
#' @param rowNamesAsHeader Include row names in output.
#' @param columnAlignment A character vector specifying the horizontal alignment
#' of each column.
#' @return A basic table rendered as a HTML widget.
#' @examples
#' renderBasicTable(matrix(c(1:12), nrow=3))

renderBasicTable <- function(matrix=NULL, stylePrefix=NULL, columnNamesAsHeader=FALSE, rowNamesAsHeader=FALSE,
                             columnAlignment="right") {
  checkArgument(4, TRUE, "", "renderBasicTable", matrix, missing(matrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses="matrix")
  checkArgument(4, TRUE, "", "renderBasicTable", stylePrefix, missing(stylePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(4, TRUE, "", "renderBasicTable", columnNamesAsHeader, missing(columnNamesAsHeader), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(4, TRUE, "", "renderBasicTable", rowNamesAsHeader, missing(rowNamesAsHeader), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  trows <- list()
  if(is.null(matrix)) return()
  rowCount <- nrow(matrix)
  columnCount <- ncol(matrix)
  if(rowCount==0) return()
  if(columnCount==0) return()
  if(rowNamesAsHeader) {
    m <- cbind(rownames(matrix), matrix)
    columnCount <- ncol(m)
  }
  else m <- matrix
  row <- list()
  cnames <- colnames(m)
  for(c in 1:columnCount) {
    if(c==1) styleName <- "RowHeader"
    else styleName <- "ColumnHeader"
    textAlign <- "left"
    if(c > 1) textAlign <- "right"
    if(length(columnAlignment)>=c) textAlign <- columnAlignment[c]
    textAlign <- paste0("text-align: ", textAlign)
    if(columnNamesAsHeader) row[[length(row)+1]] <- htmltools::tags$th(class=paste0(stylePrefix, styleName), style=textAlign, cnames[c])
    else row[[length(row)+1]] <- htmltools::tags$th(class=paste0(stylePrefix, styleName), style=textAlign, m[1,c])
  }
  trows[[length(trows)+1]] <- htmltools::tags$tr(row)
  if(rowCount>1) {
    rFrom <- 1
    if(columnNamesAsHeader) rFrom <- 1
    else rFrom <- 2
    for(r in rFrom:rowCount) {
      row <- list()
      for(c in 1:columnCount) {
        if((c==1)&&(rowNamesAsHeader==TRUE)) style <- paste0(stylePrefix, "RowHeader")
        else style <- paste0(stylePrefix, "Cell")
        textAlign <- "left"
        if(c > 1) textAlign <- "right"
        if(length(columnAlignment)>=c) textAlign <- columnAlignment[c]
        textAlign <- paste0("text-align: ", textAlign)
        row[[length(row)+1]] <- htmltools::tags$td(class=style, style=textAlign, m[r,c])
      }
      trows[[length(trows)+1]] <- htmltools::tags$tr(row)
    }
  }
  tbl <- htmltools::tags$table(class=paste0(stylePrefix, "Table"), trows)
  settings <- list() # may need this in the future
  widgetData <- list(
    tableCss = NULL,
    tableHtml = as.character(tbl),
    settings = settings
  )
  # viewer.fill=TRUE and browser.fill=TRUE sound like they would be good things, but they seem to prevent
  # any scroll bars being shown when the HTML tables are larger than the RStudio Viewer or the web browser window size
  sp = htmlwidgets::sizingPolicy(
    viewer.padding=10, viewer.fill=FALSE, viewer.suppress=FALSE,
    browser.padding=10, browser.fill=FALSE,
    knitr.defaultWidth="auto", knitr.defaultHeight="auto", knitr.figure = FALSE
  )
  w <- htmlwidgets::createWidget("pivottabler", widgetData, sizingPolicy=sp)
  return(w)
}
