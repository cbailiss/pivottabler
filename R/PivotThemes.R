#' Get a built-in theme for styling a pivot table.
#'
#' \code{getTheme} returns the specified theme.
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name of the theme to retrieve.
#' @return A PivotStyles object.
getTheme <- function(parentPivot, themeName=NULL) {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getTheme", themeName, missing(themeName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
  }
  if(themeName=="default") return(getDefaultTheme(parentPivot=parentPivot))
  else if(themeName=="standardtable") return(getStandardTableTheme(parentPivot=parentPivot))
  else if(themeName=="largeplain") return(getLargePlainTheme(parentPivot=parentPivot))
  else if(themeName=="compact") return(getCompactTheme(parentPivot=parentPivot))
  else if(themeName=="blank") return(getBlankTheme(parentPivot=parentPivot))
  else stop(paste0("getTheme(): Theme '", themeName, "' is not a recognised theme."), call.=FALSE)
}

#' Get an empty theme for applying no styling to a table.
#'
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getBlankTheme <- function(parentPivot, themeName="blank") {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getBlankTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getBlankTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  # build styles
  pivotStyles$addStyle(styleName="Table", list())
  pivotStyles$addStyle(styleName="ColumnHeader", list())
  pivotStyles$addStyle(styleName="RowHeader", list())
  pivotStyles$addStyle(styleName="Cell", list())
  pivotStyles$addStyle(styleName="OutlineColumnHeader", list())
  pivotStyles$addStyle(styleName="OutlineRowHeader", list())
  pivotStyles$addStyle(styleName="OutlineCell", list())
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", list())
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineRowHeader"
  pivotStyles$outlineColHeaderStyle <- "OutlineColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}

#' Get the default theme for styling a pivot table.
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @return A PivotStyles object.
getDefaultTheme <- function(parentPivot, themeName="default") {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getDefaultTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getDefaultTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  # header lists
  baseHeaderList <- list(
    "font-family"="Arial",
    "font-size"="0.75em",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "font-weight"="bold",
    "background-color"="#F2F2F2",
    "xl-wrap-text"="wrap"
  )
  colHeaderList <- baseHeaderList
  colHeaderList[["padding"]] <- "2px"
  colHeaderList[["text-align"]] <- "center"
  rowHeaderList <- baseHeaderList
  rowHeaderList[["padding"]] <- "2px 8px 2px 2px"
  rowHeaderList[["text-align"]] <- "left"
  # cell lists
  cellList <- list(
    "font-family"="Arial",
    "font-size"="0.75em",
    padding="2px 2px 2px 8px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "text-align"="right"
  )
  outlineCellList <- cellList
  outlineCellList[["background-color"]] <- "#F5F5F5"
  # build styles
  pivotStyles$addStyle(styleName="ColumnHeader", colHeaderList)
  pivotStyles$addStyle(styleName="RowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="Cell", cellList)
  pivotStyles$addStyle(styleName="OutlineColumnHeader", colHeaderList)
  pivotStyles$addStyle(styleName="OutlineRowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="OutlineCell", outlineCellList)
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", cellList)
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineRowHeader"
  pivotStyles$outlineColHeaderStyle <- "OutlineColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}

#' Get the a theme for styling to a pivot table that looks more like a standard table (i.e. no row column headings).
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @return A PivotStyles object.
getStandardTableTheme <- function(parentPivot, themeName="standardtable") {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getStandardTableTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getStandardTableTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
    "border-collapse"="collapse"
  ))
  # column style lists
  baseColumnList <- list(
    "font-family"="Arial",
    "font-size"="0.75em",
    padding="2px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "font-weight"="bold",
    "background-color"="#F2F2F2",
    "xl-wrap-text"="wrap"
  )
  leftColumnList <- baseColumnList
  leftColumnList[["text-align"]] <- "left"
  centreColumnList <- baseColumnList
  centreColumnList[["text-align"]] <- "center"
  # cell style lists
  baseCellList <- list(
    "font-family"="Arial",
    "font-size"="0.75em",
    padding="2px 8px 2px 2px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "font-weight"="normal",
  )
  leftCellList <- baseCellList
  leftCellList[["text-align"]] <- "left"
  leftCellList[["xl-wrap-text"]] <- "wrap"
  rightCellList <- baseCellList
  rightCellList[["text-align"]] <- "right"
  # build styles
  pivotStyles$addStyle(styleName="LeftColumnHeader", leftColumnList)
  pivotStyles$addStyle(styleName="CentreColumnHeader", centreColumnList)
  pivotStyles$addStyle(styleName="LeftCell", leftCellList)
  pivotStyles$addStyle(styleName="RightCell", rightCellList)
  pivotStyles$addStyle(styleName="OutlineCentreColumnHeader", centreColumnList)
  pivotStyles$addStyle(styleName="OutlineLeftCell", leftCellList)
  pivotStyles$addStyle(styleName="OutlineRightCell", rightCellList)
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", rightCellList)
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "LeftColumnHeader"
  pivotStyles$rowHeaderStyle <- "LeftCell"
  pivotStyles$colHeaderStyle <- "CentreColumnHeader"
  pivotStyles$cellStyle <- "RightCell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineLeftCell"
  pivotStyles$outlineColHeaderStyle <- "OutlineCentreColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineRightCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}

#' Get the large plain theme for styling a pivot table.
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @return A PivotStyles object.
getLargePlainTheme <- function(parentPivot, themeName="largeplain") {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  # header lists
  baseHeaderList <- list(
    "font-family"="Arial",
    "font-size"="0.875em",
    padding="4px",
    "min-width"="100px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "text-align"="center",
    "font-weight"="bold",
    "xl-wrap-text"="wrap"
  )
  columnHeaderList <- baseHeaderList
  columnHeaderList[["text-align"]] <- "center"
  rowHeaderList <- baseHeaderList
  rowHeaderList[["text-align"]] <- "left"
  # cell lists
  cellList <- list(
    "font-family"="Arial",
    "font-size"="0.875em",
    padding="4px",
    "min-width"="100px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "text-align"="right"
  )
  outlineCellList <- cellList
  outlineCellList[["background-color"]] <- "#F5F5F5"
  # build styles
  pivotStyles$addStyle(styleName="ColumnHeader", columnHeaderList)
  pivotStyles$addStyle(styleName="RowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="Cell", cellList)
  pivotStyles$addStyle(styleName="OutlineColumnHeader", colHeaderList)
  pivotStyles$addStyle(styleName="OutlineRowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="OutlineCell", outlineCellList)
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", cellList)
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineRowHeader"
  pivotStyles$outlineColHeaderStyle <- "OutlineColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}

#' Get the compact theme for styling a pivot table.
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @return A PivotStyles object.
getCompactTheme <- function(parentPivot, themeName="compact") {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  # header lists
  baseHeaderList <- list(
    "font-family"="Arial",
    "font-size"="0.625em",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "font-weight"="bold",
    "background-color"="#F2F2F2",
    "xl-wrap-text"="wrap"
  )
  columnHeaderList <- baseHeaderList
  columnHeaderList[["padding"]] <- "2px"
  columnHeaderList[["text-align"]] <- "center"
  rowHeaderList <- baseHeaderList
  rowHeaderList[["padding"]] <- "2px 4px 2px 2px"
  rowHeaderList[["text-align"]] <- "left"
  # cell lists
  cellList <- list(
    "font-family"="Arial",
    "font-size"="0.625em",
    padding="2px 2px 2px 6px",
    border="1px solid lightgray",
    "vertical-align"="middle",
    "text-align"="right"
  )
  outlineCellList <- cellList
  outlineCellList[["background-color"]] <- "#F5F5F5"
  # build styles
  pivotStyles$addStyle(styleName="ColumnHeader", columnHeaderList)
  pivotStyles$addStyle(styleName="RowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="Cell", cellList)
  pivotStyles$addStyle(styleName="OutlineColumnHeader", colHeaderList)
  pivotStyles$addStyle(styleName="OutlineRowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="OutlineCell", outlineCellList)
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", cellList)
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineRowHeader"
  pivotStyles$outlineColHeaderStyle <- "OutlineColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}

#' Get a simple coloured theme.
#'
#' Get a simple coloured theme that can be used to style a pivot table into a custom colour scheme.
#'
#' @export
#' @param parentPivot Owning pivot table.
#' @param themeName The name to use as the new theme name.
#' @param colors The set of colours to use when generating the theme (see the Styling vignette for details).
#' @param fontName The name of the font to use, or a comma separated list (for font-fall-back).
#' @return A PivotStyles object.
getSimpleColoredTheme <- function(parentPivot, themeName="coloredTheme", colors, fontName) {
  if(R6::is.R6Class(parentPivot)&&(parentPivot$classname=="PivotTable")) argumentCheckMode <- parentPivot$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", colors, missing(colors), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", fontName, missing(fontName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse",
      "border"=paste0("2px solid ", colors$borderColor)
    ))
  # header lists
  baseHeaderList <- list(
    "font-family"=fontName,
    "font-size"="0.75em",
    padding="2px",
    "border"=paste0("1px solid ", colors$borderColor),
    "vertical-align"="middle",
    "text-align"="center",
    "font-weight"="bold",
    color=colors$headerColor,
    "background-color"=colors$headerBackgroundColor,
    "xl-wrap-text"="wrap"
  )
  columnHeaderList <- baseHeaderList
  columnHeaderList[["padding"]] <- "2px"
  columnHeaderList[["text-align"]] <- "center"
  rowHeaderList <- baseHeaderList
  rowHeaderList[["padding"]] <- "2px 8px 2px 2px"
  rowHeaderList[["text-align"]] <- "left"
  # cell lists
  cellList <- list(
    "font-family"=fontName,
    "font-size"="0.75em",
    padding="2px 2px 2px 8px",
    "border"=paste0("1px solid ", colors$borderColor),
    "vertical-align"="middle",
    "text-align"="right",
    color=colors$cellColor,
    "background-color"=colors$cellBackgroundColor
  )
  outlineCellList <- cellList
  outlineCellList[["color"]] <-  ifelse(is.null(colors$outlineCellColor), colors$cellColor, colors$outlineCellColor)
  outlineCellList[["background-color"]] <- ifelse(is.null(colors$outlineCellBackgroundColor), colors$cellBackgroundColor, colors$outlineCellBackgroundColor)
  totalCellList <- cellList
  totalCellList[["color"]] <-  colors$totalColor
  totalCellList[["background-color"]] <-  colors$totalBackgroundColor
  # build styles
  pivotStyles$addStyle(styleName="ColumnHeader", columnHeaderList)
  pivotStyles$addStyle(styleName="RowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="Cell", cellList)
  pivotStyles$addStyle(styleName="OutlineColumnHeader", columnHeaderList)
  pivotStyles$addStyle(styleName="OutlineRowHeader", rowHeaderList)
  pivotStyles$addStyle(styleName="OutlineCell", outlineCellList)
  if(!isTRUE(parentPivot$compatibility$totalStyleIsCellStyle)) {
    pivotStyles$addStyle(styleName="Total", totalCellList)
  }
  # set style names
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "ColumnHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$outlineRowHeaderStyle <- "OutlineRowHeader"
  pivotStyles$outlineColHeaderStyle <- "OutlineColumnHeader"
  pivotStyles$outlineCellStyle <- "OutlineCell"
  pivotStyles$totalStyle <- ifelse(isTRUE(parentPivot$compatibility$totalStyleIsCellStyle), "Cell", "Total")
  return(invisible(pivotStyles))
}
