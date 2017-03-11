getTheme <- function(parentPivot=NULL, themeName=NULL) {
  checkArgument("", "getTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getTheme", themeName, missing(themeName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
  if(themeName=="default") return(getDefaultTheme(parentPivot=parentPivot))
  else if(themeName=="largeplain") return(getLargePlainTheme(parentPivot=parentPivot))
  else if(themeName=="compact") return(getCompactTheme(parentPivot=parentPivot))
  else stop(paste0("getTheme(): Theme '", themeName, "' is not a recognised theme."))
}

getDefaultTheme <- function(parentPivot=NULL, themeName="default") {
  checkArgument("", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  pivotStyles$addStyle(styleName="ColumnHeader", list(
      font="0.75em arial",
      padding="2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "background-color"="#F2F2F2"
    ))
  pivotStyles$addStyle(styleName="RowHeader", list(
      font="0.75em arial",
      padding="2px 8px 2px 2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      "background-color"="#F2F2F2"
    ))
  pivotStyles$addStyle(styleName="Cell", list(
      font="0.75em arial",
      padding="2px 2px 2px 8px",
      border="1px solid lightgray",
      "text-align"="right"
    ))
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$totalStyle <- "Cell"
  return(invisible(pivotStyles))
}

getLargePlainTheme <- function(parentPivot=NULL, themeName="largeplain") {
  checkArgument("", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  pivotStyles$addStyle(styleName="ColumnHeader", list(
      font="0.875em arial",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold"
    ))
  pivotStyles$addStyle(styleName="RowHeader", list(
      font="0.875em arial",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold"
    ))
  pivotStyles$addStyle(styleName="Cell", list(
      font="0.875em arial",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "text-align"="right"
    ))
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$totalStyle <- "Cell"
  return(invisible(pivotStyles))
}

getCompactTheme <- function(parentPivot=NULL, themeName="compact") {
  checkArgument("", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  pivotStyles$addStyle(styleName="ColumnHeader", list(
      font="0.625em arial",
      padding="2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "background-color"="#F2F2F2"
    ))
  pivotStyles$addStyle(styleName="RowHeader", list(
      font="0.625em arial",
      padding="2px 4px 2px 2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      "background-color"="#F2F2F2"
    ))
  pivotStyles$addStyle(styleName="Cell", list(
      font="0.625em arial",
      padding="2px 2px 2px 6px",
      border="1px solid lightgray",
      "text-align"="right"
    ))
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "RowHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$totalStyle <- "Cell"
  return(invisible(pivotStyles))
}
