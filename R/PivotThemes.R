getTheme <- function(parentPivot=NULL, themeName=NULL) {
  checkArgument("", "getTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getTheme", themeName, missing(themeName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
  if(themeName=="default") return(getDefaultTheme(parentPivot=parentPivot))
  else stop(paste0("getTheme(): Theme '", themeName, "' is not a recognised theme."))
}

getDefaultTheme <- function(parentPivot=NULL, themeName="default") {
  checkArgument("", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
  checkArgument("", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  pivotStyles <- PivotStyles$new(parentPivot=parentPivot, themeName=themeName)
  pivotStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse"
    ))
  pivotStyles$addStyle(styleName="Header", list(
      font="0.9em arial",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold"
    ))
  pivotStyles$addStyle(styleName="Cell", list(
      font="0.9em arial",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "text-align"="right"
    ))
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "Header"
  pivotStyles$rowHeaderStyle <- "Header"
  pivotStyles$colHeaderStyle <- "Header"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$totalStyle <- "Cell"
  return(pivotStyles)
}

