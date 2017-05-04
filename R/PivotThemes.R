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
  else if(themeName=="largeplain") return(getLargePlainTheme(parentPivot=parentPivot))
  else if(themeName=="compact") return(getCompactTheme(parentPivot=parentPivot))
  else stop(paste0("getTheme(): Theme '", themeName, "' is not a recognised theme."))
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
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
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
  pivotStyles$addStyle(styleName="ColumnHeader", list(
      font=paste0("0.75em ", fontName),
      padding="2px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      color=colors$headerColor,
      "background-color"=colors$headerBackgroundColor
    ))
  pivotStyles$addStyle(styleName="RowHeader", list(
      font=paste0("0.75em ", fontName),
      padding="2px 8px 2px 2px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      color=colors$headerColor,
      "background-color"=colors$headerBackgroundColor
    ))
  pivotStyles$addStyle(styleName="Cell", list(
      font=paste0("0.75em ", fontName),
      padding="2px 2px 2px 8px",
      "border"=paste0("1px solid ", colors$borderColor),
      "text-align"="right",
      color=colors$cellColor,
      "background-color"=colors$cellBackgroundColor
    ))
  pivotStyles$addStyle(styleName="Total", list(
      font=paste0("0.75em ", fontName),
      padding="2px 2px 2px 8px",
      "border"=paste0("1px solid ", colors$borderColor),
      "text-align"="right",
      color=colors$totalColor,
      "background-color"=colors$totalBackgroundColor
    ))
  pivotStyles$tableStyle <- "Table"
  pivotStyles$rootStyle <- "ColumnHeader"
  pivotStyles$rowHeaderStyle <- "RowHeader"
  pivotStyles$colHeaderStyle <- "ColumnHeader"
  pivotStyles$cellStyle <- "Cell"
  pivotStyles$totalStyle <- "Total"
  return(invisible(pivotStyles))
}
