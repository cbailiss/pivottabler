
#' Quickly build a basic pivot table.
#'
#' The \code{qpvt} function builds a basic pivot table with one line of R.
#'
#' @export
#' @param dataFrame The data frame containing the data to be summarised in the
#'   pivot table.
#' @param rows A character vector of variable names to be plotted on the rows of
#'   the pivot table, or "=" to specify the position of the calculations.
#' @param columns A character vector of variable names to be plotted on the
#'   columns of the pivot table, or "=" to specify the position of the
#'   calculations.
#' @param calculations One or more summary calculations to use to calculate the
#'   values of the cells in the pivot table.
#' @param theme Either the name of a built-in theme (default, largeplain,
#'   compact or blank/none) or a list which specifies the default formatting for
#'   the table.
#' @param replaceExistingStyles TRUE to completely replace the default styling
#'   with the specified tableStyle, headingStyle, cellStyle and/or totalStyle
#' @param tableStyle A list of CSS style declarations that apply to the table.
#' @param headingStyle A list of CSS style declarations that apply to the
#'   heading cells in the table.
#' @param cellStyle A list of CSS style declarations that apply to the normal
#'   cells in the table.
#' @param totalStyle A list of CSS style declarations that apply to the total
#'   cells in the table.
#' @param ... Additional arguments, currently format, formats, totals,
#'   compatibility and/or argumentCheckMode.
#' @return A pivot table.
#' @examples
#' qpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qpvt(bhmtrains, c("=", "TOC"), c("TrainCategory", "PowerType"),
#'      c("Number of Trains"="n()",
#'        "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"))

qpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL,
                 theme=NULL, replaceExistingStyles=FALSE,
                 tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qpvt", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
  checkArgument(3, TRUE, "", "qpvt", rows, missing(rows), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qpvt", columns, missing(columns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qpvt", calculations, missing(calculations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qpvt", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyles"), allowedListElementClasses="character")
  checkArgument(3, TRUE, "", "qpvt", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qpvt", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qpvt", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qpvt", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qpvt", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  compatibility <- arguments$compatibility
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]], # can't use $format as this also matches formats
                   totalsSpecified=("totals" %in% names(arguments)),
                   totals=arguments[["totals"]],
                   theme=theme, replaceExistingStyles=replaceExistingStyles,
                   tableStyle=tableStyle, headingStyle=headingStyle, cellStyle=cellStyle, totalStyle=totalStyle,
                   compatibility=compatibility)
  pt$evaluatePivot()
  return(pt)
}

#' Quickly render a basic pivot table in HTML.
#'
#' The \code{qhpvt} function renders a basic pivot table as a HTML widget with
#' one line of R.
#'
#' @export
#' @param dataFrame The data frame containing the data to be summarised in the
#'   pivot table.
#' @param rows A character vector of variable names to be plotted on the rows of
#'   the pivot table, or "=" to specify the position of the calculations.
#' @param columns A character vector of variable names to be plotted on the
#'   columns of the pivot table, or "=" to specify the position of the
#'   calculations.
#' @param calculations One or more summary calculations to use to calculate the
#'   values of the cells in the pivot table.
#' @param theme Either the name of a built-in theme (default, largeplain,
#'   compact or blank/none) or a list which specifies the default formatting for
#'   the table.
#' @param replaceExistingStyles TRUE to completely replace the default styling
#'   with the specified tableStyle, headingStyle, cellStyle and/or totalStyle
#' @param tableStyle A list of CSS style declarations that apply to the table.
#' @param headingStyle A list of CSS style declarations that apply to the
#'   heading cells in the table.
#' @param cellStyle A list of CSS style declarations that apply to the normal
#'   cells in the table.
#' @param totalStyle A list of CSS style declarations that apply to the total
#'   cells in the table.
#' @param ... Additional arguments, currently format, formats, totals,
#'   styleNamePrefix, compatibility and/or argumentCheckMode.
#' @return A HTML widget.
#' @examples
#' qhpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qhpvt(bhmtrains, "TOC", "TrainCategory",
#'      c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)",
#'        "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
#'      formats=list("%.0f", "%.1f"),
#'      totals=list("TOC"="All TOCs",
#'        "TrainCategory"="All Categories"))

qhpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL,
                  theme=NULL, replaceExistingStyles=FALSE,
                  tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qhpvt", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
  checkArgument(3, TRUE, "", "qhpvt", rows, missing(rows), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", columns, missing(columns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", calculations, missing(calculations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyles"), allowedListElementClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhpvt", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qhpvt", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qhpvt", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  checkArgument(3, TRUE, "", "qhpvt", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  compatibility <- arguments$compatibility
  styleNamePrefix <- arguments$styleNamePrefix
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qhpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]], # can't use $format as this also matches formats
                   totalsSpecified=("totals" %in% names(arguments)),
                   totals=arguments[["totals"]],
                   theme=theme, replaceExistingStyles=replaceExistingStyles,
                   tableStyle=tableStyle, headingStyle=headingStyle, cellStyle=cellStyle, totalStyle=totalStyle,
                   compatibility=compatibility)
  w <- pt$renderPivot(styleNamePrefix=styleNamePrefix)
  return(w)
}

#' Quickly get a Latex representation of a basic pivot table.
#'
#' The \code{qlpvt} function returns the Latex for a basic pivot table with
#' one line of R.
#'
#' @export
#' @param dataFrame The data frame containing the data to be summarised in the
#'   pivot table.
#' @param rows A character vector of variable names to be plotted on the rows of
#'   the pivot table, or "=" to specify the position of the calculations.
#' @param columns A character vector of variable names to be plotted on the
#'   columns of the pivot table, or "=" to specify the position of the
#'   calculations.
#' @param calculations One or more summary calculations to use to calculate the
#'   values of the cells in the pivot table.
#' @param ... Additional arguments, currently format, formats, totals,
#'   argumentCheckMode, compatibility, caption and/or label.  See the Latex
#'   output vignette for a description of caption and label.
#' @return Latex.
#' @examples
#' qlpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qlpvt(bhmtrains, "TOC", "TrainCategory", "n()",
#'       caption="my caption", label="mylabel")

qlpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qlpvt", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
  checkArgument(3, TRUE, "", "qlpvt", rows, missing(rows), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qlpvt", columns, missing(columns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qlpvt", calculations, missing(calculations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  compatibility <- arguments$compatibility
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qlpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]], # can't use $format as this also matches formats
                   totalsSpecified=("totals" %in% names(arguments)),
                   totals=arguments[["totals"]],
                   compatibility=compatibility)
  return(pt$getLatex(caption=arguments$caption, label=arguments$label))
}


# internal functions for quickly building a pivot table

addCalculations <- function(pt, calculations, format=NULL, formats=NULL) {
  nms <- names(calculations)
  for(i in 1:length(calculations)) {
    calc <- calculations[i]
    nme <- nms[i]
    if(is.null(nme)) nme <- paste0("calc", sprintf("%06d", i))
    cf <- NULL
    if(is.null(format)==FALSE) cf <- format
    else {
      if(is.null(formats)==FALSE) {
        if(length(formats)>=i) cf <-formats[[i]]
      }
    }
    # quick-pivot functions will allow spaces to be specified in the calculation name
    # as these are often desired in the captions, but need to remove them from the calculation name
    # when adding the calculation to the pivot table
    pt$defineCalculation(calculationName=make.names(nme), caption=nme, summariseExpression=calc, format=cf)
  }
}

buildPivot <- function(functionName=NULL, argumentCheckMode=NULL,
                       dataFrame=NULL, dataName=NULL,
                       rows=NULL, columns=NULL, calculations=NULL,
                       format=NULL, formats=NULL,
                       totalsSpecified=FALSE, totals=NULL,
                       theme=NULL, replaceExistingStyles=FALSE,
                       tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL,
                       compatibility=compatibility) {
  if(is.null(dataFrame)) stop(paste0(functionName, "():  dataFrame argument must not be NULL."), call. = FALSE)
  if(!is.data.frame(dataFrame)) stop(paste0(functionName, "():  dataFrame argument must be a data frame."), call. = FALSE)
  if((!is.null(rows))&&(!anyNA(rows))) {
    if(!is.character(rows)) stop(paste0(functionName, "():  rows must be a character vector."), call. = FALSE)
  }
  if((!is.null(columns))&&(!anyNA(columns))) {
    if(!is.character(columns)) stop(paste0(functionName, "():  columns must be a character vector."), call. = FALSE)
  }
  if((length(rows[rows=="="])+length(columns[columns=="="]))>1) {
    stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
  }
  totalNames <- NULL
  totalCaptions <- NULL
  if((totalsSpecified==TRUE)&&(!is.null(totals))&&(length(totals)>0)) {
    if(is.character(totals)) {
      totalNames <- totals
    }
    else if(is.list(totals)) {
      for(i in 1:length(totals)) {
        if(!is.character(totals[[i]])) {
          stop(paste0(functionName, "():  elements of the totals list must be character values."), call. = FALSE)
        }
      }
      totalNames <- names(totals)
      totalCaptions <- totals
    }
    else {
      stop(paste0(functionName, "():  totals must be a character vector."), call. = FALSE)
    }
  }
  pt <- PivotTable$new(argumentCheckMode=argumentCheckMode, theme=theme, replaceExistingStyles=replaceExistingStyles,
                       tableStyle=tableStyle, headingStyle=headingStyle, cellStyle=cellStyle, totalStyle=totalStyle,
                       compatibility=compatibility)
  pt$addData(dataFrame, dataName=dataName)
  bCalculationsAdded <- FALSE
  if((!is.null(rows))&&(!anyNA(rows))) {
    for(i in 1:length(rows)) {
      if(rows[i]=="=") {
        if(bCalculationsAdded==TRUE) stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
        addCalculations(pt, calculations, format=format, formats=formats)
        pt$addRowCalculationGroups()
        bCalculationsAdded <- TRUE
      }
      else {
        includeTotal <- FALSE
        totalCaption <- NULL
        if(totalsSpecified==FALSE) includeTotal <- TRUE
        else if(rows[i] %in% totalNames) {
          includeTotal <- TRUE
          totalCaption <- totalCaptions[[rows[i]]]
        }
        if(is.null(totalCaption)) totalCaption <- "Total"
        pt$addRowDataGroups(rows[i], addTotal=includeTotal, totalCaption=totalCaption)
      }
    }
  }
  if((!is.null(columns))&&(!anyNA(columns))) {
    for(i in 1:length(columns)) {
      if(columns[i]=="=") {
        if(bCalculationsAdded==TRUE) stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
        addCalculations(pt, calculations, format=format, formats=formats)
        pt$addColumnCalculationGroups()
        bCalculationsAdded <- TRUE
      }
      else {
        includeTotal <- FALSE
        totalCaption <- NULL
        if(totalsSpecified==FALSE) includeTotal <- TRUE
        else if(columns[i] %in% totalNames) {
          includeTotal <- TRUE
          totalCaption <- totalCaptions[[columns[i]]]
        }
        if(is.null(totalCaption)) totalCaption <- "Total"
        pt$addColumnDataGroups(columns[i], addTotal=includeTotal, totalCaption=totalCaption)
      }
    }
  }
  if(bCalculationsAdded==FALSE) {
    addCalculations(pt, calculations, format=format, formats=formats)
    pt$addColumnCalculationGroups()
  }
  return(pt)
}

