
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
#' @param ... Additional arguments, currently format, formats and/or
#'   argumentCheckMode.
#' @return A pivot table.
#' @examples
#' qpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qpvt(bhmtrains, c("=", "TOC"), c("TrainCategory", "PowerType"),
#'      c("Number of Trains"="n()",
#'        "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"))

qpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qpvt", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
  checkArgument(3, TRUE, "", "qpvt", rows, missing(rows), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qpvt", columns, missing(columns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qpvt", calculations, missing(calculations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]]) # can't use $format as this also matches formats
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
#' @param ... Additional arguments, currently format, formats and/or
#'   argumentCheckMode.
#' @return A HTML widget.
#' @examples
#' qhpvt(bhmtrains, "TOC", "TrainCategory", "n()")

qhpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qhpvt", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
  checkArgument(3, TRUE, "", "qhpvt", rows, missing(rows), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", columns, missing(columns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhpvt", calculations, missing(calculations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qhpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]]) # can't use $format as this also matches formats
  w <- pt$renderPivot()
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
#' @param ... Additional arguments, currently format, formats,
#'   argumentCheckMode, caption and/or label.  See the Latex output vignette for
#'   a description of caption and label.
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
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qlpvt", argumentCheckMode=argumentCheckMode,
                   dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations,
                   format=arguments[["format"]], formats=arguments[["formats"]]) # can't use $format as this also matches formats
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
                       format=NULL, formats=NULL) {
  if(is.null(dataFrame)) stop(paste0(functionName, "():  dataFrame argument must not be NULL."), call. = FALSE)
  if(!is.data.frame(dataFrame)) stop(paste0(functionName, "():  dataFrame argument must be a data frame."), call. = FALSE)
  if((!is.null(rows))&&(!is.na(rows))) {
    if(!is.character(rows)) stop(paste0(functionName, "():  rows must be a character vector."), call. = FALSE)
  }
  if((!is.null(columns))&&(!is.na(columns))) {
    if(!is.character(columns)) stop(paste0(functionName, "():  columns must be a character vector."), call. = FALSE)
  }
  if((length(rows[rows=="="])+length(columns[columns=="="]))>1) {
    stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
  }
  pt <- PivotTable$new(argumentCheckMode=argumentCheckMode)
  pt$addData(dataFrame, dataName=dataName)
  bCalculationsAdded <- FALSE
  if((!is.null(rows))&&(!is.na(rows))) {
    for(i in 1:length(rows)) {
      if(rows[i]=="=") {
        if(bCalculationsAdded==TRUE) stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
        addCalculations(pt, calculations, format=format, formats=formats)
        pt$addRowCalculationGroups()
        bCalculationsAdded <- TRUE
      }
      else {
        pt$addRowDataGroups(rows[i])
      }
    }
  }
  if((!is.null(columns))&&(!is.na(columns))) {
    for(i in 1:length(columns)) {
      if(columns[i]=="=") {
        if(bCalculationsAdded==TRUE) stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
        addCalculations(pt, calculations, format=format, formats=formats)
        pt$addColumnCalculationGroups()
        bCalculationsAdded <- TRUE
      }
      else {
        pt$addColumnDataGroups(columns[i])
      }
    }
  }
  if(bCalculationsAdded==FALSE) {
    addCalculations(pt, calculations, format=format, formats=formats)
    pt$addColumnCalculationGroups()
  }
  return(pt)
}

