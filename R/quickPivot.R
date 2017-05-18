
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
#' @return A pivot table.
#' @examples
#' qpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qpvt(bhmtrains, c("=", "TOC"), c("TrainCategory", "PowerType"),
#'      c("Number of Trains"="n()",
#'        "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"))

qpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL) {
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qpvt", dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations)
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
#' @return A HTML widget.
#' @examples
#' qhpvt(bhmtrains, "TOC", "TrainCategory", "n()")

qhpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL) {
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qhpvt", dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations)
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
#' @param ... Additional arguments, currently caption and/or label.  See the
#'   Latex output vignette for details.
#' @return Latex.
#' @examples
#' qlpvt(bhmtrains, "TOC", "TrainCategory", "n()")
#' qlpvt(bhmtrains, "TOC", "TrainCategory", "n()",
#'       caption="my caption", label="mylabel")

qlpvt <- function(dataFrame, rows=NULL, columns=NULL, calculations=NULL, ...) {
  arguments <- list(...)
  dataName <- deparse(substitute(dataFrame))
  pt <- buildPivot(functionName="qlpvt", dataFrame=dataFrame, dataName=dataName,
                   rows=rows, columns=columns, calculations=calculations)
  return(pt$getLatex(caption=arguments$caption, label=arguments$label))
}


# internal functions for quickly building a pivot table

addCalculations <- function(pt, calculations) {
  nms <- names(calculations)
  for(i in 1:length(calculations)) {
    calc <- calculations[i]
    nme <- nms[i]
    if(is.null(nme)) nme <- paste0("calc", sprintf("%06d", i))
    # quick-pivot functions will allow spaces to be specified in the calculation name
    # as these are often desired in the captions, but need to remove them from the calculation name
    # when adding the calculation to the pivot table
    pt$defineCalculation(calculationName=make.names(nme), caption=nme, summariseExpression=calc)
  }
}

buildPivot <- function(functionName=NULL, dataFrame=NULL, dataName=NULL, rows=NULL, columns=NULL, calculations=NULL) {
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
  pt <- PivotTable$new()
  pt$addData(dataFrame, dataName=dataName)
  bCalculationsAdded <- FALSE
  if((!is.null(rows))&&(!is.na(rows))) {
    for(i in 1:length(rows)) {
      if(rows[i]=="=") {
        if(bCalculationsAdded==TRUE) stop(paste0(functionName, "():  Calculations cannot be added more than once."), call. = FALSE)
        addCalculations(pt, calculations)
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
        addCalculations(pt, calculations)
        pt$addColumnCalculationGroups()
        bCalculationsAdded <- TRUE
      }
      else {
        pt$addColumnDataGroups(columns[i])
      }
    }
  }
  if(bCalculationsAdded==FALSE) {
    addCalculations(pt, calculations)
    pt$addColumnCalculationGroups()
  }
  return(pt)
}

