
#' R6 class the represents a Calculation Batch
#'
#' @description
#' The `PivotBatch` class represents one combination of data, variables and
#' calculations that are needed when calculating the values of cells in a pivot table.
#'
#' @details
#' The combination of data name and variable names defines a batch.
#' When the batch is calculated, the calculations specified in the batch
#' are evaluated against the specified data, with the data being grouped by the
#' variables specified in the batch.  Individual result values can then be retrieved
#' from the batch.  See the "Performance" vignette for details.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotBatch <- R6::R6Class("PivotBatch",
  public = list(

    #' @description
    #' Create a new `PivotBatch` object.
    #' @param parentPivot The pivot table that this `PivotBatch`
    #' instance belongs to.
    #' @param batchId The unique identifier for the batch.
    #' @param dataName The name of the data frame (as specified in
    #' `pt$addData()`) that this batch relates to.
    #' @param variableNames Specifies the combination of variable names
    #' (i.e. dimensionality) of the batch.
    #' @param values A list specifying the distinct list of values for each
    #' variable, i.e. `list(varName1=values1, varName2=values2, ...)`.
    #' `values` is not currently used and does not affect the batch
    #' compatibility logic.
    #' @param calculationName The first calculation added to this batch.
    #' Does not affect the batch compatibility logic.
    #' @param calculationGroupName The calculation group of the first
    #' calculation added to this batch.  Does not affect the batch
    #' compatibility logic.
    #' @return A new `PivotBatch` object.
    initialize = function(parentPivot=NULL, batchId=0, dataName=NULL,
                          variableNames=NULL, values=NULL,
                          calculationName=NULL, calculationGroupName=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", batchId, missing(batchId), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatch", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$new", "Creating new Pivot Batch...")
      private$p_batchId <- batchId
      private$p_batchName <- paste0("batch", sprintf("%06d", batchId))
      private$p_dataName <- dataName
      private$p_variableNames <- variableNames
      private$p_values <- values
      private$p_nextCalcId <- 1
      private$p_calculations <- list(c(calculationGroupName=calculationGroupName, calculationName=calculationName,
                                       calcInternalName=paste0("calc", sprintf("%06d", private$p_nextCalcId))))
      private$p_nextCalcId <- 2
      private$p_compatibleCount <- 1
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$new", "Created new Pivot Batch.")
    },

    #' @description
    #' Determine whether a combination of data and variables is compatible
    #' with this batch.
    #' @param dataName The name of the data frame (as specified in
    #' `pt$addData()`).
    #' @param variableNames Specifies the combination of variable names
    #' (i.e. dimensionality)..
    #' @return `TRUE` or `FALSE`.
    isCompatible = function(dataName=NULL, variableNames=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "isCompatible", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "isCompatible", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$isCompatible", "Checking batch compatibility...")
      bIsCompatible <- FALSE
      if(private$p_dataName==dataName) {
        if(is.null(private$p_variableNames)&&is.null(variableNames)) {
          bIsCompatible <- TRUE
        }
        else if((!is.null(private$p_variableNames))&&(!is.null(variableNames))) {
          if(length(private$p_variableNames)==length(variableNames)) {
            intrsct <- intersect(private$p_variableNames, variableNames)
            if(length(private$p_variableNames)==length(intrsct)) {
              bIsCompatible <- TRUE
            }
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$isCompatible", "Checked batch compatibility.")
      return(bIsCompatible)
    },

    #' @description
    #' Add a new set of values or a new calculation to the batch.
    #' with this batch.
    #' @param values A list specifying the distinct list of values for each
    #' variable, i.e. `list(varName1=values1, varName2=values2, ...)`.
    #' `values` is not currently used and does not affect the batch
    #' compatibility logic.
    #' @param calculationName The calculation to add to the batch.
    #' Does not affect the batch compatibility logic.
    #' @param calculationGroupName The calculation group of the
    #' calculation to add to the batch.  Does not affect the batch
    #' compatibility logic.
    #' @return No return value.
    addCompatible = function(values=NULL, calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "addCompatible", values, missing(values), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "addCompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "addCompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$addCompatible", "Adding compatibile calculation...")
      if(!is.null(values)) {
        if(length(values)>0) {
          nms <- names(values)
          for(i in 1:length(values)) {
            private$p_values[[nms[i]]] <- union(private$p_values[[nms[i]]], values[[i]])
          }
        }
      }
      bCalcAlreadyExists <- FALSE
      if(!is.null(private$p_calculations)) {
        if(length(private$p_calculations)>0) {
          for(i in 1:length(private$p_calculations)) {
            calc <- private$p_calculations[[i]]
            if(calc["calculationGroupName"]==calculationGroupName) {
              if(calc["calculationName"]==calculationName) {
                bCalcAlreadyExists <- TRUE
                break
              }
            }
          }
        }
      }
      if(!bCalcAlreadyExists) {
        calc <- c(calculationGroupName=calculationGroupName, calculationName=calculationName,
                  calcInternalName=paste0("calc", sprintf("%06d", private$p_nextCalcId)))
        private$p_nextCalcId <- private$p_nextCalcId+1
        private$p_calculations[[length(private$p_calculations)+1]] <- calc
      }
      private$p_compatibleCount <- private$p_compatibleCount+1
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$addCompatible", "Added compatibile calculation.")
    },

    #' @description
    #' Find the internal name of a calculation in the batch.
    #' @param calculationName The name of the calculation to find.
    #' @param calculationGroupName The calculation group of the
    #' calculation to find.
    #' @return The internal name of the calculation in the batch.
    getCalculationInternalName = function(calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "getCalculationInternalName", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "getCalculationInternalName", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$getCalculationInternalName", "Getting calculation internal name...")
      calcInternalName <- NULL
      if(length(private$p_calculations)>0) {
        for(i in 1:length(private$p_calculations)) {
          calc <- private$p_calculations[[i]]
          if(calc["calculationGroupName"]==calculationGroupName) {
            if(calc["calculationName"]==calculationName) {
              calcInternalName <- calc["calcInternalName"]
              break
            }
          }
        }
      }
      if(is.null(calcInternalName))
        stop(paste0("PivotTable$getCalculationInternalName:  Unable to find a calculation named ",
                    calculationGroupName, ":", calculationName, " in this batch."), call. = FALSE)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$getCalculationInternalName", "Got calculation internal name.")
      return(calcInternalName)
    },

    #' @description
    #' Carry out grouping and calculations to evaluate the batch.
    #' @return No return value.
    evaluateBatch = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$evaluateBatch", "Executing batch...")
      # get the data frame
      data <- private$p_parentPivot$data$getData(private$p_dataName)
      # dplyr calculation
      if(private$p_parentPivot$processingLibrary=="dplyr") {
        # group by
        if(!is.null(private$p_variableNames)) {
          if(length(private$p_variableNames)>0) {
            groupByVars <- paste(processIdentifiers(private$p_variableNames), sep="", collapse=", ")
            groupByCmd <- paste0("data <- dplyr::group_by(data, ", groupByVars, ")")
            eval(parse(text=groupByCmd))
          }
        }
        # calculations
        if(is.null(private$p_calculations))
          stop(paste0("PivotBatch$evaluateBatch(): Batch encountered with no calculations."), call. = FALSE)
        if(length(private$p_calculations)==0)
          stop(paste0("PivotBatch$evaluateBatch(): Batch encountered with no calculations."), call. = FALSE)
        calcStr <- ""
        for(i in 1:length(private$p_calculations)) {
          calcNms <- private$p_calculations[[i]]
          calcInternalName <- calcNms["calcInternalName"]
          calcGrp <- private$p_parentPivot$calculationGroups$getCalculationGroup(calcNms["calculationGroupName"])
          calc <- calcGrp$getCalculation(calcNms["calculationName"])
          if(nchar(calcStr)>0) calcStr <- paste0(calcStr, ", ", calcInternalName, " = ", calc$summariseExpression)
          else calcStr <- paste0(calcInternalName, " = ", calc$summariseExpression)
        }
        summaryCmd <- paste0("data <- dplyr::summarise(data, ", calcStr, ")")
        eval(parse(text=summaryCmd))
        data <- dplyr::collect(data)
      }
      # data.table calculation
      else if(private$p_parentPivot$processingLibrary=="data.table") {
        # check is a data table
        if(private$p_parentPivot$argumentCheckMode == 4) {
          if(!data.table::is.data.table(data))
            stop(paste0("PivotBatch$evaluateBatch(): A data.table was expected but the following was encountered: ",
                        paste(class(data), sep="", collapse=", ")), call. = FALSE)
        }
        # group by
        groupByVars <- NULL
        if(!is.null(private$p_variableNames)) {
          if(length(private$p_variableNames)>0) {
            groupByVars <- paste0(", by=.(", paste(processIdentifiers(private$p_variableNames), sep="", collapse=", "), ")")
          }
        }
        # calculations
        if(is.null(private$p_calculations))
          stop(paste0("PivotBatch$evaluateBatch(): Batch encountered with no calculations."), call. = FALSE)
        if(length(private$p_calculations)==0)
          stop(paste0("PivotBatch$evaluateBatch(): Batch encountered with no calculations."), call. = FALSE)
        calcStr <- ""
        for(i in 1:length(private$p_calculations)) {
          calcNms <- private$p_calculations[[i]]
          calcInternalName <- calcNms["calcInternalName"]
          calcGrp <- private$p_parentPivot$calculationGroups$getCalculationGroup(calcNms["calculationGroupName"])
          calc <- calcGrp$getCalculation(calcNms["calculationName"])
          if(nchar(calcStr)>0) calcStr <- paste0(calcStr, ", ", calcInternalName, " = ", calc$summariseExpression)
          else calcStr <- paste0(calcInternalName, " = ", calc$summariseExpression)
        }
        calcStr <- paste0(".(", calcStr, ")")
        # data.table query
        dtqry <- paste0("data <- data[, ", calcStr, groupByVars, "]")
        eval(parse(text=dtqry))
      }
      else stop(paste0("PivotBatch$evaluateBatch(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
      # return the results
      private$p_evaluated <- TRUE
      private$p_results <- data
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$evaluateBatch", "Executed batch.")
      return(invisible())
    },

    #' @description
    #' Retrieve one calculation value from the batch, typically for the value
    #'  of one cell in a pivot table.
    #' @param filters A `PivotFilters` instance that specifies which value to
    #'  retrieve.  This filters object is a combination of the row, column
    #'  and calculation filters.
    #' @param calculationName The name of the calculation value to retrieve.
    #' @param calculationGroupName The calculation group of the
    #' calculation to retrieve.
    #' @return A single calculation value.
    getSummaryValueFromBatch = function(filters=NULL, calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "getSummaryValueFromBatch", filters, missing(filters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "getSummaryValueFromBatch", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatch", "getSummaryValueFromBatch", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$getSummaryValueFromBatch", "Getting value from batch...")
      # check evaluated
      if(!private$p_evaluated)
        stop("PivotTable$getSummaryValueFromBatch:  Attempt to get a value from a batch that has not been evaluated.", call. = FALSE)
      # return value
      value <- NULL
      # get the internal name of this calculation
      calculationInternalName <- self$getCalculationInternalName(calculationName=calculationName,
                                                                 calculationGroupName=calculationGroupName)
      # filters matching ALL?
      bIsALL <- FALSE
      if(is.null(filters)) bIsALL <- TRUE
      else bIsALL <- filters$isALL
      # get the value
      if(is.null(private$p_results)) {
        # no value
      }
      else if(bIsALL==TRUE) {
        if(nrow(private$p_results)==0) {
          # no value
        }
        else if(nrow(private$p_results)==1) {
          value <- private$p_results[[calculationInternalName]][1]
        }
        else {
          stop(paste0("PivotTable$getSummaryValueFromBatch:  An 'ALL' Filters object has matched ",
                      nrow(private$p_results), " row(s).  Should have matched at most one row."), call. = FALSE)
        }
      }
      else if(filters$isNONE==FALSE) {
        if(nrow(private$p_results)==0) {
          # no value
        }
        else {
          # summary is more than likely a small data frame, so use base filtering
          row <- filters$getFilteredDataFrame(private$p_results)
          if(is.null(row)) {
            # no value
          }
          else if(nrow(row)==0) {
            # no value
          }
          else if(nrow(row)==1) {
            value <- row[[calculationInternalName]][1]
          }
          else {
            stop(paste0("PivotTable$getSummaryValueFromBatch:  A 'VALUE' Filters object has matched ",
                        nrow(private$p_results), " row(s).  Should have matched at most one row."), call. = FALSE)
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$getSummaryValueFromBatch", "Got value from batch.")
      return(invisible(value))
    }
  ),
  active = list(
    #' @field batchId The unique identifier for the batch.
    batchId = function(value) { return(invisible(private$p_batchId)) },

    #' @field batchName The unique name of the batch.
    batchName = function(value) { return(invisible(private$p_batchName)) },

    #' @field compatibleCount The number of pivot cell calculations that this batch supports.
    compatibleCount = function(value) { return(invisible(private$p_compatibleCount)) },

    #' @field evaluated TRUE if this batch has been evaluated.
    evaluated = function(value) { return(invisible(private$p_evaluated)) },

    #' @field results The results (a data frame) of the evaluation of the batch
    results = function(value) { return(invisible(private$p_results)) },

    #' @field asString A text description of the batch.
    asString = function(value) {
      vstr <- ""
      if(length(private$p_variableNames)==0) {
        vstr <- "0 VARS: []"
      }
      else {
        if(length(private$p_variableNames)==1) {
          vstr <- paste0("1 VAR: [", private$p_variableNames, "]")
        }
        else {
          vstr <- paste0(length(private$p_variableNames), " VARS: [",
                         paste(private$p_variableNames, sep="", collapse=", "), "]")
        }
      }
      cstr <- ""
      calcCount <- 0
      if(length(private$p_calculations)>0) {
        for(i in 1:length(private$p_calculations)) {
          calc <- private$p_calculations[[i]]
          calcCount <- calcCount + 1
          if(nchar(cstr)>0) cstr <- paste0(cstr, ", ", calc["calculationGroupName"], ":", calc["calculationName"])
          else cstr <- paste0(calc["calculationGroupName"], ":", calc["calculationName"])
        }
      }
      bstr <- paste0("BATCH ", private$p_batchId, ": DATA: ", private$p_dataName, ", ",
                     vstr, ", ",
                     ifelse(calcCount==1, "1 CALC: [", paste0(calcCount, " CALCS: [")), cstr, "] ",
                     " => ", private$p_compatibleCount, " CELL CALC", ifelse(private$p_compatibleCount==1, "", "S"))
      if(private$p_evaluated) {
        if(is.null(private$p_results)) bstr <- paste0(bstr, ", RESULTS: (none)")
        else bstr <- paste0(bstr, ", RESULTS: ", nrow(private$p_results), " row(s) x ", ncol(private$p_results), " col(s), ",
                            "COL NAMES: ", paste(colnames(private$p_results), sep="", collapse=", "))
      }
      else bstr <- paste0(bstr, ", RESULTS: (not evaluated)")
      return(invisible(bstr))
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_batchId = 0,
    p_batchName = NULL,
    p_dataName = NULL,            # the name of the data frame
    p_variableNames = NULL,       # a character vector specifying the grain of the calculation
    p_values = NULL,              # a list, where the element names are variable names, and the elements are lists of values
    p_calculations = NULL,        # a list, where each list element is a three element character vector (vector element names are: calculationGroupName, calculationName, calcInternalName)
    p_nextCalcId = 0,
    p_compatibleCount = 0,
    p_evaluated = FALSE,
    p_results = NULL
  )
)
