
#' R6 class that calculates the values for multiple cells in batches.
#'
#' @description
#' The `PivotBatchCalculator` class calculates the values for multiple cells in
#' the pivot table in one evaluation step (per batch), instead of evaluating
#' every calculation once per pivot table cell.
#'
#' @details
#' Evaluating a set of filters and calculations repetitively for each cell is
#' inefficient and slow.  The Batch Calculator executes a much small number of
#' calculations which greatly reduces the CPU time and elapsed time required.
#' See the "Performance" vignette for details.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotBatchCalculator <- R6::R6Class("PivotBatchCalculator",
  public = list(

    #' @description
    #' Create a new `PivotBatchCalculator` object.
    #' @param parentPivot The pivot table that this `PivotBatchCalculator`
    #' instance belongs to.
    #' @return A new `PivotBatchCalculator` object.
    initialize = function(parentPivot=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$new", "Creating new Pivot Batch Calculator...")
      private$p_batches <- list()
      private$p_statistics <- PivotBatchStatistics$new(parentPivot)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$new", "Created new Pivot Batch Calculator.")
    },

    #' @description
    #' Reset the batch calculator, clearing all existing batches.
    #' @return No return value.
    reset = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$reset", "Resetting batches...")
      private$p_batches <- list()
      private$p_nextBatchId <- 1
      private$p_statistics$reset()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$reset", "Reset batches.")
    },

    #' @description
    #' Run some additional checks to see whether the working data is valid.
    #' Typically only used in development builds of the package.
    #' @param workingData The working data to check.
    #' @return No return value.
    checkValidWorkingData = function(workingData=NULL) {
      if(private$p_parentPivot$argumentCheckMode != 4) return()
      checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "checkValidWorkingData", workingData, missing(workingData), allowMissing=FALSE, allowNull=TRUE, allowedClasses="list")
      if(is.null(workingData)) return()
      workingFilters <- lapply(workingData, function(x) { return(x$workingFilters) })
      checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "checkValidWorkingData", workingFilters, missing(workingData), allowMissing=FALSE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotFilters")
      batchNames <- lapply(workingData, function(x) { return(x$batchName) })
      batchNames <- unlist(batchNames[!sapply(batchNames, is.null)])
      checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "checkValidWorkingData", batchNames, missing(workingData), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
    },

    #' @description
    #' Examines a set of filters to see whether they are compatible with batch
    #' evaluation mode. Only filters that specify zero or one value for each
    #' variable are compatible with batch evaluation.
    #' @details It is not practical to make batch evaluation work where a filter
    #' matches more than one value for a variable.  One approach might be to add
    #' a derived column where a single value represents the multiple values, however
    #' the combination of values could partially overlap with combinations of values
    #' in other data groups.  Also the value that represents the "combined"
    #' value could collide with other existing values in the column.
    #' In summary:  Sequential mode is slower and more flexible.  Batch is faster
    #' but stricter.  Batch mode works for regular pivot tables (i.e. most cases).
    #' @param filters A `PivotFilters` object that represents a set of filters
    #' to examine.
    #' @return `TRUE` if the filters are batch compatible, `FALSE` otherwise.
    isFiltersBatchCompatible = function(filters=NULL) {
      # only filters that specify zero or one value for each variable are compatible with batch evaluation
      # (A filter that matches more than one value (a) would need a derived column calculating, (b) the specified values
      # could partially overlap with other cells and (c) the value that represents the "combined" value could collide with
      # existing values in the column).  Bottom line:  Sequential mode is slower and more flexible.  Batch is faster but stricter.
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "isFiltersBatchCompatible", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$isFiltersBatchCompatible", "Checking if filter is batch compatible...")
      isCompatible <- TRUE
      for (i in 1:length(filters$filters)) {
        filter <- filters$filters[[i]]
        if(filter$type=="ALL") next
        if(is.null(filter$values)) next
        if(length(filter$values) > 1) {
          isCompatible <- FALSE
          break
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$isFiltersBatchCompatible", "Checked if filter is batch compatible.")
      return(isCompatible)
    },

    #' @description
    #' Generates a new batch or finds a relevant existing batch for
    #' a named calculation and single working filters object.
    #' @param dataName The name of the data frame (as specified in
    #' `pt$addData()`).
    #' @param calculationName The name of the calculation.
    #' @param calculationGroupName The calculation group of the
    #' calculation.
    #' @param workingFilters A `PivotFilters` object that represents the
    #' working filters to generate the batch for.
    #' @return The name of either the batch that was created or the
    #' relevant existing batch.
    generateBatchesForNamedCalculationEvaluation1 = function(dataName=NULL, calculationName=NULL,
                                                            calculationGroupName=NULL, workingFilters=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", workingFilters, missing(workingFilters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generating batches for named calculation evaluation...")
      # batch generation depends on the filter type
      bIsBatchCompatible <- FALSE
      if(is.null(workingFilters)) {
        # no filters means ALL, i.e. so always compatible
        private$p_statistics$incrementCompatible(calculationName=calculationName, calculationGroupName=calculationGroupName)
        bIsBatchCompatible <- TRUE
      }
      else if(workingFilters$isALL) {
        # ALL filters have no criteria, so are always compatible
        private$p_statistics$incrementCompatible(calculationName=calculationName, calculationGroupName=calculationGroupName)
        bIsBatchCompatible <- TRUE
      }
      else if(workingFilters$isNONE) {
        # NONE filters result in no data
        private$p_statistics$incrementNoData(calculationName=calculationName, calculationGroupName=calculationGroupName)
      }
      else {
        if(self$isFiltersBatchCompatible(workingFilters)) {
          private$p_statistics$incrementCompatible(calculationName=calculationName, calculationGroupName=calculationGroupName)
          bIsBatchCompatible <- TRUE
        }
        else {
          private$p_statistics$incrementIncompatible(calculationName=calculationName, calculationGroupName=calculationGroupName)
        }
      }
      # finish here if not batch compatible
      if(!bIsBatchCompatible) {
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generated 0 batches for named calculation evaluation.")
        return(invisible())
      }
      # add to the batches:  first get the distinct list of variables and variable values
      variableNames <- workingFilters$filteredVariables
      values <- workingFilters$filteredValues
      # find a matching batch
      bMatched <- FALSE
      batchName <- NULL
      if(length(private$p_batches)>0) {
        for(i in 1:length(private$p_batches)) {
          batch <- private$p_batches[[i]]
          if(batch$isCompatible(dataName=dataName, variableNames=variableNames)) {
            batch$addCompatible(values=values, calculationName=calculationName, calculationGroupName=calculationGroupName)
            bMatched <- TRUE
            batchName <- batch$batchName
            break
          }
        }
      }
      if(!bMatched) {
        batchId <- private$p_nextBatchId
        private$p_nextBatchId <- private$p_nextBatchId + 1
        batch <- PivotBatch$new(parentPivot=private$p_parentPivot, batchId=batchId, dataName=dataName,
                                variableNames=variableNames, values=values, # values(varName1=values1, varName2=values2, ...)
                                calculationName=calculationName, calculationGroupName=calculationGroupName)
        private$p_batches[[batch$batchName]] <- batch
        batchName <- batch$batchName
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generated batches for named calculation evaluation.")
      return(invisible(batchName))
    },

    #' @description
    #' Generates one or more batches for the named calculations and set of
    #' working working data associated with a cell.
    #' @details
    #' A wrapper around `generateBatchesForNamedCalculationEvaluation1()`, which
    #' invokes this function as appropriate, depending on whether a calculation
    #' if either of type "summary" or type "calculation".
    #' @param calculationName The name of the calculation.
    #' @param calculationGroupName The calculation group of the
    #' calculation.
    #' @param workingData A list containing filter and/or filter overrides.
    #' @return One or more batch names of either the batches that were created or
    #' the relevant existing batches.
    generateBatchesForNamedCalculationEvaluation2 = function(calculationName=NULL, calculationGroupName=NULL, workingData=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", workingData, missing(workingData), allowMissing=FALSE, allowNull=TRUE, allowedClasses="list")
        if(private$p_parentPivot$argumentCheckMode==4) self$checkValidWorkingData(workingData=workingData)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation2", "Generating batches for named calculation evaluation...")
      # get the calculation
      calcGrp <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
      calc <- calcGrp$getCalculation(calculationName)
      # return value
      batchNames <- list()
      # call the inner function as appropriate
      if(calc$type=="summary") {
        filters <- workingData[[calculationName]]$workingFilters
        batchNames[[calculationName]] <- self$generateBatchesForNamedCalculationEvaluation1(dataName=calc$dataName, calculationName=calculationName,
                                                               calculationGroupName=calculationGroupName, workingFilters=filters)
      }
      else if(calc$type=="calculation") {
        if(length(calc$basedOn)>0) {
          for(i in 1:length(calc$basedOn)) {
            baseCalc <- calcGrp$getCalculation(calc$basedOn[i])
            if(baseCalc$type != "summary") next
            filters <- workingData[[baseCalc$calculationName]]$workingFilters
            batchNames[[calculationName]] <- self$generateBatchesForNamedCalculationEvaluation1(dataName=baseCalc$dataName, calculationName=baseCalc$calculationName,
                                                               calculationGroupName=calculationGroupName, workingFilters=filters)
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation2", "Generated batches for named calculation evaluation.")
      return(invisible(batchNames))
    },

    #' @description
    #' Generates the batches for batch evaluation mode.
    #' @return One or more batch names of either the batches that were created or
    #' the relevant existing batches.
    generateBatchesForCellEvaluation = function() {
      # generates batches for:
      # 1) type=summary and visible=TRUE
      # 2) type=calculation where specified in basedOn
      if(private$p_parentPivot$evaluationMode=="sequential") {
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$generateBatchesForCellEvaluation", "Pivot table is using sequential evaluation mode, so not creating batches.")
        return(invisible())
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$new", "Generating batches for cell evaluation...")
      if(is.null(private$p_parentPivot$cells)) stop("PivotBatchCalculator$generateBatchesForCellEvaluation():  No cells exist to process.", call. = FALSE)
      # reset
      self$reset()
      # statistics for use when generating batches
      private$p_statistics$reset()
      # track the names
      allNames <- vector("character", 0)
      # iterate the cells
      rowCount <- private$p_parentPivot$cells$rowCount
      columnCount <- private$p_parentPivot$cells$columnCount
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          # for each cell
          cell <- private$p_parentPivot$cells$getCell(r, c)
          # ignore empty cells
          if(cell$isEmpty) next
          # check calculation present
          if(is.null(cell$calculationName)) next
          if(is.null(cell$calculationGroupName)) next
          # examine the calculation and filters, generate a new batch or add to an existing batch
          batchNames <- self$generateBatchesForNamedCalculationEvaluation2(calculationName=cell$calculationName,
                                                             calculationGroupName=cell$calculationGroupName, workingData=cell$workingData)
          # add the batch names to the working data
          if(!is.null(batchNames)) {
            if(length(batchNames)>0) {
              nms <- names(batchNames)
              for(i in 1:length(batchNames)) {
                cell$workingData[[nms[i]]]$batchName <- batchNames[[i]]
              }
            }
          }

          # capture the names
          allNames <- unique(c(allNames, batchNames))
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$new", "Generated batches for cell evaluation.")
      return(invisible(allNames))
    },

    #' @description
    #' Evaluate each of the batches defined in the batch calculator.
    #' @return The number of batches that were evaluated.
    evaluateBatches = function() {
      if(private$p_parentPivot$evaluationMode=="sequential") {
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$evaluateBatches", "Pivot table is using sequential evaluation mode, so not executing batches.")
        return(invisible())
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$evaluateBatches", "Evaluating batches...")
      batchEvalCount <- 0
      if(!is.null(private$p_batches)) {
        if(length(private$p_batches)>0) {
          for(i in 1:length(private$p_batches)) {
            batch <- private$p_batches[[i]]
            if(batch$compatibleCount<=1) next
            batch$evaluateBatch()
            batchEvalCount <- batchEvalCount+1
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$evaluateBatches", "Evaluated batches.")
      return(invisible(batchEvalCount))
    },

    #' @description
    #' Retrieve one calculation value from one batch, typically for the value
    #'  of one cell in a pivot table.
    #' @param batchName The name of the batch containing the calculation result.
    #' @param calculationName The name of the calculation.
    #' @param calculationGroupName The calculation group of the
    #' calculation.
    #' @param workingFilters A `PivotFilters` object that represents the
    #' working filters to retrieve the value for.
    #' @return A single calculation value.
    getSummaryValueFromBatch = function(batchName=NULL, calculationName=NULL, calculationGroupName=NULL, workingFilters=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "getSummaryValueFromBatch", batchName, missing(batchName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "getSummaryValueFromBatch", workingFilters, missing(workingFilters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "getSummaryValueFromBatch", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchCalculator", "getSummaryValueFromBatch", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$getSummaryValueFromBatch", "Getting value from batch...")
      # ensure we have a batch name
      if(is.null(batchName)) stop("PivotTable$getSummaryValueFromBatch:  No batch name was specified.", call. = FALSE)
      # get the batch
      batch <- private$p_batches[[batchName]]
      if(is.null(batch)) stop("PivotTable$getSummaryValueFromBatch:  Unable to find a batch with the specified name.", call. = FALSE)
      # was the batch evaluated?
      if(batch$evaluated==FALSE) {
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchCalculator$getSummaryValueFromBatch", "Unable to get value from batch as batch not evaluated.")
        return(list(batchEvaluated=FALSE, value=NULL))
      }
      # get the value
      value <- batch$getSummaryValueFromBatch(filters=workingFilters,
                                              calculationName=calculationName, calculationGroupName=calculationGroupName)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatch$getSummaryValueFromBatch", "Got value from batch.")
      return(list(batchEvaluated=TRUE, value=value))
    }
  ),
  active = list(

    #' @field batchCount The number of batches generated for the pivot table.
    batchCount = function(value) { return(invisible(length(private$p_batches))) },

    #' @field calculationSummary A summary of the batch compatibility for each
    #' calculation.
    calculationSummary = function(value) {
      return(private$p_statistics$asString)
    },

    #' @field batchSummary A summary of the batches in the pivot table.
    batchSummary = function(value) {
      str <- ""
      if(!is.null(private$p_batches)) {
        if(length(private$p_batches)>0) {
          for(i in 1:length(private$p_batches)) {
            batch <- private$p_batches[[i]]
            bstr <- batch$asString
            if(nchar(str)>0) str <- paste0(str, "\n", bstr)
            else str <- bstr
          }
        }
      }
      return(str)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_nextBatchId = NULL,
    p_batches = NULL,
    p_statistics = NULL
  )
)
