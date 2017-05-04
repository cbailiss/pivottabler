#' A class that calculates the values for multiple cells.
#'
#' The PivotBatchCalculator class calculates the values for multiple cells in
#' the pivot table in one evaluation step.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   to perform calculations in batch.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field batchCount The number of batches generated for the pivot table.
#' @field calculationSummary A summary of the batch compatibility for each
#'   calculation.
#' @field batchSummary A summary of the batches in the pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new batch calculator.}
#'
#'   \item{\code{reset()}}{Clears any batches that currently exist in the batch
#'   calculator.}
#'   \item{\code{isFiltersBatchCompatible(filters=NULL)}}{Determines whether a
#'   set of filters are compatible with batch calculations.}
#'   \item{\code{generateBatchesForNamedCalculationEvaluation1(dataName=NULL,
#'   calculationName=NULL, calculationGroupName=NULL,
#'   workingFilters=NULL)}}{Generates one or more batches for a named
#'   calculation and single working filters object.}
#'   \item{\code{generateBatchesForNamedCalculationEvaluation2(calculationName=NULL,
#'   calculationGroupName=NULL, workingFilters=NULL)}}{Generates one or more
#'   batches for a named calculation and set of working filters objects
#'   associated with a cell.}
#'   \item{\code{generateBatchesForCellEvaluation()}}{Generates one or batches
#'   for a pivot table cell.}
#'   \item{\code{evaluateBatches()}}{Evaluates batch calculations.}
#'   \item{\code{getSummaryValueFromBatch(dataName=NULL, calculationName=NULL,
#'   calculationGroupName=NULL, workingFilters=NULL)}}{Retrieve a value from a
#'   batch that has already been evaluated.}
#' }

PivotBatchCalculator <- R6::R6Class("PivotBatchCalculator",
  public = list(
    initialize = function(parentPivot=NULL) {
      checkArgument("PivotBatchCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotBatchCalculator$new", "Creating new Pivot Batch Calculator...")
      private$p_batches <- list()
      private$p_statistics <- PivotBatchStatistics$new(parentPivot)
      private$p_parentPivot$message("PivotBatchCalculator$new", "Created new Pivot Batch Calculator.")
    },
    reset = function() {
      private$p_parentPivot$message("PivotBatchCalculator$reset", "Resetting batches...")
      private$p_batches <- list()
      private$p_nextBatchId <- 1
      private$p_statistics$reset()
      private$p_parentPivot$message("PivotBatchCalculator$reset", "Reset batches.")
    },
    isFiltersBatchCompatible = function(filters=NULL) {
      # only filters that specify zero or one value for each variable are compatible with batch evaluation
      # (A filter that matches more than one value (a) would need a derived column calculating, (b) the specified values
      # could partially overlap with other cells and (c) the value that represents the "combined" value could collide with
      # existing values in the column).  Bottom line:  Sequential mode is slower and more flexible.  Batch is faster but stricter.
      checkArgument("PivotBatchCalculator", "isFiltersBatchCompatible", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
      private$p_parentPivot$message("PivotBatchCalculator$isFiltersBatchCompatible", "Checking if filter is batch compatible...")
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
      private$p_parentPivot$message("PivotBatchCalculator$isFiltersBatchCompatible", "Checked if filter is batch compatible.")
      return(isCompatible)
    },
    generateBatchesForNamedCalculationEvaluation1 = function(dataName=NULL, calculationName=NULL,
                                                            calculationGroupName=NULL, workingFilters=NULL) {
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation1", workingFilters, missing(workingFilters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
      private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generating batches for named calculation evaluation...")
      # very similar code to the below is present in the getSummaryValueFromBatch function below
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
        private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generated 0 batches for named calculation evaluation.")
        return(invisible())
      }
      # add to the batches:  first get the distinct list of variables and variable values
      variableNames <- workingFilters$filteredVariables
      values <- workingFilters$filteredValues
      # find a matching batch
      bMatched <- FALSE
      if(length(private$p_batches)>0) {
        for(i in 1:length(private$p_batches)) {
          batch <- private$p_batches[[i]]
          if(batch$isCompatible(dataName=dataName, variableNames=variableNames)) {
            batch$addCompatible(values=values, calculationName=calculationName, calculationGroupName=calculationGroupName)
            bMatched <- TRUE
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
        private$p_batches[[length(private$p_batches)+1]] <- batch
      }
      private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation1", "Generated batches for named calculation evaluation.")
      return(invisible())
    },
    # this function looks at the different types of calculation (type="calculation" will have basedOn calcs that each need examining)
    generateBatchesForNamedCalculationEvaluation2 = function(calculationName=NULL, calculationGroupName=NULL, workingFilters=NULL) {
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "generateBatchesForNamedCalculationEvaluation2", workingFilters, missing(workingFilters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotFilters")
      private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation2", "Generating batches for named calculation evaluation...")
      # get the calculation
      calcGrp <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
      calc <- calcGrp$getCalculation(calculationName)
      # call the inner function as appropriate
      if(calc$type=="summary") {
        filters <- workingFilters[[calculationName]]
        self$generateBatchesForNamedCalculationEvaluation1(dataName=calc$dataName, calculationName=calculationName,
                                                           calculationGroupName=calculationGroupName, workingFilters=filters)
      }
      else if(calc$type=="calculation") {
        if(length(calc$basedOn)>0) {
          for(i in 1:length(calc$basedOn)) {
            baseCalc <- calcGrp$getCalculation(calc$basedOn[i])
            if(baseCalc$type != "summary") next
            filters <- workingFilters[[baseCalc$calculationName]]
            self$generateBatchesForNamedCalculationEvaluation1(dataName=baseCalc$dataName, calculationName=baseCalc$calculationName,
                                                               calculationGroupName=calculationGroupName, workingFilters=filters)
          }
        }
      }
      private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForNamedCalculationEvaluation2", "Generated batches for named calculation evaluation.")
      return(invisible())
    },
    generateBatchesForCellEvaluation = function() {
      # generates batches for:
      # 1) type=summary and visible=TRUE
      # 2) type=calculation where specified in basedOn
      if(private$p_parentPivot$evaluationMode=="sequential") {
        private$p_parentPivot$message("PivotBatchCalculator$generateBatchesForCellEvaluation", "Pivot table is using sequential evaluation mode, so not creating batches.")
        return(invisible())
      }
      private$p_parentPivot$message("PivotBatchCalculator$new", "Generating batches for cell evaluation...")
      if(is.null(private$p_parentPivot$cells)) stop("PivotBatchCalculator$generateBatchesForCellEvaluation():  No cells exist to process.", call. = FALSE)
      # reset
      self$reset()
      # statistics for use when generating batches
      private$p_statistics$reset()
      # iterate the cells
      rowCount <- private$p_parentPivot$cells$rowCount
      columnCount <- private$p_parentPivot$cells$columnCount
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          # for each cell
          cell <- private$p_parentPivot$cells$getCell(r, c)
          if(is.null(cell$calculationName)) next
          if(is.null(cell$calculationGroupName)) next
          # examine the calculation and filters, generate a new batch or add to an existing batch
          self$generateBatchesForNamedCalculationEvaluation2(calculationName=cell$calculationName,
                                                             calculationGroupName=cell$calculationGroupName,
                                                             workingFilters=cell$workingFilters)
        }
      }
      private$p_parentPivot$message("PivotBatchCalculator$new", "Generated batches for cell evaluation.")
    },
    evaluateBatches = function() {
      if(private$p_parentPivot$evaluationMode=="sequential") {
        private$p_parentPivot$message("PivotBatchCalculator$evaluateBatches", "Pivot table is using sequential evaluation mode, so not executing batches.")
        return(invisible())
      }
      private$p_parentPivot$message("PivotBatchCalculator$evaluateBatches", "Evaluating batches...")
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
      private$p_parentPivot$message("PivotBatchCalculator$evaluateBatches", "Evaluated batches.")
      return(invisible(batchEvalCount))
    },
    getSummaryValueFromBatch = function(dataName=NULL, calculationName=NULL,
                                        calculationGroupName=NULL, workingFilters=NULL) {
      checkArgument("PivotBatchCalculator", "getSummaryValueFromBatch", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "getSummaryValueFromBatch", workingFilters, missing(workingFilters), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
      checkArgument("PivotBatchCalculator", "getSummaryValueFromBatch", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchCalculator", "getSummaryValueFromBatch", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotBatchCalculator$getSummaryValueFromBatch", "Getting value from batch...")
      # very similar code to the below is present in the generateBatchesForNamedCalculationEvaluation1 function above
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
        private$p_parentPivot$message("PivotBatchCalculator$getSummaryValueFromBatch", "Unable to get value from batch as incompatible.")
        return(list(isBatchCompatible=FALSE, batchEvaluated=FALSE, value=NULL))
      }
      # find the matching batch:  first get the distinct list of variables and variable values
      variableNames <- workingFilters$filteredVariables
      values <- workingFilters$filteredValues
      # find the matching batch
      bMatched <- FALSE
      matchingBatch <- NULL
      if(length(private$p_batches)>0) {
        for(i in 1:length(private$p_batches)) {
          batch <- private$p_batches[[i]]
          if(batch$isCompatible(dataName=dataName, variableNames=variableNames)) {
            matchingBatch <- batch
            bMatched <- TRUE
            break
          }
        }
      }
      if(bMatched==FALSE) {
        stop("PivotTable$getSummaryValueFromBatch:  Unable to find a matching batch when is supposedly batch compatible.", call. = FALSE)
      }
      # was the batch evaluated?
      if(matchingBatch$evaluated==FALSE) {
        private$p_parentPivot$message("PivotBatchCalculator$getSummaryValueFromBatch", "Unable to get value from batch as batch not evaluated.")
        return(list(isBatchCompatible=TRUE, batchEvaluated=FALSE, value=NULL))
      }
      # get the value
      value <- batch$getSummaryValueFromBatch(filters=workingFilters,
                                              calculationName=calculationName, calculationGroupName=calculationGroupName)
      private$p_parentPivot$message("PivotBatch$getSummaryValueFromBatch", "Got value from batch.")
      return(list(isBatchCompatible=TRUE, batchEvaluated=TRUE, value=value))
    }
  ),
  active = list(
    batchCount = function(value) { return(invisible(length(private$p_batches))) },
    calculationSummary = function(value) {
      return(private$p_statistics$asString)
    },
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
