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

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new batch calculator.}
#'
#'   \item{\code{exampleMethod(df, dataName)}}{Example to be removed later.}
#' }

PivotBatchCalculator <- R6::R6Class("PivotBatchCalculator",
  public = list(
    initialize = function(parentPivot=NULL) {
      checkArgument("PivotBatchCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotBatchCalculator$new", "Creating new Pivot Batch Calculator...")
      private$p_batches <- list()
      private$p_parentPivot$message("PivotBatchCalculator$new", "Created new Pivot Batch Calculator.")
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
        if(is.null(filter$values)) next
        if(length(filter$values) > 1) {
          isCompatible <- FALSE
          break
        }
      }
      private$p_parentPivot$message("PivotBatchCalculator$isFiltersBatchCompatible", "Checked if filter is batch compatible.")
      return(isCompatible)
    },
    generateBatchesForNamedCalculationEvaluation = function(calculationName=NULL, calculationGroupName=NULL, rowColFilters=NULL) {

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
      # iterate the cells
      rowCount <- private$p_parentPivot$cells$rowCount
      columnCount <- private$p_parentPivot$cells$columnCount
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_parentPivot$cells$getCell(r, c)
          # get the evaluation filters for this cell
          filters <- self$getEvaluationFilters(...)
         }
      }
      private$p_parentPivot$message("PivotBatchCalculator$new", "Generated batches for cell evaluation.")
    }
  ),
  active = list(
    count = function(value) { return(invisible(length(private$p_data))) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_batches = NULL
  )
)
