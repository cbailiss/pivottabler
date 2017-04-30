#' A class that represents a batch calculation.
#'
#' The PivotBatch class represents one combination of variables and calculations
#' that are needed when calculating the values of cells in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   to (do xyz).
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new Pivot Batch.}
#'
#'   \item{\code{exampleMethod(df, dataName)}}{Example to be removed later.}
#' }

PivotBatch <- R6::R6Class("PivotBatch",
  public = list(
    initialize = function(parentPivot=NULL) {
      checkArgument("PivotBatch", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotBatch$new", "Creating new Pivot Batch...")
      private$p_variableNames <- list()
      private$p_calculationNames <- list()
      private$p_parentPivot$message("PivotBatch$new", "Created new Pivot Batch.")
    },
    exampleMethod = function() {

    }
  ),
  active = list(
    compatibleCount = function(value) { return(invisible(private$p_compatibleCount)) },
    incompatibleCount = function(value) { return(invisible(private$p_incompatibleCount)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_variableNames = NULL,       # a character vector specifying the grain of the calculation
    p_calculations = NULL,        # a list of PivotCalculation objects
    p_compatibleCount = 0,        # a count of the number of data groups/cells that are compatible with batch evaluation
    p_incompatibleCount = 0       # a count of the number of data groups/cells that are incompatible with batch evaluation
  )
)
