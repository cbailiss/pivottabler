#' A class that (does xyz).
#'
#' The (classname) class (does xyz in a couple of sentances).
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
#'   \item{\code{new(...)}}{Create a new xyz.}
#'
#'   \item{\code{exampleMethod(df, dataName)}}{Example to be removed later.}
#' }

(classname) <- R6::R6Class("(classname)",
  public = list(
    initialize = function(parentPivot=NULL) {
      checkArgument("(classname)", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("(classname)$new", "Creating new Pivot XYZ...")
      private$p_data <- list()
      private$p_parentPivot$message("(classname)$new", "Created new Pivot XYZ.")
    },
    exampleMethod = function() {

    }
  ),
  active = list(
    count = function(value) { return(invisible(length(private$p_data))) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_data = NULL
  )
)
