# template

#' A class that defines a calculation.
#'
#' The PivotCalculation class defines one calculation in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @keywords calculation
#' @return Object of \code{\link{R6Class}} with properties and methods that define a single pivot table calculation.
#' @format \code{\link{R6Class}} object.
#' @examples
#' This class should only be created by the pivot table.
#' It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field calculationName Calculation unique name.  Recommendation:  Do not have spaces in this name.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot calculation, specifying the field values documented above.}
#'
#'   \item{\code{asList()}}{Get a list representation of this calculation.}
#' }

ClassName <- R6::R6Class("ClassName",
  public = list(
   initialize = function(parentPivot, data) {
     checkArgument("ClassName", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("ClassName$new", "Creating new ClassName...", list())
     private$p_data <- data
     private$p_parentPivot$message("ClassName$new", "Created new ClassName.")
   },
   getCopy = function() {
     copy <- list()
     return(copy)
   },
   asList = function() {
     lst <- list(
       data = private$p_data
     )
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_data = NULL
  )
)
