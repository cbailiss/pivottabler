#' A class that contains multiple calculation groups.
#'
#' The PivotCalculationGroups class stores all of the calculation groups for a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @keywords calculation
#' @return Object of \code{\link{R6Class}}.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new container for pivot table calculation groups, specifying the field values documented above.}
#'
#'   \item{\code{isExistingCalculationGroup(calculationGroupName)}}{Check if a calculation group exists with the specified name.}
#'   \item{\code{getCalculationGroup(calculationGroupName)}}{Get the calculation group with the specified name.}
#'   \item{\code{addCalculationGroup(calculationGroupName)}}{Create a new calculation group with the specified name.}
#'   \item{\code{asList()}}{Get a list representation of these calculation groups.}
#'   \item{\code{asJSON()}}{Get a JSON representation of these calculation groups.}
#'   \item{\code{asString()}}{Get a text representation of these calculation groups.}
#' }

PivotCalculationGroups <- R6::R6Class("PivotCalculationGroups",
  public = list(
    initialize = function(parentPivot) {
      checkArgument("PivotCalculationGroups", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotCalculationGroups$new", "Creating new Pivot Calculation Groups...")
      private$p_groups <- list()
      private$p_parentPivot$message("PivotCalculationGroups$new", "Created new Pivot Calculation Groups.")
    },
    isExistingCalculationGroup = function(calculationGroupName=NULL) {
      checkArgument("PivotCalculationGroups", "isExistingCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotCalculationGroups$isExistingCalculationGroup", "Checking calculation group exists...",
                                    list(calculationGroupName=calculationGroupName))
      calcGroupExists <- calculationGroupName %in% names(private$p_groups)
      private$p_parentPivot$message("PivotCalculationGroups$isExistingCalculationGroup", "Checked calculation group exists.")
      return(invisible(calcGroupExists))
    },
    getCalculationGroup = function(calculationGroupName=NULL) {
      checkArgument("PivotCalculationGroups", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotCalculationGroups$getCalculationGroup", "Getting calculation group...",
                                    list(calculationGroupName=calculationGroupName))
      calculationGroup <- private$p_groups[[calculationGroupName]]
      if(is.null(calculationGroup)) {
        stop(paste0("PivotCalculationGroups$getCalculationGroup(): No calculationGroup exists with the name '",
                    calculationGroupName, "'"), call. = FALSE)
      }
      private$p_parentPivot$message("PivotCalculationGroups$getCalculationGroup", "Got calculation group.")
      return(invisible(calculationGroup))
    },
    addCalculationGroup = function(calculationGroupName=NULL) {
      checkArgument("PivotCalculationGroups", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotCalculationGroups$addCalculationGroup", "Adding calculation group...",
                                    list(calculationGroupName=calculationGroupName))
      if(calculationGroupName %in% names(private$p_groups)) {
        stop(paste0("PivotCalculationGroups$addCalculationGroup():  A calculation group already exists",
                    " in the Pivot Table with the name '", calculationGroupName, "'.  calculationGroupName must unique."), call. = FALSE)
      }
      calculationGroup <- PivotCalculationGroup$new(private$p_parentPivot, calculationGroupName)
      private$p_groups[[calculationGroupName]] <- calculationGroup
      if(is.null(private$p_defaultGroup)) private$p_defaultGroup <- calculationGroup
      private$p_parentPivot$message("PivotCalculationGroups$addCalculationGroup", "Added calculation group.")
      return(invisible(calculationGroup))
    },
    asList = function() {
      lst <- list()
      if(length(private$p_groups) > 0) {
        groupNames <- names(private$p_groups)
        for (i in 1:length(private$p_groups)) {
          groupName <- groupNames[i]
          lst[[groupName]] = private$p_groups[[groupName]]$asList()
        }
      }
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(seperator=", ") {
       checkArgument("PivotCalculationGroups", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       cstr <- ""
       if(length(private$p_groups)>0) {
         for(i in 1:length(private$p_groups)) {
           cg <- private$p_groups[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           cstr <- paste0(cstr, sep, cg$asString())
         }
       }
       return(cstr)
    }
  ),
  active = list(
    count = function(value) { return(invisible(length(private$p_groups))) },
    groups = function(value) { return(invisible(private$p_groups)) },
    defaultGroup = function(value) { return(invisible(private$p_defaultGroup)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_groups = NULL,
    p_defaultGroup = NULL
  )
)
