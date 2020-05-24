
#' R6 class that contains multiple calculation groups.
#'
#' @description
#' The `PivotCalculationGroups` class stores all of the
#' calculation groups for a pivot table.  Every pivot table
#' has at least one pivot calculation group and this is
#' sufficient for all regular pivot tables.  Additional
#' calculation groups are typically only created for
#' irregular/custom pivot tables.
#' See the "Irregular Layout" vignette for an example.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCalculationGroups <- R6::R6Class("PivotCalculationGroups",
  public = list(

    #' @description
    #' Create a new `PivotCalculationGroups` object.
    #' @param parentPivot The pivot table that this `PivotCalculationGroups`
    #' instance belongs to.
    #' @return A new `PivotCalculationGroups` object.
    initialize = function(parentPivot) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$new", "Creating new Pivot Calculation Groups...")
      private$p_groups <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$new", "Created new Pivot Calculation Groups.")
    },

    #' @description
    #' Check if a calculation group exists with the specified name.
    #' @param calculationGroupName The name of the calculation group.
    #' @return `TRUE` if the calculation group already exists, `FALSE` otherwise.
    isExistingCalculationGroup = function(calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "isExistingCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$isExistingCalculationGroup", "Checking calculation group exists...",
                                    list(calculationGroupName=calculationGroupName))
      calcGroupExists <- calculationGroupName %in% names(private$p_groups)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$isExistingCalculationGroup", "Checked calculation group exists.")
      return(invisible(calcGroupExists))
    },

    #' @description
    #' Retrieve a calculation group by index.
    #' @param index An integer specifying the calculation group to retrieve.
    #' @return The calculation group that exists at the specified index.
    item = function(index) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "item", index, missing(index), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$item", "Getting calculation group...")
      if(index<1) {
        stop(paste0("PivotCalculationGroups$index(): index must be greater than 0."), call. = FALSE)
      }
      if(index>length(private$p_groups)) {
        stop(paste0("PivotCalculationGroups$index(): index must be less than or equal to ", length(private$p_groups), "."), call. = FALSE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$item", "Got calculation group....")
      return(invisible(private$p_groups[[index]]))
    },

    #' @description
    #' Retrieve a calculation group by name.
    #' @param calculationGroupName The name of the calculation group to retrieve.
    #' @return The calculation group with the specified name.
    getCalculationGroup = function(calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$getCalculationGroup", "Getting calculation group...",
                                    list(calculationGroupName=calculationGroupName))
      calculationGroup <- private$p_groups[[calculationGroupName]]
      if(is.null(calculationGroup)) {
        stop(paste0("PivotCalculationGroups$getCalculationGroup(): No calculation group exists with the name '",
                    calculationGroupName, "'"), call. = FALSE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$getCalculationGroup", "Got calculation group.")
      return(invisible(calculationGroup))
    },

    #' @description
    #' Create a new calculation group.
    #' @param calculationGroupName The name of the calculation group to create
    #' @return The new calculation group.
    addCalculationGroup = function(calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$addCalculationGroup", "Adding calculation group...",
                                    list(calculationGroupName=calculationGroupName))
      if(calculationGroupName %in% names(private$p_groups)) {
        stop(paste0("PivotCalculationGroups$addCalculationGroup():  A calculation group already exists",
                    " in the Pivot Table with the name '", calculationGroupName, "'.  calculationGroupName must unique."), call. = FALSE)
      }
      calculationGroup <- PivotCalculationGroup$new(private$p_parentPivot, calculationGroupName)
      private$p_groups[[calculationGroupName]] <- calculationGroup
      if(is.null(private$p_defaultGroup)) private$p_defaultGroup <- calculationGroup
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroups$addCalculationGroup", "Added calculation group.")
      return(invisible(calculationGroup))
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
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

    #' @description
    #' Return the contents of this object as JSON for debugging.
    #' @return A JSON representation of various object properties.
    asJSON = function() {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("The jsonlite package is needed to convert to JSON.  Please install the jsonlite package.", call. = FALSE)
      }
      jsonliteversion <- utils::packageDescription("jsonlite")$Version
      if(numeric_version(jsonliteversion) < numeric_version("1.1")) {
        stop("Version 1.1 or above of the jsonlite package is needed to convert to JSON.  Please install an updated version of the jsonlite package.", call. = FALSE)
      }
      return(jsonlite::toJSON(self$asList()))
    },

    #' @description
    #' Return a representation of this object as a character value.
    #' @param seperator A character value used when concatenating
    #' the text representations of different calculation groups.
    #' @return A character summary of various object properties.
    asString = function(seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroups", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       }
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
    #' @field count The number of calculation groups in the pivot table.
    count = function(value) { return(invisible(length(private$p_groups))) },

    #' @field groups A list containing the calculation groups in the pivot table.
    groups = function(value) { return(invisible(private$p_groups)) },

    #' @field defaultGroup The default calculation group in the pivot table.
    defaultGroup = function(value) { return(invisible(private$p_defaultGroup)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_groups = NULL,
    p_defaultGroup = NULL
  )
)
