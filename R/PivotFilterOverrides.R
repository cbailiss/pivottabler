
#' R6 class that defines a set of filter overrides.
#'
#' @description
#' The `PivotFilterOverrides` class contains multiple  \code{\link{PivotFilter}}
#' objects that can be used later to override a set of filters, e.g. in a
#' pivot table calculation.
#'
#' @details
#' Each cell in a pivot table has context (i.e. filters) coming from the row
#' and column groups that are applicable to the cell.
#' The `PivotFilterOverrides` class contains several different ways of changing
#' this filter criteria as part of a calculation.  In most use cases, only
#' one of the available approaches will be used.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' # PivotFilterOverrides constructor allows a filter to be defined
#' # e.g. to enable %of row or column type calculations
#' filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="Volume")
#' # Alternatively/in addition, create a new filter
#' filter <- PivotFilter$new(pt, variableName="Country", values="England")
#' # Add the filter to the set of overrides
#' filterOverrides$add(filter=filter, action="replace")

PivotFilterOverrides <- R6::R6Class("PivotFilterOverrides",
  public = list(

    #' @description
    #' Create a new `PivotFilterOverrides` object.
    #' @param parentPivot The pivot table that this `PivotFilterOverrides`
    #' instance belongs to.
    #' @param removeAllFilters Specifies whether to clear all existing filters,
    #' before applying the filter overrides.  Default value `FALSE`
    #' @param keepOnlyFiltersFor A character vector specifying the variable names
    #' to retain the filter criteria for. Filter criteria for all other variables
    #' will be cleared.
    #' @param removeFiltersFor A character vector specifying the variable names
    #' for which the filter criteria will be cleared.  Filter criteria for all
    #' other variables will be retained.
    #' @param overrideFunction A custom R function which will be invoked for each
    #' cell to modify the filters before the calculation is carried out.
    #' @param filter A PivotFilter object containing filter criteria which will
    #' be combined with the current set of filters using the specified combine method.
    #' @param variableName The variable name for a new filter to apply to.  Specified
    #' in conjunction with the `type` and `values` parameters.
    #' @param type The type of a new filter to apply, must be either "ALL", "VALUES"
    #' or "NONE".
    #' @param values A single data value or a vector of multiple data values that
    #' a new filter will match on.
    #' @param action Specifies how the new filter defined in `filter` (or
    #' `variableName`, `type` and `values`) should be combined with the existing filter
    #' criteria for the cell.  Must be one of "intersect", "replace" or "union".
    #' @return A new `PivotFilterOverrides` object.
    initialize = function(parentPivot=NULL, removeAllFilters=FALSE, keepOnlyFiltersFor=NULL, removeFiltersFor=NULL, overrideFunction=NULL,
                          filter=NULL, variableName=NULL, type="ALL", values=NULL, action="replace") {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", removeAllFilters, missing(removeAllFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", keepOnlyFiltersFor, missing(keepOnlyFiltersFor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", removeFiltersFor, missing(removeFiltersFor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", overrideFunction, missing(overrideFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", filter, missing(filter), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilter")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$new", "Creating new Pivot Filter Overrides...",
                                                                               list(removeAllFilters=removeAllFilters, keepOnlyFiltersFor=keepOnlyFiltersFor, removeFiltersFor=removeFiltersFor,
                                                                                    filter=ifelse(is.null(filter), NULL, filter$asString()),
                                                                                    variableName=variableName, type=type, values=values, action=action))
      private$p_removeAllFilters=removeAllFilters
      private$p_keepOnlyFiltersFor=keepOnlyFiltersFor
      private$p_removeFiltersFor=removeFiltersFor
      private$p_overrideFunction <- overrideFunction
      private$p_intersectFilters <- list()
      private$p_replaceFilters <- list()
      private$p_unionFilters <- list()
      if(!missing(filter)) self$add(filter=filter, action=action)
      else if(!missing(variableName)) self$add(variableName=variableName, type=type, values=values, action=action)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$new", "Created new Pivot Filter Overrides.")
    },

    #' @description
    #' Add additional filter criteria into this `PivotFilterOverrides` object.
    #' Either `filter` is specified, or `variableName`, `type` and `values` are specified.
    #' @param filter A `PivotFilter` to take criteria from.
    #' @param variableName The variable name the additional criteria applies to.
    #' @param type The type of the additional filter criteria, must be either
    #' "ALL", "VALUES" or "NONE".
    #' @param values A single data value or a vector of multiple data values that
    #' compromise the additional filter criteria.
    #' @param action Specifies how the additional filter should be combined with
    #' the existing filter criteria for the cell.  Must be one of "intersect",
    #' "replace" or "union".
    #' @return No return value.
    add = function(filter=NULL, variableName=NULL, type="ALL", values=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", filter, missing(filter), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilter")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Adding filter override...")
      if(missing(filter)&&missing(variableName))
        stop("PivotFilterOverrides$add():  Either the filter parameter or the variableName parameter must be specified.", call. = FALSE)
      if(is.null(filter)) filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, type=type, values=values)
      if(action=="intersect") private$p_intersectFilters[[length(private$p_intersectFilters)+1]] <- filter
      else if(action=="replace") private$p_replaceFilters[[length(private$p_replaceFilters)+1]] <- filter
      else if(action=="union") private$p_unionFilters[[length(private$p_unionFilters)+1]] <- filter
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Added filter override.")
    },

    #' @description
    #' Apply the filter overrides to an existing `PivotFilters` object.
    #' @param filters A `PivotFilters` object to apply the filter overrides to.
    #' @param cell A `PivotCell` object representing the cell that the `filters` relate to.
    #' @return No return value.
    apply = function(filters=NULL, cell=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "apply", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "apply", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Applying overrides...")
      if(private$p_removeAllFilters) filters$clearFilters()
      if(length(private$p_keepOnlyFiltersFor)>0) filters$keepOnlyFiltersFor(private$p_keepOnlyFiltersFor)
      if(length(private$p_removeFiltersFor)>0) filters$removeFiltersFor(private$p_removeFiltersFor)
      if(!is.null(private$p_intersectFilters)) {
        if(length(private$p_intersectFilters)>0) {
          for(i in 1:length(private$p_intersectFilters)) {
            filters$setFilter(filter=private$p_intersectFilters[[i]], action="intersect")
          }
        }
      }
      if(!is.null(private$p_replaceFilters)) {
        if(length(private$p_replaceFilters)>0) {
          for(i in 1:length(private$p_replaceFilters)) {
            filters$setFilter(filter=private$p_replaceFilters[[i]], action="replace")
          }
        }
      }
      if(!is.null(private$p_unionFilters)) {
        if(length(private$p_unionFilters)>0) {
          for(i in 1:length(private$p_unionFilters)) {
            filters$setFilter(filter=private$p_unionFilters[[i]], action="union")
          }
        }
      }
      if(!is.null(private$p_overrideFunction)) private$p_overrideFunction(private$p_parentPivot, filters, cell)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Applied overrides.")
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      lstAnd <- list()
      lstReplace <- list()
      lstOr <- list()
      if(length(private$p_intersectFilters) > 0) {
        for (i in 1:length(private$p_intersectFilters)) {
          lstAnd[[i]] = private$p_intersectFilters[[i]]$asList()
        }
      }
      lst$andFilters <- lstAnd
      if(length(private$p_replaceFilters) > 0) {
        for (i in 1:length(private$p_replaceFilters)) {
          lstReplace[[i]] = private$p_replaceFilters[[i]]$asList()
        }
      }
      lst$replaceFilters <- lstReplace
      if(length(private$p_unionFilters) > 0) {
        for (i in 1:length(private$p_unionFilters)) {
          lstOr[[i]] = private$p_unionFilters[[i]]$asList()
        }
      }
      lst$orFilters <- lstOr
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
    #' @param includeVariableName `TRUE` (default) to include the variable name in
    #'  the string.
    #' @param seperator A character value used when concatenating
    #' multiple filter overrides.
    #' @return A character summary of various object properties.
    asString = function(includeVariableName=TRUE, seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      }
      fstr <- ""
      sep <- ""
      if(private$p_removeAllFilters) {
        if(nchar(fstr) > 0) { sep <- seperator }
        fstr <- paste0(fstr, sep, "remove all existing")
      }
      if(length(private$p_keepOnlyFiltersFor)>0) {
        if(nchar(fstr) > 0) { sep <- seperator }
        fstr <- paste0(fstr, sep, "keep only: ", paste(private$p_keepOnlyFiltersFor, collapse=","))
      }
      if(length(private$p_removeFiltersFor)>0) {
        if(nchar(fstr) > 0) { sep <- seperator }
        fstr <- paste0(fstr, sep, "remove: ", paste(private$p_removeFiltersFor, collapse=","))
      }
      if(length(private$p_intersectFilters)>0) {
        for(i in 1:length(private$p_intersectFilters)) {
          if(nchar(fstr) > 0) { sep <- seperator }
          f <- private$p_intersectFilters[[i]]
          fstr <- paste0(fstr, sep, "intersect: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      if(length(private$p_replaceFilters)>0) {
        for(i in 1:length(private$p_replaceFilters)) {
          if(nchar(fstr) > 0) { sep <- seperator }
          f <- private$p_replaceFilters[[i]]
          fstr <- paste0(fstr, sep, "replace: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      if(length(private$p_unionFilters)>0) {
        for(i in 1:length(private$p_unionFilters)) {
          if(nchar(fstr) > 0) { sep <- seperator }
          f <- private$p_unionFilters[[i]]
          fstr <- paste0(fstr, sep, "union: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      return(fstr)
    }
  ),
  active = list(

    #' @field removeAllFilters TRUE to remove all existing filters before applying
    #' any other and/replace/or filters.
    removeAllFilters = function(value) { return(invisible(private$p_removeAllFilters)) },

    #' @field keepOnlyFiltersFor Specify the names of existing variables to retain
    #' the filters for.  All other filters will be removed.
    keepOnlyFiltersFor = function(value) { return(invisible(private$p_keepOnlyFiltersFor)) },

    #' @field removeFiltersFor Specify the names of variables to remove filters for.
    removeFiltersFor = function(value) { return(invisible(private$p_removeFiltersFor)) },

    #' @field overrideFunction A custom R function to amend the filters in each cell.
    overrideFunction = function(value) { return(invisible(private$overrideFunction)) },

    #' @field countAnd The number of `PivotFilters` that will be combined with other
    #' pivot filters by intersecting their lists of allowed values.
    countAnd = function(value) { return(invisible(length(private$p_intersectFilters))) },

    #' @field countReplace The number of `PivotFilters` that will be combined with other
    #' pivot filters by entirely replacing existing PivotFilter objects.
    countReplace = function(value) { return(invisible(length(private$p_replaceFilters))) },

    #' @field countOr The number of `PivotFilters` that will be combined with other
    #' pivot filters by unioning their lists of allowed values.
    countOr = function(value) { return(invisible(length(private$p_unionFilters))) },

    #' @field countTotal The total number of `PivotFilters` that will be combined with
    #' other pivot filters.
    countTotal = function(value) { return(invisible(length(private$p_intersectFilters)+length(private$p_replaceFilters)+length(private$p_unionFilters))) },

    #' @field andFilters The `PivotFilters` that will be combined with other
    #' pivot filters by intersecting their lists of allowed values.
    andFilters = function(value) { return(invisible(private$p_intersectFilters)) },

    #' @field replaceFilters The `PivotFilters` that will be combined with other
    #' pivot filters by entirely replacing existing PivotFilter objects.
    replaceFilters = function(value) { return(invisible(private$p_replaceFilters)) },

    #' @field orFilters The `PivotFilters` that will be combined with other
    #' pivot filters by unioning their lists of allowed values.
    orFilters = function(value) { return(invisible(private$p_unionFilters)) },

    #' @field allFilters The complete set of `PivotFilters` that will be combined with
    #' other pivot filters.
    allFilters = function(value) { return(invisible(union(private$p_intersectFilters, private$p_replaceFilters, private$p_unionFilters))) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_removeAllFilters = NULL,
    p_keepOnlyFiltersFor = NULL,
    p_removeFiltersFor = NULL,
    p_overrideFunction = NULL,
    p_intersectFilters = NULL,
    p_replaceFilters = NULL,
    p_unionFilters = NULL
  )
)
