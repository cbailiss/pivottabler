#' A class that defines a set of filter conditions
#'
#' The PivotFilters class allows multiple filter conditions relating to
#' different data frame columns to be combined, i.e. a PivotFilters object can
#' contain multiple \code{\link{PivotFilter}} objects.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a set of filter conditions.
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' # PivotFilters constructor allows a filter to be defined
#' filters <- PivotFilters$new(pt, variableName="Year", values=2017)
#' # Create a new filter
#' filter <- PivotFilter$new(pt, variableName="Country", values="England")
#' # Combine the filters
#' filters$setFilter(filter)
#' # filters now contains criteria for both Year and Country
#' # Now add another filter, this time via an alternative method
#' filters$setFilterValues(variableName="Product", values="Cadbury Dairy Milk
#' Chocolate 100g")
#' # filters now contains criteria for Year, Country and Product
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot calculation, specifying the field
#'   values documented above.}
#'
#'   \item{\code{getFilter(variableName)}}{Find a filter with the specified
#'   variable name.}
#'   \item{\code{isFilterMatch(variableNames=NULL, variableValues=NULL)}}{Tests
#'   whether these filters match the specified criteria.}
#'   \item{\code{setFilters(filters, action="replace")}}{Update the value of this
#'   PivotFilters object with the filters from the specified PivotFilters
#'   object, either unioning, intersecting or replacing the filter criteria.}
#'   \item{\code{setFilter(filter, action="replace")}}{Update the value of this
#'   PivotFilters object with the specified PivotFilter object, either unioning,
#'   intersecting or replacing the filter criteria.}
#'   \item{\code{setFilterValues(variableName=NULL, values=NULL,
#'   action="replace")}}{Update the value of this PivotFilters object with the
#'   specified criteria, either unioning, intersecting or replacing the filter
#'   criteria.}
#'   \item{\code{addFilter()}}{Directly add a PivotFilter object to this
#'   PivotFilters object.}
#'   \item{\code{getCopy()}}{Get a copy of this set of filters.}
#'   \item{\code{asList()}}{Get a list representation of this PivotFilters
#'   object.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this PivotFilters
#'   object.}
#'   \item{\code{asString(includeVariableName=TRUE, seperator=", ")}}{Get a text
#'   representation of this PivotFilters object.}
#' }

PivotFilters <- R6::R6Class("PivotFilters",
  public = list(
    initialize = function(parentPivot, variableName=NULL, values=NULL) {
      checkArgument("PivotFilters", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      checkArgument("PivotFilters", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotFilters", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotFilters$new", "Creating new Pivot Filters...", list(variableName=variableName, values=values))

      private$p_filters <- list()
      if(!missing(variableName)&&!is.null(variableName)) {
        self$setFilterValues(variableName, values, action="replace")
      }
      private$p_parentPivot$message("PivotFilters$new", "Created new Pivot Filters.")
    },
    getFilter = function(variableName) {
      checkArgument("PivotFilters", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_parentPivot$message("PivotFilters$getFilter", "Getting filter...", list(variableName=variableName))
      if(length(private$p_filters)>0) {
        for(i in 1:length(private$p_filters)) {
          if(private$p_filters[[i]]$variableName==variableName) {
            return(invisible(private$p_filters[[i]]))
          }
        }
      }
      private$p_parentPivot$message("PivotFilters$getFilter", "Got filter.")
      return(invisible())
    },
    isFilterMatch = function(variableNames=NULL, variableValues=NULL) {
      checkArgument("PivotFilters", "isFilterMatch", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotFilters", "isFilterMatch", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      private$p_parentPivot$message("PivotFilters$isFilterMatch", "Checking if is filter match...")
      if(!is.null(variableNames)) {
        if(length(variableNames) > 0) {
          for(i in 1:length(variableNames)) {
            filter <- self$getFilter(variableNames[i])
            if(is.null(filter)) return(invisible(FALSE))
          }
        }
      }
      if(!is.null(variableValues)) {
        if(length(variableValues) > 0) {
          varNames <- names(variableValues)
          for(i in 1:length(varNames)) {
            filter <- self$getFilter(varNames[i])
            if(is.null(filter)) return(invisible(FALSE))
            varValues <- variableValues[[i]]
            if(is.null(varValues)) next
            if(length(varValues)==0) next
            intrsct <- intersect(filter$values, varValues)
            if(is.null(intrsct)) return(invisible(FALSE))
            if(length(intrsct)==0) return(invisible(FALSE))
          }
        }
      }
      private$p_parentPivot$message("PivotFilters$isFilterMatch", "Checked if is filter match.")
      return(invisible(TRUE))
    },
    setFilters = function(filters, action="replace") {
      checkArgument("PivotFilters", "setFilters", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
      checkArgument("PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilters", "Setting filters...", list(action=action, filters=filters$asString()))
      if(length(filters$filters)>0) {
        for(i in 1:length(filters$filters)) {
          self$setFilter(filters$filters[[i]], action)
        }
      }
      private$p_parentPivot$message("PivotFilters$setFilters", "Set filters.")
      return(invisible())
    },
    setFilter = function(filter, action="replace") {
      checkArgument("PivotFilters", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      checkArgument("PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilters", "Setting filter...", list(action=action, filter=filter$asString()))
      self$setFilterValues(filter$variableName, filter$values, action)
      private$p_parentPivot$message("PivotFilters$setFilter", "Set filter.")
      return(invisible())
    },
    setFilterValues = function(variableName=NULL, values=NULL, action="replace") {
      checkArgument("PivotFilters", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotFilters", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      checkArgument("PivotFilters", "setFilterValues", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilterValues", "Setting filter values...",
                                    list(action=action, variableName=variableName, values=values))
      filter <- PivotFilter$new(private$p_parentPivot, variableName, values)
      variablesNames <- names(private$p_filters)
      if(action=="union") {
        if(!(variableName %in% variablesNames)) { private$p_filters[[length(private$p_filters)+1]] <- filter }
        else { private$p_filters[[variableName]]$union(filter) }
      }
      else if(action=="intersect") {
        if(!(variableName %in% variablesNames))
          stop("PivotFilters$setFilterValues():  Cannot intersect as variable name does not exist in current filters.", call. = FALSE)
        private$p_filters[[variableName]]$intersect(filter)
      }
      else if(action=="replace") {
        private$p_filters[[variableName]] <- filter
      }
      else {
        stop(paste0("PivotFilters$setFilterValues():  action must be one of union, intersect, replace.  Unknown action: ", action), call. = FALSE)
      }
      private$p_parentPivot$message("PivotFilters$setFilterValues", "Set filter values.")
      return(invisible())
    },
    addFilter = function(filter) {
      checkArgument("PivotFilters", "addFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      private$p_parentPivot$message("PivotFilters$addFilter", "Adding filter...", list(filter=filter$asString()))

      index <- length(private$p_filters) + 1
      private$p_filters[[index]] <- filter
      private$p_parentPivot$message("PivotFilters$addFilter", "Added filter.")
      return(invisible())
    },
    getCopy = function() {
      copy <- PivotFilters$new(private$p_parentPivot)
      if(length(private$p_filters) > 0) {
        for(i in 1:length(private$p_filters)) {
          copy$addFilter(private$p_filters[[i]]$getCopy())
        }
      }
      return(invisible(copy))
    },
    asList = function() {
      lst <- list()
      if(length(private$p_filters) > 0) {
        for (i in 1:length(private$p_filters)) {
          lst[[i]] = private$p_filters[[i]]$asList()
        }
      }
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(includeVariableName=TRUE, seperator=", ") {
       checkArgument("PivotFilters", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument("PivotFilters", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       fstr <- ""
       if(length(private$p_filters)>0) {
         for(i in 1:length(private$p_filters)) {
           f <- private$p_filters[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           fstr <- paste0(fstr, sep, f$asString(includeVariableName=includeVariableName))
         }
       }
       return(fstr)
    }
  ),
  active = list(
    count = function(Value) { return(invisible(length(private$p_filters))) },
    filters = function(value) { return(invisible(private$p_filters)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_filters = NULL
  )
)
