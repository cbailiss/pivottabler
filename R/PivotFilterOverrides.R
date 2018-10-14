#' A class that defines a set of filter overrides
#'
#' The PivotFilterOverrides class contains multiple  \code{\link{PivotFilter}}
#' objects that can be used later to override a set of filters, e.g. in a
#' pivot table calculation.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a set of filters and associated override actions
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' # PivotFilterOverrides constructor allows a filter to be defined
#' filterOverrides <- PivotFilterOverrides$new(pt)
#' # Create a new filter
#' filter <- PivotFilter$new(pt, variableName="Country", values="England")
#' # Add the filter to the set of overrides
#' filterOverrides$add(filter=filter, action="replace")
#' @field parentPivot Owning pivot table.
#' @field countAnd The number of PivotFilters that will be combined with other
#' pivot filters using the AND operator.
#' @field countReplace The number of PivotFilters that will be combined with other
#' pivot filters by entirely replaceing existing PivotFilter objects.
#' @field countOr The number of PivotFilters that will be combined with other
#' pivot filters using the OR operator.
#' @field countTotal The total number of PivotFilters that will be combined with
#' other pivot filters.
#' @field andFilter The PivotFilters that will be combined with other
#' pivot filters using the AND operator.
#' @field replaceFilters The PivotFilters that will be combined with other
#' pivot filters by entirely replaceing existing PivotFilter objects.
#' @field orFilters The PivotFilters that will be combined with other
#' pivot filters using the OR operator.
#' @field allFilters The complete set of PivotFilters that will be combined with
#' other pivot filters.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot filter overrides object, specifying
#'   the field values documented above.}
#'
#'   \item{\code{add(filter=NULL, variableName=NULL, type="ALL", values=NULL,
#'   action="and")}}{Add a pivot filter override, either from an existing
#'   PivotFilter object or by specifying a variableName and values.}
#'   \item{\code{apply(filters)}}{Apply the filter overrides to a PivotFilters
#'   object.}
#'   \item{\code{asList()}}{Get a list representation of this PivotFilterOverrides
#'   object.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this PivotFilterOverrides
#'   object.}
#'   \item{\code{asString(includeVariableName=TRUE, seperator=", ")}}{Get a text
#'   representation of this PivotFilterOverrides object.}
#' }

PivotFilterOverrides <- R6::R6Class("PivotFilterOverrides",
  public = list(
    initialize = function(parentPivot=NULL, filter=NULL, variableName=NULL, type="ALL", values=NULL, action="and") {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", filter, missing(filter), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilter")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$new", "Creating new Pivot Filter Overrides...", list(variableName=variableName, values=values))
      private$p_andFilters <- list()
      private$p_replaceFilters <- list()
      private$p_orFilters <- list()
      if(!missing(filter)) self$add(filter=filter, action=action)
      else if(!missing(variableName)) self$add(variableName=variableName, type=type, values=values, action=action)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$new", "Created new Pivot Filter Overrides.")
    },
    add = function(filter=NULL, variableName=NULL, type="ALL", values=NULL, action="and") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", filter, missing(filter), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilter")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "add", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("and", "replace", "or"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Adding filter override...")
      if(missing(filter)&&missing(variableName))
        stop("PivotFilterOverrides$add():  Either the filter parameter or the variableName parameter must be specified.", call. = FALSE)
      if(is.null(filter)) filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, type=type, values=values)
      if(action=="and") private$p_andFilters[[length(private$p_andFilters)+1]] <- filter
      else if(action=="replace") private$p_replaceFilters[[length(private$p_replaceFilters)+1]] <- filter
      else if(action=="or") private$p_orFilters[[length(private$p_orFilters)+1]] <- filter
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Added filter override.")
    },
    apply = function(filters=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilterOverrides", "apply", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Applying overrides...")
      if(!is.null(private$p_andFilters)) {
        if(length(private$p_andFilters)>0) {
          for(i in 1:length(private$p_andFilters)) {
            filters$setFilter(filter=private$p_andFilters[[i]], action="and")
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
      if(!is.null(private$p_orFilters)) {
        if(length(private$p_orFilters)>0) {
          for(i in 1:length(private$p_orFilters)) {
            filters$setFilter(filter=private$p_orFilters[[i]], action="or")
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilterOverrides$add", "Applied overrides.")
    },
    asList = function() {
      lst <- list()
      lstAnd <- list()
      lstReplace <- list()
      lstOr <- list()
      if(length(private$p_andFilters) > 0) {
        for (i in 1:length(private$p_andFilters)) {
          lstAnd[[i]] = private$p_andFilters[[i]]$asList()
        }
      }
      lst$andFilters <- lstAnd
      if(length(private$p_replaceFilters) > 0) {
        for (i in 1:length(private$p_replaceFilters)) {
          lstReplace[[i]] = private$p_replaceFilters[[i]]$asList()
        }
      }
      lst$replaceFilters <- lstReplace
      if(length(private$p_orFilters) > 0) {
        for (i in 1:length(private$p_orFilters)) {
          lstOr[[i]] = private$p_orFilters[[i]]$asList()
        }
      }
      lst$orFilters <- lstOr
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(includeVariableName=TRUE, seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      }
      fstr <- ""
      if(length(private$p_andFilters)>0) {
        for(i in 1:length(private$p_andFilters)) {
          f <- private$p_andFilters[[i]]
          sep <- ""
          if(i > 1) { sep <- seperator }
          fstr <- paste0(fstr, sep, "and: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      if(length(private$p_replaceFilters)>0) {
        for(i in 1:length(private$p_replaceFilters)) {
          f <- private$p_replaceFilters[[i]]
          sep <- ""
          if(i > 1) { sep <- seperator }
          fstr <- paste0(fstr, sep, "replace: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      if(length(private$p_orFilters)>0) {
        for(i in 1:length(private$p_orFilters)) {
          f <- private$p_orFilters[[i]]
          sep <- ""
          if(i > 1) { sep <- seperator }
          fstr <- paste0(fstr, sep, "or: ", f$asString(includeVariableName=includeVariableName))
        }
      }
      return(fstr)
    }
  ),
  active = list(
    countAnd = function(value) { return(invisible(length(private$p_andFilters))) },
    countReplace = function(value) { return(invisible(length(private$p_replaceFilters))) },
    countOr = function(value) { return(invisible(length(private$p_orFilters))) },
    countTotal = function(value) { return(invisible(length(private$p_andFilters)+length(private$p_replaceFilters)+length(private$p_orFilters))) },
    andFilters = function(value) { return(invisible(private$p_andFilters)) },
    replaceFilters = function(value) { return(invisible(private$p_replaceFilters)) },
    orFilters = function(value) { return(invisible(private$p_orFilters)) },
    allFilters = function(value) { return(invisible(union(private$p_andFilters, private$p_replaceFilters, private$p_orFilters))) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_andFilters = NULL,
    p_replaceFilters = NULL,
    p_orFilters = NULL
  )
)
