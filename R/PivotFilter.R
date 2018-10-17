#' A class that defines a filter condition.
#'
#' The PivotFilter class represents a single filter condition.  The condition
#' relates to one column and is of the form [ColumnName] IN c(Value1, Value2,
#' Value3, ...).  Often in a pivot table, each filter specifies only one data
#' value, as typically each distinct data value exists in a separate row or
#' column.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a single pivot table filter.
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' PivotFilter$new(pt, variableName="Country", values="England")
#' @field parentPivot Owning pivot table.
#' @field variableName The name of the column in the data frame that this filter
#'   will apply to.
#' @field safeVariableName The name of the column, surrounded by back-ticks, if
#'   the name is not legal.
#' @field values A single data value or a vector of data values that could/can
#'   be found in the data frame column.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot filter, specifying the field
#'   values documented above.}
#'
#'   \item{\code{intersect(filter)}}{Update this PivotFilter by intersecting
#'   the allowed values in this filter with the allowed values in the specified
#'   filter.}
#'   \item{\code{union(filter)}}{Update this PivotFilter by unioning
#'   the allowed values in this filter with the allowed values in the specified
#'   filter.}
#'   \item{\code{replace(filter)}}{Update this PivotFilter by replacing the
#'   allowed values in this filter with the allowed values from the specified
#'   filter.}
#'   \item{\code{getCopy()}}{Get a copy of this PivotFilter.}
#'   \item{\code{asList()}}{Get a list representation of this PivotFilter.}
#'   \item{\code{asJSON()}}{Get a list representation of this PivotFilter.}
#'   \item{\code{asString(includeVariableName=TRUE, seperator=" ")}}{Get a text
#'   representation of this PivotFilter.}
#' }

PivotFilter <- R6::R6Class("PivotFilter",
  public = list(
   initialize = function(parentPivot, variableName=NULL, type="ALL", values=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilter", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilter", "initialize", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilter", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilter", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$new", "Creating new Pivot Filter...", list(variableName=variableName, values=values))
     private$p_variableName <- variableName
     private$p_safeVariableName <- processIdentifier(variableName)
     if(is.null(values)) {
       if(type=="VALUES")
         stop("PivotFilter$new():  One or more values must be specified when type=VALUES.", call. = FALSE)
       private$p_type <- type
     }
     else {
       if((type=="VALUES")&&(length(values)==0)) {
         private$p_type <- "NONE"
       }
       else {
         private$p_type <- "VALUES" # if some values are specified, then change the type, for easier construction of the PivotFilter
         private$p_values <- values
         if(is.factor(values)) { private$p_values <- as.character(values) }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$new", "Created new Pivot Filter.")
   },
   intersect = function(filter) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "intersect", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$intersect", "Intersecting filter...", list(filter=filter$asString()))
     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$intersect():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     if(private$p_type=="ALL") {
       if(filter$type=="ALL") {
         # do nothing
       }
       else if(filter$type=="VALUES") {
         private$p_type <- "VALUES"
         private$p_values <- filter$values
       }
       else if(filter$type=="NONE") {
         private$p_type <- "NONE"
       }
       else stop(paste0("PivotFilter$intersect():  Invalid filter$type (A): ", filter$type), call. = FALSE)
     }
     else if(private$p_type=="VALUES") {
       if(filter$type=="ALL") {
         # do nothing
       }
       else if(filter$type=="VALUES") {
         private$p_values <- intersect(private$p_values, filter$values)
         if(is.null(private$p_values)) {
           private$p_type <- "NONE"
         }
         else if(length(private$p_values)==0) {
           private$p_type <- "NONE"
           private$p_values <- NULL
         }
       }
       else if(filter$type=="NONE") {
         private$p_type <- "NONE"
         private$p_values <- NULL
       }
       else stop(paste0("PivotFilter$intersect():  Invalid filter$type (B): ", filter$type), call. = FALSE)
     }
     else if(private$p_type=="NONE") {
       if(filter$type=="ALL") {
         # do nothing
       }
       else if(filter$type=="VALUES") {
         # do nothing
       }
       else if(filter$type=="NONE") {
         # do nothing
       }
       else stop(paste0("PivotFilter$intersect():  Invalid filter$type (C): ", filter$type), call. = FALSE)
     }
     else stop(paste0("PivotFilter$intersect():  Invalid type (D): ", private$p_type), call. = FALSE)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$intersect", "Intersected filter.")
     return(invisible())
   },
   union = function(filter) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "union", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$union", "Unioning filter...", list(filter=filter$asString()))
     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$union():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     if(private$p_type=="ALL") {
       if(filter$type=="ALL") {
         # do nothing
       }
       else if(filter$type=="VALUES") {
         # do nothing
       }
       else if(filter$type=="NONE") {
         # do nothing
       }
       else stop(paste0("PivotFilter$union():  Invalid filter$type (A): ", filter$type), call. = FALSE)
     }
     else if(private$p_type=="VALUES") {
       if(filter$type=="ALL") {
         private$p_type <- "ALL"
         private$p_values <- NULL
       }
       else if(filter$type=="VALUES") {
         private$p_values <- union(private$p_values, filter$values)
       }
       else if(filter$type=="NONE") {
         # do nothing
       }
       else stop(paste0("PivotFilter$union():  Invalid filter$type (B): ", filter$type), call. = FALSE)
     }
     else if(private$p_type=="NONE") {
       if(filter$type=="ALL") {
         private$p_type <- "ALL"
       }
       else if(filter$type=="VALUES") {
         private$p_type <- "VALUES"
         private$p_values <- filter$values
       }
       else if(filter$type=="NONE") {
         # do nothing
       }
       else stop(paste0("PivotFilter$union():  Invalid filter$type (C): ", filter$type), call. = FALSE)
     }
     else stop(paste0("PivotFilter$union():  Invalid type (D): ", private$p_type), call. = FALSE)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$union", "Unioned filter.")
     return(invisible())
   },
   replace = function(filter) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "replace", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$replace", "Replacing filter...", list(filter=filter$asString()))

     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$replace():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     private$p_type <- filter$type
     private$p_values <- filter$values
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilter$union", "Replaced filter.")
     return(invisible())
   },
   getCopy = function() {
     copy <- PivotFilter$new(private$p_parentPivot, variableName=rep(private$p_variableName),
                             type=rep(private$p_type), values=rep(private$p_values))
     return(invisible(copy))
   },
   asList = function() {
     lst <- list(
       variableName = private$p_variableName,
       type = private$p_type,
       values = private$p_values
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) },
   asString = function(includeVariableName=TRUE, seperator=" ") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     }
     vs <- NULL
     if(private$p_type=="ALL") vs <- "ALL"
     else if(private$p_type=="VALUES") vs <- paste(private$p_values, collapse=seperator)
     else if(private$p_type=="NONE") vs <- "NONE"
     else vs <- "??"
     fstr <- NULL
     if(includeVariableName) fstr <- paste0(private$p_variableName, " = ", vs)
     else fstr <- vs
     return(fstr)
   }
  ),
  active = list(
    variableName = function(value) { return(invisible(private$p_variableName)) },
    safeVariableName = function(value) { return(invisible(private$p_safeVariableName)) },
    type = function(value) { return(invisible(private$p_type)) },
    values = function(newValues) {
      if(missing(newValues)) return(invisible(private$p_values))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilter", "values", newValues, missing(newValues), allowMissing=FALSE, allowNull=FALSE, mustBeAtomic=TRUE)
        }
        private$p_values <- newValues
      }
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_variableName = NULL,
    p_safeVariableName = NULL,
    p_values = NULL,
    p_type = NULL
  )
)
