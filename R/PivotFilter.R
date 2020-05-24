
#' R6 class that defines a filter condition.
#'
#' @description
#' The `PivotFilter` class represents a single filter condition.
#'
#' @details
#' The filter condition represented by a `PivotFilter` instance relates to
#' one data frame variable/column and is of the form
#' [ColumnName] IN c(Value1, Value2, Value3, ...).
#' Often in a pivot table, each filter specifies only one data
#' value, as typically each distinct data value exists in a separate row or
#' column.
#' The `PivotFilter` class contains methods to perform set based operations
#' on filter values when combining filters.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' PivotFilter$new(pt, variableName="Country", values="England")

PivotFilter <- R6::R6Class("PivotFilter",
  public = list(

    #' @description
    #' Create a new `PivotFilter` object.
    #' @param parentPivot The pivot table that this `PivotFilter`
    #' instance belongs to.
    #' @param variableName The name of the column in the data frame that this filter
    #' applies to.
    #' @param type Must be either "ALL", "VALUES" or "NONE".  "VALUES" is the most
    #' common type and means the data is filtered to a subset of values.  "ALL" means
    #' there is no filtering, i.e. all values match.  "NONE" means there can be no
    #' matching values/data.
    #' @param values A single data value or a vector of multiple data values that
    #' this filter will match on.
    #' @return A new `PivotFilter` object.
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

   #' @description
   #' Updates this filter by intersecting the values in this filter with the
   #' values from another `PivotFilter` object.
   #' @param filter A `PivotFilter` object.
   #' @return No return value.
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

   #' @description
   #' Updates this filter by unioning the values in this filter with the
   #' values from another `PivotFilter` object.
   #' @param filter A `PivotFilter` object.
   #' @return No return value.
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

   #' @description
   #' Updates this filter by replacing the values in this filter with the
   #' values from another `PivotFilter` object.
   #' @param filter A `PivotFilter` object.
   #' @return No return value.
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

   #' @description
   #' Create a copy of this `PivotFilter` object.
   #' @return A copy of this `PivotFilter` object.
   getCopy = function() {
     copy <- PivotFilter$new(private$p_parentPivot, variableName=rep(private$p_variableName),
                             type=rep(private$p_type), values=rep(private$p_values))
     return(invisible(copy))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       variableName = private$p_variableName,
       type = private$p_type,
       values = private$p_values
     )
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
   #' multiple filter values.
   #' @return A character summary of various object properties.
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

    #' @field variableName The name of the column in the data frame that this filter
    #' applies to.
    variableName = function(value) { return(invisible(private$p_variableName)) },

    #' @field safeVariableName The name of the column in the data frame that this filter
    #' applies to,  surrounded by back-ticks if the name is not legal.
    safeVariableName = function(value) { return(invisible(private$p_safeVariableName)) },

    #' @field type Either "ALL", "VALUES" or "NONE".  "VALUES" is the most
    #' common type and means the data is filtered to a subset of values.  "ALL" means
    #' there is no filtering, i.e. all values match.  "NONE" means there can be no
    #' matching values/data.
    type = function(value) { return(invisible(private$p_type)) },

    #' @field values The subset of values that this filter matches.
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
