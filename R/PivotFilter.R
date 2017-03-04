PivotFilter <- R6::R6Class("PivotFilter",
  public = list(
   initialize = function(parentPivot, variableName=NULL, values=NULL) {
     checkArgument("PivotFilter", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotFilter", "initialize", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotFilter", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotFilter$new", "Creating new Pivot Filter...", list(variableName=variableName, values=values))
     private$p_variableName <- variableName
     if(!is.null(values)) {
       private$p_values <- values
       if(is.factor(values)) { private$p_values <- as.character(values) }
     }
     private$p_parentPivot$message("PivotFilter$new", "Created new Pivot Filter.")
   },
   union = function(filter) {
     checkArgument("PivotFilter", "union", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     private$p_parentPivot$message("PivotFilter$union", "Unioning filter...", list(filter=filter$asString()))

     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$union():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     private$p_values <- union(private$p_values, filter$values)
     private$p_parentPivot$message("PivotFilter$union", "Unioned filter.")
     return(invisible())
   },
   intersect = function(filter) {
     checkArgument("PivotFilter", "intersect", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     private$p_parentPivot$message("PivotFilter$intersect", "Intersecting filter...", list(filter=filter$asString()))

     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$intersect():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     private$p_values <- intersect(private$p_values, filter$values)
     private$p_parentPivot$message("PivotFilter$union", "Intersected filter.")
     return(invisible())
   },
   replace = function(filter) {
     checkArgument("PivotFilter", "replace", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     private$p_parentPivot$message("PivotFilter$replace", "Replacing filter...", list(filter=filter$asString()))

     if(private$p_variableName != filter$variableName)
       stop(paste0("PivotFilter$replace():  filter variable name mismatch. ",
                   "Expected: ", private$p_variableName, " Encountered: ", filter$variableName), call. = FALSE)
     private$p_values <- values
     private$p_parentPivot$message("PivotFilter$union", "Replaced filter.")
     return(invisible())
   },
   getCopy = function() {
     copy <- PivotFilter$new(private$p_parentPivot, rep(private$p_variableName), rep(private$p_values))
     return(invisible(copy))
   },
   asList = function() {
     lst <- list(
       variableName = private$p_variableName,
       values = private$p_values
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) },
   asString = function(includeVariableName=TRUE, seperator=" ") {
     checkArgument("PivotFilter", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotFilter", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     fstr <- NULL
     if(includeVariableName) fstr <- paste0(private$p_variableName, " = ", paste(private$p_values, collapse=seperator))
     else fstr <- paste0(paste(private$p_values, collapse=seperator))
     return(fstr)
   }
  ),
  active = list(
    variableName = function(value) { return(invisible(private$p_variableName)) },
    values = function(value) { return(invisible(private$p_values)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_variableName = NULL,
    p_values = NULL
  )
)
