# template

ClassName <- R6::R6Class("ClassName",
  public = list(
   initialize = function(parentPivot, data) {
     if(missing(parentPivot)) stop("ClassName$new():  parentPivot must be specified")
     if(is.null(parentPivot)) stop("ClassName$new():  parentPivot must not be null")
     if(!("PivotTable" %in% class(parentPivot))) stop("ClassName$new():  parentPivot must be a PivotTable")
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
