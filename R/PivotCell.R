PivotCell <- R6::R6Class("PivotCell",
  public = list(
   initialize = function(parentPivot, rowNumber=NULL, columnNumber=NULL,
                         calculationName=NULL, calculationGroupName=NULL,
                         rowFilters=NULL, columnFilters=NULL, rowColFilters=NULL,
                         rowLeafGroup=NULL, columnLeafGroup=NULL) {
     checkArgument("PivotCell", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotCell", "initialize", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCell", "initialize", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCell", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCell", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCell", "initialize", rowFilters, missing(rowFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCell", "initialize", columnFilters, missing(columnFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCell", "initialize", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCell", "initialize", rowLeafGroup, missing(rowLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
     checkArgument("PivotCell", "initialize", columnLeafGroup, missing(columnLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotCell$new", "Creating new PivotCell",
                                   list(rowNumber=rowNumber, columnNumber=columnNumber))
     private$p_parentPivot = parentPivot
     private$p_rowNumber = rowNumber
     private$p_columnNumber = columnNumber
     private$p_calculationName = calculationName
     private$p_calculationGroupName = calculationGroupName
     private$p_rowFilters = rowFilters
     private$p_columnFilters = columnFilters
     private$p_rowColFilters = rowColFilters
     private$p_calculationFilters = NULL
     private$p_rowLeafGroup = rowLeafGroup
     private$p_columnLeafGroup = columnLeafGroup
     private$p_parentPivot$message("PivotCell$new", "Created new PivotCell")
   },
   getCopy = function() {
     copy <- list()
     return(copy)
   },
   asList = function() {
     fstr1 <- NULL
     fstr2 <- NULL
     fstr3 <- NULL
     if(!is.null(private$p_rowColFilters)) fstr1 <- private$p_rowColFilters$asString()
     if(!is.null(private$p_rowFilters)) fstr2 <- private$p_rowFilters$asString()
     if(!is.null(private$p_columnFilters)) fstr3 <- private$p_columnFilters$asString()
     lst <- list(
       row=private$p_rowNumber,
       column=private$p_columnNumber,
       calculationName=private$p_calculationName,
       calculationGroupName=private$p_calculationGroupName,
       rowColFilters=fstr1,
       rowFilters=fstr2,
       columnFilters=fstr3,
       rowLeafCaption=private$p_rowLeafGroup$caption,
       columnLeafCaption=private$p_columnLeafGroup$caption,
       formattedValue=self$formattedValue
     )
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowNumber = function(value) { return(private$p_rowNumber) },
   columnNumber = function(value) { return(private$p_columnNumber) },
   calculationName = function(value) { return(private$p_calculationName) },
   calculationGroupName = function(value) { return(private$p_calculationGroupName) },
   rowFilters = function(value) { return(private$p_rowFilters) },
   columnFilters = function(value) { return(private$p_columnFilters) },
   rowColFilters = function(value) { return(private$p_rowColFilters) },
   calculationFilters = function(value) {
     if(missing(value)) { return(private$p_calculationFilters) }
     else {
       checkArgument("PivotCell", "calculationFilters", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
       private$p_calculationFilters <- value
     }
   },
   rowLeafGroup = function(value) { return(private$p_rowLeafGroup) },
   columnLeafGroup = function(value) { return(private$p_columnLeafGroup) },
   isTotal = function(Value) { return(private$p_rowLeafGroup$isTotal|private$p_columnLeafGroup$isTotal)},
   rawValue = function(value) {
     if(missing(value)) return(private$p_rawValue)
     else private$p_rawValue <- value
   },
   formattedValue = function(value) {
     if(missing(value)) return(private$p_formattedValue)
     else private$p_formattedValue <- value
   }
  ),
  private = list(
    p_parentPivot = NULL,             # an object ref (the single pivot instance)
    p_rowNumber = NULL,               # an integer
    p_columnNumber = NULL,            # an integer
    p_calculationName = NULL,         # a string
    p_calculationGroupName = NULL,    # a string
    p_rowFilters = NULL,              # an object ref (shared across this row)
    p_columnFilters = NULL,           # an object ref (shared across this column)
    p_rowColFilters = NULL,           # an object ref (unique to this cell)
    p_calculationFilters = NULL,      # an object ref (unique to this cell)
    p_rowLeafGroup = NULL,            # an object ref (shared across this row)
    p_columnLeafGroup = NULL,         # an object ref (shared across this column)
    p_rawValue = NULL ,               # a value (unique to this cell)
    p_formattedValue = NULL           # a value (unique to this cell)
  )
)
