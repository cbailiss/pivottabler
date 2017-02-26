PivotCells <- R6::R6Class("PivotCells",
  public = list(
   initialize = function(parentPivot, rowGroups=NULL, columnGroups=NULL) {
     checkArgument("PivotCells", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotCells$new", "Creating new PivotCells...",
                                   list(rowCount=length(rowGroups), columnCount=length(columnGroups)))
     checkArgument("PivotCells", "initialize", rowGroups, missing(rowGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotCells", "initialize", columnGroups, missing(columnGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_rowGroups <- rowGroups
     private$p_columnGroups <- columnGroups
     private$p_rows <- list() # a list of rows, each containing a list of values in the row
     for(r in 1:length(rowGroups)) {
       cellsInRow <- list()
       for(c in 1:length(columnGroups)) {
         cellsInRow[[c]] <- NULL
       }
       private$p_rows[[r]] <- cellsInRow
     }
     private$p_parentPivot$message("PivotCells$new", "Created new PivotCells.")
   },
   getCell = function(r=NULL, c=NULL) {
     checkArgument("PivotCells", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
     checkArgument("PivotCells", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
     return(private$p_rows[[r]][[c]])
   },
   setCell = function(r, c, cell) {
     checkArgument("PivotCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
     checkArgument("PivotCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
     checkArgument("PivotCells", "setCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     private$p_rows[[r]][[c]] <- cell
   },
   asList = function() {
     lst <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         rlst <- list()
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             rlst[[c]] <- private$p_rows[[r]][[c]]$asList()
           }
         }
         lst[[r]] <- rlst
       }
     }
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowCount = function(value) { return(length(private$p_rowGroups)) },
   columnCount = function(value) { return(length(private$p_columnGroups)) },
   rowGroups = function(value) { return(private$p_rowGroups) },
   columnGroups = function(value) { return(private$p_columnGroups) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_rowGroups = NULL,
    p_columnGroups = NULL,
    p_rows = NULL
  )
)
