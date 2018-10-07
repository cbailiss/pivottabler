#' A class that represents a cell in a pivot table
#'
#' The PivotCell class represents a cell in the body of a pivot table (i.e. not
#' a row/column heading, rather a cell typically containing a numerical value).
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a single pivot table cell
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field rowNumber The row number of the cell.  1 = the first (i.e. top) data
#'   row.
#' @field columnNumber The column number of the cell.  1 = the first (i.e.
#'   leftmost) data column.
#' @field rowLeafGroup The row data group linked to this row.
#' @field columnLeafGroup The column data group linked to this column.
#' @field calculationName The name of the calculation that is displayed in the
#'   cell.
#' @field calculationGroupName The name of the calculation group that owns the
#'   above calculation.
#' @field rowFilters The data filters applied to this cell from the row
#'   headings.
#' @field columnFilters The data filters applied to this cell from the column
#'   headings.
#' @field rowColFilters The data filters applied to this cell from both the row
#'   and column headings.
#' @field calculationFilters The data filters applied to this cell from the
#'   calculation definition.
#' @field workingData The data filters and batchNames used applied when running
#'   calculations (including the filters needed for base calculations when
#'   calculation type="calculation").
#' @field evaluationFilters The final and actual data filters used in the
#'   calculation of the cell value (e.g. custom calculation functions can
#'   override the working filters).
#' @field isTotal Whether this cell is a total cell.
#' @field rawValue The numerical calculation result.
#' @field formattedValue The formatted calculation result (i.e. character data type).
#' @field baseStyleName The name of the style applied to this cell (a character
#'   value).  The style must exist in the PivotStyles object associated with the
#'   PivotTable.
#' @field style A PivotStyle object that can apply overrides to the base style
#'   for this cell.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot table cell, specifying the field
#'   values documented above.}
#'
#'   \item{\code{getCopy())}}{Get a copy of this cell.}
#'   \item{\code{asList())}}{Get a list representation of this cell}
#'   \item{\code{asJSON()}}{Get a JSON representation of this cell}
#' }

PivotCell <- R6::R6Class("PivotCell",
  public = list(
   initialize = function(parentPivot, rowNumber=NULL, columnNumber=NULL,
                         calculationName=NULL, calculationGroupName=NULL,
                         rowFilters=NULL, columnFilters=NULL, rowColFilters=NULL,
                         rowLeafGroup=NULL, columnLeafGroup=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowFilters, missing(rowFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnFilters, missing(columnFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowLeafGroup, missing(rowLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnLeafGroup, missing(columnLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCell$new", "Creating new PivotCell",
                                   list(rowNumber=rowNumber, columnNumber=columnNumber))
     private$p_rowNumber <- rowNumber
     private$p_columnNumber <- columnNumber
     private$p_calculationName <- calculationName
     private$p_calculationGroupName <- calculationGroupName
     private$p_rowFilters <- rowFilters
     private$p_columnFilters <- columnFilters
     private$p_rowColFilters <- rowColFilters
     private$p_calculationFilters <- NULL
     private$p_workingData <- NULL
     private$p_evaluationFilters <- NULL
     private$p_rowLeafGroup <- rowLeafGroup
     private$p_columnLeafGroup <- columnLeafGroup
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCell$new", "Created new PivotCell")
   },
   getCopy = function() {
     copy <- list()
     return(invisible(copy))
   },
   asList = function() {
     fstr1 <- NULL
     fstr2 <- NULL
     fstr3 <- NULL
     fstr4 <- NULL
     fstr5 <- NULL
     if(!is.null(private$p_rowColFilters)) fstr1 <- private$p_rowColFilters$asString()
     if(!is.null(private$p_rowFilters)) fstr2 <- private$p_rowFilters$asString()
     if(!is.null(private$p_columnFilters)) fstr3 <- private$p_columnFilters$asString()
     if(!is.null(private$p_calculationFilters)) fstr4 <- private$p_calculationFilters$asString()
     if(!is.null(private$p_evaluationFilters)) fstr5 <- private$p_evaluationFilters$asString()
     wdlst <- NULL
     if(!is.null(private$p_workingData)) {
       wdlst <- list()
       if(length(private$p_workingData)>0) {
         for(i in 1:length(private$p_workingData)) {
           clst <- list()
           fstrW <- NULL
           if(!is.null(private$p_workingData[[i]]$workingFilters)) fstrW <- private$p_workingData[[i]]$workingFilters$asString()
           clst$workingFilters <- fstrW
           clst$batchName <- private$p_workingData[[i]]$batchName
           wdlst[[names(private$p_workingData)[i]]] <- clst
         }
       }
     }
     lst <- list(
       row=private$p_rowNumber,
       column=private$p_columnNumber,
       calculationName=private$p_calculationName,
       calculationGroupName=private$p_calculationGroupName,
       rowColFilters=fstr1,
       rowFilters=fstr2,
       columnFilters=fstr3,
       calculationFilters=fstr4,
       workingData=wdlst,
       evaluationFilters=fstr5,
       rowLeafCaption=private$p_rowLeafGroup$caption,
       columnLeafCaption=private$p_columnLeafGroup$caption,
       formattedValue=self$formattedValue
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowNumber = function(value) { return(invisible(private$p_rowNumber)) },
   columnNumber = function(value) { return(invisible(private$p_columnNumber)) },
   calculationName = function(value) { return(invisible(private$p_calculationName)) },
   calculationGroupName = function(value) { return(invisible(private$p_calculationGroupName)) },
   rowFilters = function(value) { return(invisible(private$p_rowFilters)) },
   columnFilters = function(value) { return(invisible(private$p_columnFilters)) },
   rowColFilters = function(value) { return(invisible(private$p_rowColFilters)) },
   calculationFilters = function(value) {
     if(missing(value)) { return(invisible(private$p_calculationFilters)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "calculationFilters", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
       }
       private$p_calculationFilters <- value
       return(invisible())
     }
   },
   workingData = function(value) {
     if(missing(value)) { return(invisible(private$p_workingData)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "workingData", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list")
       }
       private$p_workingData <- value
       return(invisible())
     }
   },
   evaluationFilters = function(value) {
     if(missing(value)) { return(invisible(private$p_evaluationFilters)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "evaluationFilters", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotFilters")
       }
       private$p_evaluationFilters <- value
       return(invisible())
     }
   },
   rowLeafGroup = function(value) { return(invisible(private$p_rowLeafGroup)) },
   columnLeafGroup = function(value) { return(invisible(private$p_columnLeafGroup)) },
   isTotal = function(Value) { return(invisible(private$p_rowLeafGroup$isTotal|private$p_columnLeafGroup$isTotal)) },
   rawValue = function(value) {
     if(missing(value)) return(invisible(private$p_rawValue))
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "rawValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct"))
       }
       private$p_rawValue <- value
       return(invisible())
     }
   },
   formattedValue = function(value) {
     if(missing(value)) return(invisible(private$p_formattedValue))
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "formattedValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric", "character"))
       }
       private$p_formattedValue <- value
       return(invisible())
     }
   },
   baseStyleName = function(value) {
     if(missing(value)) { return(invisible(private$p_baseStyleName)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "baseStyleName", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_baseStyleName <- value
       return(invisible())
     }
   },
   style = function(value) {
     if(missing(value)) { return(invisible(private$p_style)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "style", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyle")
       }
       private$p_style <- value
       return(invisible())
     }
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
    p_calculationFilters = NULL,      # an object ref (shared across this calculation)
    p_workingData = NULL,             # a list:  element = calculationName, value = a list of two elements (workingFilters & batchName) for the calculation
    p_evaluationFilters = NULL,       # an obejct ref (unique to this cell)
    p_rowLeafGroup = NULL,            # an object ref (shared across this row)
    p_columnLeafGroup = NULL,         # an object ref (shared across this column)
    p_rawValue = NULL ,               # a value (unique to this cell)
    p_formattedValue = NULL,          # a value (unique to this cell)
    p_baseStyleName = NULL,           # a string
    p_style = NULL                    # an object ref (may or may not be shared) to a PivotStyle object
  )
)
