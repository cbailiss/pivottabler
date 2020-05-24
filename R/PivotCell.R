
#' R6 class that represents a cell in a pivot table.
#'
#' @description
#' The `PivotCell` class represents a cell in the body of a pivot table (i.e. not
#' a row/column heading, rather a cell typically containing a numerical value).
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCell <- R6::R6Class("PivotCell",
  public = list(

   #' @description
   #' Create a new `PivotCell` object.
   #' @param parentPivot The pivot table that this `PivotCell`
   #' instance belongs to.
   #' @param rowNumber The row number of the cell.  1 = the first (i.e. top) data
   #' row.
   #' @param columnNumber The column number of the cell.  1 = the first (i.e.
   #' leftmost) data column.
   #' @param calculationName The name of the calculation that is displayed in the
   #' cell.
   #' @param calculationGroupName The name of the calculation group that owns the
   #' calculation.
   #' @param isEmpty `TRUE` if this cell contains no data (e.g. if it is
   #'   part of a header / outline row), `FALSE` otherwise.
   #' @param rowFilters A `PivotFilters` object containing the filters applied to
   #' this cell from the row data groups (i.e. row headings).
   #' @param columnFilters A `PivotFilters` object containing the filters applied to
   #' this cell from the column data groups (i.e. column headings).
   #' @param rowColFilters A `PivotFilters` object containing the combined filters
   #' applied to this cell from both the row and column data groups.
   #' @param rowLeafGroup The row data group linked to this row.
   #' @param columnLeafGroup The column data group linked to this column.
   #' @return A new `PivotCell` object.
   initialize = function(parentPivot, rowNumber=NULL, columnNumber=NULL,
                         calculationName=NULL, calculationGroupName=NULL,
                         isEmpty=FALSE, rowFilters=NULL, columnFilters=NULL, rowColFilters=NULL,
                         rowLeafGroup=NULL, columnLeafGroup=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowFilters, missing(rowFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnFilters, missing(columnFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", rowLeafGroup, missing(rowLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCell", "initialize", columnLeafGroup, missing(columnLeafGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCell$new", "Creating new PivotCell",
                                   list(rowNumber=rowNumber, columnNumber=columnNumber))
     private$p_instanceId <- parentPivot$getNextInstanceId()
     private$p_rowNumber <- rowNumber
     private$p_columnNumber <- columnNumber
     private$p_calculationName <- calculationName
     private$p_calculationGroupName <- calculationGroupName
     private$p_isEmpty <- isEmpty
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

   #' @description
   #' An internal method used to set style declarations on the cell.
   #' Using `pt$setStyling(cells=x)` is preferred for users.
   #' @param styleDeclarations A list containing CSS style declarations.
   #' @return No return value.
   setStyling = function(styleDeclarations=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "setStyling", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCell$setStyling", "Setting style declarations...", list(styleDeclarations))
      if(is.null(styleDeclarations)) private$p_style = NULL
      else if(is.null(private$p_style)) private$p_style = private$p_parentPivot$createInlineStyle(declarations=styleDeclarations)
      else private$p_style$setPropertyValues(styleDeclarations)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCell$setStyling", "Set style declarations.")
   },

   #' @description
   #' Non-functional legacy method soon to be removed.
   #' @return Returns an empty list.
   getCopy = function() {
     copy <- list()
     return(invisible(copy))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
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
       isEmpty=private$p_isEmpty,
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
   }
  ),
  active = list(

   #' @field instanceId An integer value that uniquely identifies this cell.
   #' NB:  This number is guaranteed to be unique within the pivot table,
   #' but the method of generation of the values may change in future, so
   #' you are advised not to base any logic on specific values.
   instanceId = function(value) { return(invisible(private$p_instanceId)) },

   #' @field rowNumber The row number of the cell.  1 = the first (i.e. top) data
   #' row.
   rowNumber = function(value) { return(invisible(private$p_rowNumber)) },

   #' @field columnNumber The column number of the cell.  1 = the first (i.e.
   #' leftmost) data column.
   columnNumber = function(value) { return(invisible(private$p_columnNumber)) },

   #' @field calculationName The name of the calculation that is displayed in the
   #' cell.
   calculationName = function(value) { return(invisible(private$p_calculationName)) },

   #' @field calculationGroupName The name of the calculation group that owns the
   #' calculation.
   calculationGroupName = function(value) { return(invisible(private$p_calculationGroupName)) },

   #' @field isEmpty `TRUE` if this cell contains no data (e.g. if it is
   #'   part of a header / outline row), `FALSE` otherwise.
   isEmpty = function(value) { return(invisible(private$p_isEmpty)) },

   #' @field rowFilters A `PivotFilters` object containing the filters applied to
   #' this cell from the row data groups (i.e. row headings).
   rowFilters = function(value) { return(invisible(private$p_rowFilters)) },

   #' @field columnFilters A `PivotFilters` object containing the filters applied to
   #' this cell from the column data groups (i.e. column headings).
   columnFilters = function(value) { return(invisible(private$p_columnFilters)) },

   #' @field rowColFilters A `PivotFilters` object containing the combined filters
   #' applied to this cell from both the row and column data groups.
   rowColFilters = function(value) { return(invisible(private$p_rowColFilters)) },

   #' @field calculationFilters The set of filters that apply to this cell to support
   #' calculation logic.  Either a `PivotFilters` object or a `PivotFilterOverrides`
   #' object.  See the "Appendix: Calculations" vignette for details.
   calculationFilters = function(value) {
     if(missing(value)) { return(invisible(private$p_calculationFilters)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCell", "calculationFilters", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("PivotFilters", "PivotFilterOverrides"))
       }
       private$p_calculationFilters <- value
       return(invisible())
     }
   },

   #' @field workingData A list of filter objects that results when the
   #' `rowColFilters` and `calculationFilters` are combined prior to calculating
   #' the cell value.  This is a list since some cells involve multiple
   #' calculations - where `calc$type` is "calculation" or "function", the
   #' calculation can be based on the values of other calculations.
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

   #' @field evaluationFilters The same as `workingData` generally, except
   #' when custom calculation functions modify the filters whilst executing.
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

   #' @field rowLeafGroup The row data group linked to this row.
   rowLeafGroup = function(value) { return(invisible(private$p_rowLeafGroup)) },

   #' @field columnLeafGroup The column data group linked to this column.
   columnLeafGroup = function(value) { return(invisible(private$p_columnLeafGroup)) },

   #' @field isTotal `TRUE` is this cell is a total, `FALSE` otherwise-
   isTotal = function(Value) { return(invisible(private$p_rowLeafGroup$isTotal|private$p_columnLeafGroup$isTotal)) },

   #' @field rawValue The raw cell value - i.e. unformatted, typically a numeric value.
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

   #' @field formattedValue The formatted value - typically a character value.
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

   #' @field baseStyleName The name of the style that defines the visual
   #' appearance of the cell.
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

   #' @field style A `PivotStyle` object that assists in managing the CSS
   #' style declarations that override the base style.
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
    p_instanceId = NULL,              # a unique identifier (unique across all row/column groups and cells) to allow "grp1 is grp2" type comparisons
    p_rowNumber = NULL,               # an integer
    p_columnNumber = NULL,            # an integer
    p_calculationName = NULL,         # a string
    p_calculationGroupName = NULL,    # a string
    p_isEmpty = NULL,                 # a logical value that indicates whether this cell is a header/separator (e.g. as used in the outline layout)
    p_rowFilters = NULL,              # an object ref (shared across this row)
    p_columnFilters = NULL,           # an object ref (shared across this column)
    p_rowColFilters = NULL,           # an object ref (unique to this cell)
    p_calculationFilters = NULL,      # an object ref (shared across this calculation)
    p_workingData = NULL,             # a list:  element = calculationName, value = a list of two elements (workingFilters & batchName) for the calculation
    p_evaluationFilters = NULL,       # an object ref (unique to this cell)
    p_rowLeafGroup = NULL,            # an object ref (shared across this row)
    p_columnLeafGroup = NULL,         # an object ref (shared across this column)
    p_rawValue = NULL ,               # a value (unique to this cell)
    p_formattedValue = NULL,          # a value (unique to this cell)
    p_baseStyleName = NULL,           # a string
    p_style = NULL                    # an object ref (may or may not be shared, but better if not) to a PivotStyle object.
                                      # The code tries to encourage this to be unique (e.g. when generating the headings/cells of a pvt
                                      # a unique PivotStyle object is generated per heading/cell).  If want the same PivotStyle object to
                                      # be used across cells, then can either define a named style and set baseStyleName to that, or can use
                                      # a function after the pivot table has been generated to set the cell$style object for multiple cells
                                      # to the same PivotStyle object.  A shared object will prevent styles being changed for individual
                                      # cells, but may offer performance advantages for very large pivot tables.  However, the bottom line
                                      # is that the proper general way to create a shared style is to create a NAMED pivot style, and not
                                      # to create a PivotStyle object and start sharing that around.
  )
)
