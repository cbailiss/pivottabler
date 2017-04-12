#' A class that contains the cells from a pivot table.
#'
#' The PivotCells class contains all of the PivotCell objects that comprise the
#' body of a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @keywords cell
#' @return Object of \code{\link{R6Class}} with properties and methods relating
#'   to the cells of a pivot table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field rowGroups The row data groups that represent the row headings in the
#'   pivot table.
#' @field columnGroups The column data groups that represent the column headings
#'   in the pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new set of pivot table cells, specifying
#'   the field values documented above.}
#'
#'   \item{\code{getCell(r, c))}}{Get the PivotCell at the specified row and
#'   column coordinates in the pivot table.}
#'   \item{\code{setCell(r, c, cell))}}{Set the PivotCell at the specified row
#'   and column coordinates in the pivot table.}
#'   \item{\code{asMatrix(rawValue=TRUE))}}{Get a matrix containing all of the
#'   numerical values from the body of the pivot table (for rawValue=TRUE) or
#'   all of the formatted (i.e. character) values (for rawValue=FALSE).}
#'   \item{\code{asList())}}{Get a list representation of the pivot table
#'   cells.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the pivot table cells.}
#' }

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
     return(invisible(private$p_rows[[r]][[c]]))
   },
   setCell = function(r, c, cell) {
     checkArgument("PivotCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
     checkArgument("PivotCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
     checkArgument("PivotCells", "setCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     private$p_rows[[r]][[c]] <- cell
     return(invisible())
   },
   asMatrix = function(rawValue=TRUE) {
     checkArgument("PivotCells", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     if((self$rowCount==0)||(self$columnCount==0)) return(matrix())
     m <- matrix(data=NA, nrow=self$rowCount, ncol=self$columnCount)
     for(r in 1:self$rowCount) {
       for(c in 1:self$columnCount) {
         if(rawValue==TRUE) {
           v <- private$p_rows[[r]][[c]]$rawValue
           if(!(("integer" %in% class(v))||("numeric" %in% class(v)))) v <- NA
         }
         else {
           v <- private$p_rows[[r]][[c]]$formattedValue
         }
         if(is.null(v)) v <- NA
         m[r, c] <- v
       }
     }
     return(m)
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
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowCount = function(value) { return(invisible(length(private$p_rowGroups))) },
   columnCount = function(value) { return(invisible(length(private$p_columnGroups))) },
   rowGroups = function(value) { return(invisible(private$p_rowGroups)) },
   columnGroups = function(value) { return(invisible(private$p_columnGroups)) },
   rows = function(value) { return(invisible(private$p_rows)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_rowGroups = NULL,
    p_columnGroups = NULL,
    p_rows = NULL
  )
)
