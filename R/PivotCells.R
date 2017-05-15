#' A class that contains the cells from a pivot table.
#'
#' The PivotCells class contains all of the PivotCell objects that comprise the
#' body of a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
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
#'   \item{\code{getCells(specifyCellsAsList=FALSE, rowNumbers=NULL,
#'   columnNumbers=NULL, cellCoordinates=NULL)}}{Retrieve cells by a combination
#'   of row and/or column numbers.}
#'   \item{\code{findCells(variableNames=NULL, variableValues=NULL,
#'   totals="include", calculationNames=NULL, minValue=NULL, maxValue=NULL,
#'   exactValues=NULL, includeNull=TRUE, includeNA=TRUE)}}{Find cells matching
#'   the specified criteria.}
#'   \item{\code{getColumnWidths())}}{Retrieve the width of the longest value
#'   (in characters) in each column.}
#'   \item{\code{asMatrix(rawValue=TRUE))}}{Get a matrix containing all of the
#'   numerical values from the body of the pivot table (for rawValue=TRUE) or
#'   all of the formatted (i.e. character) values (for rawValue=FALSE).}
#'   \item{\code{asList())}}{Get a list representation of the pivot table
#'   cells.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the pivot table cells.}
#' }

PivotCells <- R6::R6Class("PivotCells",
  public = list(
   initialize = function(parentPivot=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCells", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$new", "Creating new PivotCells...")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$new", "Created new PivotCells.")
   },
   reset = function() {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$resetCells", "Resetting cells...")
     private$p_rowGroups <- NULL
     private$p_columnGroups <- NULL
     private$p_rows <- NULL
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$resetCells", "Reset cells.")
   },
   setGroups = function(rowGroups=NULL, columnGroups=NULL) {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$setGroups", "Creating new PivotCells...",
                                   list(rowCount=length(rowGroups), columnCount=length(columnGroups)))
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setGroups", rowGroups, missing(rowGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setGroups", columnGroups, missing(columnGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     }
     private$p_rowGroups <- rowGroups
     private$p_columnGroups <- columnGroups
     private$p_rows <- vector("list", length = length(rowGroups)) # this pre-sizes the list
     for(r in 1:length(rowGroups)) {
       private$p_rows[[r]] <- vector("list", length = length(columnGroups)) # this pre-sizes the list
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$setGroups", "Created new PivotCells.")
   },
   getCell = function(r=NULL, c=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
     }
     if(r < 1)
       stop(paste0("PivotCells$getCell(): r (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("PivotCells$getCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("PivotCells$getCell(): c (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("PivotCells$getCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     if(length(private$p_rows[[r]]) < c) return(invisible(NULL))
     return(invisible(private$p_rows[[r]][[c]]))
   },
   setCell = function(r, c, cell) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     }
     if(r < 1)
       stop(paste0("PivotCells$setCell(): r (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("PivotCells$setCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("PivotCells$setCell(): c (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("PivotCells$setCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     private$p_rows[[r]][[c]] <- cell
     return(invisible())
   },
   getCells = function(specifyCellsAsList=FALSE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getCells", "Getting cells...")
     # warning of impending change to default parameter value
     if((!private$p_getCellsWarning)&&(!specifyCellsAsList)&&missing(specifyCellsAsList)) {
       message("getCells():  The default value of the specifyCellsAsList argument will be changed to TRUE in a future version of the pivottabler package.\nPlease specify the value of this argument explicitly to avoid being impacted when this change occurs.")
       private$p_getCellsWarning <- TRUE
     }
     if(specifyCellsAsList==FALSE) {
       # NA is allowed in rowNumbers or columnNumbers
       # cells are specified as in the rowNumbers and columnNumbers
       if((!is.null(cellCoordinates))&&(length(cellCoordinates)>0)) {
         stop("PivotCells$getCells():  When specifyCellsAsList=FALSE, cell coordinates should be specified using the rowNumbers and columnNumbers arguments.  Please see the \"Finding and Formatting\" vignette for more details.", call. = FALSE)
       }
       # pre-processing to put the arguments into the same structure as-if specifyCellsAsList==TRUE
       newRowNumbers <- NULL
       newColumnNumbers <- NULL
       newCellCoordinates <- list()
       rmax <- length(rowNumbers)
       cmax <- length(columnNumbers)
       imax <- max(rmax, cmax)
       if(imax>0) {
         for(i in 1:imax) {
           if((i<=rmax)&&(i<=cmax)) {
             if(rowNumbers[i] %in% NA) {
               if(columnNumbers[i] %in% NA) next
               else newColumnNumbers[length(newColumnNumbers)+1] <- columnNumbers[i]
             }
             else if(columnNumbers[i] %in% NA) newRowNumbers[length(newRowNumbers)+1] <- rowNumbers[i]
             else newCellCoordinates[[length(newCellCoordinates)+1]] <- c(rowNumbers[i], columnNumbers[i])
           }
           else if(i<=rmax) {
             if(!(rowNumbers[i] %in% NA)) newRowNumbers[length(newRowNumbers)+1] <- rowNumbers[i]
           }
           else if(i<=cmax) {
             if(!(columnNumbers[i] %in% NA)) newColumnNumbers[length(newColumnNumbers)+1] <- columnNumbers[i]
           }
           else stop("PivotCells$getCells():  argument pre-processing logic failure.", call. = FALSE)
         }
       }
       rowNumbers <- newRowNumbers
       columnNumbers <- newColumnNumbers
       cellCoordinates <- newCellCoordinates
       if((length(rowNumbers[rowNumbers %in% NA])>0)||(length(columnNumbers[columnNumbers %in% NA])>0)) {
         stop("PivotCells$getCells():  Pre-processing of the row and column numbers has failed to remove the NA values.", call. = FALSE)
       }
     }
     else {
       # NA is not allowed in rowNumbers or columnNumbers
       # cells are specified as a list in the cellCoordinates parameter
       if((length(rowNumbers[rowNumbers %in% NA])>0)||(length(columnNumbers[columnNumbers %in% NA])>0)) {
         stop("PivotCells$getCells():  When specifyCellsAsList=TRUE, rowNumbers/columnNumbers should not contain NA and cell coordinates should be specified using the specifyCellsAsList argument.  Please see the \"Finding and Formatting\" vignette for more details.", call. = FALSE)
       }
     }
     # if no rows, columns or cells specified, then return all cells
     cells <- list()
     if(is.null(rowNumbers)&&is.null(columnNumbers)&&(length(cellCoordinates)==0)) {
       if(length(private$p_rows) > 0) {
         for(r in 1:length(private$p_rows)) {
           if(length(private$p_rows[[r]]) > 0) {
             for(c in 1:length(private$p_rows[[r]])) {
               if(length(private$p_rows[[r]]) < c) next
               cell <- private$p_rows[[r]][[c]]
               cells[[length(cells)+1]] <- cell
             }
           }
         }
       }
       if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getCells", "Got cells.")
       return(invisible(cells))
     }
     # check the row and column coordinates
     if(length(rowNumbers[rowNumbers > self$rowCount])>0) {
       stop("PivotCells$getCells():  All rowNumbers should be less than or equal to the row count in the pivot table.", call. = FALSE)
     }
     if(length(columnNumbers[columnNumbers > self$columnCount])>0) {
       stop("PivotCells$getCells():  All columnNumbers should be less than or equal to the column count in the pivot table.", call. = FALSE)
     }
     cellRowNumbers <- sapply(cellCoordinates, function(x) { return(x[1]) })
     cellColumnNumbers <- sapply(cellCoordinates, function(x) { return(x[2]) })
     if((length(cellRowNumbers[cellRowNumbers %in% NA])>0)||(length(cellColumnNumbers[cellColumnNumbers %in% NA])>0)) {
       stop("PivotCells$getCells():  Each element in the cellCoordinates list must be a vector of length two (i.e. c(rowNumber, columnNumber)).", call. = FALSE)
     }
     if(length(cellRowNumbers[cellRowNumbers > self$rowCount])>0) {
       stop("PivotCells$getCells():  All row numbers in cellCoordinates should be less than or equal to the row count in the pivot table.", call. = FALSE)
     }
     if(length(cellColumnNumbers[cellColumnNumbers > self$columnCount])>0) {
       stop("PivotCells$getCells():  All column numbers in cellCoordinates should be less than or equal to the column count in the pivot table.", call. = FALSE)
     }
     # iterate the cells and return
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             if(length(private$p_rows[[r]]) < c) next
             rowMatch <- sum(r==rowNumbers) > 0
             columnMatch <- sum(c==columnNumbers) > 0
             cellMatch <- sum((r==cellRowNumbers)&(c==cellColumnNumbers)) > 0
             if(rowMatch||columnMatch||cellMatch) {
               cell <- private$p_rows[[r]][[c]]
               cells[[length(cells)+1]] <- cell
             }
           }
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getCells", "Got cells.")
     return(invisible(cells))
   },
   findCells = function(variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL,
                        minValue=NULL, maxValue=NULL, exactValues=NULL, includeNull=TRUE, includeNA=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findCells", "Finding cells...")
     matches <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             cell <- private$p_rows[[r]][[c]]
             rowColFilters <- cell$rowColFilters
             # a) check the filter match
             if((!is.null(variableNames))||(!is.null(variableValues))) {
               if(is.null(rowColFilters)) next
               isMatch <- rowColFilters$isFilterMatch(matchMode="combinations", variableNames=variableNames, variableValues=variableValues)
               if(isMatch==FALSE) next
             }
             # b) check totals criteria
             if((totals=="exclude")&&(cell$isTotal==TRUE)) next
             if((totals=="only")&&(cell$isTotal==FALSE)) next
             # c) check calculation criteria
             if(!is.null(calculationNames)) {
               calcName <- cell$calculationName
               if(is.null(calcName)) next
               if(!(calcName %in% calculationNames)) next
             }
             # d) value tests:  is null, NA, minValue, maxValue, exactValues
             if(is.null(cell$rawValue)) {
               if(includeNull==FALSE) next
             }
             else if(length(cell$rawValue)==0) {
               if(includeNull==FALSE) next
             }
             else {
               if(is.na(cell$rawValue)) {
                 if(includeNA==FALSE) next
               }
               else {
                 if((!is.null(minValue))||(!is.null(maxValue))) {
                   cls <- class(cell$rawValue)
                   if(("integer" %in% cls)||("numeric" %in% cls)) {
                     if(!is.null(minValue)) {
                       if(cell$rawValue < minValue) next
                     }
                     if(!is.null(maxValue)) {
                       if(cell$rawValue > maxValue) next
                     }
                   }
                   else next
                 }
                 if(!is.null(exactValues)) {
                   if(!(cell$rawValue %in% exactValues)) next
                 }
               }
             }
             # is a match
             matches[[length(matches)+1]] <- cell
           }
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findCells", "Found cells.")
     return(invisible(matches))
   },
   getColumnWidths = function() {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getColumnWidths", "Getting column widths...")
     widths <- integer(0)
     if((self$rowCount>0)&&(self$columnCount>0)) {
       widths <- integer(self$columnCount)
       for(r in 1:self$rowCount) {
         for(c in 1:length(private$p_rows[[r]])) {
           cell <- private$p_rows[[r]][[c]]
           if(is.null(cell$formattedValue)) next
           if(is.na(cell$formattedValue)) next
           widths[c] <- max(widths[c], nchar(cell$formattedValue))
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getColumnWidths", "Got column widths.")
     return(invisible(widths))
   },
   asMatrix = function(rawValue=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
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
         else if(length(v)==0) v <- NA
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
    p_rows = NULL,
    p_getCellsWarning = FALSE
  )
)
