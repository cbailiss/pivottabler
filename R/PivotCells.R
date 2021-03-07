
#' R6 class that contains the cells in a pivot table.
#'
#' @description
#' The `PivotCells` class contains all of the `PivotCell` instances that
#' comprise the body of a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCells <- R6::R6Class("PivotCells",
  public = list(

   #' @description
   #' Create a new `PivotCells` object.
   #' @param parentPivot The pivot table that this `PivotCells`
   #' instance belongs to.
   #' @return A new `PivotCells` object.
   initialize = function(parentPivot=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCells", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$new", "Creating new PivotCells...")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$new", "Created new PivotCells.")
   },

   #' @description
   #' Remove all cells from the pivot table and reset row and column
   #' counts back to zero.
   #' @return No return value.
   reset = function() {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$resetCells", "Resetting cells...")
     private$p_rowGroups <- NULL
     private$p_columnGroups <- NULL
     private$p_rows <- NULL
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$resetCells", "Reset cells.")
   },

   #' @description
   #' Get the leaf-level data group that is associated with a specific column
   #' or columns in the pivot table.
   #' @param c The column number or numbers.  The first column is column 1,
   #' excluding the column(s) associated with row-headings.
   #' @return A `PivotDataGroup` that is associated with the specified column.
   getColumnGroup = function(c=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getColumnGroup", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
      }
      # multiple columns
      if(length(c)>1) {
         return(invisible(lapply(c, self$getColumnGroup)))
      }
      # single column
      if(c < 1)
         stop(paste0("PivotCells$getColumnGroup(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
      if(c > self$columnCount)
         stop(paste0("PivotCells$getColumnGroup(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
      if(length(private$p_columnGroups) < c) return(invisible(NULL))
      return(invisible(private$p_columnGroups[[c]]))
   },

   #' @description
   #' Get the leaf-level data group that is associated with a specific row
   #' or rows in the pivot table.
   #' @param r The row number or numbers.  The first row is row 1, excluding
   #' the row(s) associated with column-headings.
   #' @return A `PivotDataGroup` that is associated with the specified row
   getRowGroup = function(r=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getRowGroup", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
      }
      # multiple rows
      if(length(r)>1) {
         return(invisible(lapply(r, self$getRowGroup)))
      }
      # single row
      if(r < 1)
         stop(paste0("PivotCells$getRowGroup(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
      if(r > self$rowCount)
         stop(paste0("PivotCells$getRowGroup(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
      if(length(private$p_rowGroups) < r) return(invisible(NULL))
      return(invisible(private$p_rowGroups[[r]]))
   },

   #' @description
   #' An internal method used when building the cell structure
   #' of the pivot table.
   #' @param rowGroups A list of `PivotDataGroup` objects to be set as the leaf-level row groups in the pivot table.
   #' @param columnGroups A list of `PivotDataGroup` objects to be set as the leaf-level column groups in the pivot table.
   #' @return No return value.
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

   #' @description
   #' Get the cell at the specified row and column coordinates in the pivot table.
   #' @details
   #' The row and column numbers refer only to the cells in the body of the pivot
   #' table, i.e. row and column headings are excluded, e.g. row 1 is the first
   #' row of cells underneath the column headings.
   #' @param r Row number of the cell to retrieve.
   #' @param c Column number of the cell to retrieve.
   #' @return A `PivotCell` object representing the cell.
   getCell = function(r=NULL, c=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
     }
     if(r < 1)
       stop(paste0("PivotCells$getCell(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("PivotCells$getCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("PivotCells$getCell(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("PivotCells$getCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     if(length(private$p_rows[[r]]) < c) return(invisible(NULL))
     return(invisible(private$p_rows[[r]][[c]]))
   },

   #' @description
   #' Set the cell at the specified row and column coordinates in the pivot table.
   #' @details
   #' This method is intended for internal package use only, used when building #
   #' the cell structure.
   #' The row and column numbers refer only to the cells in the body of the pivot
   #' table, i.e. row and column headings are excluded, e.g. row 1 is the first
   #' row of cells underneath the column headings.
   #' @param r Row number of the cell to retrieve.
   #' @param c Column number of the cell to retrieve.
   #' @param cell A `PivotCell` object to set into the pivot table cells.
   #' @return No return value.
   setCell = function(r, c, cell) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_rowGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=length(private$p_columnGroups))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     }
     if(r < 1)
       stop(paste0("PivotCells$setCell(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("PivotCells$setCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("PivotCells$setCell(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("PivotCells$setCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     private$p_rows[[r]][[c]] <- cell
     return(invisible())
   },

   #' @description
   #' Retrieve cells by a combination of row and/or column numbers.
   #' See the "Finding and Formatting" vignette for graphical examples.
   #' @details
   #' When `specifyCellsAsList=TRUE` (the default):\cr
   #' Get one or more rows by specifying the row numbers as a vector as
   #' the rowNumbers argument and leaving the columnNumbers argument set
   #' to the default value of `NULL`, or\cr
   #' Get one or more columns by specifying the column numbers as a vector
   #' as the columnNumbers argument and leaving the rowNumbers argument
   #' set to the default value of `NULL`, or\cr
   #' Get one or more individual cells by specifying the cellCoordinates
   #' argument as a list of vectors of length 2, where each element in the
   #' list is the row and column number of one cell,\cr
   #' e.g. `list(c(1, 2), c(3, 4))` specifies two cells, the first located
   #' at row 1, column 2 and the second located at row 3, column 4.\cr
   #' When `specifyCellsAsList=FALSE`:\cr
   #' Get one or more rows by specifying the row numbers as a vector as the
   #' rowNumbers argument and leaving the columnNumbers argument set to the
   #' default value of `NULL`, or\cr
   #' Get one or more columns by specifying the column numbers as a vector
   #' as the columnNumbers argument and leaving the rowNumbers argument set
   #' to the default value of `NULL`, or\cr
   #' Get one or more cells by specifying the row and column numbers as vectors
   #' for the rowNumbers and columnNumbers arguments, or\cr
   #' a mixture of the above, where for entire rows/columns the element in the
   #' other vector is set to `NA`, e.g. to retrieve whole rows, specify the row
   #' numbers as the rowNumbers but set the corresponding elements in the
   #' columnNumbers vector to `NA`.
   #' @param specifyCellsAsList Specify how cells are retrieved.
   #' Default `TRUE`. More information is provided in the details section.
   #' @param rowNumbers A vector of row numbers that specify the rows or
   #' cells to retrieve.
   #' @param columnNumbers A vector of row numbers that specify the columns
   #' or cells to retrieve.
   #' @param cellCoordinates A list of two-element vectors that specify the
   #' coordinates of cells to retrieve.  Ignored when `specifyCellsAsList=FALSE`.
   #' @param excludeEmptyCells Default `FALSE`.  Specify `TRUE` to exclude empty
   #' cells.
   #' @param groups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on either the rows or columns axes.  The cells to be retrieved
   #' must be related to at least one of these groups.
   #' @param rowGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on the rows axis.  The cells to be retrieved must be related to
   #' at least one of these row groups.  If both `rowGroups` and `columnGroups`
   #' are specified, then the cells to be retrieved must be related to at least
   #' one of the specified row groups and one of the specified column groups.
   #' @param columnGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on the columns axis.  The cells to be retrieved must be related to
   #' at least one of these column groups.  If both `rowGroups` and `columnGroups`
   #' are specified, then the cells to be retrieved must be related to at least
   #' one of the specified row groups and one of the specified column groups.
   #' @param matchMode Either "simple" (default) or "combinations"\cr
   #' "simple" specifies that row and column arguments are considered separately
   #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
   #' row 1 and all cells in column 2.\cr
   #' "combinations" specifies that row and column arguments are considered together
   #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
   #' cell single at location (1, 2).\cr
   #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
   #' affected by the match mode.  All other arguments are not.
   #' @return A list of `PivotCell` objects.
   getCells = function(specifyCellsAsList=TRUE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL, excludeEmptyCells=FALSE,
                       groups=NULL, rowGroups=NULL, columnGroups=NULL, matchMode="simple") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", excludeEmptyCells, missing(excludeEmptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", groups, missing(groups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", rowGroups, missing(rowGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", columnGroups, missing(columnGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getCells", "Getting cells...")
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
     # prepare and check the groups
     grpsRowNumbers <- NULL
     grpsColumnNumbers <- NULL
     if(!is.null(groups)) {
        if("PivotDataGroup" %in% class(groups)) groups <- list(groups)
        grpsRowNumbers <- self$findGroupRowNumbers(group=groups, collapse=TRUE)
        grpsColumnNumbers <- self$findGroupColumnNumbers(group=groups, collapse=TRUE)
     }
     rowGrpsRowNumbers <- NULL
     if(!is.null(rowGroups)) {
        if("PivotDataGroup" %in% class(rowGroups)) rowGroups <- list(rowGroups)
        rowGroupTypes <- sapply(rowGroups, function(x) { x$rowOrColumn })
        if(length(rowGroupTypes[rowGroupTypes != "row"])>0) {
           stop("PivotCells$getCells():  All data groups specified as rowGroups must be row data groups.", call. = FALSE)
        }
        rowGrpsRowNumbers <- self$findGroupRowNumbers(group=rowGroups, collapse=TRUE)
     }
     columnGrpsColumnNumbers <- NULL
     if(!is.null(columnGroups)) {
        if("PivotDataGroup" %in% class(columnGroups)) columnGroups <- list(columnGroups)
        columnGroupTypes <- sapply(columnGroups, function(x) { x$rowOrColumn })
        if(length(columnGroupTypes[columnGroupTypes != "column"])>0) {
           stop("PivotCells$getCells():  All data groups specified as columnGroups must be row data groups.", call. = FALSE)
        }
        columnGrpsColumnNumbers <- self$findGroupColumnNumbers(group=columnGroups, collapse=TRUE)
     }
     # compatibility setting
     legacyEmptyCellMatching <- isTRUE(private$p_parentPivot$compatibility$legacyEmptyCellMatching)
     # iterate the cells and return
     cells <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             if(length(private$p_rows[[r]]) < c) next
             rowMatch <- sum(r==rowNumbers) > 0
             columnMatch <- sum(c==columnNumbers) > 0
             cellMatch <- sum((r==cellRowNumbers)&(c==cellColumnNumbers)) > 0
             grpsMatch <- (sum(r==grpsRowNumbers) > 0) || (sum(c==grpsColumnNumbers) > 0)
             grpsRowMatch <- sum(r==rowGrpsRowNumbers) > 0
             grpsColMatch <- sum(c==columnGrpsColumnNumbers) > 0
             isMatch <- FALSE
             if(matchMode=="simple") {
                isMatch <- rowMatch||columnMatch||cellMatch||grpsMatch||grpsRowMatch||grpsColMatch
             }
             else if(matchMode=="combinations") {
                noRowCriteria <- (length(rowNumbers)==0)&&(length(rowGrpsRowNumbers)==0)
                noColCriteria <- (length(columnNumbers)==0)&&(length(columnGrpsColumnNumbers)==0)
                netRowMatch <- noRowCriteria||rowMatch||grpsRowMatch
                netColMatch <- noColCriteria||columnMatch||grpsColMatch
                isMatch <- (netRowMatch&&netColMatch)||cellMatch||grpsMatch
             }
             if(isMatch) {
               cell <- private$p_rows[[r]][[c]]
               cellIsEmpty <- cell$isEmpty
               if(!legacyEmptyCellMatching) {
                  cellIsEmpty <- cellIsEmpty || is.null(cell$rawValue)
               }
               if(excludeEmptyCells && cellIsEmpty) next
               cells[[length(cells)+1]] <- cell
             }
           }
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$getCells", "Got cells.")
     return(invisible(cells))
   },

   #' @description
   #' Find cells matching specified criteria.
   #' See the "Finding and Formatting" vignette for graphical examples.
   #' @param variableNames A character vector specifying the name/names of the
   #' variables to find.  This is useful generally only in pivot tables with
   #' irregular layouts, since in regular pivot tables every cell is related
   #' to every variable.
   #' @param variableValues A list specifying the variable names and values to find,
   #' e.g. `variableValues=list("PowerType"=c("DMU", "HST"))`.\cr
   #' Specify "**" as the variable value to match totals for the specified variable.\cr
   #' Specify "!*" as the variable value to match non-totals for the specified variable.\cr
   #' NB: The totals/non-totals criteria above won’t work when visual totals are used.
   #' @param totals A word that specifies how totals are matched (overrides the finer
   #' settings above) - must be one of "include" (default), "exclude" or "only".
   #' @param calculationNames A character vector specifying the name/names of the
   #' calculations to find.
   #' @param minValue A numerical value specifying a minimum value threshold.
   #' @param maxValue A numerical value specifying a maximum value threshold.
   #' @param exactValues A vector or list specifying a set of allowed values.
   #' @param valueRanges A vector specifying one or more value range expressions which
   #' the cell values must match.  If multiple value range expressions are specified,
   #' then the cell value must match any of one the specified expressions.
   #' @param includeNull Specify TRUE to include `NULL` in the matched cells,
   #' FALSE to exclude `NULL` values.
   #' @param includeNA Specify TRUE to include `NA` in the matched cells,
   #' FALSE to exclude `NA` values.
   #' @param emptyCells A word that specifies how empty cells are matched -
   #' must be one of "include" (default), "exclude" or "only".
   #' @param outlineCells A word that specifies how outline cells are matched -
   #' must be one of "include", "exclude" (default) or "only".
   #' @param rowNumbers A vector of row numbers that specify the rows or
   #' cells to constrain the search.
   #' @param columnNumbers A vector of column numbers that specify the columns
   #' or cells to constrain the search.
   #' @param cellCoordinates A list of two-element vectors that specify the
   #' coordinates of cells to constrain the search.
   #' @param groups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on either the rows or columns axes.  The cells to be searched
   #' must be related to at least one of these groups.
   #' @param rowGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on the rows axis.  The cells to be searched must be related to
   #' at least one of these row groups.  If both `rowGroups` and `columnGroups`
   #' are specified, then the cells to be searched must be related to at least
   #' one of the specified row groups and one of the specified column groups.
   #' @param columnGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
   #' objects on the columns axis.  The cells to be searched must be related to
   #' at least one of these column groups.  If both `rowGroups` and `columnGroups`
   #' are specified, then the cells to be searched must be related to at least
   #' one of the specified row groups and one of the specified column groups.
   #' @param cells A `PivotCell` object or a list of `PivotCell`
   #' objects to constrain the scope of the search.
   #' @param rowColumnMatchMode Either "simple" (default) or "combinations":\cr
   #' "simple" specifies that row and column arguments are considered separately
   #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
   #' row 1 and all cells in column 2.\cr
   #' "combinations" specifies that row and column arguments are considered together
   #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
   #' cell single at location (1, 2).\cr
   #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
   #' affected by the match mode.  All other arguments are not.
   #' @return A list of `PivotCell` objects.
   findCells = function(variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL,
                        minValue=NULL, maxValue=NULL, exactValues=NULL, valueRanges=NULL, includeNull=TRUE, includeNA=TRUE,
                        emptyCells="include", outlineCells="exclude",
                        # additional arguments to constrain cells matched
                        rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL,
                        groups=NULL, rowGroups=NULL, columnGroups=NULL,
                        rowColumnMatchMode="simple", cells=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct", "list"), listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", valueRanges, missing(valueRanges), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", emptyCells, missing(emptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", outlineCells, missing(outlineCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       # additional arguments to constrain cells matched
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", groups, missing(groups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", rowGroups, missing(rowGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", columnGroups, missing(columnGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotCell", "list"), allowedListElementClasses="PivotCell")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findCells", rowColumnMatchMode, missing(rowColumnMatchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findCells", "Finding cells...")
     # if some constraining row/column numbers, data groups or cells are specified, then need to build the constraining lists
     if("PivotDataGroup" %in% class(groups)) groups <- list(groups)
     if("PivotDataGroup" %in% class(rowGroups)) rowGroups <- list(rowGroups)
     if("PivotDataGroup" %in% class(columnGroups)) columnGroups <- list(columnGroups)
     if("PivotCell" %in% class(cells)) cells <- list(cells)
     cellInstanceIds <- NULL
     if((length(rowNumbers)>0)||(length(columnNumbers)>0)|(length(cellCoordinates)>0)||
        (length(groups)>0)||(length(rowGroups)>0)||(length(columnGroups)>0)) {
        exclEmptyCells <- FALSE
        if(emptyCells=="exclude") exclEmptyCells <- TRUE
        constrainingCells <- self$getCells(specifyCellsAsList=TRUE, excludeEmptyCells=exclEmptyCells,
                                           rowNumbers=rowNumbers, columnNumbers=columnNumbers, cellCoordinates=cellCoordinates,
                                           groups=groups, rowGroups=rowGroups, columnGroups=columnGroups, matchMode=rowColumnMatchMode)
        cellInstanceIds <- as.integer(sapply(constrainingCells, function(x) { x$instanceId }))
     }
     if(length(cells)>0) {
        cellInstanceIds <- union(cellInstanceIds, as.integer(sapply(cells, function(x) { x$instanceId })))
     }
     # compatibility setting
     legacyEmptyCellMatching <- isTRUE(private$p_parentPivot$compatibility$legacyEmptyCellMatching)
     # do the searching
     matches <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             cell <- private$p_rows[[r]][[c]]
             # a) check if one of allowed cells
             if(length(cellInstanceIds)>0) {
                if(!(cell$instanceId %in% cellInstanceIds)) next
             }
             # b) check isEmpty
             cellIsEmpty <- cell$isEmpty
             if(!legacyEmptyCellMatching) {
                cellIsEmpty <- cellIsEmpty || is.null(cell$rawValue)
             }
             if((emptyCells=="exclude")&&(cellIsEmpty==TRUE)) next
             if((emptyCells=="only")&&(cellIsEmpty==FALSE)) next
             # c) check isOutline
             if((outlineCells=="exclude")&&(isTRUE(cell$isOutline))) next
             if((outlineCells=="only")&&(!isTRUE(cell$isOutline))) next
             # d) check the filter match
             rowColFilters <- cell$rowColFilters
             if((!is.null(variableNames))||(!is.null(variableValues))) {
               if(is.null(rowColFilters)) next
               isMatch <- rowColFilters$isFilterMatch(matchMode="combinations", variableNames=variableNames, variableValues=variableValues)
               if(isMatch==FALSE) next
             }
             # e) check totals criteria
             if((totals=="exclude")&&(cell$isTotal==TRUE)) next
             if((totals=="only")&&(cell$isTotal==FALSE)) next
             # f) check calculation criteria
             if(!is.null(calculationNames)) {
               calcName <- cell$calculationName
               if(is.null(calcName)) next
               if(!(calcName %in% calculationNames)) next
             }
             # g) value tests:  is null, NA, minValue, maxValue, exactValues
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
             # h) value range expressions
             if(length(valueRanges)>0) {
                vreMatch <- FALSE
                for (vre in valueRanges) {
                   if(vreIsMatch(vre, cell$rawValue)) {
                      vreMatch <- TRUE
                      break
                   }
                }
                if(!vreMatch) next
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

   #' @description
   #' Find the column numbers associated with a specific data group or groups.
   #' @param group A `PivotDataGroup` in the column data groups (i.e. a
   #' column heading)  or a list of column data groups..
   #' @param collapse A logical value specifying whether the return value should be
   #' simplified.  See details.
   #' @details
   #' If `group` is a list:  If `collapse` is `FALSE`, then a list of vectors is
   #' returned, if `collapse` is `TRUE`, then a single combined vector is returned.
   #' @return Either a vector of column numbers related to the single specified group
   #' or a list of vectors containing column numbers related to the specified groups.
   findGroupColumnNumbers = function(group=NULL, collapse=FALSE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findGroupColumnNumbers", group, missing(group), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findGroupColumnNumbers", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple groups
      if("list" %in% class(group)) {
         fx <- function(x) { return(self$findGroupColumnNumbers(group=x)) }
         if(isTRUE(collapse)) return(invisible(unique(unlist(lapply(group, fx)))))
         else return(invisible(lapply(group, fx)))
      }
      # single group
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findGroupColumnNumbers", "Finding group column numbers...")
      # get lowest level of groups
      grps <- group$getLeafGroups()
      # get instance ids
      fx <- function(x) { return(x$instanceId) }
      instanceIds <- sapply(grps, fx)
      # find matching columns
      matchingColumnNumbers <- vector("integer", 0)
      if(length(private$p_columnGroups) > 0) {
         for(c in 1:length(private$p_columnGroups)) {
            instanceId <- private$p_columnGroups[[c]]$instanceId
            if(instanceId %in% instanceIds) {
               matchingColumnNumbers[[length(matchingColumnNumbers)+1]] <- c
            }
         }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findGroupColumnNumbers", "Found group column numbers.")
      return(invisible(matchingColumnNumbers))
   },

   #' @description
   #' Find the row numbers associated with a specific data group or groups.
   #' @param group A `PivotDataGroup` in the row data groups (i.e. a
   #' row heading) or a list of row data groups.
   #' @param collapse A logical value specifying whether the return value should be
   #' simplified.  See details.
   #' @details
   #' If `group` is a list:  If `collapse` is `FALSE`, then a list of vectors is
   #' returned, if `collapse` is `TRUE`, then a single combined vector is returned.
   #' @return Either a vector of row numbers related to the single specified group
   #' or a list of vectors containing row numbers related to the specified groups.
   findGroupRowNumbers = function(group=NULL, collapse=FALSE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findGroupRowNumbers", group, missing(group), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "findGroupRowNumbers", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple groups
      if("list" %in% class(group)) {
         fx <- function(x) { return(self$findGroupRowNumbers(group=x)) }
         if(isTRUE(collapse)) return(invisible(unique(unlist(lapply(group, fx)))))
         else return(invisible(lapply(group, fx)))
      }
      # single group
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findGroupRowNumbers", "Finding group row numbers...")
      # get lowest level of groups
      grps <- group$getLeafGroups()
      # get instance ids
      fx <- function(x) { return(x$instanceId) }
      instanceIds <- sapply(grps, fx)
      # find matching rows
      matchingRowNumbers <- vector("integer", 0)
      if(length(private$p_rowGroups) > 0) {
         for(r in 1:length(private$p_rowGroups)) {
            instanceId <- private$p_rowGroups[[r]]$instanceId
            if(instanceId %in% instanceIds) {
               matchingRowNumbers[[length(matchingRowNumbers)+1]] <- r
            }
         }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$findGroupRowNumbers", "Found group row numbers.")
      return(invisible(matchingRowNumbers))
   },

   #' @description
   #' Retrieve the width (in characters) of the longest value in each column.
   #' @return A vector containing the length of the longest value in each column.
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

   #' @description
   #' Remove a column from the pivot table.
   #' @details
   #' This method removes both the related column group and cells.
   #' @param c The column number.  The first column is column 1, excluding the
   #' column(s) associated with row-headings.
   #' @param renumberGroups `TRUE` (default) to renumber the `rowColumnNumber`
   #' property of the data groups after removing the row.
   #' @return No return value.
   removeColumn = function(c=NULL, renumberGroups=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeColumn", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeColumn", renumberGroups, missing(renumberGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeColumn", "Removing column...")
      # checks
      if(c < 1)
         stop(paste0("PivotCells$removeColumn(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
      if(c > self$columnCount)
         stop(paste0("PivotCells$removeColumn(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
      # remove column
      private$p_columnGroups[[c]]$removeGroup(removeAncestorsIfNoRemainingChildren=TRUE, resetCells=FALSE)
      private$p_columnGroups[[c]] <- NULL
      for(r in 1:self$rowCount) {
         private$p_rows[[r]][[c]] <- NULL
      }
      if(renumberGroups==TRUE) {
         private$renumberColumnGroups()
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeColumn", "Removed column.")
      return(invisible())
   },

   #' @description
   #' Remove multiple column from the pivot table.
   #' @details
   #' This method removes both the related column groups and cells.
   #' @param columnNumbers The column numbers.  The first column is column 1, excluding the
   #' column(s) associated with row-headings.
   #' @param renumberGroups `TRUE` (default) to renumber the `rowColumnNumber`
   #' property of the data groups after removing the row.
   #' @return No return value.
   removeColumns = function(columnNumbers=NULL, renumberGroups=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeColumns", columnNumbers, missing(columnNumbers), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeColumns", renumberGroups, missing(renumberGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeColumns", "Removing columns...")
      # sort the column numbers into descending order (because otherwise removing a column at the top shifts the column numbers of those to the right)
      descColumnNumbers <- columnNumbers[order(-columnNumbers)]
      # iterate and remove
      invisible(lapply(descColumnNumbers, self$removeColumn, renumberGroups=FALSE))
      if(renumberGroups==TRUE) {
         private$renumberColumnGroups()
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeColumns", "Removed columns.")
      return(invisible())
   },

   #' @description
   #' Remove a row from the pivot table.
   #' @details
   #' This method removes both the related row group and cells.
   #' @param r The row number.  The first row is row 1, excluding the
   #' row(s) associated with column-headings.
   #' @param renumberGroups `TRUE` (default) to renumber the `rowColumnNumber`
   #' property of the data groups after removing the row.
   #' @return No return value.
   removeRow = function(r=NULL, renumberGroups=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeRow", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeRow", renumberGroups, missing(renumberGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeRow", "Removing row...")
      # checks
      if(r < 1)
         stop(paste0("PivotCells$removeRow(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
      if(r > self$rowCount)
         stop(paste0("PivotCells$removeRow(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
      # remove row
      private$p_rowGroups[[r]]$removeGroup(removeAncestorsIfNoRemainingChildren=TRUE, resetCells=FALSE)
      private$p_rowGroups[[r]] <- NULL
      private$p_rows[[r]] <- NULL
      if(renumberGroups==TRUE) {
         private$renumberRowGroups()
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeRow", "Removed row.")
      return(invisible())
   },

   #' @description
   #' Remove multiple rows from the pivot table.
   #' @details
   #' This method removes both the related row groups and cells.
   #' @param rowNumbers The row numbers.  The first row is row 1, excluding the
   #' rows(s) associated with column-headings.
   #' @param renumberGroups `TRUE` (default) to renumber the `rowColumnNumber`
   #' property of the data groups after removing the row.
   #' @return No return value.
   removeRows = function(rowNumbers=NULL, renumberGroups=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeRows", rowNumbers, missing(rowNumbers), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "removeRows", renumberGroups, missing(renumberGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeRows", "Removing rows...")
      # sort the row numbers into descending order (because otherwise removing a row at the top shifts the row numbers of those below)
      descRowNumbers <- rowNumbers[order(-rowNumbers)]
      # iterate and remove
      invisible(lapply(descRowNumbers, self$removeRow, renumberGroups=FALSE))
      if(renumberGroups==TRUE) {
         private$renumberRowGroups()
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCells$removeRows", "Removed rows.")
      return(invisible())
   },

   #' @description
   #' Get a matrix containing all of the values from the body of the pivot table.
   #' @param rawValue `TRUE` (default) to populate the matrix with the numerical
   #' raw values, `FALSE` to populate the matrix with the character formatted values.
   #' @return A `matrix` containing the values from the body of the pivot table.
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

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
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

   #' @field rowCount The number of rows in the pivot table (excluding column headings).
   rowCount = function(value) { return(invisible(length(private$p_rowGroups))) },

   #' @field columnCount The number of columns in the pivot table (excluding column headings).
   columnCount = function(value) { return(invisible(length(private$p_columnGroups))) },

   #' @field rowGroups A list of the leaf-level data groups on the rows axis.
   rowGroups = function(value) { return(invisible(private$p_rowGroups)) },

   #' @field columnGroups A list of the leaf-level data groups on the columns axis.
   columnGroups = function(value) { return(invisible(private$p_columnGroups)) },

   #' @field rows A list of the rows in the pivot table.  Each element in this list is
   #' a list of `PivotCell` objects comprising the row.
   rows = function(value) { return(invisible(private$p_rows)) },

   #' @field all A list of the cells in the pivot table.  Each element in this list is
   #' a `PivotCell` object.
   all = function(value) { return(self$getCells(rowNumbers=1:self$rowCount)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_rowGroups = NULL,
    p_columnGroups = NULL,
    p_rows = NULL,
    # after rows or columns have been removed, the rowColumnNumber property on the data groups will be wrong
    # these two functions can reset them
    renumberColumnGroups = function() {
       if((!is.null(private$p_columnGroups))&&(length(private$p_columnGroups)>0)) {
          for(c in 1:length(private$p_columnGroups)) {
             private$p_columnGroups[[c]]$rowColumnNumber <- c
          }
       }
    },
    renumberRowGroups = function() {
       if((!is.null(private$p_rowGroups))&&(length(private$p_rowGroups)>0)) {
          for(r in 1:length(private$p_rowGroups)) {
             private$p_rowGroups[[r]]$rowColumnNumber <- r
          }
       }
    }
  )
)
