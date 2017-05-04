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
#'   \item{\code{getCells = function(rowNumbers=NULL,
#'   columnNumbers=NULL)}}{Retrieve cells by a combination of row and/or column
#'   numbers.}
#'   \item{\code{findCells(variableNames=NULL, variableValues=NULL,
#'   totals="include", calculationNames=NULL, minValue=NULL, maxValue=NULL,
#'   exactValues=NULL, includeNull=TRUE, includeNA=TRUE)}}{Find cells matching
#'   the specified criteria.}
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
     private$p_parentPivot$message("PivotCells$new", "Creating new PivotCells...")
     private$p_parentPivot$message("PivotCells$new", "Created new PivotCells.")
   },
   reset = function() {
     private$p_parentPivot$message("PivotCells$resetCells", "Resetting cells...")
     private$p_rowGroups <- NULL
     private$p_columnGroups <- NULL
     private$p_rows <- NULL
     private$p_parentPivot$message("PivotCells$resetCells", "Reset cells.")
   },
   setGroups = function(rowGroups=NULL, columnGroups=NULL) {
     private$p_parentPivot$message("PivotCells$setGroups", "Creating new PivotCells...",
                                   list(rowCount=length(rowGroups), columnCount=length(columnGroups)))
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setGroups", rowGroups, missing(rowGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "setGroups", columnGroups, missing(columnGroups), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     }
     private$p_rowGroups <- rowGroups
     private$p_columnGroups <- columnGroups
     private$p_rows <- list() # a list of rows, each containing a list of values in the row
     for(r in 1:length(rowGroups)) {
       private$p_rows[[r]] <- list()
     }
     private$p_parentPivot$message("PivotCells$setGroups", "Created new PivotCells.")
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
   getCells = function(rowNumbers=NULL, columnNumbers=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCells", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      private$p_parentPivot$message("PivotCells$getCells", "Getting cells...")
      # if no rows or columns specified, return all cells
      cells <- list()
      if(is.null(rowNumbers)&&is.null(columnNumbers)) {
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
        private$p_parentPivot$message("PivotCells$getCells", "Got cells.")
        return(invisible(cells))
      }
      # rows but not columns
      if((!is.null(rowNumbers))&&(is.null(columnNumbers))) {
        for(i in 1:length(rowNumbers)) {
          r <- rowNumbers[i]
          if(r < 1)
            stop(paste0("PivotCells$getCells(): rowNumber (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
          if(r > self$rowCount)
            stop(paste0("PivotCells$getCells(): rowNumber (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
          if(length(private$p_rows[[r]]) > 0) {
            for(c in 1:length(private$p_rows[[r]])) {
              if(length(private$p_rows[[r]]) < c) next
              cell <- private$p_rows[[r]][[c]]
              cells[[length(cells)+1]] <- cell
            }
          }
        }
        private$p_parentPivot$message("PivotCells$getCells", "Got cells.")
        return(invisible(cells))
      }
      # columns but not rows
      if((is.null(rowNumbers))&&(!is.null(columnNumbers))) {
        for(i in 1:length(columnNumbers)) {
          c <- columnNumbers[i]
          if(c < 1)
            stop(paste0("PivotCells$getCells(): columnNumber (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
          if(c > self$columnCount)
            stop(paste0("PivotCells$getCells(): columnNumber (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
          if(length(private$p_rows) > 0) {
            for(r in 1:length(private$p_rows)) {
              if(length(private$p_rows[[r]]) > 0) {
                if(c <= length(private$p_rows[[r]])) {
                  if(length(private$p_rows[[r]]) < c) next
                  cell <- private$p_rows[[r]][[c]]
                  cells[[length(cells)+1]] <- cell
                }
              }
            }
          }
        }
        private$p_parentPivot$message("PivotCells$getCells", "Got cells.")
        return(invisible(cells))
      }
      # lengths of the two arguments must be equal
      if(length(rowNumbers)!=length(columnNumbers)) {
        stop(paste0("PivotCells$getCells(): The lengths of the rowNumbers and columnNumbers vectors should be equal (or one/both vectors should be NULL)."), call. = FALSE)
      }
      if(length(rowNumbers)==0) {
        private$p_parentPivot$message("PivotCells$getCells", "Got cells.")
        return(invisible(cells))
      }
      for(i in 1:length(rowNumbers)) {
        r <- rowNumbers[i]
        c <- columnNumbers[i]
        # both NA (ignored)
        if(is.na(r)&&is.na(c)) next
        # whole row?
        if(is.na(c)) {
          if(r < 1)
            stop(paste0("PivotCells$getCells(): rowNumber (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
          if(r > self$rowCount)
            stop(paste0("PivotCells$getCells(): rowNumber (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
          if(length(private$p_rows[[r]]) > 0) {
            for(c in 1:length(private$p_rows[[r]])) {
              if(length(private$p_rows[[r]]) < c) next
              cell <- private$p_rows[[r]][[c]]
              cells[[length(cells)+1]] <- cell
            }
          }
          next
        }
        # whole column?
        if(is.na(r)) {
          if(c < 1)
            stop(paste0("PivotCells$getCells(): columnNumber (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
          if(c > self$columnCount)
            stop(paste0("PivotCells$getCells(): columnNumber (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
          if(length(private$p_rows) > 0) {
            for(r in 1:length(private$p_rows)) {
              if(length(private$p_rows[[r]]) > 0) {
                if(c <= length(private$p_rows[[r]])) {
                  if(length(private$p_rows[[r]]) < c) next
                  cell <- private$p_rows[[r]][[c]]
                  cells[[length(cells)+1]] <- cell
                }
              }
            }
          }
          next
        }
        # specific cell
        if(length(private$p_rows[[r]]) < c) next
        cell <- private$p_rows[[r]][[c]]
        cells[[length(cells)+1]] <- cell
      }
      private$p_parentPivot$message("PivotCells$getCells", "Got cells.")
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
     private$p_parentPivot$message("PivotCells$findCells", "Finding cells...")
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
     private$p_parentPivot$message("PivotCells$findCells", "Found cells.")
     return(invisible(matches))
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
    p_rows = NULL
  )
)
