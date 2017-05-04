#' A class that renders a pivot table in Latex.
#'
#' The PivotLatexRenderer class creates a Latex representation of a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmltools
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   render to Latex.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot table renderer, specifying the
#'   field value documented above.}
#'
#'   \item{\code{clearIsRenderedFlags()}}{Clear the IsRendered flags that exist
#'   on the PivotDataGroup class.}
#'   \item{\code{resetVisibleRange()}}{Clears the visible range that has been
#'   set, so the next call to getTableLatex() will render the whole table.}
#'   \item{\code{setVisibleRange(fromRow=NULL, toRow=NULL, fromColumn=NULL,
#'   toColumn=NULL)}}{Specifies a subset of the pivot table to be rendered, e.g.
#'   for use when a pivot table will not fit into a single A4 page.}
#'   \item{\code{clearIsRenderedFlags()}}{}
#'   \item{\code{getTableLatex = function(caption=NULL, label=NULL,
#'   boldHeadings=FALSE, italicHeadings=FALSE))}}{Get a Latex representation of
#'   the pivot table, specifying the caption to appear above the table, the
#'   label to use when referring to the table elsewhere in the document and how
#'   headings should be styled.}
#' }
#'
PivotLatexRenderer <- R6::R6Class("PivotLatexRenderer",
  public = list(
    initialize = function(parentPivot=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotLatexRenderer$new", "Creating new Latex Renderer...")
      private$p_parentPivot$message("PivotLatexRenderer$new", "Created new Latex Renderer.")
    },
    clearIsRenderedFlags = function() {
      private$p_parentPivot$message("PivotLatexRenderer$clearIsRenderedFlags", "Clearing isRendered flags...")
      clearFlags <- function(dg) {
        dg$isRendered <- FALSE
      }
      rowGroups <- private$p_parentPivot$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(rowGroups, clearFlags)
      columnGroups <- private$p_parentPivot$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(columnGroups, clearFlags)
      private$p_parentPivot$message("PivotLatexRenderer$clearIsRenderedFlags", "Cleared isRendered flags...")
      return(invisible())
    },
    resetVisibleRange = function() {
      private$p_parentPivot$message("PivotLatexRenderer$resetVisibleRange", "Resetting visible range...")
      private$p_fromRow <- NULL
      private$p_toRow <- NULL
      private$p_fromColumn <- NULL
      private$p_toColumn <- NULL
      private$p_parentPivot$message("PivotLatexRenderer$resetVisibleRange", "Reset visible range.")
      return(invisible())
    },
    setVisibleRange = function(fromRow=NULL, toRow=NULL, fromColumn=NULL, toColumn=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "setVisibleRange", fromRow, missing(fromRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "setVisibleRange", toRow, missing(toRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "setVisibleRange", fromColumn, missing(fromColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "setVisibleRange", toColumn, missing(toColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
      }
      private$p_parentPivot$message("PivotLatexRenderer$setVisibleRange", "Setting visible range...",
                                    list(fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn))
      # references
      topRowGroup <- private$p_parentPivot$rowGroup
      topColumnGroup <- private$p_parentPivot$columnGroup
      cells <- private$p_parentPivot$cells
      # get the data groups:  these are the leaf level groups
      rowGroups <- private$p_parentPivot$cells$rowGroups
      columnGroups <- private$p_parentPivot$cells$columnGroups
      # ranges
      if(is.null(fromRow)) fromRow <- 1
      if(is.null(toRow)) toRow <- cells$rowCount
      if(is.null(fromColumn)) fromColumn <- 1
      if(is.null(toColumn)) toColumn <- cells$columnCount
      # check the ranges
      if(toRow < fromRow) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromRow (", fromRow, ") must be less than or equal to toRow (", toRow, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      if(toColumn < fromColumn) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromColumn (", fromColumn, ") must be less than or equal to toColumn (", toColumn, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      if(fromRow > cells$rowCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromRow (", fromRow, ") must be less than or equal to the number of rows (", cells$rowCount, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      if(toRow > cells$rowCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): toRow (", toRow, ") must be less than or equal to the number of rows (", cells$rowCount, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      if(fromColumn > cells$columnCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromColumn (", fromColumn, ") must be less than or equal to the number of columns (", cells$columnCount, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      if(toColumn > cells$columnCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): toColumn (", toColumn, ") must be less than or equal to the number of columns (", cells$columnCount, ")", call. = FALSE)
        stop(errorMsg, call. = FALSE)
      }
      # save the ranges
      private$p_fromRow <- fromRow
      private$p_toRow <- toRow
      private$p_fromColumn <- fromColumn
      private$p_toColumn <- toColumn
      # clear the existing visible range
      clearFlags <- function(dg) {
        dg$isWithinVisibleRange <- FALSE
      }
      grps <- private$p_parentPivot$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(grps, clearFlags)
      grps <- private$p_parentPivot$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(grps, clearFlags)
      # set new visible rows
      for(r in fromRow:toRow) {
        grp <- rowGroups[[r]]
        while(!is.null(grp)) {
          grp$isWithinVisibleRange <- TRUE
          grp <- grp$parentGroup
        }
      }
      # set new visible columns
      for(c in fromColumn:toColumn) {
        grp <- columnGroups[[c]]
        while(!is.null(grp)) {
          grp$isWithinVisibleRange <- TRUE
          grp <- grp$parentGroup
        }
      }
      private$p_rangesSet <- TRUE
      private$p_parentPivot$message("PivotLatexRenderer$setVisibleRange", "Set visible range.")
      return(invisible())
    },
    getTableLatex = function(caption=NULL, label=NULL, boldHeadings=FALSE, italicHeadings=FALSE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      private$p_parentPivot$message("PivotLatexRenderer$getTableLatex", "Getting table Latex...")
      # reset rendered flags
      self$clearIsRenderedFlags()
      # get the dimensions of the various parts of the table...
      # ...headings:
      rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
      columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
      # special case: "no data"
      if((rowGroupLevelCount==0)&&(columnGroupLevelCount==0)) {
        ltx <- list()
        ltx[[length(ltx)+1]] <- "\\begin{table}[h!]"
        ltx[[length(ltx)+1]] <- "  \\centering"
        if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\caption{", private$getSafeString(caption), "}")
        if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\label{tab:", private$getSafeString(label), "}")
        ltx[[length(ltx)+1]] <- "  \\begin{tabular}{|l|}"
        ltx[[length(ltx)+1]] <- "    \\hline"
        ltx[[length(ltx)+1]] <- "    (no data)\\\\"
        ltx[[length(ltx)+1]] <- "    \\hline"
        ltx[[length(ltx)+1]] <- "  \\end{tabular}"
        ltx[[length(ltx)+1]] <- "\\end{table}"
        ltx <- paste(ltx, sep = '', collapse = '\n')
        return(invisible(ltx))
      }
      # there must always be at least one row and one column
      insertDummyRowHeading <- (rowGroupLevelCount==0) & (columnGroupLevelCount > 0)
      insertDummyColumnHeading <- (columnGroupLevelCount==0) & (rowGroupLevelCount > 0)
      # run the appropriate function
      if(insertDummyRowHeading)
        ltx <- private$getTableLatex1Row(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings)
      else if(insertDummyColumnHeading)
        ltx <- private$getTableLatex1Column(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings)
      else
        ltx <- private$getTableLatexNormal(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings)
      return(invisible(ltx))
    }
 ),
 private = list(
   p_parentPivot = NULL,
   p_rangesSet = FALSE,
   p_fromRow = NULL,
   p_toRow = NULL,
   p_fromColumn = NULL,
   p_toColumn = NULL,
   getSafeString = function(str) {
     gsub('([#$%&~_\\^\\\\{}])', '\\\\\\1', str, perl = TRUE);
   },
   getTableLatex1Row = function(caption=NULL, label=NULL, boldHeadings=FALSE, italicHeadings=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Row", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Row", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Row", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Row", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatex1Row", "Getting table Latex...")
     # references
     topRowGroup <- private$p_parentPivot$rowGroup
     topColumnGroup <- private$p_parentPivot$columnGroup
     cells <- private$p_parentPivot$cells
     # get the dimensions of the various parts of the table...
     # ...headings:
     columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
     # ...cells:
     rowCount <- cells$rowCount
     columnCount <- cells$columnCount
     # get the data groups:  these are the leaf level groups
     rowGroups <- private$p_parentPivot$cells$rowGroups
     columnGroups <- private$p_parentPivot$cells$columnGroups
     # ranges
     if(private$p_rangesSet==FALSE) {
       self$setVisibleRange()
     }
     fromRow <- private$p_fromRow
     toRow <- private$p_toRow
     fromColumn <- private$p_fromColumn
     toColumn <- private$p_toColumn
     # heading prefix and suffix
     headingPrefix <- ""
     headingSuffix <- ""
     if(boldHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textbf{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     if(italicHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textit{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     # build the pivot
     ltx <- list()
     ltx[[length(ltx)+1]] <- "\\begin{table}[h!]"
     ltx[[length(ltx)+1]] <- "  \\centering"
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\caption{", private$getSafeString(caption), "}")
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\label{tab:", private$getSafeString(label), "}")
     # column alignments in the body of the table
     # \begin{tabular}{|l|l|lcr|lcr|lcr|} - first few columns cover the row headings, the remainder cover the data cells
     s <- "  \\begin{tabular}{|l|" # now generate a dividing line between each parent group
     lastParentGrp <- NULL
     for(c in fromColumn:toColumn) {
       grp <- columnGroups[[c]]
       parentGrp <- grp$parentGroup
       if(!is.null(lastParentGrp)) {
         if(!identical(parentGrp, lastParentGrp)) s <- paste0(s, "|")
       }
       lastParentGrp <- parentGrp
       s <- paste0(s, "r")
     }
     s <- paste0(s, "|}")
     ltx[[length(ltx)+1]] <- s
     # top line of the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     # column groups (i.e. column headings)
     for(r in 1:columnGroupLevelCount) {
       # if not the first set of column groups, then add a horizontal line
       if(r > 1) {
         s <- paste0("    \\cline{", 2, "-", (1 + toColumn - fromColumn + 1), "}")
         ltx[[length(ltx)+1]] <- s
       }
       #     \multicolumn{2}{|c|}{} & \multicolumn{3}{|c|}{n} & \multicolumn{3}{|c|}{m} & \multicolumn{3}{|c|}{e} \\
       s <- ""
       s <- "    & "
       # get the groups at this level
       grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r)
       firstColumn <- TRUE
       for(c in 1:length(grps)) {
         grp <- grps[[c]]
         if(!grp$isWithinVisibleRange) next
         if(firstColumn==TRUE) firstColumn <- FALSE
         else s <- paste0(s, " & ")
         leafGroupCount <- grp$visibleLeafGroupCount
         if(leafGroupCount > 1) {
           s <- paste0(s, "\\multicolumn{", leafGroupCount, "}{|c|}{", headingPrefix, private$getSafeString(grp$caption), headingSuffix, "}")
         }
         else {
           s <- paste0(s, headingPrefix, private$getSafeString(grp$caption), headingSuffix)
         }
       }
       s <- paste0(s, "\\\\")
       ltx[[length(ltx)+1]] <- s
     }
     # a line underneath the headings
     ltx[[length(ltx)+1]] <- "    \\hline"
     # output the row headings and data values
     lastParentGrp <- NULL
     for(r in fromRow:toRow) {
       # render this line
       # \multirow{3}{*}{p} & x & 1 & 2 & 3 & 1 & 2 & 3 & 1 & 2 & 3\\
       s <- "    "
       # now render the data values
       for(c in fromColumn:toColumn) {
         s <- paste0(s, " & ")
         cell <- cells$getCell(r, c)
         s <- paste0(s, private$getSafeString(cell$formattedValue))
       }
       # finish the line
       s <- paste0(s, "\\\\")
       ltx[[length(ltx)+1]] <- s
     }
     # finish the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     ltx[[length(ltx)+1]] <- "  \\end{tabular}"
     ltx[[length(ltx)+1]] <- "\\end{table}"
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatex1Row", "Got table Latex.")
     ltx <- paste(ltx, sep = '', collapse = '\n')
     return(invisible(ltx))
   },
   getTableLatex1Column = function(caption=NULL, label=NULL, boldHeadings=FALSE, italicHeadings=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Column", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Column", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Column", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatex1Column", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatex1Column", "Getting table Latex...")
     # references
     topRowGroup <- private$p_parentPivot$rowGroup
     topColumnGroup <- private$p_parentPivot$columnGroup
     cells <- private$p_parentPivot$cells
     # get the dimensions of the various parts of the table...
     # ...headings:
     rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
     # ...cells:
     rowCount <- cells$rowCount
     columnCount <- cells$columnCount
     # get the data groups:  these are the leaf level groups
     rowGroups <- private$p_parentPivot$cells$rowGroups
     columnGroups <- private$p_parentPivot$cells$columnGroups
     # ranges
     if(private$p_rangesSet==FALSE) {
       self$setVisibleRange()
     }
     fromRow <- private$p_fromRow
     toRow <- private$p_toRow
     fromColumn <- private$p_fromColumn
     toColumn <- private$p_toColumn
     # heading prefix and suffix
     headingPrefix <- ""
     headingSuffix <- ""
     if(boldHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textbf{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     if(italicHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textit{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     # build the pivot
     ltx <- list()
     ltx[[length(ltx)+1]] <- "\\begin{table}[h!]"
     ltx[[length(ltx)+1]] <- "  \\centering"
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\caption{", private$getSafeString(caption), "}")
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\label{tab:", private$getSafeString(label), "}")
     # column alignments in the body of the table
     # \begin{tabular}{|l|l|lcr|lcr|lcr|} - first few columns cover the row headings, the remainder cover the data cells
     s <- "  \\begin{tabular}{|" # now generate a dividing line between each parent group
     s <- paste0(s, paste(rep("l|", rowGroupLevelCount), sep = '', collapse = ''))
     lastParentGrp <- NULL
     for(c in fromColumn:toColumn) {
       grp <- columnGroups[[c]]
       parentGrp <- grp$parentGroup
       if(!is.null(lastParentGrp)) {
         if(!identical(parentGrp, lastParentGrp)) s <- paste0(s, "|")
       }
       lastParentGrp <- parentGrp
       s <- paste0(s, "r")
     }
     s <- paste0(s, "|}")
     ltx[[length(ltx)+1]] <- s
     # top line of the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     # column group (i.e. one fake column heading)
     #     \multicolumn{2}{|c|}{} & \multicolumn{3}{|c|}{n} & \multicolumn{3}{|c|}{m} & \multicolumn{3}{|c|}{e} \\
     s <- ""
     if(rowGroupLevelCount==1) s <- "    & "
     else s <- paste0("    \\multicolumn{", rowGroupLevelCount, "}{|c|}{} & ")
     s <- paste0(s, "\\\\")
     ltx[[length(ltx)+1]] <- s
     # a line underneath the headings
     ltx[[length(ltx)+1]] <- "    \\hline"
     # output the row headings and data values
     lastParentGrp <- NULL
     for(r in fromRow:toRow) {
       rg <- rowGroups[[r]]
       parentGrp <- rg$parentGroup
       # draw a partial horizontal line before this row?
       if(!is.null(lastParentGrp)) {
         if(!identical(parentGrp, lastParentGrp)) {
           # need to work out how where to draw the line from
           # get the ancestors of this and the previous group and compare
           prevAncs <- rowGroups[[r-1]]$getAncestorGroups(includeCurrentGroup=TRUE)
           thisAncs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
           startLineFromColumn <- 0
           for(i in 1:length(prevAncs)) { # length(ancrgs)-1):1
             if(!identical(prevAncs[[length(ancrgs)-i+1]], thisAncs[[length(ancrgs)-i+1]])) {
               startLineFromColumn <- i
               break
             }
           }
           s <- paste0("    \\cline{", startLineFromColumn-1, "-", (rowGroupLevelCount + toColumn - fromColumn + 1), "}")
           ltx[[length(ltx)+1]] <- s
         }
       }
       lastParentGrp <- parentGrp
       # render this line
       # \multirow{3}{*}{p} & x & 1 & 2 & 3 & 1 & 2 & 3 & 1 & 2 & 3\\
       s <- "    "
       # first render the row headings
       ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
       firstColumn <- TRUE
       for(c in (length(ancrgs)-1):1) {
         ancg <- ancrgs[[c]]
         if(firstColumn) firstColumn <- FALSE
         else s <- paste0(s, "& ")
         if(ancg$isRendered==FALSE) {
           leafGroupCount <- ancg$visibleLeafGroupCount
           if(leafGroupCount > 1) {
             s <- paste0(s, "\\multirow{", leafGroupCount, "}{*}{", headingPrefix, private$getSafeString(ancg$caption), headingSuffix, "} ")
           }
           else {
             s <- paste0(s, headingPrefix, private$getSafeString(ancg$caption), headingSuffix, " ")
           }
           ancg$isRendered <- TRUE
         }
       }
       # now render the data values
       for(c in fromColumn:toColumn) {
         s <- paste0(s, " & ")
         cell <- cells$getCell(r, c)
         s <- paste0(s, private$getSafeString(cell$formattedValue))
       }
       # finish the line
       s <- paste0(s, "\\\\")
       ltx[[length(ltx)+1]] <- s
     }
     # finish the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     ltx[[length(ltx)+1]] <- "  \\end{tabular}"
     ltx[[length(ltx)+1]] <- "\\end{table}"
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatex1Column", "Got table Latex.")
     ltx <- paste(ltx, sep = '', collapse = '\n')
     return(invisible(ltx))
   },
   getTableLatexNormal = function(caption=NULL, label=NULL, boldHeadings=FALSE, italicHeadings=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatexNormal", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatexNormal", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatexNormal", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotLatexRenderer", "getTableLatexNormal", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatexNormal", "Getting table Latex...")
     # references
     topRowGroup <- private$p_parentPivot$rowGroup
     topColumnGroup <- private$p_parentPivot$columnGroup
     cells <- private$p_parentPivot$cells
     # get the dimensions of the various parts of the table...
     # ...headings:
     rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
     columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
     # ...cells:
     rowCount <- cells$rowCount
     columnCount <- cells$columnCount
     # get the data groups:  these are the leaf level groups
     rowGroups <- private$p_parentPivot$cells$rowGroups
     columnGroups <- private$p_parentPivot$cells$columnGroups
     # ranges
     if(private$p_rangesSet==FALSE) {
       self$setVisibleRange()
     }
     fromRow <- private$p_fromRow
     toRow <- private$p_toRow
     fromColumn <- private$p_fromColumn
     toColumn <- private$p_toColumn
     # heading prefix and suffix
     headingPrefix <- ""
     headingSuffix <- ""
     if(boldHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textbf{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     if(italicHeadings) {
       headingPrefix <- paste0(headingPrefix, "\\textit{")
       headingSuffix <- paste0(headingSuffix, "}")
     }
     # build the pivot
     ltx <- list()
     ltx[[length(ltx)+1]] <- "\\begin{table}[h!]"
     ltx[[length(ltx)+1]] <- "  \\centering"
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\caption{", private$getSafeString(caption), "}")
     if(!is.null(caption)) ltx[[length(ltx)+1]] <- paste0("  \\label{tab:", private$getSafeString(label), "}")
     # column alignments in the body of the table
     # \begin{tabular}{|l|l|lcr|lcr|lcr|} - first few columns cover the row headings, the remainder cover the data cells
     s <- "  \\begin{tabular}{|" # now generate a dividing line between each parent group
     s <- paste0(s, paste(rep("l|", rowGroupLevelCount), sep = '', collapse = ''))
     lastParentGrp <- NULL
     for(c in fromColumn:toColumn) {
       grp <- columnGroups[[c]]
       parentGrp <- grp$parentGroup
       if(!is.null(lastParentGrp)) {
         if(!identical(parentGrp, lastParentGrp)) s <- paste0(s, "|")
       }
       lastParentGrp <- parentGrp
       s <- paste0(s, "r")
     }
     s <- paste0(s, "|}")
     ltx[[length(ltx)+1]] <- s
     # top line of the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     # column groups (i.e. column headings)
     for(r in 1:columnGroupLevelCount) {
       # if not the first set of column groups, then add a horizontal line
       if(r > 1) {
         s <- paste0("    \\cline{", (rowGroupLevelCount + 1), "-", (rowGroupLevelCount + toColumn - fromColumn + 1), "}")
         ltx[[length(ltx)+1]] <- s
       }
       #     \multicolumn{2}{|c|}{} & \multicolumn{3}{|c|}{n} & \multicolumn{3}{|c|}{m} & \multicolumn{3}{|c|}{e} \\
       s <- ""
       if(rowGroupLevelCount==1) s <- "    & "
       else s <- paste0("    \\multicolumn{", rowGroupLevelCount, "}{|c|}{} & ")
       # get the groups at this level
       grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r)
       firstColumn <- TRUE
       for(c in 1:length(grps)) {
         grp <- grps[[c]]
         if(!grp$isWithinVisibleRange) next
         if(firstColumn==TRUE) firstColumn <- FALSE
         else s <- paste0(s, " & ")
         leafGroupCount <- grp$visibleLeafGroupCount
         if(leafGroupCount > 1) {
           s <- paste0(s, "\\multicolumn{", leafGroupCount, "}{|c|}{", headingPrefix, private$getSafeString(grp$caption), headingSuffix, "}")
         }
         else {
           s <- paste0(s, headingPrefix, private$getSafeString(grp$caption), headingSuffix)
         }
       }
       s <- paste0(s, "\\\\")
       ltx[[length(ltx)+1]] <- s
     }
     # a line underneath the headings
     ltx[[length(ltx)+1]] <- "    \\hline"
     # output the row headings and data values
     lastParentGrp <- NULL
     for(r in fromRow:toRow) {
       rg <- rowGroups[[r]]
       parentGrp <- rg$parentGroup
       # draw a partial horizontal line before this row?
       if(!is.null(lastParentGrp)) {
         if(!identical(parentGrp, lastParentGrp)) {
           # need to work out how where to draw the line from
           # get the ancestors of this and the previous group and compare
           prevAncs <- rowGroups[[r-1]]$getAncestorGroups(includeCurrentGroup=TRUE)
           thisAncs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
           startLineFromColumn <- 0
           for(i in 1:length(prevAncs)) { # length(ancrgs)-1):1
             if(!identical(prevAncs[[length(ancrgs)-i+1]], thisAncs[[length(ancrgs)-i+1]])) {
               startLineFromColumn <- i
               break
             }
           }
           s <- paste0("    \\cline{", startLineFromColumn-1, "-", (rowGroupLevelCount + toColumn - fromColumn + 1), "}")
           ltx[[length(ltx)+1]] <- s
         }
       }
       lastParentGrp <- parentGrp
       # render this line
       # \multirow{3}{*}{p} & x & 1 & 2 & 3 & 1 & 2 & 3 & 1 & 2 & 3\\
       s <- "    "
       # first render the row headings
       ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
       firstColumn <- TRUE
       for(c in (length(ancrgs)-1):1) {
         ancg <- ancrgs[[c]]
         if(firstColumn) firstColumn <- FALSE
         else s <- paste0(s, "& ")
         if(ancg$isRendered==FALSE) {
           leafGroupCount <- ancg$visibleLeafGroupCount
           if(leafGroupCount > 1) {
             s <- paste0(s, "\\multirow{", leafGroupCount, "}{*}{", headingPrefix, private$getSafeString(ancg$caption), headingSuffix, "} ")
           }
           else {
             s <- paste0(s, headingPrefix, private$getSafeString(ancg$caption), headingSuffix, " ")
           }
           ancg$isRendered <- TRUE
         }
       }
       # now render the data values
       for(c in fromColumn:toColumn) {
         s <- paste0(s, " & ")
         cell <- cells$getCell(r, c)
         s <- paste0(s, private$getSafeString(cell$formattedValue))
       }
       # finish the line
       s <- paste0(s, "\\\\")
       ltx[[length(ltx)+1]] <- s
     }
     # finish the table
     ltx[[length(ltx)+1]] <- "    \\hline"
     ltx[[length(ltx)+1]] <- "  \\end{tabular}"
     ltx[[length(ltx)+1]] <- "\\end{table}"
     private$p_parentPivot$message("PivotLatexRenderer$getTableLatexNormal", "Got table Latex.")
     ltx <- paste(ltx, sep = '', collapse = '\n')
     return(invisible(ltx))
   }
 )
)
