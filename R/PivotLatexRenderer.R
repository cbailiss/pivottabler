PivotLatexRenderer <- R6::R6Class("PivotLatexRenderer",
  public = list(
    initialize = function(parentPivot) {
      checkArgument("PivotLatexRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
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
      checkArgument("PivotLatexRenderer", "setVisibleRange", fromRow, missing(fromRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
      checkArgument("PivotLatexRenderer", "setVisibleRange", toRow, missing(toRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
      checkArgument("PivotLatexRenderer", "setVisibleRange", fromColumn, missing(fromColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
      checkArgument("PivotLatexRenderer", "setVisibleRange", toColumn, missing(toColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
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
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromRow (", fromRow, ") must be less than or equal to toRow (", toRow, ")")
        stop(errorMsg, call. = FALSE)
      }
      if(toColumn < fromColumn) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromColumn (", fromColumn, ") must be less than or equal to toColumn (", toColumn, ")")
        stop(errorMsg, call. = FALSE)
      }
      if(fromRow > cells$rowCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromRow (", fromRow, ") must be less than or equal to the number of rows (", cells$rowCount, ")")
        stop(errorMsg, call. = FALSE)
      }
      if(toRow > cells$rowCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): toRow (", toRow, ") must be less than or equal to the number of rows (", cells$rowCount, ")")
        stop(errorMsg, call. = FALSE)
      }
      if(fromColumn > cells$columnCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): fromColumn (", fromColumn, ") must be less than or equal to the number of columns (", cells$columnCount, ")")
        stop(errorMsg, call. = FALSE)
      }
      if(toColumn > cells$columnCount) {
        errorMsg <- paste0("PivotLatexRenderer$setVisibleRange(): toColumn (", toColumn, ") must be less than or equal to the number of columns (", cells$columnCount, ")")
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
    getTableLatex = function(caption=NULL, label=NULL) {
      checkArgument("PivotLatexRenderer", "getTableLatex", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotLatexRenderer", "getTableLatex", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_parentPivot$message("PivotLatexRenderer$getTableLatex", "Getting table Latex...")
      # reset rendered flags
      self$clearIsRenderedFlags()
      # references
      topRowGroup <- private$p_parentPivot$rowGroup
      topColumnGroup <- private$p_parentPivot$columnGroup
      cells <- private$p_parentPivot$cells
      # get the dimensions of the various parts of the table...
      # ...headings:
      rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
      columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
      # "no data" check
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
      message(rowGroupLevelCount)
      message(columnGroupLevelCount)
      message(cells$rowCount)
      message(cells$columnCount)
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
        for(c in 1:length(grps)) {
          grp <- grps[[c]]
          if(!grp$isWithinVisibleRange) next
          if(c > 1) s <- paste0(s, " & ")
          leafGroupCount <- grp$visibleLeafGroupCount
          if(leafGroupCount > 1) {
            s <- paste0(s, "\\multicolumn{", leafGroupCount, "}{|c|}{", private$getSafeString(grp$caption), "}")
          }
          else {
            s <- paste0(s, private$getSafeString(grp$caption))
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
              s <- paste0(s, "\\multirow{", leafGroupCount, "}{*}{", private$getSafeString(ancg$caption), "} ")
            }
            else {
              s <- paste0(s, private$getSafeString(ancg$caption), " ")
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
      private$p_parentPivot$message("PivotLatexRenderer$getTableLatex", "Got table Latex.")
      ltx <- paste(ltx, sep = '', collapse = '\n')
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
   }
 )
)
