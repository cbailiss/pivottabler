
#' R6 class that renders a pivot table in HTML.
#'
#' @description
#' The `PivotHtmlRenderer` class creates a HTML representation of a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmltools
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotHtmlRenderer <- R6::R6Class("PivotHtmlRenderer",
  public = list(

   #' @description
   #' Create a new `PivotHtmlRenderer` object.
   #' @param parentPivot The pivot table that this `PivotHtmlRenderer`
   #' instance belongs to.
   #' @return A new `PivotHtmlRenderer` object.
   initialize = function(parentPivot) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$new", "Creating new Html Renderer...")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$new", "Created new Html Renderer.")
   },

   #' @description
   #' An internal method used when rendering a pivot table to HTML.
   #' Clear the IsRendered flags that exist on the `PivotDataGroup` class.
   #' @return No return value.
   clearIsRenderedFlags = function() {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$clearIsRenderedFlags", "Clearing isRendered flags...")
     clearFlags <- function(dg) {
       grp <- dg
       while(!is.null(grp)) {
         grp$isRendered <- FALSE
         grp <- grp$parentGroup
       }
     }
     rowGroups <- private$p_parentPivot$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
     lapply(rowGroups, clearFlags)
     columnGroups <- private$p_parentPivot$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
     lapply(columnGroups, clearFlags)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$clearIsRenderedFlags", "Cleared isRendered flags...")
     return(invisible())
   },

   #' @description
   #' Generate a HTML representation of the pivot table, optionally including
   #' additional detail for debugging purposes.
   #' @param styleNamePrefix A character variable specifying a prefix for all named
   #' CSS styles, to avoid style name collisions where multiple pivot tables exist.
   #' @param includeHeaderValues Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeRCFilters Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeCalculationFilters Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeWorkingData Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeEvaluationFilters Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeCalculationNames Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeRawValue Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param includeTotalInfo Default `FALSE`, specify `TRUE` to render
   #' this debug information.
   #' @param exportOptions A list of additional export options - see the
   #' "A1. Appendix" for details.
   #' @param showRowGroupHeaders Default `FALSE`, specify `TRUE` to render the row
   #' group headings.  See the "Data Groups" vignette for details.
   #' @return A list containing HTML tags from the `htmltools` package.
   #' Convert this to a character variable using `as.character()`.
   getTableHtml = function(styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                           includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                           includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE,
                           exportOptions=NULL, showRowGroupHeaders=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeWorkingData, missing(includeWorkingData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeEvaluationFilters, missing(includeEvaluationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", includeTotalInfo, missing(includeTotalInfo), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getTableHtml", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$getTableHtml", "Getting table HTML...")
     # get the style names
     styles <- names(private$p_parentPivot$styles$styles)
     defaultTableStyle = private$p_parentPivot$styles$tableStyle
     defaultRootStyle = private$p_parentPivot$styles$rootStyle
     defaultRowHeaderStyle = private$p_parentPivot$styles$rowHeaderStyle
     defaultColHeaderStyle = private$p_parentPivot$styles$colHeaderStyle
     defaultCellStyle = private$p_parentPivot$styles$cellStyle
     defaultOutlineRowHeaderStyle = private$p_parentPivot$styles$outlineRowHeaderStyle
     if(is.null(defaultOutlineRowHeaderStyle)) defaultOutlineRowHeaderStyle <- defaultRowHeaderStyle
     defaultOutlineColHeaderStyle = private$p_parentPivot$styles$outlineColHeaderStyle
     if(is.null(defaultOutlineColHeaderStyle)) defaultOutlineColHeaderStyle <- defaultColHeaderStyle
     defaultOutlineCellStyle = private$p_parentPivot$styles$outlineCellStyle
     if(is.null(defaultOutlineCellStyle)) defaultOutlineCellStyle <- defaultCellStyle
     defaultTotalStyle = private$p_parentPivot$styles$totalStyle
     # get the actual style names to use, including the styleNamePrefix
     tableStyle <- paste0(styleNamePrefix, defaultTableStyle)
     rootStyle <- paste0(styleNamePrefix, defaultRootStyle)
     rowHeaderStyle <- paste0(styleNamePrefix, defaultRowHeaderStyle)
     colHeaderStyle <- paste0(styleNamePrefix, defaultColHeaderStyle)
     cellStyle <- paste0(styleNamePrefix, defaultCellStyle)
     outlineRowHeaderStyle <- paste0(styleNamePrefix, defaultOutlineRowHeaderStyle)
     outlineColHeaderStyle <- paste0(styleNamePrefix, defaultOutlineColHeaderStyle)
     outlineCellStyle <- paste0(styleNamePrefix, defaultOutlineCellStyle)
     totalStyle <- paste0(styleNamePrefix, defaultTotalStyle)
     # get the data groups:  these are the leaf level groups
     rowGroups <- private$p_parentPivot$cells$rowGroups
     columnGroups <- private$p_parentPivot$cells$columnGroups
     # clear the isRendered flags
     self$clearIsRenderedFlags()
     # get the dimensions of the various parts of the table...
     # ...headings:
     rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
     columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
     # ...cells:
     rowCount <- private$p_parentPivot$cells$rowCount
     columnCount <- private$p_parentPivot$cells$columnCount
     # ... merges
     rowMerges <- private$p_parentPivot$getMerges(axis="row")
     # compatibility to prevent outputing of nbsp if data group caption is blank (null or "")
     dataGroupNBSP <- !isTRUE(private$p_parentPivot$compatibility$noDataGroupNBSP)
     # compatibility to keep the explicit row/col span even if span is only 1
     o2n <- !isTRUE(private$p_parentPivot$compatibility$explicitHeaderSpansOfOne)
     # special case of no rows and no columns, return a blank empty table
     if((rowGroupLevelCount==0)&&(columnGroupLevelCount==0)) {
       tbl <- htmltools::tags$table(class=tableStyle, htmltools::tags$tr(
         htmltools::tags$td(class=cellStyle, style="text-align: center; padding: 6px", htmltools::HTML("(no data)"))))
       return(tbl)
     }
     # there must always be at least one row and one column
     insertDummyRowHeading <- (rowGroupLevelCount==0) & (columnGroupLevelCount > 0)
     insertDummyColumnHeading <- (columnGroupLevelCount==0) & (rowGroupLevelCount > 0)
     # build the table up row by row
     trows <- list()
     # render the column headings, with a large blank cell at the start over the row headings
     if(insertDummyColumnHeading) {
       # rendering the column headings (special case of no column groups existing)
       trow <- list()
       if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
          rowGrpHeaders <- private$p_parentPivot$rowGrpHeaders
          for(c in 1:rowGroupLevelCount) {
             rowGrpHeader <- NULL
             if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
             if(is.null(rowGrpHeader)) rowGrpHeader <- "&nbsp;"
             trow[[c]] <- htmltools::tags$th(class=rootStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), htmltools::HTML(rowGrpHeader))
          }
          trow[[rowGroupLevelCount+1]] <- htmltools::tags$th(class=colHeaderStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), htmltools::HTML("&nbsp;"))
       }
       else {
          trow[[1]] <- htmltools::tags$th(class=rootStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), colspan=oneToNULL(rowGroupLevelCount, o2n), htmltools::HTML("&nbsp;"))
          trow[[2]] <- htmltools::tags$th(class=colHeaderStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), htmltools::HTML("&nbsp;"))
       }
       trows[[1]] <- htmltools::tags$tr(trow)
     }
     else {
       # rendering the column headings (normal scenario)
       for(r in 1:columnGroupLevelCount) {
         trow <- list()
         if(r==1) {
            # generate the large top-left blank cell or cells
            if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
               rowGrpHeaders <- private$p_parentPivot$rowGrpHeaders
               for(c in 1:rowGroupLevelCount) {
                  rowGrpHeader <- NULL
                  if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
                  if(is.null(rowGrpHeader)) rowGrpHeader <- "&nbsp;"
                  trow[[c]] <- htmltools::tags$th(class=rootStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), htmltools::HTML(rowGrpHeader))
               }
            }
            else {
               trow[[1]] <- htmltools::tags$th(class=rootStyle, rowspan=oneToNULL(columnGroupLevelCount, o2n), colspan=oneToNULL(rowGroupLevelCount, o2n), htmltools::HTML("&nbsp;"))
            }
         }
         # get the groups at this level
         grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r)
         for(c in 1:length(grps)) {
           grp <- grps[[c]]
           chs <- colHeaderStyle
           if(!is.null(grp$baseStyleName)) chs <- paste0(styleNamePrefix, grp$baseStyleName)
           colstyl <- NULL
           if(!is.null(grp$style)) colstyl <- grp$style$asCSSRule()
           if(includeHeaderValues||includeTotalInfo) {
             detail <- list()
             if(includeHeaderValues) {
               lst <- NULL
               if(is.null(grp$filters)) { lst <- "No filters" }
               else {
                 lst <- list()
                 if(grp$filters$count > 0) {
                   for(i in 1:grp$filters$count){
                     lst[[length(lst)+1]] <- htmltools::tags$li(grp$filters$filters[[i]]$asString(seperator=", "))
                   }
                 }
               }
               detail[[length(detail)+1]] <- htmltools::tags$p(style="text-align: left; font-size: 75%;", "Filters: ")
               detail[[length(detail)+1]] <- htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst)
             }
             if(includeTotalInfo) {
               lst <- list()
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isTotal = ", grp$isTotal))
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isLevelSubTotal = ", grp$isLevelSubTotal))
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isLevelTotal = ", grp$isLevelTotal))
               detail[[length(detail)+1]] <- htmltools::tags$p(style="text-align: left; font-size: 75%;", "Totals: ")
               detail[[length(detail)+1]] <- htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst)
             }
             trow[[length(trow)+1]] <- htmltools::tags$th(class=chs, style=colstyl,  colspan=oneToNULL(length(grp$leafGroups), o2n), htmltools::tags$p(exportValueAs(grp$sortValue, grp$caption, exportOptions, blankValue="")), detail) # todo: check escaping
           }
           else {
             thValue <- exportValueAs(grp$sortValue, grp$caption, exportOptions, blankValue="")
             if(dataGroupNBSP&&((is.null(thValue)||(length(thValue)==0)||(nchar(thValue)==0)))) thValue <- htmltools::HTML("&nbsp;")
             trow[[length(trow)+1]] <- htmltools::tags$th(class=chs, style=colstyl, colspan=oneToNULL(length(grp$leafGroups), o2n), thValue) # todo: check escaping
           }
         }
         trows[[length(trows)+1]] <- htmltools::tags$tr(trow)
       }
     }
     # render the rows
     for(r in 1:rowCount) {
       trow <- list()
       rowMerge <- rowMerges[[r]]
       rg <- NULL
       # render the row headings
       if(insertDummyRowHeading) {
         # special case of no row groups existing
         trow[[1]] <- htmltools::tags$th(class=rowHeaderStyle, htmltools::HTML("&nbsp;"))
       }
       else {
         # get the leaf row group, then render any parent data groups that haven't yet been rendered
         rg <- rowGroups[[r]]
         ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
         for(c in (length(ancrgs)-1):1) { # start iterating at last item minus 1, which is first visible level in the pivot (level number=1)
           ancg <- ancrgs[[c]]
           if(ancg$isRendered) next
           rowGroupLevelNumber <- rowGroupLevelCount - c + 1
           # merge info
           rowMrgColumnSpan <- NULL
           lastDataGroupInRow <- FALSE
           if(rowMerge$merge && rowMerge$mergeGroups && (rowGroupLevelNumber==rowMerge$mergeGroupsFromLevel)) {
             rowMrgColumnSpan <- rowMerge$mergeGroupSpan
             lastDataGroupInRow <- TRUE
           }
           # style info
           if(ancg$isOutline && ancg$styleAsOutline) rhs <- outlineRowHeaderStyle
           else rhs <- rowHeaderStyle
           if(!is.null(ancg$baseStyleName)) rhs <- paste0(styleNamePrefix, ancg$baseStyleName)
           rwstyl <- NULL
           if(!is.null(ancg$style)) rwstyl <- ancg$style$asCSSRule()
           # showing extra info?
           if(includeHeaderValues||includeTotalInfo) {
             detail <- list()
             if(includeHeaderValues) {
               lst <- NULL
               if(is.null(ancg$filters)) { lst <- "No filters" }
               else {
                 lst <- list()
                 if(ancg$filters$count > 0) {
                   for(i in 1:ancg$filters$count){
                     lst[[length(lst)+1]] <- htmltools::tags$li(ancg$filters$filters[[i]]$asString(seperator=", "))
                   }
                 }
               }
               detail[[length(detail)+1]] <- htmltools::tags$p(style="text-align: left; font-size: 75%;", "Filters: ")
               detail[[length(detail)+1]] <- htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst)
             }
             if(includeTotalInfo) {
               lst <- list()
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isTotal = ", ancg$isTotal))
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isLevelSubTotal = ", ancg$isLevelSubTotal))
               lst[[length(lst)+1]] <- htmltools::tags$li(paste0("isLevelTotal = ", ancg$isLevelTotal))
               detail[[length(detail)+1]] <- htmltools::tags$p(style="text-align: left; font-size: 75%;", "Totals: ")
               detail[[length(detail)+1]] <- htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst)
             }
             trow[[length(trow)+1]] <- htmltools::tags$th(class=rhs, style=rwstyl, rowspan=oneToNULL(length(ancg$leafGroups), o2n), colspan=rowMrgColumnSpan, htmltools::tags$p(exportValueAs(ancg$sortValue, ancg$caption, exportOptions, blankValue="")), detail) # todo: check escaping
           }
           else {
             thValue <- exportValueAs(ancg$sortValue, ancg$caption, exportOptions, blankValue="")
             if(dataGroupNBSP&&((is.null(thValue)||(length(thValue)==0)||(nchar(thValue)==0)))) thValue <- htmltools::HTML("&nbsp;")
             trow[[length(trow)+1]] <- htmltools::tags$th(class=rhs, style=rwstyl, rowspan=oneToNULL(length(ancg$leafGroups), o2n), colspan=rowMrgColumnSpan, thValue) # todo: check escaping
           }
           ancg$isRendered <- TRUE
           if(lastDataGroupInRow) break
         }
       }
       # render the cell values
       if(!(rowMerge$merge && isTRUE(rowMerge$skipCells))) {
         styleAsOutline <- FALSE
         rgCellBaseStyleName <- NULL
         rgCellStyle <- NULL
         if(!is.null(rg)) {
           styleAsOutline <- rg$isOutline && rg$styleAsOutline
           rgCellBaseStyleName <- rg$netCellBaseStyleName
           rgCellStyle <- rg$netCellStyle
         }
         trowcells <- private$getCellsInRowHtml(r=r, columnCount=columnCount, exportOptions=exportOptions,
                                                styleNamePrefix=styleNamePrefix, cellStyle=cellStyle, totalStyle=totalStyle,
                                                styleAsOutline=styleAsOutline, outlineCellStyle=outlineCellStyle,  mergeCells=rowMerge$mergeCells,
                                                rowGroupCellBaseStyleName=rgCellBaseStyleName, rowGroupCellStyle=rgCellStyle,
                                                includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                includeWorkingData=includeWorkingData, includeEvaluationFilters=includeEvaluationFilters,
                                                includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
         trow <- append(trow, trowcells)
       }
       # finished this row
       trows[[length(trows)+1]] <- htmltools::tags$tr(trow)
     }
     tbl <- htmltools::tags$table(class=tableStyle, trows)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$getTableHtml", "Got table HTML.")
     return(invisible(tbl))
   }
  ),
  private = list(
    p_parentPivot = NULL,
    getCellsInRowHtml = function(r=NULL, columnCount=NULL, exportOptions=NULL, styleNamePrefix=NULL, cellStyle=NULL, totalStyle=NULL,
                                 styleAsOutline=FALSE, outlineCellStyle=NULL, mergeCells=FALSE, rowGroupCellBaseStyleName=NULL, rowGroupCellStyle=NULL, # the last two are forcing cell styles from the row groups (that override the default cell styles, but does not override cell styles specified on the cell)
                                 includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
       if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", columnCount, missing(columnCount), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", styleAsOutline, missing(styleAsOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", outlineCellStyle, missing(outlineCellStyle), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", mergeCells, missing(mergeCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", rowGroupCellBaseStyleName, missing(rowGroupCellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", rowGroupCellStyle, missing(rowGroupCellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotStyle")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeWorkingData, missing(includeWorkingData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeEvaluationFilters, missing(includeEvaluationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotHtmlRenderer", "getCellsInRowHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       }
       if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$getCellsInRowHtml", "Getting row cells HTML...")
       trowcells <- list()
       if(mergeCells) {
         # special case of all the cells being merged
         cssCell <- cellStyle
         if(!is.null(rowGroupCellBaseStyleName)) cssCell <-  paste0(styleNamePrefix, rowGroupCellBaseStyleName)
         else if(styleAsOutline && !is.null(outlineCellStyle)) cssCell <- outlineCellStyle
         cllstyl <- NULL
         if(!is.null(rowGroupCellStyle)) cllstyl <- rowGroupCellStyle$asCSSRule()
         trowcells[[length(trowcells)+1]] <- htmltools::tags$td(class=cssCell, style=cllstyl, colspan=columnCount, htmltools::HTML("&nbsp;"))
       }
       else {
         # normal case
         for(c in 1:columnCount) {
            # get the cell
            cell <- private$p_parentPivot$cells$getCell(r, c)
            # get the column group
            columnGroup <- private$p_parentPivot$getLeafColumnGroup(c=c)
            # base style name precedence:  from the cell, from the column group, from the row group,
            #                              then default outline (if outline), default total (if total), default cell
            cgBaseStyleName <- columnGroup$netCellBaseStyleName
            if(!is.null(cell$baseStyleName)) cssCell <- paste0(styleNamePrefix, cell$baseStyleName)
            else if(!is.null(cgBaseStyleName)) cssCell <- paste0(styleNamePrefix, cgBaseStyleName)
            else if(!is.null(rowGroupCellBaseStyleName)) cssCell <- paste0(styleNamePrefix, rowGroupCellBaseStyleName)
            else if(styleAsOutline && (!is.null(outlineCellStyle))) cssCell <- outlineCellStyle
            else if(cell$isTotal) cssCell <- totalStyle
            else cssCell <- cellStyle
            # style overrides precedence:  from the cell, from the column group, from the row group
            #                              given these styles are not single values, but lists of declarations, the styles
            #                              aggregate via a set operation, i.e. intersect/union from row -> col -> cell
            cllstyl <- NULL
            if(!is.null(rowGroupCellStyle)) {
               if(is.null(cllstyl)) { cllstyl <- rowGroupCellStyle$getCopy("") }
               else { cllstyl$setPropertyValues(rowGroupCellStyle$declarations) }
            }
            colGroupStyle <- columnGroup$netCellStyle
            if(!is.null(colGroupStyle)) {
               if(is.null(cllstyl)) { cllstyl <- colGroupStyle$getCopy("") }
               else { cllstyl$setPropertyValues(colGroupStyle$declarations) }
            }
            if(!is.null(cell$style)) {
               if(is.null(cllstyl)) { cllstyl <- cell$style$getCopy("") }
               else { cllstyl$setPropertyValues(cell$style$declarations) }
            }
            if(!is.null(cllstyl)) cllstyl <- cllstyl$asCSSRule()
            # render the cell
            detail <- list()
            if(includeRCFilters|includeCalculationFilters|includeWorkingData|includeEvaluationFilters|includeCalculationNames|includeRawValue)
            {
               if(includeRawValue) {
                  detail[[length(detail)+1]] <- htmltools::tags$p(style="text-align: left; font-size: 75%;", paste0("raw value = ", cell$rawValue))
               }
               if(includeRCFilters) {
                  lst <- NULL
                  if(is.null(cell$rowColFilters)) { lst <- "No RC filters" }
                  else {
                     lst <- list()
                     if(cell$rowColFilters$count > 0) {
                        for(i in 1:cell$rowColFilters$count){
                           lst[[length(lst)+1]] <- htmltools::tags$li(cell$rowColFilters$filters[[i]]$asString(seperator=", "))
                        }
                     }
                  }
                  detail[[length(detail)+1]] <- list(htmltools::tags$p(style="text-align: left; font-size: 75%;", "RC Filters: "),
                                                     htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst))
               }
               if(includeCalculationNames) {
                  cstr <- paste0("Calc: ",  cell$calculationGroupName, ": ", cell$calculationName)
                  detail[[length(detail)+1]] <- list(htmltools::tags$p(style="text-align: left; font-size: 75%;", cstr))
               }
               if(includeCalculationFilters) {
                  lst <- NULL
                  if(is.null(cell$calculationFilters)) { lst <- "No calculation filters" }
                  else {
                     lst <- list()
                     if("PivotFilters" %in% class(cell$calculationFilters)) {
                        if(cell$calculationFilters$count > 0) {
                           for(i in 1:cell$calculationFilters$count){
                              lst[[length(lst)+1]] <- htmltools::tags$li(cell$calculationFilters$filters[[i]]$asString(seperator=", "))
                           }
                        }
                     }
                     else if("PivotFilterOverrides" %in% class(cell$calculationFilters)) {
                        if(length(cell$calculationFilters) > 0) {
                           lst[[length(lst)+1]] <- htmltools::tags$li(cell$calculationFilters$asString(seperator=", "))
                        }
                     }
                  }
                  detail[[length(detail)+1]] <- list(htmltools::tags$p(style="text-align: left; font-size: 75%;", "Calc. Filters: "),
                                                     htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst))
               }
               if(includeWorkingData) {
                  if(length(cell$workingData)>0) {
                     wdNames <- names(cell$workingData)
                     for(w in 1:length(cell$workingData)) {
                        wd <- cell$workingData[[w]]
                        if(is.null(wd)) next
                        lst <- list()
                        if(is.null(wd$batchName)) lst[[length(lst)+1]] <- htmltools::tags$li("No batch")
                        else {
                           lst[[length(lst)+1]] <- htmltools::tags$li(paste0("Batch = ", wd$batchName))
                        }
                        if(is.null(wd$workingFilters)) { lst[[length(lst)+1]] <- htmltools::tags$li("No working filters") }
                        else {
                           if(wd$workingFilters$count > 0) {
                              for(i in 1:wd$workingFilters$count){
                                 lst[[length(lst)+1]] <- htmltools::tags$li(wd$workingFilters$filters[[i]]$asString(seperator=", "))
                              }
                           }
                        }
                        detail[[length(detail)+1]] <- list(htmltools::tags$p(style="text-align: left; font-size: 75%;",
                                                                             paste0("Working Data for ", wdNames[w], ":")),
                                                           htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst))
                     }
                  }
               }
               if(includeEvaluationFilters) {
                  lst <- NULL
                  if(is.null(cell$evaluationFilters)) { lst <- "No evaluation filters" }
                  else {
                     lst <- list()
                     if(cell$evaluationFilters$count > 0) {
                        for(i in 1:cell$evaluationFilters$count){
                           lst[[length(lst)+1]] <- htmltools::tags$li(cell$evaluationFilters$filters[[i]]$asString(seperator=", "))
                        }
                     }
                  }
                  detail[[length(detail)+1]] <- list(htmltools::tags$p(style="text-align: left; font-size: 75%;", "Eval. Filters: "),
                                                     htmltools::tags$ul(style="text-align: left; font-size: 75%; padding-left: 1em;", lst))
               }
               trowcells[[length(trowcells)+1]] <- htmltools::tags$td(class=cssCell, style=cllstyl, htmltools::tags$p(exportValueAs(cell$rawValue, cell$formattedValue, exportOptions, blankValue="")), detail) # todo: check escaping
            }
            else { trowcells[[length(trowcells)+1]] <- htmltools::tags$td(class=cssCell, style=cllstyl, exportValueAs(cell$rawValue, cell$formattedValue, exportOptions, blankValue="")) } # todo: check escaping
         }
       }
       if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotHtmlRenderer$getCellsInRowHtml", "Got row cells HTML.")
       return(invisible(trowcells))
    }
  )
)
