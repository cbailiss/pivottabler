# todo:  ensure each of these cases works
# no row groups (only the default 1) and no column groups (only the default one): render a four cell table (i.e. one data cell)
# only one row/column group and multiple groups on other axis: render a placeholder cell with no caption (or maybe use the calculation name?)
# multiple row and multiple column groups: render as normal

PivotHtmlRenderer <- R6::R6Class("PivotHtmlRenderer",
  public = list(
   initialize = function(parentPivot) {
     checkArgument("PivotHtmlRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotHtmlRenderer$new", "Creating new Html Renderer...")
     private$p_parentPivot$message("PivotHtmlRenderer$new", "Created new Html Renderer.")
   },
   clearIsRenderedFlags = function() {
     private$p_parentPivot$message("PivotHtmlRenderer$clearIsRenderedFlags", "Clearing isRendered flags...")
     clearFlags <- function(dg) { dg$isRendered <- FALSE }
     rowGroups <- private$p_parentPivot$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
     lapply(rowGroups, clearFlags)
     columnGroups <- private$p_parentPivot$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
     lapply(columnGroups, clearFlags)
     private$p_parentPivot$message("PivotHtmlRenderer$clearIsRenderedFlags", "Cleared isRendered flags...")
   },
   getTableHtml = function(includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
     checkArgument("PivotHtmlRenderer", "getTableHtml", includeRCFilters, missing(includeRCFilters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotHtmlRenderer", "getTableHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotHtmlRenderer", "getTableHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotHtmlRenderer", "getTableHtml", includeRawValue, missing(includeRawValue), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
     private$p_parentPivot$message("PivotHtmlRenderer$getTableHtml", "Getting table HTML...")
     # get the data groups:  these are the leaf level groups
     rowGroups <- private$p_parentPivot$cells$rowGroups
     columnGroups <- private$p_parentPivot$cells$columnGroups
     # clear the isRendered flags
     self$clearIsRenderedFlags()
     # get the dimensions of the various parts of the table...
     # ...headings:
     rowGroupCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
     columnGroupCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
     # ...cells:
     rowCount <- private$p_parentPivot$cells$rowCount
     columnCount <- private$p_parentPivot$cells$columnCount
     # there must always be at least one row and one column
     insertDummyRowHeading <- (rowGroupCount==0)
     insertDummyColumnHeading <- (columnGroupCount==0)
     # build the table up row by row
     trows <- list()
     # render the column headings, with a large blank cell at the start over the row headings
     if(insertDummyColumnHeading) {
       trow <- list()
       trow[[1]] <- htmltools::tags$th(rowspan=columnGroupCount, colspan=rowGroupCount, htmltools::HTML("&nbsp;"))
       trow[[2]] <- htmltools::tags$th()
       trows[[1]] <- htmltools::tags$tr(trow)
     }
     else {
       for(r in 1:columnGroupCount) {
         trow <- list()
         if(r==1) { # generate the large top-left blank cell
           trow[[1]] <- htmltools::tags$th(rowspan=columnGroupCount, colspan=rowGroupCount, htmltools::HTML("&nbsp;"))
         }
         # get the groups at this level
         grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r+1)
         for(c in 1:length(grps)) {
           grp <- grps[[c]]
           trow[[length(trow)+1]] <- htmltools::tags$th(colspan=length(grp$leafGroups), grp$caption) # todo: check escaping
         }
         trows[[length(trows)+1]] <- htmltools::tags$tr(trow)
       }
     }
     # render the rows
     for(r in 1:rowCount) {
       trow <- list()
       # render the row headings
       if(insertDummyRowHeading) {
         trow[[1]] <- htmltools::tags$th()
       }
       else {
         # get the leaf row group, then render any parent data groups that haven't yet been rendered
         rg <- rowGroups[[r]]
         ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
         for(c in (length(ancrgs)-1):1) { # 2 (not 1) since the top ancestor is parentPivot private$rowGroup, which is just a container
           ancg <- ancrgs[[c]]
           if(ancg$isRendered==FALSE) {
             trow[[length(trow)+1]] <- htmltools::tags$th(rowspan=length(ancg$leafGroups), ancg$caption) # todo: check escaping
             ancg$isRendered <- TRUE
           }
         }
       }
       # render the cell values
       for(c in 1:columnCount) {
         cell <- private$p_parentPivot$cells$getCell(r, c)
         detail <- list()
         if(includeRCFilters|includeCalculationFilters|includeCalculationNames|includeRawValue)
         {
           if(includeRawValue) {
             detail[[length(detail)+1]] <- htmltools::tags$p(style="font-size: 75%;", paste0("raw value = ", cell$rawValue))
           }
           if(includeRCFilters) {
             fstr <- NULL
             if(is.null(cell$rowColFilters)) { fstr <- "No RC filters" }
             else {
               lst <- list()
               if(cell$rowColFilters$count > 0) {
                 for(i in 1:cell$rowColFilters$count){
                   lst[[length(lst)+1]] <- htmltools::tags$li(cell$rowColFilters$filters[[i]]$asString(seperator=", "))
                 }
               }
             }
             detail[[length(detail)+1]] <- list(htmltools::tags$p(style="font-size: 75%;", "RC Filters: "),
                                                htmltools::tags$ul(style="font-size: 75%; padding-left: 1em;", lst))
           }
           if(includeCalculationFilters) {
             fstr <- NULL
             if(is.null(cell$calculationFilters)) { fstr <- "No calculation filters" }
             else {
               lst <- list()
               if(cell$calculationFilters$count > 0) {
                 for(i in 1:cell$calculationFilters$count){
                   lst[[length(lst)+1]] <- htmltools::tags$li(cell$calculationFilters$filters[[i]]$asString(seperator=", "))
                 }
               }
             }
             detail[[length(detail)+1]] <- list(htmltools::tags$p(style="font-size: 75%;", "Calc. Filters: "),
                                                htmltools::tags$ul(style="font-size: 75%; padding-left: 1em;", lst))
           }
           if(includeCalculationNames) {
             cstr <- paste0("Calc: ",  cell$calculationGroupName, ": ", cell$calculationName)
             detail[[length(detail)+1]] <- list(htmltools::tags$p(style="font-size: 75%;", cstr))
           }
           trow[[length(trow)+1]] <- htmltools::tags$td(htmltools::tags$p(cell$formattedValue), detail) # todo: check escaping
         }
         else { trow[[length(trow)+1]] <- htmltools::tags$td(cell$formattedValue) } # todo: check escaping
       }
       # finished this row
       trows[[length(trows)+1]] <- htmltools::tags$tr(trow)
     }
     tbl <- htmltools::tags$table(trows)
     private$p_parentPivot$message("PivotHtmlRenderer$getTableHtml", "Got table HTML.")
     return(tbl)
   }
  ),
  private = list(
    p_parentPivot = NULL
  )
)
