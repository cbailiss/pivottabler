#' A class that defines a row or column heading.
#'
#' The PivotDataGroup class represents one row or column heading in a pivot
#' table.  PivotDataGroups have a parent-child relationship, i.e. each
#' PivotDataGroup can have one or more child PivotDataGroups.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @import dplyr
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   define the row and column headings in a pivot table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentGroup Parent PivotDataGroup.
#' @field parentPivot Owning pivot table.
#' @field rowOrColumn "row" or "column" indicating which axis this data group
#'   exists on.
#' @field doNotExpand Indicates whether addDataGroups() will expand this group.
#' @field isEmpty Indicates whether this data group contains no data (e.g. if
#'   it is part of a header / outline row).
#' @field isOutline Indicates whether this data group is an outline group used
#'   in outlined layouts (e.g. a header row for each data group).
#' @field caption The friendly display name for this data group.
#' @field variableName The name of the related column in the data frame(s) of
#'   the pivot table.
#' @field values The data value(s) which this data group represents.  Can be a
#'   vector of values.
#' @field sortValue The data value used when sorting the data groups.
#' @field isTotal Whether this data group is a total group.
#' @field isLevelSubTotal Whether this data group is a sub-total group in the
#'   current level.
#' @field isLevelTotal Whether this data group is a total group in the current
#'   level.
#' @field visualTotals Whether visual totals are enabled for this data group.
#' @field calculationGroupName The name of the calculation group applied to this
#'   data group.
#' @field calculationName The name of the calculation applied to this data group.
#' @field rowColumnNumber The row or column number of this data group, i.e.
#'   where it exists in the pivot table.
#' @field baseStyleName The name of the style applied to this data group (i.e.
#'   this row/column heading).  The style must exist in the PivotStyles object
#'   associated with the PivotTable.
#' @field fixedWidthSize the width of this data group in characters.  Only
#'   applies to column data groups, not row data groups.
#' @field style A PivotStyle object that can apply overrides to the base style
#'   for this data group.
#' @field mergeEmptySpace Specifies if/how empty space groups/cells should be
#'   merged when the pivot table is rendered.
#' @field cellBaseStyleName Specifies the name of the style applied to
#'   cells.  Overrides the default cell styles.
#' @field cellStyle A PivotStyle object that can apply
#'   overrides to cells.  Overrides the default cell styles.
#' @field isMatch Whether or not this data group matches the criteria of the
#'   last find.
#' @field isRendered Whether or not this data group has been rendered yet (used
#'   as part of the rendering routines).
#' @field isWithinVisibleRange whether or not this data group is visible (used
#'   as part of the rendering routines).
#' @field visibleChildGroupCount The number of visible child groups (used
#'   as part of the rendering routines)
#' @field visibleDescendantGroupCount The number of visible descendant groups
#'   (used as part of the rendering routines).
#' @field visibleLeafGroupCount The number of visible leaf groups beneath this
#'   group (used as part of the rendering routines).

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot data group, specifying the field
#'   values documented above.}
#'
#'   \item{\code{getLevelNumber()}}{Get the level number of this data group,
#'   where level 1 is the top data group.}
#'   \item{\code{getAncestorGroups(ancestors, includeCurrentGroup=FALSE)}}{Get
#'   all of the data groups above the current data group in the parent-child
#'   data group hierarchy.}
#'   \item{\code{getDescendantGroups(descendants,
#'   includeCurrentGroup=FALSE)}}{Get all of the data groups below the current
#'   data group in the parent-child data group hierarchy.}
#'   \item{\code{getLeafGroups(leafGroups)}}{Get all of the data groups across
#'   the bottom level of the data group hierarchy.}
#'   \item{\code{getLevelCount(includeCurrentLevel=FALSE)}}{Count the number of
#'   levels in the data group hierarchy.}
#'   \item{\code{getLevelGroups(level, levelGroups)}}{Get all of the data groups
#'   at a specific level in the data group hierarchy.}
#'   \item{\code{addChildGroup(variableName, filterType="ALL", values=NULL,
#'   doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, caption=NULL,
#'   isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
#'   calculationGroupName=NULL, calculationName=NULL,
#'   baseStyleName=NULL, styleDeclarations=NULL,
#'   insertAtIndex=NULL, insertBeforeGroup=NULL, insertAfterGroup=NULL,
#'   mergeEmptySpace=NULL, cellBaseStyleName=NULL,
#'   cellStyleDeclarations=NULL, sortAnchor=NULL, resetCells=TRUE)}}{
#'   Add a new data group as the child of the current data group.}
#'   \item{\code{removeChildGroup(index=NULL, group=NULL, resetCells=TRUE)}}{
#'   Remove a data group that is a child of the current data group.}
#'   \item{\code{removeGroup(resetCells=TRUE)}}{
#'   Remove the current data group.}
#'   \item{\code{addDataGroups(variableName, atLevel, fromData=TRUE, dataName,
#'   dataSortOrder="asc", customSortOrder, dataFormat,
#'   onlyCombinationsThatExist=TRUE, explicitListOfValues,
#'   calculationGroupName, expandExistingTotals=FALSE,
#'   addTotal=TRUE, visualTotals=FALSE, totalPosition="after",
#'   totalCaption="Total", onlyAddGroupIf, preGroupData=TRUE,
#'   baseStyleName=NULL, styleDeclarations=NULL,
#'   outlineBefore=NULL, outlineAfter=NULL,
#'   resetCells=TRUE)}}{
#'   Generate new data groups based on the distinct
#'   values in a data frame or using explicitly specified data values.}
#'   \item{\code{sortDataGroups(levelNumber=1, orderBy="calculation",
#'   customOrder, sortOrder="desc",
#'   calculationGroupName="default", calculationName,
#'   fromIndex=NULL, toIndex=NULL, resetCells=TRUE)}}{Sort
#'   data groups either by the data group data value, caption, a custom
#'   order or based on calculation result values.}
#'   \item{\code{addCalculationGroups(calculationGroupName, atLevel,
#'   outlineBefore, outlineAfter, resetCells=TRUE)}}{Add a
#'   calculation group to the data group hierarchy.}
#'   \item{\code{normaliseDataGroup(resetCells=TRUE)}}{Normalise the
#'   data group hierarchy so that all branches have the same number
#'   of levels - accomplished by adding empty child data groups where needed.}
#'   \item{\code{getNetFilters()}}{Get a PivotFilters object that contains all
#'   of the filters applied in this data group and all of its ancestors.}
#'   \item{\code{getNetCalculationName()}}{Get the calculation name applied in
#'   this data group or its nearest ancestor.}
#'   \item{\code{isFindMatch(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL,
#'   emptyGroups="exclude", outlineGroups="exclude")}}{
#'   Tests whether this data group matches the specified criteria.}
#'   \item{\code{findDataGroups(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL,
#'   includeChildGroups=FALSE, emptyGroups="exclude",
#'   outlineGroups="exclude")}}{Searches all data
#'   groups underneath this data group to find groups that match the specified
#'   criteria.}
#'   \item{\code{setStyling(styleDeclarations=NULL)}}{Used to set style declarations.}
#'   \item{\code{clearSortGroups()}}{Used internally during sort operations.}
#'   \item{\code{addSortGroupBefore()}}{Used internally during sort operations.}
#'   \item{\code{addSortGroupAfter()}}{Used internally during sort operations.}
#'   \item{\code{asList()}}{Get a list representation of the data group(s).}
#'   \item{\code{asJSON()}}{Get a JSON representation of the data group(s).}
#' }

PivotDataGroup <- R6::R6Class("PivotDataGroup",
  public = list(
   initialize = function(parentGroup=NULL, parentPivot=NULL, rowOrColumn=NULL, # common properties
                         doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, caption=NULL, # common properties
                         isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE, # total properties
                         variableName=NULL, filterType="ALL", values=NULL, # filter properties
                         calculationGroupName=NULL, calculationName=NULL, # calculation properties
                         baseStyleName=NULL, styleDeclarations=NULL,
                         mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                         sortAnchor=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotDataGroup")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", rowOrColumn, missing(rowOrColumn), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", mergeEmptySpace, missing(mergeEmptySpace), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$new", "Creating new data group...",
                                   list(rowOrColumn=rowOrColumn, caption=caption,
                                        variableName=variableName, filterType=filterType, values=values,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName))
     if(!(rowOrColumn %in% c("row", "column"))) stop("PivotDataGroup$new(): rowOrColumn must be either row or column", call. = FALSE)
     private$p_instanceId <- parentPivot$getNextInstanceId()
     private$p_parentGroup <- parentGroup
     private$p_rowOrColumn <- rowOrColumn
     private$p_doNotExpand <- doNotExpand
     private$p_isEmpty <- isEmpty
     private$p_isOutline <- isOutline
     private$p_caption <- caption
     private$p_sortValue <- values[1]
     private$p_isTotal <- isTotal
     private$p_isLevelSubTotal <- isLevelSubTotal
     private$p_isLevelTotal <- isLevelTotal
     private$p_filters <- PivotFilters$new(parentPivot, variableName=variableName, type=filterType, values=values)
     private$p_groups <- list() # child groups
     private$p_calculationGroupName <- calculationGroupName
     private$p_calculationName <- calculationName
     private$p_baseStyleName = baseStyleName
     if (!is.null(styleDeclarations)) private$p_style = private$p_parentPivot$createInlineStyle(declarations=styleDeclarations)
     private$p_mergeEmptySpace <- mergeEmptySpace
     private$p_cellBaseStyleName <- cellBaseStyleName
     if (!is.null(cellStyleDeclarations)) private$p_cellStyle = private$p_parentPivot$createInlineStyle(declarations=cellStyleDeclarations)
     private$p_sortAnchor <- sortAnchor
     private$p_sortGroupsBefore <- list()
     private$p_sortGroupsAfter <- list()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$new", "Created new data group.")
   },
   getLevelNumber = function() {
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLevelNumber", "Getting level number...")
     levelNumber <- 0
     grp <- self
     while(!is.null(grp$parentGroup)) {
       levelNumber <- levelNumber + 1
       grp <- grp$parentGroup
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLevelNumber", "Got level number.")
     return(invisible(levelNumber))
   },
   getAncestorGroups = function(ancestors=NULL, includeCurrentGroup=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getAncestorGroups", ancestors, missing(ancestors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getAncestorGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getAncestorGroups", "Getting ancestors...",
                                   list(ancestorCount=length(ancestors), includeCurrentGroup=includeCurrentGroup))
     acs <- NULL
     if(missing(ancestors)||is.null(ancestors)) {
       acs <- list()
       if(includeCurrentGroup==TRUE) { acs[[1]] <- self }
     }
     else { acs <- ancestors }
     if(!is.null(private$p_parentGroup)) {
       index <- length(acs) + 1
       acs[[index]] <- private$p_parentGroup
       acs <- private$p_parentGroup$getAncestorGroups(acs)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getAncestorGroups", "Got ancestors.", list(count=length(acs)))
     return(invisible(acs)) # note the top-most parent will be at the bottom of the return list
   },
   getDescendantGroups = function(descendants=NULL, includeCurrentGroup=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDescendantGroups", descendants, missing(descendants), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDescendantGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getDescendantGroups", "Getting descendant groups...")
     dgs <- NULL
     if(missing(descendants)||is.null(descendants)) { dgs <- list() }
     else { dgs <- descendants }
     if(includeCurrentGroup==TRUE) {
       index <- length(dgs) + 1
       dgs[[index]] <- self
     }
     if(length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         dgs <- private$p_groups[[i]]$getDescendantGroups(dgs, includeCurrentGroup=TRUE)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getDescendantGroups", "Got descendant groups", list(count=length(dgs)))
     return(invisible(dgs))
   },
   getLeafGroups = function(leafGroups=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getLeafGroups", leafGroups, missing(leafGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLeafGroups", "Getting leaf groups...", list(leafGroupCount=length(leafGroups)))
     lgs <- NULL
     if(missing(leafGroups)||is.null(leafGroups)) { lgs <- list() }
     else { lgs <- leafGroups }
     if(length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         lgs <- private$p_groups[[i]]$getLeafGroups(lgs)
       }
     }
     else {
       index <- length(lgs) + 1
       lgs[[index]] <- self
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLeafGroups", "Got leaf groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   getLevelCount = function(includeCurrentLevel=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getLevelCount", includeCurrentLevel, missing(includeCurrentLevel), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$levelCount", "Counting levels...")
     # get the leaf level groups
     leafGroups <- self$getLeafGroups()
     # get the maximum number of parents of each group
     maxParents <- 0
     if(length(leafGroups)==0) return()
     for(i in 1:length(leafGroups)) {
       ancestors <- leafGroups[[i]]$getAncestorGroups(includeCurrentGroup=includeCurrentLevel)
       maxParents <- max(maxParents, length(ancestors))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$levelCount", "Counted levels.")
     return(invisible(maxParents))
   },
   getLevelGroups = function(level=NULL, levelGroups=NULL) { #level=0 is the current data group
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getLevelGroups", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getLevelGroups", levelGroups, missing(levelGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLevelGroups", "Getting level groups...",
                                   list(level=level, levelGroupCount=length(levelGroups)))
     lgs <- NULL
     if(missing(levelGroups)||is.null(levelGroups)) { lgs <- list() }
     else { lgs <- levelGroups }
     if(level==0) {
       index <- length(lgs) + 1
       lgs[[index]] <- self
     }
     else if (length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         lgs <- private$p_groups[[i]]$getLevelGroups(level-1, lgs)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getLevelGroups", "Got level groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   getChildIndex = function(childGroup=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "childGroup", childGroup, missing(childGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getChildIndex", "Getting child index...")
      childIndex <- NULL
      if (length(private$p_groups) > 0) {
         instanceId <- childGroup$instanceId
         for (i in 1:length(private$p_groups)) {
            if(private$p_groups[[i]]$instanceId==instanceId) {
               childIndex <- i
               break
            }
         }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getChildIndex", "Got child index.", list(index=childIndex))
      return(invisible(childIndex))
   },
   addChildGroup = function(variableName=NULL, filterType="ALL", values=NULL,
                            doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, caption=NULL,
                            isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
                            calculationGroupName=NULL, calculationName=NULL,
                            baseStyleName=NULL, styleDeclarations=NULL,
                            insertAtIndex=NULL, insertBeforeGroup=NULL, insertAfterGroup=NULL,
                            mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                            sortAnchor=NULL, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", insertAtIndex, missing(insertAtIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", insertBeforeGroup, missing(insertBeforeGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", insertAfterGroup, missing(insertAfterGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", mergeEmptySpace, missing(mergeEmptySpace), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addChildGroup", "Adding child group...",
                                   list(caption=caption, doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, isTotal=isTotal,
                                        variableName=variableName, values=values,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName,
                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                        mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName,
                                        cellStyleDeclarations=cellStyleDeclarations, sortAnchor=sortAnchor, resetCells=resetCells))
     total <- isTotal | self$isTotal
     grp <- PivotDataGroup$new(parentGroup=self, parentPivot=private$p_parentPivot, rowOrColumn=private$p_rowOrColumn,
                               doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, caption=caption,
                               isTotal=total, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                               variableName=variableName, filterType=filterType, values=values,
                               calculationGroupName=calculationGroupName, calculationName=calculationName,
                               baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                               mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName,
                               cellStyleDeclarations=cellStyleDeclarations, sortAnchor=sortAnchor)
     # outline groups must either be marked doNotExpand, isEmpty or isTotal (otherwise we will accidentally expand them, which would look odd)
     if(isOutline) {
        if((!doNotExpand)&&(!isEmpty)&&(!total)) doNotExpand <- TRUE # Outline groups must either be set as doNotExpand=TRUE, isEmpty=TRUE or isTotal=TRUE.
     }
     # insert the group...
     if(is.null(insertAtIndex) && is.null(insertBeforeGroup) && is.null(insertAfterGroup)) {
        # append to end
        index <- length(private$p_groups) + 1
        private$p_groups[[index]] <- grp
     }
     else {
        if (!is.null(insertAtIndex)) {
           # do nothing more, we know the index
        }
        else if (!is.null(insertBeforeGroup)) {
           beforeIndex <- self$getChildIndex(insertBeforeGroup)
           if(is.null(beforeIndex)) stop("PivotDataGroup$addChildGroup():  Data group insertBeforeGroup is not a child of the current data group.", call. = FALSE)
           insertAtIndex <- beforeIndex
        }
        else if (!is.null(insertAfterGroup)) {
           afterIndex <- self$getChildIndex(insertAfterGroup)
           if(is.null(afterIndex)) stop("PivotDataGroup$addChildGroup():  Data group insertAfterGroup is not a child of the current data group.", call. = FALSE)
           insertAtIndex <- afterIndex + 1
        }
        newGroupLength <- length(private$p_groups) + 1
        if(insertAtIndex<1) insertAtIndex <- 1
        else if (insertAtIndex > newGroupLength) insertAtIndex <- newGroupLength
        for(i in newGroupLength:insertAtIndex) {
           if(i==1) break
           private$p_groups[[i]] <- private$p_groups[[i-1]]
        }
        private$p_groups[[insertAtIndex]] <- grp
     }
     if(resetCells) private$p_parentPivot$resetCells()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addChildGroup", "Added child group.")
     return(invisible(grp))
   },
   removeChildGroup = function(index=NULL, group=NULL, resetCells=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeChildGroup", index, missing(index), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeChildGroup", group, missing(group), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeChildGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeChildGroup", "Removing child group...")
      if(is.null(index)&&is.null(group)) stop("PivotDataGroup$removeChildGroup():  Either index or group must be specified.", call. = FALSE)
      # remove the group from the list of child groups
      if(is.null(index)) {
        index <- self$getChildIndex(group)
        if(is.null(index)) stop("PivotDataGroup$removeChildGroup():  Specified group is not a child of this data group.", call. = FALSE)
      }
      private$p_groups[[index]] <- NULL # removes the item and shuffles the list
      # remove the group from the sort lists
      private$removeGroupFromPrivateSortLists(group)
      # done
      if(resetCells) private$p_parentPivot$resetCells()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeChildGroup", "Removed child group.")
   },
   removeGroup = function(removeAncestorsIfNoRemainingChildren=FALSE, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroup", removeAncestorsIfNoRemainingChildren, missing(removeAncestorsIfNoRemainingChildren), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroup", "Removing group...")
     if(is.null(private$p_parentGroup)) stop("PivotDataGroup$removeGroup():  Cannot remove the top group on rows/columns.", call. = FALSE)
     private$p_parentGroup$removeChildGroup(group=self, resetCells=resetCells)
     if((removeAncestorsIfNoRemainingChildren==TRUE)&&(private$p_parentGroup$childGroupCount==0)) {
       private$p_parentGroup$removeGroup(removeAncestorsIfNoRemainingChildren=removeAncestorsIfNoRemainingChildren, resetCells=resetCells)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroup", "Removed group.")
   },
   # permutations:
   # dataName="...", fromData=TRUE, onlyCombinationsThatExist=TRUE >> generate the list (from the data at leaf level), 1 value per group
   # dataName="...", fromData=TRUE, onlyCombinationsThatExist=FALSE >> generate the list (from the data at top level), 1 value per group
   # fromData=FALSE, explicitListOfValues=list(...), simply generates the groups from the values passed in
   # explicitListOfValues should be a LIST of values, each element in the list can be single value or a vector of values (to allow a
   # single pivot table row/column to represent multiple values)
   # atLevel is the number of levels below the current level
   addDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=0 is the current level, 1 = one level below, etc
                            dataName=NULL, dataSortOrder="asc", customSortOrder=NULL, dataFormat=NULL, dataFmtFuncArgs=NULL,
                            onlyCombinationsThatExist=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                            expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                            onlyAddGroupIf=NULL, preGroupData=TRUE, baseStyleName=NULL, styleDeclarations=NULL,
                            outlineBefore=NULL, outlineAfter=NULL, outlineTotal=FALSE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "custom", "none"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", customSortOrder, missing(customSortOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataFmtFuncArgs, missing(dataFmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", onlyAddGroupIf, missing(onlyAddGroupIf), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", preGroupData, missing(preGroupData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", outlineBefore, missing(outlineBefore), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", outlineAfter, missing(outlineAfter), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", outlineTotal, missing(outlineTotal), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addDataGroups", "Adding data groups...",
                                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                                        dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                                        dataFormat=dataFormat, onlyCombinationsThatExist=onlyCombinationsThatExist,
                                        explicitListOfValues=explicitListOfValues, calculationGroupName=calculationGroupName,
                                        expandExistingTotals=expandExistingTotals, addTotal=addTotal, visualTotals=visualTotals,
                                        totalPosition=totalPosition, totalCaption=totalCaption,
                                        onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData,
                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                        outlineBefore=outlineBefore, outlineAfter=outlineAfter, outlineTotal=outlineTotal))
     private$p_parentPivot$resetCells()
     # check variable name
     if(missing(variableName)||is.null(variableName)) stop("PivotDataGroup$addDataGroups(): variableName must be specified.", call. = FALSE)
     safeVariableName <- processIdentifier(variableName)
     # check sort order
     if((dataSortOrder=="custom")&&(is.null(customSortOrder))) stop("PivotDataGroup$addDataGroups(): if dataSortOrder=custom, then customSortOrder must be specified.", call. = FALSE)
     # check outline settings
     outlineBefore <- private$cleanOutlineArg(outlineBefore)
     outlineAfter <- private$cleanOutlineArg(outlineAfter, defaultCaption="")
     outlineTotal <- private$cleanOutlineArg(outlineTotal, defaultCaption="Total", defaultIsEmpty=FALSE)
     # visual totals
     if(addTotal==TRUE){ private$p_visualTotals <- visualTotals }
     # if onlyAddGroupIf is specified, then preGroupData cannot be used
     if(!is.null(onlyAddGroupIf)) {
       preGroupData <- FALSE
       if(fromData==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddGroupIf cannot be used when fromData=FALSE.", call. = FALSE)
       if(onlyCombinationsThatExist==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddGroupIf cannot be used when onlyCombinationsThatExist=FALSE.", call. = FALSE)
     }
     # data values
     df <- NULL
     topLevelDisinctValues <- NULL
     topLevelCaptions <- NULL
     allValues <- NULL
     if(fromData==TRUE) {
       # check that a data frame has been specified (or that we have a default data frame)
       if(missing(dataName)||is.null(dataName)) {
         if (private$p_parentPivot$data$count < 1) stop("PivotDataGroup$addDataGroups():  No data frames.  Specify data before calling addLeafGroup.", call. = FALSE)
         df <- private$p_parentPivot$data$defaultData
         if(!is.null(df)) {
           if(!(variableName %in% colnames(df))) {
             dn <- paste0("'", private$p_parentPivot$data$defaultName, "'")
             if(is.null(dn)) dn <- "default"
             stop(paste0("PivotDataGroup$addDataGroups():  Column name '", variableName, "' not found in ", dn, " data frame."), call. = FALSE)
           }
         }
       }
       else {
         df <- private$p_parentPivot$data$getData(dataName)
         if(is.null(df)) stop(paste0("PivotDataGroup$addDataGroups():  No data frame found in PivotTable with name '", dataName, "'."), call. = FALSE)
         if(!is.null(df)) {
           if(!(variableName %in% colnames(df))) {
             stop(paste0("PivotDataGroup$addDataGroups():  Column name '", variableName, "' not found in '", dataName, "' data frame."), call. = FALSE)
           }
         }
       }
     }
     else {
       if (missing(explicitListOfValues)) stop("PivotDataGroup$addDataGroups():  An explicitListOfValues must be specified when fromData=FALSE", call. = FALSE)
       if (is.null(explicitListOfValues)) stop("PivotDataGroup$addDataGroups():  explicitListOfValues must not be null when fromData=FALSE", call. = FALSE)
       topLevelDisinctValues <- explicitListOfValues
       topLevelCaptions <- names(explicitListOfValues)
       allValues <- topLevelDisinctValues
       if("list" %in% class(topLevelDisinctValues)) { allValues <- unlist(topLevelDisinctValues) }
     }
     # ignore the filters from the other heading groups?
     if((fromData==TRUE)&&(onlyCombinationsThatExist==FALSE)) {
       data <- df
       if(private$p_parentPivot$processingLibrary=="dplyr") {
         # build a dplyr query
         # todo: see escaping note 50 or so lines below
         eval(parse(text=paste0("data <- dplyr::select(data, ", safeVariableName, ")")))
         data <- dplyr::distinct(data)
         if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", safeVariableName, ")")))
         else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", safeVariableName, "))")))
         topLevelDisinctValues <- dplyr::collect(data)[[variableName]]
         if(dataSortOrder=="custom") topLevelDisinctValues <- topLevelDisinctValues[order(match(topLevelDisinctValues, customSortOrder))]
       }
       else if(private$p_parentPivot$processingLibrary=="data.table") {
         # check is a data table
         if(private$p_parentPivot$argumentCheckMode == 4) {
           if(!data.table::is.data.table(data))
             stop(paste0("PivotDataGroup$addDataGroups(): A data.table was expected but the following was encountered: ",
                         paste(class(data), sep="", collapse=", ")), call. = FALSE)
         }
         # seem to need a dummy row count in order to get the distinct values
         rcName <- "rc"
         if(variableName==rcName) rcName <- "rowCount"
         eval(parse(text=paste0("topLevelDisinctValues <- data[order(", ifelse(dataSortOrder=="desc", "-", ""), safeVariableName, "), .(", rcName, "=.N), by=.(", safeVariableName, ")][, ", safeVariableName, "]")))
         if(dataSortOrder=="custom") topLevelDisinctValues <- topLevelDisinctValues[order(match(topLevelDisinctValues, customSortOrder))]
       }
       else stop(paste0("PivotDataGroup$addDataGroups(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
       if(is.factor(topLevelDisinctValues)) { topLevelDisinctValues <- as.character(topLevelDisinctValues) }
       allValues=topLevelDisinctValues
     }
     # where are the new groups being added?
     if(is.null(atLevel)) {
       # get the current set of leaf groups
       parentGroups <- self$getLeafGroups()
       if((length(private$p_groups)==0)||(is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     else if(atLevel<=0) {
       # immediately below this data group
       parentGroups <- list()
       parentGroups[[1]] <- self
     }
     else {
       # at some number of levels below this group
       levelsBelowThisLevel <- self$getLevelCount()
       if (atLevel>levelsBelowThisLevel) atLevel <- levelsBelowThisLevel
       parentGroups <- self$getLevelGroups(level=atLevel)
       if((is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     # pre-group the data?
     if((fromData==TRUE)&&(onlyCombinationsThatExist==TRUE)&&(preGroupData==TRUE)) {
       ascGroups <- self$getAncestorGroups(includeCurrentGroup=TRUE)
       descGroups <- self$getDescendantGroups(includeCurrentGroup=FALSE)
       varNamesInUse1 <- lapply(ascGroups, function(grp) { grp$filters$filteredVariables })
       varNamesInUse1 <- unlist(varNamesInUse1[!sapply(varNamesInUse1, is.null)])
       varNamesInUse2 <- lapply(descGroups, function(grp) { grp$filters$filteredVariables })
       varNamesInUse2 <- unlist(varNamesInUse2[!sapply(varNamesInUse2, is.null)])
       varNamesInUse3 <- union(union(varNamesInUse1, varNamesInUse2), variableName)
       if(length(varNamesInUse3)>0) {
         if(private$p_parentPivot$processingLibrary=="dplyr") {
           # build a dplyr query
           # todo: checking the escaping of the variable names and values below
           # get the distinct values for these variables
           eval(parse(text=paste0("df <- dplyr::distinct(df, ", paste(processIdentifiers(varNamesInUse3), sep="", collapse=", "), ")")))
           df <- dplyr::collect(df)
         }
         else if(private$p_parentPivot$processingLibrary=="data.table") {
           # check is a data table
           if(private$p_parentPivot$argumentCheckMode == 4) {
             if(!data.table::is.data.table(df))
               stop(paste0("PivotDataGroup$addDataGroups(): A data.table was expected but the following was encountered: ",
                           paste(class(df), sep="", collapse=", ")), call. = FALSE)
           }
           # seem to need a dummy row count in order to get the distinct values
           rcName <- "rc"
           while(rcName %in% varNamesInUse3) {
             rcName <- paste0(rcName, "N")
           }
           eval(parse(text=paste0("df <- df[, .(", rcName, "=.N), by=.(", paste(processIdentifiers(varNamesInUse3), sep="", collapse=", "), ")]")))
         }
         else stop(paste0("PivotDataGroup$addDataGroups(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
       }
     }
     # for each group...
     newGroups <- list()
     for(i in 1:length(parentGroups))
     {
       grp <- parentGroups[[i]]
       # if this group is marked not to be expanded, or if this is an empty group, then don't expand it (just add an empty child)
       if(grp$doNotExpand || grp$isEmpty) {
          # add a single group that is also empty
          newGrp <- grp$addChildGroup(doNotExpand=grp$doNotExpand, isEmpty=grp$isEmpty, isOutline=grp$isOutline,
                                      mergeEmptySpace=grp$mergeEmptySpace, cellBaseStyleName=grp$cellBaseStyleName,
                                      cellStyleDeclarations=grp$cellStyle$declarations, resetCells=FALSE)
          index <- length(newGroups) + 1
          newGroups[[index]] <- newGrp
          next
       }
       # total?
       if((grp$isTotal==TRUE)&&(grp$isOutline||(expandExistingTotals==FALSE))) {
         if(length(grp$childGroups)==0) { # if there is a total cell present already, don't add another (this can happen if addDataGroups(atLevel=N) is called multiple times with the same level N)
           # add a single group that is an unexpanded total
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       calculationGroupName=calculationGroupName, isOutline=grp$isOutline,
                                       isTotal=TRUE, isLevelSubTotal=grp$isLevelSubTotal, isLevelTotal=grp$isLevelTotal,
                                       baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, sortAnchor="fixed", resetCells=FALSE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         next
       }

       # use top level groups?
       distinctValues <- NULL
       distinctCaptions <- NULL
       if(!is.null(topLevelDisinctValues)) {
         distinctValues <- topLevelDisinctValues
         distinctCaptions <- topLevelCaptions
       }
       else {
         # get the ancestor groups for this group, starting with the current object
         ancestors <- grp$getAncestorGroups(includeCurrentGroup=TRUE)
         # construct the parent filter settings using "intersect" filter logic
         rowColFilters <- PivotFilters$new(private$p_parentPivot)
         for(j in length(ancestors):1) {
           acs <- ancestors[[j]]
           filters <- acs$filters
           if(is.null(filters)) next
           if(filters$count==0) next
           for(k in 1:length(filters$filters)) {
             filter <- filters$filters[[k]]
             rowColFilters$setFilter(filter, action="intersect")
           }
         }
         if(!rowColFilters$isNONE) {
           if(private$p_parentPivot$processingLibrary=="dplyr") {
             # build a dplyr query
             data <- df
             # todo: checking the escaping of the variable names and values below
             if((!rowColFilters$isALL)&&(rowColFilters$count > 0)) {
               data <- rowColFilters$getFilteredDataFrame(dataFrame=data)
             }
             # if onlyAddGroupIf specified, then need to further filter the data frame by this
             if(!is.null(onlyAddGroupIf)) {
               eval(parse(text=paste0("data <- dplyr::filter(data, ", onlyAddGroupIf, ")")))
             }
             # get the distinct values for the current variable
             eval(parse(text=paste0("data <- dplyr::select(data, ", safeVariableName, ")")))
             data <- dplyr::distinct(data)
             if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", safeVariableName, ")")))
             else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", safeVariableName, "))")))
             distinctValues <- dplyr::collect(data)[[variableName]]
             if(dataSortOrder=="custom") distinctValues <- distinctValues[order(match(distinctValues, customSortOrder))]
           }
           else if(private$p_parentPivot$processingLibrary=="data.table") {
             # build a data.table query
             data <- df
             # check is a data table
             if(private$p_parentPivot$argumentCheckMode == 4) {
               if(!data.table::is.data.table(data))
                 stop(paste0("PivotDataGroup$addDataGroups(): A data.table was expected but the following was encountered: ",
                             paste(class(data), sep="", collapse=", ")), call. = FALSE)
             }
             # filters
             filterCmd <- NULL
             filterCount <- 0
             if(length(rowColFilters$filters) > 0)
             {
               for(j in 1:length(rowColFilters$filters)) {
                 filter <- rowColFilters$filters[[j]]
                 if(is.null(filter$variableName))
                   stop("PivotCalculator$evaluateSummariseExpression(): filter$variableName must not be null", call. = FALSE)
                 if(is.null(filter$values)) next
                 if(length(filter$values)==0) next
                 if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
                 if(length(filter$values)>0) {
                   # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
                   filterCmd <- paste0(filterCmd, "(", filter$safeVariableName, " %in% rowColFilters$filters[[", j, "]]$values)")
                   filterCount <- filterCount + 1
                 }
               }
             }
             # if onlyAddGroupIf specified, then add this to the filter criteria
             if(!is.null(onlyAddGroupIf)) {
               if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
               filterCmd <- paste0(filterCmd, "(", onlyAddGroupIf, ")")
             }
             # seem to need a dummy row count in order to get the distinct values
             rcName <- "rc"
             if(variableName==rcName) rcName <- "rowCount"
             eval(parse(text=paste0("distinctValues <- data[", filterCmd, ", .(", rcName, "=.N), by=.(", safeVariableName, ")][order(", ifelse(dataSortOrder=="desc", "-", ""), safeVariableName, ")][, ", safeVariableName, "]")))
             if(dataSortOrder=="custom") distinctValues <- distinctValues[order(match(distinctValues, customSortOrder))]
           }
           else stop(paste0("PivotDataGroup$addDataGroups(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
           if("factor" %in% class(distinctValues)) { distinctValues <- as.character(distinctValues) }
           allValues <- union(allValues, distinctValues)
         }
       }

       # check we have some values
       if(is.null(distinctValues)||(length(distinctValues)==0)) {
         # add a blank group
         newGrp <- grp$addChildGroup(variableName=variableName, filterType="NONE", values=NULL, caption="",
                                     calculationGroupName=calculationGroupName,
                                     isTotal=grp$isTotal, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal,
                                     baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, resetCells=FALSE)
         index <- length(newGroups) + 1
         newGroups[[index]] <- newGrp
       }
       # append the child groups
       else {
         # total before?
         if((addTotal==TRUE)&&(totalPosition=="before")&&(!outlineTotal$outline)) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal,
                                       baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, sortAnchor="fixed", resetCells=FALSE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         # child groups for values
         isValueList <- ("list" %in% class(distinctValues))
         for(j in 1:length(distinctValues)) {
           # get values
           if(isValueList) values <- distinctValues[[j]]
           else values <- distinctValues[j]
           # get caption
           caption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) caption <- distinctCaptions[j]
           if(is.null(caption)&&(!is.null(dataFormat))) caption <- private$formatValue(values, dataFormat, dataFmtFuncArgs)
           grpCaption <- caption
           if(outlineBefore$outline) grpCaption <- ""
           # outline before value
           if(outlineBefore$outline){
              newGrp <- private$createOutlineGroup(parentGroup=grp, outline=outlineBefore,
                                                   variableName=variableName, values=values, caption=caption,
                                                   calculationGroupName=calculationGroupName,
                                                   baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                   sortAnchor="next", resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newGrp
           }
           # value
           newGrp <- grp$addChildGroup(variableName=variableName, values=values, caption=grpCaption,
                                       calculationGroupName=calculationGroupName, isTotal=grp$isTotal,
                                       baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, resetCells=FALSE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
           # outline after value
           if(outlineAfter$outline){
              newGrp <- private$createOutlineGroup(parentGroup=grp, outline=outlineAfter,
                                                   variableName=variableName, values=values, caption=caption,
                                                   calculationGroupName=calculationGroupName,
                                                   baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                   sortAnchor="previous", resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newGrp
           }
         }
         # total after?
         if((addTotal==TRUE)&&(totalPosition=="after")&&(!outlineTotal$outline)) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal,
                                       baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, sortAnchor="fixed", resetCells=FALSE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         # outline total?
         if(outlineTotal$outline) {
            newGrp <- private$createOutlineGroup(parentGroup=grp, outline=outlineTotal, caption=totalCaption,
                                                 variableName=variableName, values=NULL, calculationGroupName=calculationGroupName,
                                                 baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                 isLevelTotal=TRUE, sortAnchor="fixed", resetCells=FALSE)
            index <- length(newGroups) + 1
            newGroups[[index]] <- newGrp
         }
       }
     }
     # if visual totals are enabled...
     # get all the totals that are the descendants of this (i.e. self) data group and add in this additional criteria
     if(visualTotals==TRUE) {
       descdnts <- self$getDescendantGroups(includeCurrentGroup=TRUE)
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=allValues)
       for(i in 1:length(descdnts)) {
         descdnt <- descdnts[[i]]
         if(descdnt$isTotal) descdnt$filters$setFilter(topLevelFilter, action="intersect")
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addDataGroups", "Added groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
   },
   sortDataGroups = function(levelNumber=1, orderBy="calculation", customOrder=NULL, sortOrder="desc",
                             calculationGroupName="default", calculationName=NULL,
                             fromIndex=NULL, toIndex=NULL, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation","customByValue","customByCaption"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", customOrder, missing(customOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", fromIndex, missing(fromIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", toIndex, missing(toIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "sortDataGroups", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$sortDataGroups", "Sorting data groups...",
                                   list(levelNumber=levelNumber, orderBy=orderBy, customOrder=customOrder, sortOrder=sortOrder,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName,
                                        fromIndex=fromIndex, toIndex=toIndex, resetCells=resetCells))
     if(is.null(private$p_groups)) return(invisible())
     if(length(private$p_groups)==0) return(invisible())
     if(resetCells) private$p_parentPivot$resetCells()
     calculationGroup <- NULL
     calculation <- NULL
     if(orderBy=="calculation") {
       if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
         stop(paste0("PivotDataGroup$sortDataGroups():  There is no Calculation Group named '", calculationGroupName, "'"), call. = FALSE)
       calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
       if(is.null(calculationName)) {
         calculationName <- calculationGroup$defaultCalculationName
         if(is.null(calculationName))
           stop(paste0("PivotDataGroup$sortDataGroups():  No calculation has been specified and there is no default calculation."), call. = FALSE)
       }
       if(!calculationGroup$isExistingCalculation(calculationName))
         stop(paste0("PivotDataGroup$sortDataGroups():  There is no Calculation named '", calculationName , "' in group '", calculationGroupName, "'"), call. = FALSE)
       calculation <- calculationGroup$getCalculation(calculationName)
     }
     # sort at this level, or a level below?
     if(levelNumber>0) {
        # sort below this level
        for(i in 1:length(private$p_groups)) {
           private$p_groups[[i]]$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                                calculationGroupName=calculationGroupName, calculationName=calculationName)
        }
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$sortDataGroups", "Sorted data groups.")
        return(invisible())
     }
     # sort at this level
     # first, clear the sort groups, get the fixed groups and populate the linked groups
     fixedgrps <- list()
     for(i in 1:length(private$p_groups)) {
        grp <- private$p_groups[[i]]
        grp$clearSortGroups()
        if((!is.null(grp$sortAnchor))&&(grp$sortAnchor=="fixed")) {
           fixedgrps[[length(fixedgrps)+1]] <- i
        }
        else if((!is.null(fromIndex))&&(i<fromIndex)) {
           fixedgrps[[length(fixedgrps)+1]] <- i
        }
        else if((!is.null(toIndex))&&(i>toIndex)) {
           fixedgrps[[length(fixedgrps)+1]] <- i
        }
        if(i>1) {
           for(k in (i-1):1) {
              if((!is.null(private$p_groups[[k]]$sortAnchor))&&(private$p_groups[[k]]$sortAnchor=="next")) grp$addSortGroupBefore(private$p_groups[[k]])
              else break
           }
        }
        if(i<length(private$p_groups)) {
           for(k in (i+1):length(private$p_groups)) {
              if((!is.null(private$p_groups[[k]]$sortAnchor))&&(private$p_groups[[k]]$sortAnchor=="previous")) grp$addSortGroupAfter(private$p_groups[[k]])
              else break
           }
        }
     }
     # get the values to use in the sort
     groups <- list()
     values <- list()
     sortIndexes <- NULL
     if(orderBy %in% c("value", "caption")) {
       # sorting by a simple property of the data group
       j <- 0
       for(i in 1:length(private$p_groups)) {
         grp <- private$p_groups[[i]]
         if((!is.null(grp$sortAnchor))&&(grp$sortAnchor %in% c("fixed", "previous", "next"))) next
         if(i %in% fixedgrps) next # happens also when the data group is outside the specified range
         j <- j + 1
         groups[[j]] <- grp
         if(orderBy=="value") values[[j]] <- grp$sortValue # sorting by value
         else if(orderBy=="caption") values[[j]] <- grp$caption # sorting by caption
       }
     }
     else if(orderBy %in% c("customByValue", "customByCaption")) {
       # sorting by a custom sort order
       if(is.null(customOrder))
         stop(paste0("PivotDataGroup$sortDataGroups():  customOrder must be specified when orderBy=customByValue|customByCaption"), call. = FALSE)
       j <- 0
       for(i in 1:length(private$p_groups)) {
         grp <- private$p_groups[[i]]
         if((!is.null(grp$sortAnchor))&&(grp$sortAnchor %in% c("fixed", "previous", "next"))) next
         if(i %in% fixedgrps) next # happens also when the data group is outside the specified range
         j <- j + 1
         groups[[j]] <- grp
         if(orderBy=="customByValue") values[[j]] <- match(grp$sortValue, customOrder) # sorting by value
         else if(orderBy=="customByCaption") values[[j]] <- match(grp$caption, customOrder) # sorting by caption
       }
     }
     else {
       # sorting by calculation
       groups <- list()
       values <- list()
       pivotCalculator <- PivotCalculator$new(private$p_parentPivot)
       j <- 0
       for(i in 1:length(private$p_groups)) {
         grp <- private$p_groups[[i]]
         if((!is.null(grp$sortAnchor))&&(grp$sortAnchor %in% c("fixed", "previous", "next"))) next
         if(i %in% fixedgrps) next # happens also when the data group is outside the specified range
         j <- j + 1
         groups[[j]] <- grp
         # get the filter criteria from the top-most parent group down to this one
         ancestors <- grp$getAncestorGroups(includeCurrentGroup=TRUE)
         netFilters <- PivotFilters$new(private$p_parentPivot)
         for(k in length(ancestors):1) {
           acs <- ancestors[[k]]
           filters <- acs$filters
           if(is.null(filters)) next
           if(filters$count==0) next
           for(l in 1:length(filters$filters)) {
             filter <- filters$filters[[l]]
             netFilters$setFilter(filter, action="intersect")
           }
         }
         # calculate the value
         results <- pivotCalculator$evaluateNamedCalculation(calculationName=calculationName,
                                                             calculationGroupName=calculationGroupName,
                                                             rowColFilters=netFilters)
         calcResults <- results[[calculationName]]
         values[[j]] <- calcResults$rawValue
       }
     }
     # if we have some values, do the sort and reorder the child groups
     if(length(values)>0){
        if(sortOrder=="asc") sortIndexes <- order(unlist(values))
        else sortIndexes <- order(unlist(values), decreasing=TRUE)
        i <- 0
        for(j in 1:length(groups)) {
           grp <- groups[[sortIndexes[j]]]
           gList <- list()
           if(length(grp$sortGroupsBefore)>0) {
              for(k in 1:length(grp$sortGroupsBefore)) {
                 gList[[length(gList)+1]] <- grp$sortGroupsBefore[[length(grp$sortGroupsBefore)-k+1]]
              }
           }
           gList[[length(gList)+1]] <- grp
           if(length(grp$sortGroupsAfter)>0) {
              for(k in 1:length(grp$sortGroupsAfter)) {
                 gList[[length(gList)+1]] <- grp$sortGroupsAfter[[k]]
              }
           }
           for(k in 1:length(gList)) {
              i <- i + 1
              while (i %in% fixedgrps) {
                 # a fixed group already exists at this index in the correct place
                 i <- i + 1
              }
              private$p_groups[[i]] <- gList[[k]]
           }
        }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$sortDataGroups", "Sorted data groups.")
     return(invisible())
   },
   addCalculationGroups = function(calculationGroupName=NULL, atLevel=NULL, outlineBefore=NULL, outlineAfter=NULL, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addCalculationGroups", outlineBefore, missing(outlineBefore), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addCalculationGroups", outlineAfter, missing(outlineAfter), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addCalculationGroups", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addCalculationGroups", "Adding calculation groups...")
     private$p_parentPivot$calculationsPosition <- private$p_rowOrColumn # will throw an error if trying to add calcs to one axis when already present on the other axis
     if(missing(calculationGroupName)) stop("PivotDataGroup$addCalculationGroups():  calculationGroupName must be specified.", call. = FALSE)
     if(is.null(calculationGroupName)) stop("PivotDataGroup$addCalculationGroups():  calculationGroupName cannot be null.", call. = FALSE)
     if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
       stop(paste0("PivotDataGroup$addCalculationGroups():  There is no Calculation Group named '", calculationGroupName, "' in the Pivot Table."), call. = FALSE)
     if(resetCells) private$p_parentPivot$resetCells()
     # get the calculation group and the calculations to be displayed
     calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(calculationGroup$count==0)
       stop(paste0("PivotDataGroup$addCalculationGroups():  There are no calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- list()
     for (i in 1:calculationGroup$count) {
       calc <- calculationGroup$calculations[[i]]
       if(calc$visible==TRUE) {
         cname <- paste0("calc", sprintf("%06d", calc$displayOrder), "-", sprintf("%06d", i))
         calculations[[cname]] <- calc
       }
     }
     if(length(calculations)==0)
       stop(paste0("PivotDataGroup$addCalculationGroups():  There are no visible calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- calculations[order(names(calculations))]
     # outline info
     outlineBefore <- private$cleanOutlineArg(outline=outlineBefore)
     outlineAfter <- private$cleanOutlineArg(outline=outlineAfter, defaultCaption="")
     # where are the new groups being added?
     if(is.null(atLevel)) {
       # get the current set of leaf groups
       parentGroups <- self$getLeafGroups()
       if((length(private$p_groups)==0)||(is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     else if(atLevel<=0) {
       # immediately below this data group
       parentGroups <- list()
       parentGroups[[1]] <- self
     }
     else {
       # at some number of levels below this group
       levelsBelowThisLevel <- self$getLevelCount()
       if (atLevel>levelsBelowThisLevel) atLevel <- levelsBelowThisLevel
       parentGroups <- self$getLevelGroups(level=atLevel-1)
       if((is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     # render
     newGroups <- list()
     if(outlineBefore$outline||outlineAfter$outline) {
        # render outline groups and always generate calculation cells (even if there is only one calculation)
        for(i in 1:length(parentGroups)) {
           grp <- parentGroups[[i]]
           for(j in 1:length(calculations)) {
              calc <- calculations[[j]]
              # outline before value
              if(outlineBefore$outline){
                 newGrp <- private$createOutlineGroup(parentGroup=grp, outline=outlineBefore, caption=calc$caption,
                                                      calculationGroupName=calculationGroupName, calculationName=calc$calculationName,
                                                      baseStyleName=calc$headingBaseStyleName, styleDeclarations=calc$headingStyleDeclarations,
                                                      sortAnchor="next", resetCells=FALSE)
                 index <- length(newGroups) + 1
                 newGroups[[index]] <- newGrp
              }
              # render the calculation
              newGroup <- grp$addChildGroup(caption=ifelse(outlineBefore$outline, "", calc$caption),
                                            calculationGroupName=calculationGroupName, calculationName=calc$calculationName, isTotal=self$isTotal,
                                            baseStyleName=calc$headingBaseStyleName, styleDeclarations=calc$headingStyleDeclarations, resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newGroup
              # outline before value
              if(outlineAfter$outline){
                 newGrp <- private$createOutlineGroup(parentGroup=grp, outline=outlineAfter, caption=calc$caption,
                                                      calculationGroupName=calculationGroupName, calculationName=calc$calculationName,
                                                      baseStyleName=calc$headingBaseStyleName, styleDeclarations=calc$headingStyleDeclarations,
                                                      sortAnchor="previous", resetCells=FALSE)
                 index <- length(newGroups) + 1
                 newGroups[[index]] <- newGrp
              }
           }
        }
     }
     else {
        # if there is only one calculation (and this is not the top group), just set the calculation directly on the existing leaf nodes,
        # otherwise add a new row and iterate the calculations
        for(i in 1:length(parentGroups)) {
          grp <- parentGroups[[i]]
          if((!is.null(grp$parentGroup))&&(length(calculations)==1)) {
            grp$calculationGroupName <- calculationGroupName
            grp$calculationName <- calculations[[1]]$calculationName
          }
          else {
            for(j in 1:length(calculations)) {
              calc <- calculations[[j]]
              newGroup <- grp$addChildGroup(caption=calc$caption,
                                            calculationGroupName=calculationGroupName, calculationName=calc$calculationName, isTotal=self$isTotal,
                                            baseStyleName=calc$headingBaseStyleName, styleDeclarations=calc$headingStyleDeclarations, resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newGroup
            }
          }
        }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addCalculationGroups", "Added calculation groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
   },
   normaliseDataGroup = function(resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "normaliseDataGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$normaliseDataGroup", "Normalising data group...")
     if(resetCells) private$p_parentPivot$resetCells()
     # get the leaf level groups
     leafGroups <- self$getLeafGroups()
     # get the maximum number of parents of each group
     maxParents <- 0
     if(length(leafGroups)==0) return()
     for(i in 1:length(leafGroups)) {
       ancestors <- leafGroups[[i]]$getAncestorGroups()
       maxParents <- max(maxParents, length(ancestors))
     }
     # add additional child groups if the number of ancestors is less than the maximum number
     groupsAdded <- 0
     for(i in 1:length(leafGroups)) {
       ancestors <- leafGroups[[i]]$getAncestorGroups()
       ancsCount <- length(ancestors)
       if(ancsCount < maxParents) {
         dg <- leafGroups[[i]]
         for(j in 1:(maxParents-ancsCount)) {
           dg <- dg$addChildGroup(doNotExpand=dg$doNotExpand, isEmpty=dg$isEmpty, isOutline=dg$isOutline, isTotal=dg$isTotal,
                                  mergeEmptySpace=dg$mergeEmptySpace, cellBaseStyleName=dg$cellBaseStyleName,
                                  cellStyleDeclarations=dg$cellStyle$declarations)
           groupsAdded <- groupsAdded + 1
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$normaliseDataGroup", "Normalised data group.")
     return(invisible(groupsAdded))
   },
   getNetFilters = function() { # start at the root and apply the filter criteria that gets set at each lower level
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getNetFilters", "Getting net filters...")
     ancestors <- self$getAncestorGroups(includeCurrentGroup=TRUE)
     netFilters <- PivotFilters$new(private$p_parentPivot)
     for(j in length(ancestors):1) {
       acs <- ancestors[[j]]
       filters <- acs$filters
       if(is.null(filters)) next
       if(filters$count==0) next
       for(k in 1:length(filters$filters)) {
         filter <- filters$filters[[k]]
         netFilters$setFilter(filter, action="intersect")
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getNetFilters", "Got net filters.")
     return(invisible(netFilters))
   },
   getNetCalculationName = function() { # start at the current node and work upwards until the first calculation is encountered
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getNetCalculationName", "Getting net calculation...")
     grp <- self
     while (!is.null(grp)) {
       if(!is.null(grp$calculationName)) return(invisible(grp$calculationName))
       grp <- grp$parentGroup
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getNetCalculationName", "Got net calculation.")
     return(invisible())
   },
   isFindMatch = function(matchMode="simple", variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL, emptyGroups="exclude", outlineGroups="exclude") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", outlineGroups, missing(outlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$isFindMatch", "Checking if matches criteria...")
     # a) check empty space
     if((emptyGroups=="exclude")&&(self$isEmpty==TRUE)) return(invisible(FALSE))
     if((emptyGroups=="only")&&(self$isEmpty==FALSE)) return(invisible(FALSE))
     # b) check outline
     if((outlineGroups=="exclude")&&(isTRUE(self$isOutline))) return(invisible(FALSE))
     if((outlineGroups=="only")&&(!isTRUE(self$isOutline))) return(invisible(FALSE))
     # c) check the filter match
     if((!is.null(variableNames))||(!is.null(variableValues))) {
       if(matchMode=="simple") filters <- private$p_filters
       else filters <- self$getNetFilters()
       if(is.null(filters)) return(invisible(FALSE))
       isMatch <- filters$isFilterMatch(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues)
       if(isMatch==FALSE) return(invisible(FALSE))
     }
     # d) check totals criteria
     if((totals=="exclude")&&(self$isTotal==TRUE)) return(invisible(FALSE))
     if((totals=="only")&&(self$isTotal==FALSE)) return(invisible(FALSE))
     # e) check calculation criteria
     if(!is.null(calculationNames)) {
       if(matchMode=="simple") calcName <- private$p_calculationName
       else calcName <- self$getNetCalculationName()
       if(is.null(calcName)) return(invisible(FALSE))
       if(!(calcName %in% calculationNames)) return(invisible(FALSE))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$isFindMatch", "Checked if matches criteria.")
     return(invisible(TRUE))
   },
   findDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                             totals="include", calculationNames=NULL, includeDescendantGroups=FALSE, emptyGroups="exclude", outlineGroups="exclude") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", outlineGroups, missing(outlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$findDataGroups", "Finding data groups...")
     # clear the isMatch flag across all descendants
     clearFlags <- function(dg) {
       dg$isMatch <- FALSE
     }
     grps <- self$getDescendantGroups(includeCurrentGroup=TRUE)
     lapply(grps, clearFlags)
     if(length(grps)==0) return(invisible())
     # search approach changes depending on variable matching method
     setFlags <- function(dg) {
       dg$isMatch <- TRUE
     }
     if(matchMode=="simple") {
       # if simple mode, then just test every data group
       for(i in 1:length(grps)) {
         grp <- grps[[i]]
         if(grp$isMatch==TRUE) next
         if(grp$isFindMatch(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues,
                            totals=totals, calculationNames=calculationNames, emptyGroups=emptyGroups, outlineGroups=outlineGroups)) {
           grp$isMatch <- TRUE
           if(includeDescendantGroups==TRUE) {
             descgrps <- grp$getDescendantGroups(includeCurrentGroup=FALSE)
             if(length(descgrps)>0) lapply(descgrps, setFlags)
           }
         }
       }
     }
     else {
       # iterate the leaf groups...
       leafgrps <- self$getLeafGroups()
       if(is.null(leafgrps)) return(invisible(NULL))
       if(length(leafgrps)==0) return(invisible(NULL))
       for(i in 1:length(leafgrps)) {
         # for each leaf group, start at the leaf level and work up the ancestors towards the root, checking
         # at each level whether the node is a match.
         matches <- list()
         grp <- leafgrps[[i]]
         while(!is.null(grp)) {
           if(grp$isFindMatch(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues,
                              totals=totals, calculationNames=calculationNames, emptyGroups=emptyGroups, outlineGroups=outlineGroups)) {
             matches[[length(matches)+1]] <- grp
             grp <- grp$parentGroup
           }
           else break
         }
         # if we have a match, then set the isMatched flag
         if(includeDescendantGroups==TRUE) {
           # mark the top-most group and all groups beneath (in this branch) as a match
           if(length(matches)>0) {
             for(j in 1:length(matches)) {
               matches[[j]]$isMatch <- TRUE
             }
           }
         }
         else {
           # only mark the topmost group as a match
           if(length(matches)>0) matches[[length(matches)]]$isMatch <- TRUE
         }
       }
     }
     # collect the matching groups
     grps <- self$getDescendantGroups(includeCurrentGroup=TRUE)
     matches <- list()
     for(i in 1:length(grps)) {
       grp <- grps[[i]]
       if(grp$isMatch) matches[[length(matches)+1]] <- grp
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$findDataGroups", "Found data groups.")
     return(invisible(matches))
   },
   setStyling = function(styleDeclarations=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "setStyling", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$setStyling", "Setting style declarations...", list(styleDeclarations))
     if(is.null(styleDeclarations)) private$p_style = NULL
     else if(is.null(private$p_style)) private$p_style = private$p_parentPivot$createInlineStyle(declarations=styleDeclarations)
     else private$p_style$setPropertyValues(styleDeclarations)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$setStyling", "Set style declarations.")
   },
   clearSortGroups = function() {
      private$p_sortGroupsBefore <- list()
      private$p_sortGroupsAfter <- list()
   },
   addSortGroupBefore = function(grp) {
      private$p_sortGroupsBefore[[length(private$p_sortGroupsBefore)+1]]  <- grp
   },
   addSortGroupAfter = function(grp) {
      private$p_sortGroupsAfter[[length(private$p_sortGroupsAfter)+1]]  <- grp
   },
   asList = function() {
     grps <- list()
     if(length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         grps[[i]] = private$p_groups[[i]]$asList()
       }
     }
     lst <- list(
       rc = private$p_rowOrColumn,
       caption = self$caption,
       sortValue = private$p_sortValue,
       filters = private$p_filters$asList(),
       doNotExpand = private$p_doNotExpand,
       isEmpty = private$p_isEmpty,
       isOutline = private$p_isOutline,
       isTotal = private$p_isTotal,
       isLevelSubTotal = private$p_isLevelSubTotal,
       isLevelTotal = private$p_isLevelTotal,
       calculationGroupName = private$p_calculationGroupName,
       calculationName = private$p_calculationName,
       rowColumnNumber = private$p_rowColumnNumber,
       baseStyleName = private$p_baseStyleName,
       style = ifelse(is.null(private$p_style), "", private$p_style$asList()),
       groups = grps
     )
     lst <- lst[order(names(lst))]
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) }
  ),
  active = list(
   instanceId = function(value) { return(invisible(private$p_instanceId)) },
   parentGroup = function(value) { return(invisible(private$p_parentGroup)) },
   childGroups = function(value) { return(invisible(private$p_groups)) },
   childGroupCount = function(value) { return(invisible(length(private$p_groups))) },
   leafGroups = function(value) { return(invisible(self$getLeafGroups())) },
   filters = function(value) { return(invisible(private$p_filters)) },
   variableName = function(value) {
     if(is.null(private$p_filters)) return(invisible(NULL))
     if(private$p_filters$count>1) stop("PivotDataGroup$variableName(): The data group has filters for more than one variable.  Please retrieve the filters via the filters property.", call. = FALSE)
     return(invisible(private$p_filters$filters[[1]]$variableName))
   },
   values = function(value) {
     if(is.null(private$p_filters)) return(invisible(NULL))
     if(private$p_filters$count>1) stop("PivotDataGroup$variableName(): The data group has filters for more than one variable.  Please retrieve the filters via the filters property.", call. = FALSE)
     return(invisible(private$p_filters$filters[[1]]$values))
   },
   calculationGroupName = function(value) {
     if(missing(value)) { return(invisible(private$p_calculationGroupName)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "calculationGroupName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       }
       if(!(private$p_parentPivot$calculationGroups$isExistingCalculationGroup(value))) {
         stop(paste0("PivotDataGroup$calculationGroupName(): The Calculation Group '", value, "' does not exist in the Pivot Table."), call. = FALSE)
       }
       private$p_calculationGroupName <- value
       return(invisible())
     }
   },
   calculationName = function(value) {
     if(missing(value)) { return(invisible(private$p_calculationName)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "calculationName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       }
       if(is.null(private$p_calculationGroupName))
         stop("PivotDataGroup$calculationName(): Specify the Calculation Group before the Calculation.", call. = FALSE)
       calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(private$p_calculationGroupName)
       if(!(calculationGroup$isExistingCalculation(value))) {
         stop(paste0("PivotDataGroup$calculationName(): The Calculation '", value,
                     "' does not exist in the Calculation Group '", private$p_calculationGroupName, "'"), call. = FALSE)
       }
       private$p_calculationName <- value
       return(invisible())
     }
   },
   doNotExpand = function(value) { return(invisible(private$p_doNotExpand)) },
   isEmpty = function(value) { return(invisible(private$p_isEmpty)) },
   isOutline = function(value) { return(invisible(private$p_isOutline)) },
   caption = function(value) {
     if(missing(value)) {
       if(is.null(private$p_caption)) {
         if((private$p_isTotal)&&(!is.null(private$p_parentGroup))&&
            (length(private$p_parentGroup$childGroups)<2)) return(invisible(""))
         if(is.null(private$p_filters)) return(invisible(""))
         else return(invisible(private$p_filters$asString(includeVariableName=FALSE)))
       }
       else return(invisible(private$p_caption))
     }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "caption", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_caption <- value
     }
   },
   sortValue = function(value) { return(invisible(private$p_sortValue)) },
   isTotal = function(value) { return(invisible(private$p_isTotal)) },
   isLevelSubTotal = function(value) { return(invisible(private$p_isLevelSubTotal)) },
   isLevelTotal = function(value) { return(invisible(private$p_isLevelTotal)) },
   rowColumnNumber = function(value) {
     if(missing(value)) { return(invisible(private$p_rowColumnNumber)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "rowColumnNumber", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "number"))
       }
       if(!is.null(value)) {
         if(!is.integer(value)) stop("PivotDataGroup$rowColumnNumber(): rowColumnNumber must be an integer", call. = FALSE)
       }
       private$p_rowColumnNumber <- value
       return(invisible())
     }
   },
   baseStyleName = function(value) {
     if(missing(value)) { return(invisible(private$p_baseStyleName)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "baseStyleName", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_baseStyleName <- value
       return(invisible())
     }
   },
   style = function(value) {
     if(missing(value)) { return(invisible(private$p_style)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "style", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyle")
       }
       private$p_style <- value
       return(invisible())
     }
   },
   mergeEmptySpace = function(value) {
      if(missing(value)) { return(invisible(private$p_mergeEmptySpace)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "mergeEmptySpace", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
         }
         private$p_mergeEmptySpace <- value
         return(invisible())
      }
   },
   cellBaseStyleName = function(value) {
      if(missing(value)) { return(invisible(private$p_cellBaseStyleName)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cellBaseStyleName", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
         }
         private$p_cellBaseStyleName <- value
         return(invisible())
      }
   },
   cellStyle = function(value) {
      if(missing(value)) { return(invisible(private$p_cellStyle)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyle")
         }
         private$p_cellStyle <- value
         return(invisible())
      }
   },
   fixedWidthSize = function(value) {
     if(missing(value)) { return(invisible(private$p_fixedWidthSize)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "fixedWidthSize", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       }
       private$p_fixedWidthSize <- value
       return(invisible())
     }
   },
   isMatch = function(value) {
     if(missing(value)) { return(invisible(private$p_isMatch)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isMatch", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       }
       private$p_isMatch <- value
       return(invisible())
     }
   },
   isRendered = function(value) {
     if(missing(value)) { return(invisible(private$p_isRendered)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isRendered", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       }
       private$p_isRendered <- value
       return(invisible())
     }
   },
   isWithinVisibleRange = function(value) {
     if(missing(value)) { return(invisible(private$p_isWithinVisibleRange)) }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isWithinVisibleRange", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       }
       private$p_isWithinVisibleRange <- value
       return(invisible())
     }
   },
   visibleChildGroupCount = function(value) {
     if(is.null(private$p_groups)) return(invisible(0))
     if(length(private$p_groups)==0) return(invisible(0))
     visibleCount <- 0
     for(i in 1:length(private$p_groups)) {
       if(private$p_groups[[i]]$isWithinVisibleRange==TRUE) visibleCount <- visibleCount + 1
     }
     return(invisible(visibleCount))
   },
   visibleDescendantGroupCount = function(value) {
     if(is.null(private$p_groups)) return(invisible(0))
     if(length(private$p_groups)==0) return(invisible(0))
     visibleCount <- 0
     groups <- self$getDescendantGroups(includeCurrentGroup=FALSE)
     for(i in 1:length(groups)) {
       if(groups[[i]]$isWithinVisibleRange==TRUE) visibleCount <- visibleCount + 1
     }
     return(invisible(visibleCount))
   },
   visibleLeafGroupCount = function(value) {
     if(is.null(private$p_groups)) return(invisible(0))
     if(length(private$p_groups)==0) return(invisible(0))
     visibleCount <- 0
     groups <- self$getLeafGroups()
     for(i in 1:length(groups)) {
       if(groups[[i]]$isWithinVisibleRange==TRUE) visibleCount <- visibleCount + 1
     }
     return(invisible(visibleCount))
   },
   sortAnchor = function(value) {
      if(missing(value)) { return(invisible(private$p_sortAnchor)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "sortAnchor", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
         }
         private$p_sortAnchor <- value
         return(invisible())
      }
   },
   sortGroupsBefore = function(value) { return(invisible(private$p_sortGroupsBefore)) },
   sortGroupsAfter = function(value) { return(invisible(private$p_sortGroupsAfter)) }
  ),
  private = list(
   p_parentGroup = NULL,
   p_parentPivot = NULL,
   p_instanceId = NULL,              # a unique identifier (unique across all row/column groups and cells) to allow "grp1 is grp2" type comparisons
   p_rowOrColumn = NULL,
   p_doNotExpand = NULL,             # used to control whether addDataGroups expands the group (or just adds a single group underneath)
   p_isEmpty = NULL,                 # used to guarantee that a row/column/cells will have no value (PivotCalculator bypasses these cells)
   p_isOutline = NULL,               # used to indicate this is an outline row/column (i.e. will pickup the outline style)
   p_caption = NULL,
   p_sortValue = NULL,
   p_isTotal = NULL,
   p_isLevelSubTotal = NULL,
   p_isLevelTotal = NULL,
   p_visualTotals = NULL,
   p_filters = NULL,
   p_groups = NULL,
   p_calculationGroupName = NULL,
   p_calculationName = NULL,
   p_rowColumnNumber = NULL,
   p_baseStyleName = NULL,
   p_style = NULL, # this should be unique per data group (not shared across multiple data groups).
                   # See a discussion of the p_style private variable in the PivotCells class for more details.
   p_mergeEmptySpace = NULL,
   p_cellBaseStyleName = NULL,
   p_cellStyle = NULL,

   p_sortAnchor = NULL,
   p_sortGroupsBefore = NULL,  # internal property used during sorting:  a list of groups that are fixed to this and come before it
   p_sortGroupsAfter = NULL,   # internal property used during sorting:  a list of groups that are fixed to this and come after it

   # internal fields to help with complex operations
   p_fixedWidthSize = 0,
   p_isMatch = FALSE, # helper flag used when searching through data groups
   p_isWithinVisibleRange = TRUE, # helper flag to allow a subsection of the pivot table to be output
   p_isRendered = FALSE, # helper flag to keep track of which data groups have already been rendered

   # private functions:
   cleanOutlineArg = function(outline=NULL, defaultCaption="{value}", defaultIsEmpty=TRUE) { # checks values and provides defaults for settings (so following code does not need is.null checks)
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cleanOutlineArg", outline, missing(outline), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("logical", "list"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cleanOutlineArg", defaultCaption, missing(defaultCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cleanOutlineArg", defaultIsEmpty, missing(defaultIsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$cleanOutlineArg", "Cleaning outline argument...")
      if("logical" %in% class(outline)) {
         if(isTRUE(outline)) outline <- list()
         else outline <- NULL
      }
      clean <- list()
      # global switch
      if(is.null(outline)) {
         clean$outline <- FALSE
         return(clean)
      }
      clean$outline <- TRUE
      # setting: caption
      if(is.null(outline$caption)) clean$caption <- defaultCaption
      else {
         if(!("character" %in% class(outline$caption))) stop("PivotDataGroup$cleanOutlineArg(): The caption for the outline data group must be a character value.", call. = FALSE)
         clean$caption <- outline$caption
      }
      # setting: isEmpty
      if(is.null(outline$isEmpty)) clean$isEmpty <- defaultIsEmpty
      else {
         if(!("logical" %in% class(outline$isEmpty))) stop("PivotDataGroup$cleanOutlineArg(): The isEmpty value for the outline data group must be a logical (TRUE/FALSE) value.", call. = FALSE)
         clean$isEmpty <- outline$isEmpty
      }
      # setting: mergeSpace
      allowedMergeSpaceValues <- c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2")
      if(is.null(outline$mergeSpace)) clean$mergeSpace <- ifelse(defaultIsEmpty, "dataGroupsAndCellsAs2", "dataGroupsOnly")
      else {
         if(!("character" %in% class(outline$mergeSpace))) stop("PivotDataGroup$cleanOutlineArg(): The mergeSpace value for the outline data group must be a character value.", call. = FALSE)
         if(!(outline$mergeSpace %in% allowedMergeSpaceValues)) stop("PivotDataGroup$cleanOutlineArg(): The mergeSpace value for the outline data group must be one of the following values: doNotMerge, dataGroupsOnly, cellsOnly, dataGroupsAndCellsAs1, dataGroupsAndCellsAs2.", call. = FALSE)
         clean$mergeSpace <- outline$mergeSpace
      }
      # setting: styling
      allowedStylingValues <- c("plain", "outline")
      if(is.null(outline$styling)) clean$styling <- "outline"
      else {
         if(!("character" %in% class(outline$styling))) stop("PivotDataGroup$cleanOutlineArg(): The styling value for the outline data group must be a character value.", call. = FALSE)
         if(!(outline$styling %in% allowedStylingValues)) stop("PivotDataGroup$cleanOutlineArg(): The styling value for the outline data group must be one of the following values: plain, outline.", call. = FALSE)
         clean$styling <- outline$styling
      }
      # setting: groupStyleName (allow null)
      if(!is.null(outline$groupStyleName)) {
         if(!("character" %in% class(outline$groupStyleName))) stop("PivotDataGroup$cleanOutlineArg(): The groupStyleName for the outline data group must be a character value.", call. = FALSE)
         clean$groupStyleName <- outline$groupStyleName
      }
      # setting: groupStyleDeclarations (allow null)
      if(!is.null(outline$groupStyleDeclarations)) {
         if(!("list" %in% class(outline$groupStyleDeclarations))) stop("PivotDataGroup$cleanOutlineArg(): The groupStyleDeclarations for the outline data group must a list.", call. = FALSE)
         clean$groupStyleDeclarations <- outline$groupStyleDeclarations
      }
      # setting: cellStyleName (allow null)
      if(!is.null(outline$cellStyleName)) {
         if(!("character" %in% class(outline$cellStyleName))) stop("PivotDataGroup$cleanOutlineArg(): The cellStyleName for the outline data group must be a character value.", call. = FALSE)
         clean$cellStyleName <- outline$cellStyleName
      }
      # setting: cellStyleDeclarations (allow null)
      if(!is.null(outline$cellStyleDeclarations)) {
         if(!("list" %in% class(outline$cellStyleDeclarations))) stop("PivotDataGroup$cleanOutlineArg(): The cellStyleDeclarations for the outline data group must a list.", call. = FALSE)
         clean$cellStyleDeclarations <- outline$cellStyleDeclarations
      }
      # check mergeSpace and isEmpty are compatible
      if(!(clean$isEmpty)) {
         if(clean$mergeSpace=="cellsOnly") clean$mergeSpace <- "doNotMerge"
         else if(clean$mergeSpace %in% c("dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2")) clean$mergeSpace <- "dataGroupsOnly"
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$cleanOutlineArg", "Cleaned outline argument.")
      return(clean)
   },
   createOutlineGroup = function(parentGroup=NULL, outline=NULL, variableName=NULL, values=NULL, caption=NULL,
                                 calculationGroupName=NULL, calculationName=NULL,
                                 baseStyleName=NULL, styleDeclarations=NULL, isLevelTotal=FALSE, sortAnchor=NULL, resetCells=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", outline, missing(outline), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct", "POSIXlt"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$createOutlineGroup", "Creating outline group...")
      # preparation
      if(is.null(caption)) caption <- as.character(values)
      if((!is.null(caption))&&(length(caption)>0)) {
         caption <- gsub("\\{value\\}", caption, outline$caption)
      }
      isEmpty <- outline$isEmpty
      isOutline <- FALSE
      isTotal <- FALSE
      isLevelTotal <- FALSE
      isLevelSubTotal <- FALSE
      if(isLevelTotal) {
         isEmpty <- FALSE
         isTotal <- TRUE
         isLevelTotal <- TRUE
      }
      else if(!isTRUE(isEmpty)) {
         isEmpty <- FALSE
         isTotal <- TRUE
         isLevelSubTotal <- TRUE
      }
      mergeSpace <- outline$mergeSpace
      if(isTRUE(outline$styling=="outline")) isOutline <- TRUE
      groupStyleName <- baseStyleName
      groupStyleDeclarations <- styleDeclarations
      if(!is.null(outline$groupStyleName)) groupStyleName <- outline$baseStyleName
      if(!is.null(outline$groupStyleDeclarations)) groupStyleDeclarations <- outline$groupStyleDeclarations
      cellStyleName <- NULL
      cellStyleDeclarations <- NULL
      if(!is.null(outline$cellStyleName)) cellStyleName <- outline$cellStyleName
      if(!is.null(outline$cellStyleDeclarations)) cellStyleDeclarations <- outline$cellStyleDeclarations
      # generate group
      newGrp <- parentGroup$addChildGroup(variableName=variableName, values=values, # so that group has a reference to the variable & values
                                  caption=caption, calculationGroupName=calculationGroupName, calculationName=calculationName,
                                  doNotExpand=TRUE, isEmpty=isEmpty, isOutline=isOutline, mergeEmptySpace=mergeSpace,
                                  isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                  baseStyleName=groupStyleName, styleDeclarations=groupStyleDeclarations,
                                  cellBaseStyleName=cellStyleName, cellStyleDeclarations=cellStyleDeclarations,
                                  sortAnchor=sortAnchor, resetCells=resetCells)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$createOutlineGroup", "Created outline group.")
      return(newGrp)
   },
   formatValue = function(value=NULL, format=NULL, dataFmtFuncArgs=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "formatValue", dataFmtFuncArgs, missing(dataFmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$formatValue", "Formatting value...")
     if(is.null(value)) return(invisible(NULL))
     if(is.null(format)) return(base::as.character(value))
     clsv <- class(value)
     if(("numeric" %in% clsv)||("integer" %in% clsv)) {
       clsf <- class(format)
       if("character" %in% clsf) value <- sprintf(format, value)
       else if ("list" %in% clsf) {
         args <- format
         args$x <- value
         value <- do.call(base::format, args)
       }
       else if ("function" %in% class(format)) {
          if (is.null(dataFmtFuncArgs)) value <- format(value)
          else {
             args <- dataFmtFuncArgs
             args$x <- value
             value <- do.call(format, args)
          }
       }
       else value <- base::as.character(value)
     }
     else if("logical" %in% clsv) {
       clsf <- class(format)
       if("character" %in% clsf) {
         if (length(format)==2) {
           if(value==FALSE) value <- format[1]
           else if(value==TRUE) value <- format[2]
           else value <- "NA"
         }
         else if (length(format)==3) {
           if(value==FALSE) value <- format[1]
           else if(value==TRUE) value <- format[2]
           else value <- format[3]
         }
         else value <- sprintf(format, value)
       }
       else if ("list" %in% clsf) {
         args <- format
         args$x <- value
         value <- do.call(base::format, args)
       }
       else if ("function" %in% class(format)) {
          if (is.null(dataFmtFuncArgs)) value <- format(value)
          else {
             args <- dataFmtFuncArgs
             args$x <- value
             value <- do.call(format, args)
          }
       }
       else value <- base::as.character(value)
     }
     else if(("Date" %in% clsv)||("POSIXct" %in% clsv)||("POSIXlt" %in% clsv)) {
       clsf <- class(format)
       if ("character" %in% clsf) {
         if (format %in% c("%d","%i","%o","%x","%X")) value <- sprintf(format, value)
         else {
           args <- list(format)
           args$x <- value
           value <- do.call(base::format, args)
         }
       }
       else if ("list" %in% clsf) {
         args <- format
         args$x <- value
         value <- do.call(base::format, args)
       }
       else if ("function" %in% class(format)) {
          if (is.null(dataFmtFuncArgs)) value <- format(value)
          else {
             args <- dataFmtFuncArgs
             args$x <- value
             value <- do.call(format, args)
          }
       }
       else value <- base::as.character(value)
     }
     else value <- base::as.character(value)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$formatValue", "Formated value.")
     return(invisible(value))
   },
   removeGroupFromPrivateSortLists = function(group=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroupFromPrivateSortLists", group, missing(group), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotDataGroup")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroupFromPrivateSortLists", "Removing group from sort lists...")
     # group to find
     instanceId <- group$instanceId
     # remove from p_sortGroupsBefore
     index <- NULL
     if (length(private$p_sortGroupsBefore) > 0) {
       for (i in 1:length(private$p_sortGroupsBefore)) {
         if(private$p_sortGroupsBefore[[i]]$instanceId==instanceId) {
           index <- i
           break
         }
       }
     }
     if (!is.null(index)) private$p_sortGroupsBefore[[index]] <- NULL
     # remove from p_sortGroupsAfter
     index <- NULL
     if (length(private$p_sortGroupsAfter) > 0) {
       for (i in 1:length(private$p_sortGroupsAfter)) {
         if(private$p_sortGroupsAfter[[i]]$instanceId==instanceId) {
           index <- i
           break
         }
       }
     }
     if (!is.null(index)) private$p_sortGroupsAfter[[index]] <- NULL
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroupFromPrivateSortLists", "Removed group from sort lists.", list(index=childIndex))
     return(invisible())
   }
  )
)
