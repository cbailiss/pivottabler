
#' R6 class that defines a row or column data group.
#'
#' @description
#' The `PivotDataGroup` class represents one row or column heading in a pivot
#' table.  Data groups exist in a hierarchy and have a parent-child relationship,
#' i.e. each `PivotDataGroup` instance can have one or more child data groups.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @import dplyr
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotDataGroup <- R6::R6Class("PivotDataGroup",
  public = list(

   #' @description
   #' Create a new `PivotDataGroup` object.
   #' @param parentGroup The parent `PivotDataGroup` instance that this
   #' `PivotDataGroup` instance belongs to.
   #' @param parentPivot The pivot table that this `PivotDataGroup`
   #' instance belongs to.
   #' @param rowOrColumn Either "row" or "column" indicating which axis
   #' this data group exists on.
   #' @param doNotExpand Default value `FALSE` - specify `TRUE` to
   #' prevent the high-level methods such as `addDataGroups()` from adding
   #' child groups.
   #' @param isEmpty Default value `FALSE`, specify `TRUE` to mark that
   #' this group contains no data (e.g. if
   #' it is part of a header or outline row)
   #' @param isOutline Default value `FALSE` - specify `TRUE` to mark
   #' that this data group is an outline group.
   #' @param styleAsOutline Default value `FALSE` - specify `TRUE` to style
   #' this data group as an outline group.  Only applicable when
   #' `isOutline` is `TRUE`.
   #' @param captionTemplate A character value that specifies the template
   #' for the data group caption, default "{values}".
   #' @param caption Effectively a hard-coded caption that overrides the
   #' built-in logic for generating a caption.
   #' @param isTotal Default `FALSE` - specify `TRUE` to mark that this
   #' data group is a total.
   #' @param isLevelSubTotal Default `FALSE` - specify `TRUE` to mark that
   #' this data group is a sub-total within a level.
   #' @param isLevelTotal Default `FALSE` - specify `TRUE` to mark that
   #' this data group is level total.
   #' @param variableName A character value that specifies the name of the
   #' variable in the data frame that the group relates to and will filter.
   #' @param filterType Must be one of "ALL", "VALUES", or "NONE" to specify
   #' the filter type:
   #' ALL means no filtering is applied.
   #' VALUEs is the typical value used to specify that `variableName` is
   #' filtered to only `values`.
   #' NONE means no data will match this data group.
   #' @param values A vector that specifies the filter values applied to
   #' `variableName` to select the data to match this row/column in the pivot
   #' table.
   #' @param calculationGroupName For calculation groups, this character value
   #' specifies the calculation group that `calculationName` belongs to.
   #' @param calculationName For calculation groups, this character value
   #' specifies the name of the calculation.
   #' @param baseStyleName The style name for the data group.
   #' @param styleDeclarations A list of CSS style declarations to overlay
   #' on top of the base style.
   #' @param mergeEmptySpace A character value that specifies how empty space
   #' should be merged. This is typically only used with outline groups
   #' (so applies to row groups only, not column groups).  Must be one of
   #' "doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1" or
   #' "dataGroupsAndCellsAs2".  See the "Regular Layout" vignette
   #' for more information.
   #' @param cellBaseStyleName The style name for cells related to this data
   #' group.
   #' @param cellStyleDeclarations A list of CSS style declarations to overlay
   #' on top of the base style for cells related to this data group
   #' @param sortAnchor Used to specify sort behaviour for outline groups, must
   #' be one of "fixed", "next" or "previous".
   #' @param outlineLinkedGroupId Used to link an outline group to the value
   #' data group which has the child data groups.
   #' @return A new `PivotDataGroup` object.
   initialize = function(parentGroup=NULL, parentPivot=NULL, rowOrColumn=NULL, # common properties
                         doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, styleAsOutline=FALSE, # common properties
                         captionTemplate="{value}", caption=NULL, # common properties
                         isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE, # total properties
                         variableName=NULL, filterType="ALL", values=NULL, # filter properties
                         calculationGroupName=NULL, calculationName=NULL, # calculation properties
                         baseStyleName=NULL, styleDeclarations=NULL,
                         mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                         sortAnchor=NULL, outlineLinkedGroupId=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotDataGroup")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", rowOrColumn, missing(rowOrColumn), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", styleAsOutline, missing(styleAsOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", captionTemplate, missing(captionTemplate), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "initialize", outlineLinkedGroupId, missing(outlineLinkedGroupId), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
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
     private$p_styleAsOutline <- styleAsOutline
     private$p_captionTemplate <- captionTemplate
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
     private$p_outlineLinkedGroupId <- outlineLinkedGroupId
     private$p_sortGroupsBefore <- list()
     private$p_sortGroupsAfter <- list()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$new", "Created new data group.")
   },

   #' @description
   #' Retrieve the level number in the hierarchy that the current data
   #' group exists at.
   #' @return An integer value specifying the level number where the
   #' data group exists.
   getLevelNumber = function() {
      .Deprecated(new="levelNumber")
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

   #' @description
   #' Get all of the data groups above the current data group in the
   #' parent-child data group hierarchy.
   #' @param ancestors A list containing ancestors closer to the current data
   #' group - to enable recursive execution of this function, or `NULL` to
   #' begin with.
   #' @param includeCurrentGroup Specify `TRUE` to include the current group
   #' in the return value.
   #' @return A list of data groups, where element 1 is the parent of the
   #' current group, element 2 is the grandparent of the current group, etc.
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

   #' @description
   #' Get all of the data groups below the current
   #' data group in the parent-child data group hierarchy.
   #' @param descendants A list containing descendants closer to the current data
   #' group - to enable recursive execution of this function, or `NULL` to
   #' begin with.
   #' @param includeCurrentGroup Specify `TRUE` to include the current group
   #' in the return value.
   #' @return A list of descendant data groups.
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

   #' @description
   #' Get all of the data groups below the current
   #' data group in the parent-child data group hierarchy.
   #' @param leafGroups A list containing other leaf-level groups - to enable
   #' recursive execution of this function, or `NULL` to begin with.
   #' @return A list of leaf-level data groups.
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

   #' @description
   #' Count the number of levels in the data group hierarchy.
   #' @param includeCurrentLevel Default `FALSE` to exclude the current level from the
   #' level count (since this method is most often called on the hidden root group).
   #' @return The maximum number of levels in the hierarchy.
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

   #' @description
   #' Retrieve all of the data groups at a specific level in the data group
   #' hierarchy.
   #' @param level An integer specifying the level number.
   #' Level 0 represents the current data group.
   #' @param levelGroups A list containing groups accumulated so far -
   #' to enable recursive execution of this function, or `NULL` to begin with.
   #' @return A list of data groups at the specified level in the hierarchy.
   getLevelGroups = function(level=NULL, levelGroups=NULL) {
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


   #' @description
   #' Retrieve the a list of the typically two or three related data groups that
   #' were created as one outlined group.
   #' @param group The group to find the related outline groups.
   #' @return A list of related outline data groups.
   getRelatedOutlineGroups = function(group=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "getRelatedOutlineGroups", group, missing(group), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getRelatedOutlineGroups", "Getting related outline groups...")
     # if no group specified, find the outline group for the current group
     if(is.null(group)) group <- self
     # if we are at the top (i.e. the hidden row or column group) then there are no siblings
     if(is.null(group$parentGroup)) return(invisible(NULL))
     # we need to find the instance id of the non-outline group
     # i.e. there are two/three groups at the same level (outlineBefore, normal, outlineAfter)
     # the outlineLinkedGroupId on the outlineBefore and outlineAfter groups refers to the normal group between them
     outlineLinkedGroupId <- NULL
     # determine scenario
     if(group$isOutline)
     {
       # current group is an outline group
       if(is.null(group$outlineLinkedGroupId)) {
         # group is no longer linked to a non-outline group
         return(invisible(NULL))
       }
       outlineLinkedGroupId <- group$outlineLinkedGroupId
     }
     else {
       if(is.null(group$outlineLinkedGroupId)) {
         # group is not an outline group, so need to find the outline groups that refer to this group
         outlineLinkedGroupId <- group$instanceId
       }
       else {
         # this should never happen - can't have a normal group that has a value for group$outlineLinkedGroupId
         return(invisible(NULL))
       }
     }
     # iterate through the siblings
     grps <- list()
     siblings <- group$parentGroup$childGroups
     if((!is.null(siblings))&&(length(siblings)>0)) {
       for(i in 1:length(siblings)) {
         grp <- siblings[[i]]
         if(grp$instanceId==outlineLinkedGroupId) {
           grps[[length(grps)+1]] <- grp
           next
         }
         if(is.null(grp$outlineLinkedGroupId)) next
         if(grp$outlineLinkedGroupId==outlineLinkedGroupId) {
           grps[[length(grps)+1]] <- grp
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getRelatedOutlineGroups", "Got related outline groups.")
     return(invisible(grps))
   },

   #' @description
   #' Get the index of a child group (or groups) in the current groups list of child groups.
   #' @param childGroup A single data group or a list of data groups that are children
   #' of the current group.
   #' @return An integer vector.
   getChildIndex = function(childGroup=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "getChildIndex", childGroup, missing(childGroup), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("list", "PivotDataGroup"), allowedListElementClasses="PivotDataGroup")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getChildIndex", "Getting child index...")
      grps <- childGroup
      if("PivotDataGroup" %in% class(grps)) grps <- list(childGroup)
      indexes <- vector("integer", length(grps))
      if(length(grps)>0) {
         for(i in 1:length(grps)) {
            childIndex <- NULL
            if (length(private$p_groups) > 0) {
               instanceId <- grps[[i]]$instanceId
               for (j in 1:length(private$p_groups)) {
                  if(private$p_groups[[j]]$instanceId==instanceId) {
                     childIndex <- j
                     break
                  }
               }
            }
            if(is.null(childIndex)) indexes[i] <- NA
            else indexes[i] <- childIndex
         }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getChildIndex", "Got child index.", list(index=childIndex))
      return(invisible(indexes))
   },

   #' @description
   #' Find the index of a child group (or groups) corresponding to the specified
   #' instance id(s) in the current groups list of child groups.
   #' @param childGroupInstanceId An integer vector containing the instance ids
   #' of child groups of the current group.
   #' @return An integer vector.
   findChildIndex = function(childGroupInstanceId=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findChildIndex", childGroupInstanceId, missing(childGroupInstanceId), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$findChildIndex", "Finding child group index by instance Id...")
     indexes <- vector("integer", length(childGroupInstanceId))
     if(length(childGroupInstanceId)>0) {
       for(i in 1:length(childGroupInstanceId)) {
          childIndex <- NULL
          if (length(private$p_groups) > 0) {
             for (j in 1:length(private$p_groups)) {
               if(private$p_groups[[j]]$instanceId==childGroupInstanceId[i]) {
                 childIndex <- j
                 break
               }
             }
          }
          if(is.null(childIndex)) indexes[i] <- NA
          else indexes[i] <- childIndex
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$findChildIndex", "Found child group index by instance Id.")
     return(invisible(indexes))
   },

   #' @description
   #' Add a new data group as a child of the current data group.
   #' The new group is added as the last child unless an index
   #' is specified.
   #' @param variableName A character value that specifies the name of the
   #' variable in the data frame that the group relates to and will filter.
   #' @param filterType Must be one of "ALL", "VALUES", or "NONE" to specify
   #' the filter type:\cr
   #' ALL means no filtering is applied.\cr
   #' VALUEs is the typical value used to specify that `variableName` is
   #' filtered to only `values`.\cr
   #' NONE means no data will match this data group.
   #' @param values A vector that specifies the filter values applied to
   #' `variableName` to select the data to match this row/column in the pivot
   #' table.
   #' @param doNotExpand Default value `FALSE` - specify `TRUE` to
   #' prevent the high-level methods such as `addDataGroups()` from adding
   #' child groups.
   #' @param isEmpty Default value `FALSE`, specify `TRUE` to mark that
   #' this group contains no data (e.g. if
   #' it is part of a header or outline row)
   #' @param isOutline Default value `FALSE` - specify `TRUE` to mark
   #' that this data group is an outline group.
   #' @param styleAsOutline Default value `FALSE` - specify `TRUE` to style
   #' this data group as an outline group.  Only applicable when
   #' `isOutline` is `TRUE`.
   #' @param captionTemplate A character value that specifies the template
   #' for the data group caption, default "{values}".
   #' @param caption Effectively a hard-coded caption that overrides the
   #' built-in logic for generating a caption.
   #' @param isTotal Default `FALSE` - specify `TRUE` to mark that this
   #' data group is a total.
   #' @param isLevelSubTotal Default `FALSE` - specify `TRUE` to mark that
   #' this data group is a sub-total within a level.
   #' @param isLevelTotal Default `FALSE` - specify `TRUE` to mark that
   #' this data group is level total.
   #' @param calculationGroupName For calculation groups, this character value
   #' specifies the calculation group that `calculationName` belongs to.
   #' @param calculationName For calculation groups, this character value
   #' specifies the name of the calculation.
   #' @param baseStyleName The style name for the data group.
   #' @param styleDeclarations A list of CSS style declarations to overlay
   #' on top of the base style.
   #' @param insertAtIndex An integer that specifies the index in the list
   #' of child groups where the new group should be inserted.
   #' @param insertBeforeGroup Specifies an existing group that the new group
   #'  should be inserted before.
   #' @param insertAfterGroup Specifies an existing group that the new group
   #'  should be inserted after
   #' @param mergeEmptySpace A character value that specifies how empty space
   #' should be merged. This is typically only used with outline groups
   #' (so applies to row groups only, not column groups).  Must be one of
   #' "doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1" or
   #' "dataGroupsAndCellsAs2".  See the "Regular Layout" vignette
   #' for more information.
   #' @param cellBaseStyleName The style name for cells related to this data
   #' group.
   #' @param cellStyleDeclarations A list of CSS style declarations to overlay
   #' on top of the base style for cells related to this data group
   #' @param sortAnchor Used to specify sort behaviour for outline groups, must
   #' be one of "fixed", "next" or "previous".
   #' @param outlineLinkedGroupId Used to link an outline group to the value
   #' data group which has the child data groups.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return The new `PivotDataGroup` object.
   addChildGroup = function(variableName=NULL, filterType="ALL", values=NULL,
                            doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, styleAsOutline=FALSE,
                            captionTemplate="{value}", caption=NULL,
                            isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
                            calculationGroupName=NULL, calculationName=NULL,
                            baseStyleName=NULL, styleDeclarations=NULL,
                            insertAtIndex=NULL, insertBeforeGroup=NULL, insertAfterGroup=NULL,
                            mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                            sortAnchor=NULL, outlineLinkedGroupId=NULL, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", styleAsOutline, missing(styleAsOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", captionTemplate, missing(captionTemplate), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", outlineLinkedGroupId, missing(outlineLinkedGroupId), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addChildGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addChildGroup", "Adding child group...",
                                   list(captionTemplate=captionTemplate, caption=caption,
                                        doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                                        isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                        variableName=variableName, values=values,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName,
                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                        mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName,
                                        cellStyleDeclarations=cellStyleDeclarations,
                                        sortAnchor=sortAnchor, outlineLinkedGroupId=outlineLinkedGroupId, resetCells=resetCells))
     total <- isTotal | self$isTotal
     grp <- PivotDataGroup$new(parentGroup=self, parentPivot=private$p_parentPivot, rowOrColumn=private$p_rowOrColumn,
                               doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                               captionTemplate=captionTemplate, caption=caption,
                               isTotal=total, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                               variableName=variableName, filterType=filterType, values=values,
                               calculationGroupName=calculationGroupName, calculationName=calculationName,
                               baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                               mergeEmptySpace=mergeEmptySpace,
                               cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations,
                               sortAnchor=sortAnchor, outlineLinkedGroupId=outlineLinkedGroupId)
     # outline groups must either be marked doNotExpand, isEmpty or isTotal (otherwise we will accidentally expand them, which would look odd)
     if(isOutline) {
        if((!doNotExpand)&&(!isEmpty)&&(!total)) doNotExpand <- TRUE # Outline groups must either be set as doNotExpand=TRUE, isEmpty=TRUE or isTotal=TRUE.
     }
     # default to style as outline if not specified
     if(isOutline && missing(styleAsOutline)) styleAsOutline <- TRUE
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

   #' @description
   #' Remove a data group that is a child of the current data group.
   #' @param index An index that specifies the location of the group to
   #' remove in the list of child groups.
   #' @param group A `PivotDataGroup` object to be removed.  Only one of
   #' `index` or `group` needs to be specified.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return No return value.
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
      # done
      if(resetCells) private$p_parentPivot$resetCells()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeChildGroup", "Removed child group.")
   },

   #' @description
   #' Remove the current data group.
   #' @param removeAncestorsIfNoRemainingChildren Default `FALSE` - specify `TRUE` to
   #' recursively remove ancestor groups if they have no remaining child groups.
   #' @param removedRelatedOutlineGroups Default `FALSE` - specify `TRUE` to remove
   #' related outline groups.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return No return value.
   removeGroup = function(removeAncestorsIfNoRemainingChildren=FALSE, removedRelatedOutlineGroups=FALSE, resetCells=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroup", removeAncestorsIfNoRemainingChildren, missing(removeAncestorsIfNoRemainingChildren), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroup", removedRelatedOutlineGroups, missing(removedRelatedOutlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "removeGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroup", "Removing group...")
     if(is.null(private$p_parentGroup)) stop("PivotDataGroup$removeGroup():  Cannot remove the top group on rows/columns.", call. = FALSE)
     if(removedRelatedOutlineGroups==TRUE) {
       grps <- self$getRelatedOutlineGroups(group=self)
       if((!is.null(grps))&&(length(grps)>0)) {
         for(i in 1:length(grps)) {
           private$p_parentGroup$removeChildGroup(group=grps[[i]], resetCells=resetCells)
         }
       }
     }
     else {
       private$p_parentGroup$removeChildGroup(group=self, resetCells=resetCells)
     }
     if((removeAncestorsIfNoRemainingChildren==TRUE)&&(private$p_parentGroup$childGroupCount==0)) {
       private$p_parentGroup$removeGroup(removeAncestorsIfNoRemainingChildren=removeAncestorsIfNoRemainingChildren,
                                         removedRelatedOutlineGroups=removedRelatedOutlineGroups, resetCells=resetCells)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$removeGroup", "Removed group.")
   },


   #' @description
   #' Add multiple new data groups based on the distinct values in a data frame
   #' column or using explicitly specified data values.
   #' See the "Irregular Layout" vignette for example usage.
   #' @details
   #' There are broadly three different ways to call `addDataGroups()`:\cr
   #' (1) dataName=name, fromData=TRUE, onlyCombinationsThatExist=TRUE - which
   #' considers the ancestors of each existing data group to generate only those
   #' combinations of values that exist in the data frame.\cr
   #' (2) dataName=name, fromData=TRUE, onlyCombinationsThatExist=FALSE - which
   #' ignores the ancestors of each existing data group and simply adds every
   #' distinct value of the specified variable under every existing data group,
   #' which can result in combinations of values in the pivot table that don't
   #' exist in the data frame (i.e. blank rows/columns in the pivot table).\cr
   #' (3) fromData=FALSE, explicitListOfValues=list(...) - simply adds every
   #' value from the specified list under every existing data group.
   #' @param variableName The name of the related column in the data frame(s) of
   #'   the pivot table.
   #' @param atLevel The number of levels below the current group to add the new
   #' groups.  0 = at the current level, 1 = one level below the current group, etc.
   #' `NULL` = create a new level at the bottom of the hierarchy for the new
   #' groups.
   #' @param fromData Default `TRUE` to generate the new data groups based on the
   #' data values that exist in the `variableName` column in the named data frame.
   #' If `FALSE`, then `explicitListOfValues` must be specified.
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) to read the data group values from.
   #' @param dataSortOrder Must be one of "asc", "desc", "custom" or "none".
   #' @param customSortOrder A vector values sorted into the desired order.
   #' @param caption The template of data group captions to generate,
   #' default "{value}".
   #' @param dataFormat A character, list or custom function to format the
   #' data value.
   #' @param dataFmtFuncArgs A list that specifies any additional arguments to
   #' pass to a custom format function.
   #' @param onlyCombinationsThatExist Default `TRUE` to generate only
   #' combinations of data groups that exist in the data frame.
   #' @param explicitListOfValues A list of explicit values to create data
   #' groups from.  A data group is created for each element of the list.
   #' If a list element is vector of values (with length greater than 1),
   #' then a data group is created for multiple values instead of just
   #' a single value.
   #' @param calculationGroupName The calculation group that the new data groups
   #' are related to.
   #' @param expandExistingTotals Default `FALSE`, which means totals are not
   #' broken down in multi-level hierarchies.
   #' @param addTotal Default `TRUE`, which means sub-total and total data groups
   #' are automatically added.
   #' @param visualTotals Default `FALSE`, which means visual totals are disabled.
   #' See the "Data Groups" vignette for more details about visual totals.
   #' @param totalPosition Either "before" or "after" to specify where total groups
   #' are created, default "after".
   #' @param totalCaption The caption to display on total groups, default "Total".
   #' @param onlyAddGroupIf A filter expression that can be used to more finely
   #' control whether data groups are created at different locations in the
   #' hierarchy.  There must be at least one row that matches this filter and the
   #' filters from the ancestor groups in order that the child group is created.
   #' E.g. `MaxDisplayLevel>5`.
   #' @param preGroupData Default `TRUE`, which means that the pivot table
   #' pre-calculates the distinct combinations of variable values to reduce the CPU
   #' time and elapsed time required to generate data groups.
   #' Cannot be used in conjunction with the
   #' @param baseStyleName The name of the style applied to this data group (i.e.
   #'   this row/column heading).  The style must exist in the `PivotStyles` object
   #'   associated with the PivotTable.
   #' @param styleDeclarations CSS style declarations that can override the base
   #' style, expressed as a list, e.g. `list("font-weight"=bold")`.
   #' @param outlineBefore Default `FALSE` to disable the creation of outline header
   #' groups.  Specify either `TRUE` or a list of outline group settings to create
   #' outline header groups.  See the "Regular Layout" vignette for details.
   #' @param outlineAfter Default `FALSE` to disable the creation of outline footer
   #' groups.  Specify either `TRUE` or a list of outline group settings to create
   #' outline footer groups.  See the "Regular Layout" vignette for details.
   #' @param outlineTotal  Default `FALSE` to disable the creation of outline
   #' totals.  Specify either `TRUE` or a list of outline group settings to create
   #' outline totals.  See the "Regular Layout" vignette for details.
   #' @param onlyAddOutlineChildGroupIf A filter expression that can be used to
   #' more finely control whether outline child groups are created at different
   #' locations in the hierarchy.  There must be at least one row that matches
   #' this filter and the filters from the ancestor groups in order that the
   #' outline child group is created.  E.g. `MaxDisplayLevel>5`.
   #' See the "Regular Layout" vignette for an example.
   #' @return A list of new `PivotDataGroup` objects that have been added.
   addDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=0 is the current level, 1 = one level below, etc
                            dataName=NULL, dataSortOrder="asc", customSortOrder=NULL, caption="{value}", dataFormat=NULL, dataFmtFuncArgs=NULL,
                            onlyCombinationsThatExist=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                            expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                            onlyAddGroupIf=NULL, preGroupData=TRUE, baseStyleName=NULL, styleDeclarations=NULL,
                            outlineBefore=NULL, outlineAfter=NULL, outlineTotal=FALSE, onlyAddOutlineChildGroupIf=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "custom", "none"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", customSortOrder, missing(customSortOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", caption, missing(caption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "addDataGroups", onlyAddOutlineChildGroupIf, missing(onlyAddOutlineChildGroupIf), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical","character"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$addDataGroups", "Adding data groups...",
                                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                                        dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                                        caption=caption, dataFormat=dataFormat, onlyCombinationsThatExist=onlyCombinationsThatExist,
                                        explicitListOfValues=explicitListOfValues, calculationGroupName=calculationGroupName,
                                        expandExistingTotals=expandExistingTotals, addTotal=addTotal, visualTotals=visualTotals,
                                        totalPosition=totalPosition, totalCaption=totalCaption,
                                        onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData,
                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                        outlineBefore=outlineBefore, outlineAfter=outlineAfter, outlineTotal=outlineTotal,
                                        onlyAddOutlineChildGroupIf=onlyAddOutlineChildGroupIf))
     private$p_parentPivot$resetCells()
     # check variable name
     if(missing(variableName)||is.null(variableName)) stop("PivotDataGroup$addDataGroups(): variableName must be specified.", call. = FALSE)
     safeVariableName <- processIdentifier(variableName)
     # check sort order
     if((dataSortOrder=="custom")&&(is.null(customSortOrder))) stop("PivotDataGroup$addDataGroups(): if dataSortOrder=custom, then customSortOrder must be specified.", call. = FALSE)
     # check outline settings
     outlineBefore <- cleanOutlineArg(private$p_parentPivot, outlineBefore)
     outlineAfter <- cleanOutlineArg(private$p_parentPivot, outlineAfter, defaultCaption="")
     outlineTotal <- cleanOutlineArg(private$p_parentPivot, outlineTotal, defaultCaption="Total", defaultIsEmpty=FALSE)
     # visual totals
     if(addTotal==TRUE){ private$p_visualTotals <- visualTotals }
     # if onlyAddGroupIf is specified, then preGroupData cannot be used
     if(!is.null(onlyAddGroupIf)) {
       preGroupData <- FALSE
       if(fromData==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddGroupIf cannot be used when fromData=FALSE.", call. = FALSE)
       if(onlyCombinationsThatExist==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddGroupIf cannot be used when onlyCombinationsThatExist=FALSE.", call. = FALSE)
     }
     # if onlyAddOutlineChildGroupIf is specified and it is a character, then preGroupData cannot be used
     if((!is.null(onlyAddOutlineChildGroupIf))&&(class(onlyAddOutlineChildGroupIf)=="character")) {
       preGroupData <- FALSE
       if(fromData==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddOutlineChildGroupIf cannot be used when fromData=FALSE.", call. = FALSE)
       if(onlyCombinationsThatExist==FALSE) stop("PivotDataGroup$addDataGroups():  onlyAddOutlineChildGroupIf cannot be used when onlyCombinationsThatExist=FALSE.", call. = FALSE)
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
          newGrp <- grp$addChildGroup(doNotExpand=grp$doNotExpand, isEmpty=grp$isEmpty,
                                      isOutline=grp$isOutline, styleAsOutline=grp$styleAsOutline,
                                      mergeEmptySpace=grp$mergeEmptySpace,
                                      baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                      cellBaseStyleName=grp$cellBaseStyleName, cellStyleDeclarations=grp$cellStyle$declarations,
                                      resetCells=FALSE)
          index <- length(newGroups) + 1
          newGroups[[index]] <- newGrp
          next
       }
       # total?
       if((grp$isTotal==TRUE)&&(grp$isOutline||(expandExistingTotals==FALSE))) {
         if(length(grp$childGroups)==0) { # if there is a total cell present already, don't add another (this can happen if addDataGroups(atLevel=N) is called multiple times with the same level N)
           # add a single group that is an unexpanded total
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       calculationGroupName=calculationGroupName,
                                       isOutline=grp$isOutline, styleAsOutline=grp$styleAsOutline,
                                       isTotal=TRUE, isLevelSubTotal=grp$isLevelSubTotal, isLevelTotal=grp$isLevelTotal,
                                       baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                       sortAnchor="fixed", resetCells=FALSE)
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
         # calculate the distinct values that exist for this combination of parent groups
         if(!rowColFilters$isNONE) {
           distinctValues <- private$getDistinctValuesForAddDataGrps(data=df, variableName=variableName, safeVariableName=safeVariableName,
                                                                     rowColFilters=rowColFilters, additionalFilter=onlyAddGroupIf,
                                                                     dataSortOrder=dataSortOrder, customSortOrder=customSortOrder)
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
                                       captionTemplate=caption, caption=totalCaption, calculationGroupName=calculationGroupName,
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
           # if this is an outline group, do we add the child group?
           addChildGroup <- TRUE
           if(outlineBefore$outline||outlineAfter$outline) {
             if(class(onlyAddOutlineChildGroupIf)=="logical") {
               addChildGroup <- onlyAddOutlineChildGroupIf
             }
             else if(!is.null(onlyAddOutlineChildGroupIf)) {
               # need to examine the data to see if we add the child group
               childFilters <- rowColFilters$getCopy()
               childFilters$setFilterValues(variableName=variableName, type="VALUES", values=values, action="intersect")
               outlineChildValues <- private$getDistinctValuesForAddDataGrps(data=df, variableName=variableName, safeVariableName=safeVariableName,
                                                                             rowColFilters=childFilters, additionalFilter=onlyAddOutlineChildGroupIf,
                                                                             dataSortOrder="none")
               addChildGroup <- ((!is.null(outlineChildValues))&&(length(outlineChildValues)>0))
             }
           }
           # get caption
           valueForCaption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) valueForCaption <- distinctCaptions[j]
           if(is.null(valueForCaption)&&(!is.null(dataFormat))) valueForCaption <- private$formatValue(values, dataFormat, dataFmtFuncArgs)
           grpCaption <- valueForCaption
           if(outlineBefore$outline) grpCaption <- ""
           # outline before value
           newOutlineBeforeGrp <- NULL
           if(outlineBefore$outline){
             newOutlineBeforeGrp <- private$createOutlineGroup(parentGroup=grp,
                                                   outline=outlineBefore, outlineSkippedLinkedGroup=!addChildGroup,
                                                   variableName=variableName, values=values, caption=valueForCaption,
                                                   calculationGroupName=calculationGroupName,
                                                   baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                   sortAnchor=ifelse(addChildGroup, "next", "none"), resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newOutlineBeforeGrp
           }
           # value
           newGrp <- NULL
           if(addChildGroup) {
             newGrp <- grp$addChildGroup(variableName=variableName, values=values,
                                         captionTemplate=caption, caption=grpCaption,
                                         calculationGroupName=calculationGroupName, isTotal=grp$isTotal,
                                         baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, resetCells=FALSE)
             index <- length(newGroups) + 1
             newGroups[[index]] <- newGrp
             if(!is.null(newOutlineBeforeGrp)) newOutlineBeforeGrp$outlineLinkedGroupId <- newGrp$instanceId
           }
           # outline after value
           if(outlineAfter$outline){
              newOutlineAfterGrp <- private$createOutlineGroup(parentGroup=grp,
                                                   outline=outlineAfter, outlineSkippedLinkedGroup=!addChildGroup,
                                                   variableName=variableName, values=values, caption=valueForCaption,
                                                   calculationGroupName=calculationGroupName,
                                                   baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                   sortAnchor=ifelse(addChildGroup, "previous", "none"), resetCells=FALSE)
              index <- length(newGroups) + 1
              newGroups[[index]] <- newOutlineAfterGrp
              if(!is.null(newGrp)) newOutlineAfterGrp$outlineLinkedGroupId <- newGrp$instanceId
           }
         }
         # total after?
         if((addTotal==TRUE)&&(totalPosition=="after")&&(!outlineTotal$outline)) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       captionTemplate=caption, caption=totalCaption, calculationGroupName=calculationGroupName,
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

   #' @description
   #' Sort data groups either by the data group data value, caption, a custom order
   #' or based on calculation result values.
   #' @param levelNumber The number of levels below the current group to sort the
   #' data groups.  0 = at the current level, 1 = one level below the current
   #' group, etc.
   #' @param orderBy Must be either "value", "caption", "calculation",
   #' "customByValue" or "customByCaption".\cr
   #' "value" sorts by the raw (i.e. unformatted) group value.\cr
   #' "caption" sorts by the formatted character group caption.\cr
   #' "calculation" sorts using one of the calculations defined in the pivot table.
   #' "customValue" sorts by the raw (i.e. unformatted) group value according to
   #' the specified custom sort order.\cr
   #' "customCaption" sorts by the formatted character group caption according to
   #' the specified custom sort order.
   #' @param customOrder A vector values sorted into the desired order.
   #' @param sortOrder Must be either "asc" or "desc".
   #' @param calculationGroupName If sorting using a calculation, the name of the
   #' calculation group containing the specified calculation.
   #' @param calculationName If sorting using a calculation, the name of the
   #' calculation.
   #' @param fromIndex A boundary to limit the sort operation.
   #' @param toIndex A boundary to limit the sort operation.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return No return value.
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
     # first, check for each outline group that the group it is linked to still exists and if not, clear the sortAnchor
     for(i in 1:length(private$p_groups)) {
       grp <- private$p_groups[[i]]
       if(!is.null(grp$sortAnchor)) {
         if((grp$sortAnchor=="next")||(grp$sortAnchor=="previous"))
         {
           if(grp$outlineLinkedGroupExists==FALSE) grp$sortAnchor <- NULL
         }
       }
     }
     # next, clear the sort groups, get the fixed groups and populate the linked groups
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

   #' @description
   #' Add multiple new groups to the data group hierarchy to represent calculations.
   #' @param calculationGroupName The name of the calculation group to add
   #' into the data group hierarchy.
   #' @param atLevel The number of levels below the current group to add the new
   #' groups.  0 = at the current level, 1 = one level below the current group, etc.
   #' `NULL` = create a new level at the bottom of the hierarchy for the new
   #' groups.
   #' @param outlineBefore Default `FALSE` to disable the creation of outline header
   #' groups.  Specify either `TRUE` or a list of outline group settings to create
   #' outline header groups.  See the "Regular Layout" vignette for details.
   #' @param outlineAfter Default `FALSE` to disable the creation of outline footer
   #' groups.  Specify either `TRUE` or a list of outline group settings to create
   #' outline footer groups.  See the "Regular Layout" vignette for details.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return A list of new `PivotDataGroup` objects that have been added.
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
     outlineBefore <- cleanOutlineArg(private$p_parentPivot, outline=outlineBefore)
     outlineAfter <- cleanOutlineArg(private$p_parentPivot, outline=outlineAfter, defaultCaption="")
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
                                            calculationGroupName=calculationGroupName, calculationName=calc$calculationName,
                                            isTotal=grp$isTotal, isLevelSubTotal=grp$isLevelSubTotal, isLevelTotal=grp$isLevelTotal,
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
                                            calculationGroupName=calculationGroupName, calculationName=calc$calculationName,
                                            isTotal=grp$isTotal, isLevelSubTotal=grp$isLevelSubTotal, isLevelTotal=grp$isLevelTotal,
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

   #' @description
   #' Normalise the data group hierarchy so that all branches have the same number
   #' of levels - accomplished by adding empty child data groups where needed.
   #' @param resetCells Default `TRUE` to reset any cells that currently exist
   #' in the pivot table and trigger a recalculation of the pivot table when
   #' it is next rendered.
   #' @return A list of new `PivotDataGroup` objects that have been added.
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
           dg <- dg$addChildGroup(doNotExpand=dg$doNotExpand, isEmpty=dg$isEmpty,
                                  isOutline=dg$isOutline, styleAsOutline=dg$styleAsOutline,
                                  isTotal=dg$isTotal, isLevelSubTotal=dg$isLevelSubTotal, isLevelTotal=dg$isLevelTotal,
                                  mergeEmptySpace=dg$mergeEmptySpace)
           groupsAdded <- groupsAdded + 1
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$normaliseDataGroup", "Normalised data group.")
     return(invisible(groupsAdded))
   },

   #' @description
   #' Get a `PivotFilters` object that contains the filters applied in this
   #' data group and all of its ancestors in the data group hierarchy.
   #' @return A `PivotFilters` object.
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

   #' @description
   #' Get the calculation name set in this data group or its nearest ancestor.
   #' @return The name of a calculation.
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

   #' @description
   #' Test whether this data group matches specified criteria.
   #' @param matchMode Either "simple" (default) or "combinations".\cr
   #' "simple" is used when matching only one variable-value, multiple
   #' variable-value combinations are effectively logical "OR".\cr
   #' "combinations" is used when matching for combinations of variable
   #' values, multiple variable-value combinations are effectively
   #' logical "AND".  A child group is viewed as having the variable-value
   #' filters of itself and it's parent/ancestors, e.g.\cr
   #' `list("TrainCategory"="Express Passenger", "PowerType"="DMU")`,
   #' would return the "DMU" data group underneath "Express Passenger".
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
   #' @param atLevels An integer vector constraining the levels in the hierarchy to
   #' search.
   #' @param minChildCount Match only data groups with this minimum number of children.
   #' @param maxChildCount Match only data groups with this maximum number of children.
   #' @param emptyGroups A word that specifies how empty groups are matched -
   #' must be one of "include", "exclude" (default) or "only".
   #' @param outlineGroups A word that specifies how outline cells are matched -
   #' must be one of "include", "exclude" (default) or "only".
   #' @param outlineLinkedGroupExists `TRUE` to match only groups where the related
   #' outline child group still exists.  `FALSE` to match only groups where the related
   #' outline child group no longer exists.
   #' @return `TRUE` if this group matches the specified criteria, `FALSE` otherwise.
   isFindMatch = function(matchMode="simple", variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL,
                          atLevels=NULL, minChildCount=NULL, maxChildCount=NULL, emptyGroups="exclude",
                          outlineGroups="exclude", outlineLinkedGroupExists=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", atLevels, missing(atLevels), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", minChildCount, missing(minChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", maxChildCount, missing(maxChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", outlineGroups, missing(outlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "isFindMatch", outlineLinkedGroupExists, missing(outlineLinkedGroupExists), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$isFindMatch", "Checking if matches criteria...")
     # a) check empty space
     if(!isTRUE(self$isOutline)) {
       if((emptyGroups=="exclude")&&(self$isEmpty==TRUE)) return(invisible(FALSE))
       if((emptyGroups=="only")&&(self$isEmpty==FALSE)) return(invisible(FALSE))
     }
     # b) check outline
     # if the parent group is also an outline group, then this group is one of the hidden data groups under an outline group, so is irrelevant
     if((isTRUE(self$isOutline))&&(!is.null(private$p_parentGroup))&&(isTRUE(private$p_parentGroup$isOutline))) return(invisible(FALSE))
     if((outlineGroups=="exclude")&&(isTRUE(self$isOutline))) return(invisible(FALSE))
     if((outlineGroups=="only")&&(!isTRUE(self$isOutline))) return(invisible(FALSE))
     if((!is.null(outlineLinkedGroupExists))&&(isTRUE(self$isOutline))) {
       if(outlineLinkedGroupExists != self$outlineLinkedGroupExists) return(invisible(FALSE))
     }
     # c) check level
     if(!is.null(atLevels)) {
       levelNumber <- self$levelNumber
       if(!(levelNumber %in% atLevels)) return(invisible(FALSE))
     }
     # d) check child counts
     if(!is.null(minChildCount)) {
       if(self$childGroupCount < minChildCount) return(invisible(FALSE))
     }
     if(!is.null(maxChildCount)) {
       if(self$childGroupCount > maxChildCount) return(invisible(FALSE))
     }
     # e) check the filter match
     if((!is.null(variableNames))||(!is.null(variableValues))) {
       if(matchMode=="simple") filters <- private$p_filters
       else filters <- self$getNetFilters()
       if(is.null(filters)) return(invisible(FALSE))
       isMatch <- filters$isFilterMatch(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues)
       if(isMatch==FALSE) return(invisible(FALSE))
     }
     # f) check totals criteria
     if((totals=="exclude")&&(self$isTotal==TRUE)) return(invisible(FALSE))
     if((totals=="only")&&(self$isTotal==FALSE)) return(invisible(FALSE))
     # g) check calculation criteria
     if(!is.null(calculationNames)) {
       if(matchMode=="simple") calcName <- private$p_calculationName
       else calcName <- self$getNetCalculationName()
       if(is.null(calcName)) return(invisible(FALSE))
       if(!(calcName %in% calculationNames)) return(invisible(FALSE))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$isFindMatch", "Checked if matches criteria.")
     return(invisible(TRUE))
   },

   #' @description
   #' Find data groups that match specified criteria.
   #' @param matchMode Either "simple" (default) or "combinations".\cr
   #' "simple" is used when matching only one variable-value, multiple
   #' variable-value combinations are effectively logical "OR".\cr
   #' "combinations" is used when matching for combinations of variable
   #' values, multiple variable-value combinations are effectively
   #' logical "AND".  A child group is viewed as having the variable-value
   #' filters of itself and it's parent/ancestors, e.g.\cr
   #' `list("TrainCategory"="Express Passenger", "PowerType"="DMU")`,
   #' would return the "DMU" data group underneath "Express Passenger".
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
   #' @param atLevels An integer vector constraining the levels in the hierarchy to
   #' search.
   #' @param minChildCount Match only data groups with this minimum number of children.
   #' @param maxChildCount Match only data groups with this maximum number of children.
   #' @param emptyGroups A word that specifies how empty groups are matched -
   #' must be one of "include", "exclude" (default) or "only".
   #' @param outlineGroups A word that specifies how outline cells are matched -
   #' must be one of "include", "exclude" (default) or "only".
   #' @param outlineLinkedGroupExists `TRUE` to match only groups where the related
   #' outline child group still exists.  `FALSE` to match only groups where the related
   #' outline child group no longer exists.
   #' @param includeDescendantGroups Default `FALSE`.  Specify true to also return
   #' all descendants of data groups that match the specified criteria.
   #' @param includeCurrentGroup Default `TRUE`.  Specify `FALSE` to prevent the
   #' current group being included in the returned results.
   #' @return A list of data groups matching the specified criteria.
   findDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                             totals="include", calculationNames=NULL,
                             atLevels=NULL, minChildCount=NULL, maxChildCount=NULL, emptyGroups="exclude",
                             outlineGroups="exclude", outlineLinkedGroupExists=NULL,
                             includeDescendantGroups=FALSE, includeCurrentGroup=TRUE) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", atLevels, missing(atLevels), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", minChildCount, missing(minChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", maxChildCount, missing(maxChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric","integer"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", outlineGroups, missing(outlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", outlineLinkedGroupExists, missing(outlineLinkedGroupExists), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotDataGroup", "findDataGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$findDataGroups", "Finding data groups...")
     # clear the isMatch flag across all descendants
     clearFlags <- function(dg) {
       dg$isMatch <- FALSE
     }
     grps <- self$getDescendantGroups(includeCurrentGroup=includeCurrentGroup)
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
                            totals=totals, calculationNames=calculationNames,
                            atLevels=atLevels, minChildCount=minChildCount, maxChildCount=maxChildCount,
                            emptyGroups=emptyGroups,
                            outlineGroups=outlineGroups, outlineLinkedGroupExists=outlineLinkedGroupExists)) {
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
                              totals=totals, calculationNames=calculationNames,
                              atLevels=atLevels, minChildCount=minChildCount, maxChildCount=maxChildCount,
                              emptyGroups=emptyGroups,
                              outlineGroups=outlineGroups, outlineLinkedGroupExists=outlineLinkedGroupExists)) {
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

   #' @description
   #' An internal method used to set style declarations on the data group.
   #' Using `pt$setStyling(cells=x)` is preferred for users.
   #' @param styleDeclarations A list containing CSS style declarations.
   #' @return No return value.
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

   #' @description
   #' An internal method that clears state data used during sorting operations.
   #' @return No return value.
   clearSortGroups = function() {
      private$p_sortGroupsBefore <- list()
      private$p_sortGroupsAfter <- list()
   },

   #' @description
   #' An internal method used during sorting operations.
   #' @param grp The group to insert as part of sorting.
   #' @return No return value.
   addSortGroupBefore = function(grp) {
      private$p_sortGroupsBefore[[length(private$p_sortGroupsBefore)+1]]  <- grp
   },

   #' @description
   #' An internal method used during sorting operations.
   #' @param grp The group to insert as part of sorting.
   #' @return No return value.
   addSortGroupAfter = function(grp) {
      private$p_sortGroupsAfter[[length(private$p_sortGroupsAfter)+1]]  <- grp
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
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

   #' @field instanceId An integer value that uniquely identifies this group.
   #' NB:  This number is guaranteed to be unique within the pivot table,
   #' but the method of generation of the values may change in future, so
   #' you are advised not to base any logic on specific values.
   instanceId = function(value) { return(invisible(private$p_instanceId)) },

   #' @field rowOrColumn Either "row" or "column"
   rowOrColumn = function(value) { return(invisible(private$p_rowOrColumn)) },

   #' @field parentGroup The parent `PivotDataGroup` instance that this
   #' `PivotDataGroup` instance belongs to.
   parentGroup = function(value) { return(invisible(private$p_parentGroup)) },

   #' @field childGroups A list of `PivotDataGroup` objects that are the
   #' children of this data group.
   childGroups = function(value) { return(invisible(private$p_groups)) },

   #' @field childGroupCount A count of `PivotDataGroup` objects that are the
   #' children of this data group.
   childGroupCount = function(value) { return(invisible(length(private$p_groups))) },

   #' @field leafGroups A list of `PivotDataGroup` objects that are leaf-level
   #' descendants of this data group.
   leafGroups = function(value) { return(invisible(self$getLeafGroups())) },

   #' @field levelNumber An integer value specifying the level number where this
   #' data group exists in the hierarchy.
   levelNumber = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$levelNumber", "Getting level number...")
      ln <- 0
      grp <- self
      while(!is.null(grp$parentGroup)) {
         ln <- ln + 1
         grp <- grp$parentGroup
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$levelNumber", "Got level number.")
      return(invisible(ln))
   },

   #' @field filters A `PivotFilters` object containing the filters associated
   #' with this data group.
   filters = function(value) { return(invisible(private$p_filters)) },

   #' @field variableName A character value that specifies the name of the
   #' variable in the data frame that this group relates to and will filter.
   variableName = function(value) {
     if(is.null(private$p_filters)) return(invisible(NULL))
     if(private$p_filters$count>1) stop("PivotDataGroup$variableName: The data group has filters for more than one variable.  Please retrieve the filters via the filters property.", call. = FALSE)
     return(invisible(private$p_filters$filters[[1]]$variableName))
   },

   #' @field values A vector that specifies the filter values applied to
   #' `variableName` to select the data to match this row/column in the pivot
   #' table.
   values = function(value) {
     if(is.null(private$p_filters)) return(invisible(NULL))
     if(private$p_filters$count>1) stop("PivotDataGroup$variableName: The data group has filters for more than one variable.  Please retrieve the filters via the filters property.", call. = FALSE)
     return(invisible(private$p_filters$filters[[1]]$values))
   },

   #' @field calculationGroupName For calculation groups, this character value
   #' specifies the calculation group that `calculationName` belongs to.
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

   #' @field calculationName For calculation groups, this character value
   #' specifies the name of the calculation.
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

   #' @field doNotExpand `TRUE` if this data group prevent the high-level
   #' methods such as `addDataGroups()` from adding child groups.
   doNotExpand = function(value) { return(invisible(private$p_doNotExpand)) },

   #' @field isEmpty `TRUE` if this group contains no data (e.g. if
   #' it is part of a header or outline row)
   isEmpty = function(value) { return(invisible(private$p_isEmpty)) },

   #' @field isOutline `TRUE` if this data group is an outline group.
   isOutline = function(value) { return(invisible(private$p_isOutline)) },

   #' @field styleAsOutline `TRUE` if this data group is to be styled as an outline
   #' group.  Only applicable when `isOutline` is `TRUE`.
   styleAsOutline = function(value) {
      if(missing(value)) { return(invisible(private$p_styleAsOutline)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "styleAsOutline", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
         }
         private$p_styleAsOutline <- value
         return(invisible())
      }
   },

   #' @field outlineLinkedGroupId The instance id of the child group related
   #' to this group, if this group is an outline group.
   outlineLinkedGroupId = function(value) {
     if(missing(value)) return(invisible(private$p_outlineLinkedGroupId))
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "outlineLinkedGroupId", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       }
       private$p_outlineLinkedGroupId <- value
     }
   },

   #' @field outlineLinkedGroupExists `TRUE` if the group specified by
   #' `outlineLinkedGroupId` still exists.
   outlineLinkedGroupExists = function() {
     if(is.null(private$p_outlineLinkedGroupId)) return(invisible(FALSE))
     index <- private$p_parentGroup$findChildIndex(private$p_outlineLinkedGroupId)
     if(is.null(index)) return(invisible(FALSE))
     else return(invisible(TRUE))
   },

   #' @field captionTemplate A character value that specifies the template
   #' for the data group caption, default "{values}".
   captionTemplate = function(value) {
     if(missing(value)) {
       return(invisible(private$p_captionTemplate))
     }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "captionTemplate", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_captionTemplate <- value
     }
   },

   #' @field caption The data group caption.
   #' Assigning a caption effectively overrides the
   #' built-in logic for generating a caption.
   caption = function(value) {
     if(missing(value)) {
       # deal with special cases/get value
       captionValue <- NULL
       if(is.null(private$p_caption)) {
         if((private$p_isTotal)&&(!is.null(private$p_parentGroup))&&
            (length(private$p_parentGroup$childGroups)<2)) return(invisible(""))
         if(is.null(private$p_filters)) return(invisible(""))
         captionValue <- private$p_filters$asString(includeVariableName=FALSE)
       }
       else captionValue <- private$p_caption
       # apply template
       if((!is.null(captionValue))&&(length(captionValue)>0)) {
         if(length(captionValue)>1) captionValue <- paste(captionValue, collapse=" ")
         if(nchar(captionValue)>0) captionValue <- gsub("\\{value\\}", captionValue, private$p_captionTemplate)
       }
       return(invisible(captionValue))
     }
     else {
       if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "caption", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_caption <- value
     }
   },

   #' @field sortValue The raw (i.e. unformatted, typically numerical) value that
   #' represents this data group in sort operations.
   sortValue = function(value) { return(invisible(private$p_sortValue)) },

   #' @field isTotal `TRUE` if this data group is a total.
   isTotal = function(value) { return(invisible(private$p_isTotal)) },

   #' @field isLevelSubTotal `TRUE` if this data group is a sub-total.
   isLevelSubTotal = function(value) { return(invisible(private$p_isLevelSubTotal)) },

   #' @field isLevelTotal `TRUE` if this data group is a level-total.
   isLevelTotal = function(value) { return(invisible(private$p_isLevelTotal)) },

   #' @field rowColumnNumber The row or column number that this data group relates to.
   #' This property only has a value for leaf-level data groups.
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

   #' @field baseStyleName The style name for the data group.
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

   #' @field style A `PivotStyle` object that contains additional CSS style
   #' declarations that override the base style.
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

   #' @field mergeEmptySpace A logical value that specifies whether empty space
   #' should be merged.
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

   #' @field cellBaseStyleName The style name for cells related to this data
   #' group.
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

   #' @field netCellBaseStyleName The style name for cells related to this data
   #' group - either from this group or the first ancestor that specifies a
   #' cellBaseStyleName if cellBaseStyleName is not specified on this group.
   netCellBaseStyleName = function(value) {
      cellBaseStyleName <- NULL
      if(is.null(private$p_cellBaseStyleName)) {
         if(!is.null(private$p_parentGroup)) {
            cellBaseStyleName <- private$p_parentGroup$cellBaseStyleName
         }
      }
      else cellBaseStyleName <- private$p_cellBaseStyleName
      return(invisible(cellBaseStyleName))
   },

   #' @field cellStyle A `PivotStyle` object that contains additional CSS style
   #' declarations that override the base style for cells related to this data
   #' group.  If setting this property, a list can also be specified.
   cellStyle = function(value) {
      if(missing(value)) { return(invisible(private$p_cellStyle)) }
      else {
         if(private$p_parentPivot$argumentCheckMode > 0) {
            checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "cellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotStyle", "list"), allowedListElementClasses=c("character", "integer", "numeric"))
         }
         if("PivotStyle" %in% class(value)) { private$p_cellStyle <- value }
         else if("list" %in% class(value)) {
            if(is.null(private$p_cellStyle)) private$p_cellStyle <- PivotStyle$new(private$p_parentPivot, "", declarations=value)
            else private$p_cellStyle$setPropertyValues(declarations=value)
         }
         return(invisible())
      }
   },

   #' @field netCellStyle A `PivotStyle` object that contains additional CSS style
   #' declarations that override the base style for cells related to this data
   #' group - both from this group and all ancestors.
   netCellStyle = function(value) {
      style <- NULL
      if((!is.null(private$p_parentGroup))&&(!is.null(private$p_parentGroup$cellStyle))) {
         style <- private$p_parentGroup$cellStyle$getCopy("")
      }
      if(!is.null(private$p_cellStyle)) {
         if(is.null(style)) {
            style <- private$p_cellStyle$getCopy("")
         }
         else {
            style$setPropertyValues(private$p_cellStyle$declarations)
         }
      }
      return(invisible(style))
   },

   #' @field fixedWidthSize The width (in characters) needed for
   #' this data group when rendering to plain text.
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

   #' @field isMatch An internal property used when finding data groups.
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

   #' @field isRendered An internal property used when rendering data groups.
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

   #' @field isWithinVisibleRange  An internal property used when rendering
   #' data groups.
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

   #' @field visibleChildGroupCount  An internal property used when rendering
   #'  data groups.
   visibleChildGroupCount = function(value) {
     if(is.null(private$p_groups)) return(invisible(0))
     if(length(private$p_groups)==0) return(invisible(0))
     visibleCount <- 0
     for(i in 1:length(private$p_groups)) {
       if(private$p_groups[[i]]$isWithinVisibleRange==TRUE) visibleCount <- visibleCount + 1
     }
     return(invisible(visibleCount))
   },

   #' @field visibleDescendantGroupCount  An internal property used when
   #' rendering data groups.
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

   #' @field visibleLeafGroupCount  An internal property used when rendering
   #'  data groups.
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

   #' @field sortAnchor Used to specify sort behaviour for outline groups, must
   #' be one of "fixed", "next" or "previous".
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

   #' @field sortGroupsBefore An internal property used when sorting
   #'  data groups.
   sortGroupsBefore = function(value) { return(invisible(private$p_sortGroupsBefore)) },

   #' @field sortGroupsAfter  An internal property used when sorting
   #'  data groups.
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
   p_styleAsOutline = NULL,          # used to indicate the styling for an outline cell (TRUE/FALSE)
   p_outlineLinkedGroupId = NULL,    # the instance ID of the group this outline group was linked to when it was created (i.e. the group with no caption that is under the outlineBefore and above the outlineAfter groups)
   p_captionTemplate = NULL,         # e.g. "{value}"
   p_caption = NULL,                 # most of the time null, unless a format is specified or a hard coded value is specified
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
   p_sortGroupsBefore = NULL,  # internal property used transiently during sorting:  a list of groups that are fixed to this and come before it
   p_sortGroupsAfter = NULL,   # internal property used transiently during sorting:  a list of groups that are fixed to this and come after it

   # internal fields to help with complex operations
   p_fixedWidthSize = 0,
   p_isMatch = FALSE, # helper flag used when searching through data groups
   p_isWithinVisibleRange = TRUE, # helper flag to allow a subsection of the pivot table to be output
   p_isRendered = FALSE, # helper flag to keep track of which data groups have already been rendered

   # private functions:
   createOutlineGroup = function(parentGroup=NULL, outline=NULL, variableName=NULL, values=NULL, caption=NULL,
                                 calculationGroupName=NULL, calculationName=NULL,
                                 baseStyleName=NULL, styleDeclarations=NULL, isLevelTotal=FALSE,
                                 sortAnchor=NULL, outlineLinkedGroupId=NULL, outlineSkippedLinkedGroup=FALSE, resetCells=TRUE) {
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
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous", "none"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", outlineLinkedGroupId, missing(outlineLinkedGroupId), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", outlineSkippedLinkedGroup, missing(outlineSkippedLinkedGroup), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
         checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "createOutlineGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$createOutlineGroup", "Creating outline group...")
      # preparation
      if(is.null(caption)) caption <- as.character(values)
      if((!is.null(caption))&&(length(caption)>0)) {
        if(length(caption)>1) caption <- paste(caption, collapse=" ")
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
      # no outline child group handling (only applies to outline before/after, not outline total)
      styleAsOutline <- TRUE
      if(outlineSkippedLinkedGroup) {
         styleAsOutline <- is.null(outline$nocgApplyOutlineStyling) || isTRUE(outline$nocgApplyOutlineStyling)
         if(!is.null(outline$nocgGroupStyleName)) groupStyleName <- outline$nocgGroupStyleName
         if(!is.null(outline$nocgGroupStyleDeclarations)) groupStyleDeclarations <- outline$nocgGroupStyleDeclarations
         if(!is.null(outline$nocgCellStyleName)) cellStyleName <- outline$nocgCellStyleName
         if(!is.null(outline$nocgCellStyleDeclarations)) cellStyleDeclarations <- outline$nocgCellStyleDeclarations
      }
      # cleanup sort anchor (allow calling functions to specify none to simplify their logic, but remove "none" here)
      if(sortAnchor=="none") sortAnchor <- NULL
      # generate group
      newGrp <- parentGroup$addChildGroup(variableName=variableName, values=values, # so that group has a reference to the variable & values
                                  caption=caption, calculationGroupName=calculationGroupName, calculationName=calculationName,
                                  doNotExpand=TRUE, isEmpty=isEmpty,
                                  isOutline=isOutline, styleAsOutline=styleAsOutline, mergeEmptySpace=mergeSpace,
                                  isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                  baseStyleName=groupStyleName, styleDeclarations=groupStyleDeclarations,
                                  cellBaseStyleName=cellStyleName, cellStyleDeclarations=cellStyleDeclarations,
                                  sortAnchor=sortAnchor, outlineLinkedGroupId=outlineLinkedGroupId, resetCells=resetCells)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$createOutlineGroup", "Created outline group.")
      return(newGrp)
   },
   getDistinctValuesForAddDataGrps = function(data=NULL, variableName=NULL, safeVariableName=NULL, rowColFilters=NULL, additionalFilter=NULL, dataSortOrder=NULL, customSortOrder=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", data, missing(data), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("data.frame", "data.table"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", safeVariableName, missing(safeVariableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", rowColFilters, missing(rowColFilters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", additionalFilter, missing(additionalFilter), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", dataSortOrder, missing(dataSortOrder), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none", "custom"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotDataGroup", "getDistinctValuesForAddDataGrps", customSortOrder, missing(customSortOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getDistinctValuesForAddDataGrps", "Getting distinct values for AddDataGroups...")
     if(private$p_parentPivot$processingLibrary=="dplyr") {
       # build a dplyr query
       if((!rowColFilters$isALL)&&(rowColFilters$count > 0)) {
         data <- rowColFilters$getFilteredDataFrame(dataFrame=data)
       }
       # if additionalFilter specified, then need to further filter the data frame by this
       if(!is.null(additionalFilter)) {
         eval(parse(text=paste0("data <- dplyr::filter(data, ", additionalFilter, ")")))
       }
       # get the distinct values for the current variable
       eval(parse(text=paste0("data <- dplyr::select(data, ", safeVariableName, ")")))
       data <- dplyr::distinct(data)
       if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", safeVariableName, ")")))
       else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", safeVariableName, "))")))
       distinctValues <- dplyr::collect(data)[[variableName]] # this cannot be replaced with safeVariableName (safeVariableName works in R constructed as a character variable that will be executed via exec(parse(...)), variableName needs to be used here because this executed via exec(parse(...)))
       if("factor" %in% class(distinctValues)) { distinctValues <- as.character(distinctValues) }
       if(dataSortOrder=="custom") distinctValues <- distinctValues[order(match(distinctValues, customSortOrder))]
     }
     else if(private$p_parentPivot$processingLibrary=="data.table") {
       # build a data.table query
       # check is a data table
       if(private$p_parentPivot$argumentCheckMode == 4) {
         if(!data.table::is.data.table(data))
           stop(paste0("PivotDataGroup$getDistinctValuesForAddDataGrps(): A data.table was expected but the following was encountered: ",
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
             stop("PivotCalculator$getDistinctValuesForAddDataGrps(): filter$variableName must not be null", call. = FALSE)
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
       # if additionalFilter specified, then add this to the filter criteria
       if(!is.null(additionalFilter)) {
         if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
         filterCmd <- paste0(filterCmd, "(", additionalFilter, ")")
       }
       # seem to need a dummy row count in order to get the distinct values
       rcName <- "rc"
       if(safeVariableName==rcName) rcName <- "rowCount"
       eval(parse(text=paste0("distinctValues <- data[", filterCmd, ", .(", rcName, "=.N), by=.(", safeVariableName, ")][order(", ifelse(dataSortOrder=="desc", "-", ""), safeVariableName, ")][, ", safeVariableName, "]")))
       if("factor" %in% class(distinctValues)) { distinctValues <- as.character(distinctValues) }
       if(dataSortOrder=="custom") distinctValues <- distinctValues[order(match(distinctValues, customSortOrder))]
     }
     else stop(paste0("PivotDataGroup$getDistinctValuesForAddDataGrps(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotDataGroup$getDistinctValuesForAddDataGrps", "Got distinct values for AddDataGroups.")
     return(invisible(distinctValues))
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
   }
  )
)
