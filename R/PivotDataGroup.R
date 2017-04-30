#' A class that defines a row or column heading.
#'
#' The PivotDataGroup class represents one row or column heading in a pivot
#' table.  PivotDataGroups have a parent-child relationship, i.e. each
#' PivotDataGroup can have one or more child PivotDataGroups.
#'
#' @docType class
#' @importFrom R6 R6Class
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
#' @field style A PivotStyle object that can apply overrides to the base style
#'   for this data group.
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
#'   \item{\code{getLevelGroups(level, levelGroups)}}{Get all of the data groups
#'   at a specific level in the data group hierarchy.}
#'   \item{\code{addChildGroup(variableName, values, caption, isTotal=FALSE,
#'   isLevelSubTotal=FALSE, isLevelTotal=FALSE, calculationGroupName,
#'   calculationName)}}{Add a new data group as the child of the current data
#'   group.}
#'   \item{\code{addDataGroups(variableName, atLevel, fromData=TRUE,  dataName,
#'   dataSortOrder="asc", dataFormat, onlyCombinationsThatExist=TRUE,
#'   explicitListOfValues, calculationGroupName, expandExistingTotals=FALSE,
#'   addTotal=TRUE, visualTotals=FALSE, totalPosition="after",
#'   totalCaption="Total")}}{Generate new data groups based on the distinct
#'   values in a data frame or using explicitly specified data values.}
#'   \item{\code{sortDataGroups(levelNumber=1, orderBy="calculation",
#'   sortOrder="desc", calculationGroupName="default", calculationName)}}{Sort
#'   data groups either by the data group data value, caption or based on
#'   calculation result values.}
#'   \item{\code{addCalculationGroups(calculationGroupName, atLevel)}}{Add a
#'   calculation group to the data group hierarchy.}
#'   \item{\code{getLevelCount(includeCurrentLevel=FALSE)}}{Count the number of
#'   levels in the data group hierarchy.}
#'   \item{\code{normaliseDataGroup()}}{Normalise the data group hierachy so
#'   that all branches have the same number of levels - accomplished by adding
#'   empty child data groups where needed.}
#'   \item{\code{getNetFilters()}}{Get a PivotFilters object that contains all
#'   of the filters applied in this data group and all of its ancestors.}
#'   \item{\code{getNetCalculationName()}}{Get the calculation name applied in
#'   this data group or its nearest ancestor.}
#'   \item{\code{isFindMatch(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL)}}{Tests
#'   whether this data group matches the specified criteria.}
#'   \item{\code{findDataGroups(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL,
#'   includeChildGroups=FALSE)}}{Searches all data groups underneath this data
#'   group to find groups that match the specified criteria.}
#'   \item{\code{asList()}}{Get a list representation of the data group(s).}
#'   \item{\code{asJSON()}}{Get a JSON representation of the data group(s).}
#' }

PivotDataGroup <- R6::R6Class("PivotDataGroup",
  public = list(
   initialize = function(parentGroup=NULL, parentPivot=NULL, rowOrColumn=NULL, caption=NULL, # common properties
                         isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE, # total properties
                         variableName=NULL, filterType="ALL", values=NULL, # filter properties
                         calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "initialize", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotDataGroup", "initialize", rowOrColumn, missing(rowOrColumn), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
     checkArgument("PivotDataGroup", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
     checkArgument("PivotDataGroup", "initialize", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "initialize", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "initialize", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "initialize", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
     checkArgument("PivotDataGroup", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotDataGroup$new", "Creating new data group...",
                                   list(rowOrColumn=rowOrColumn, caption=caption,
                                        variableName=variableName, filterType=filterType, values=values,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName))
     if(!(rowOrColumn %in% c("row", "column"))) stop("PivotDataGroup$new(): rowOrColumn must be either row or column", call. = FALSE)
     private$p_parentGroup <- parentGroup
     private$p_rowOrColumn <- rowOrColumn
     private$p_caption <- caption
     private$p_sortValue <- values[1]
     private$p_isTotal <- isTotal
     private$p_isLevelSubTotal <- isLevelSubTotal
     private$p_isLevelTotal <- isLevelTotal
     private$p_filters <- PivotFilters$new(parentPivot, variableName=variableName, type=filterType, values=values)
     private$p_groups <- list() # child groups
     private$p_calculationGroupName <- calculationGroupName
     private$p_calculationName <- calculationName
     private$p_parentPivot$message("PivotDataGroup$new", "Created new data group.")
   },
   getLevelNumber = function() {
     private$p_parentPivot$message("PivotDataGroup$getLevelNumber", "Getting level number...")
     levelNumber <- 0
     grp <- self
     while(!is.null(grp$parentGroup)) {
       levelNumber <- levelNumber + 1
       grp <- grp$parentGroup
     }
     private$p_parentPivot$message("PivotDataGroup$getLevelNumber", "Got level number.")
     return(invisible(levelNumber))
   },
   getAncestorGroups = function(ancestors=NULL, includeCurrentGroup=FALSE) {
     checkArgument("PivotDataGroup", "getAncestorGroups", ancestors, missing(ancestors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getAncestorGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     private$p_parentPivot$message("PivotDataGroup$getAncestorGroups", "Getting ancestors...",
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
     private$p_parentPivot$message("PivotDataGroup$getAncestorGroups", "Got ancestors.", list(count=length(acs)))
     return(invisible(acs)) # note the top-most parent will be at the bottom of the return list
   },
   getDescendantGroups = function(descendants=NULL, includeCurrentGroup=FALSE) {
     checkArgument("PivotDataGroup", "getDescendantGroups", descendants, missing(descendants), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getDescendantGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     private$p_parentPivot$message("PivotDataGroup$getDescendantGroups", "Getting descendant groups...")
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
     private$p_parentPivot$message("PivotDataGroup$getDescendantGroups", "Got descendant groups", list(count=length(dgs)))
     return(invisible(dgs))
   },
   getLeafGroups = function(leafGroups=NULL) {
     checkArgument("PivotDataGroup", "getLeafGroups", leafGroups, missing(leafGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_parentPivot$message("PivotDataGroup$getLeafGroups", "Getting leaf groups...", list(leafGroupCount=length(leafGroups)))
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
     private$p_parentPivot$message("PivotDataGroup$getLeafGroups", "Got leaf groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   getLevelGroups = function(level=NULL, levelGroups=NULL) { #level=0 is the current data group
     checkArgument("PivotDataGroup", "getLevelGroups", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotDataGroup", "getLevelGroups", levelGroups, missing(levelGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_parentPivot$message("PivotDataGroup$getLevelGroups", "Getting level groups...",
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
     private$p_parentPivot$message("PivotDataGroup$getLevelGroups", "Got level groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   addChildGroup = function(variableName=NULL, filterType="ALL", values=NULL, caption=NULL,
                            isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
                            calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "addChildGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
     checkArgument("PivotDataGroup", "addChildGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "addChildGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
     checkArgument("PivotDataGroup", "addChildGroup", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "addChildGroup", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "addChildGroup", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "addChildGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotDataGroup$addChildGroup", "Adding child group...",
                                   list(caption=caption, isTotal=isTotal, variableName=variableName, values=values,
                                   calculationGroupName=calculationGroupName, calculationName=calculationName))
     private$p_parentPivot$resetCells()
     total <- isTotal | self$isTotal
     grp <- PivotDataGroup$new(parentGroup=self, parentPivot=private$p_parentPivot,
                          rowOrColumn=private$p_rowOrColumn, caption=caption,
                          isTotal=total, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                          variableName=variableName, filterType=filterType, values=values,
                          calculationGroupName=calculationGroupName, calculationName=calculationName)
     index <- length(private$p_groups) + 1
     private$p_groups[[index]] <- grp
     private$p_parentPivot$message("PivotDataGroup$addChildGroup", "Added child group.")
     return(invisible(grp))
   },
   # permutations:
   # dataName="...", fromData=TRUE, onlyCombinationsThatExist=TRUE >> generate the list (from the data at leaf level), 1 value per group
   # dataName="...", fromData=TRUE, onlyCombinationsThatExist=FALSE >> generate the list (from the data at top level), 1 value per group
   # fromData=FALSE, explicitListOfValues=list(...), simply generates the groups from the values passed in
   # explicitListOfValues should be a LIST of values, each element in the list can be single value or a vector of values (to allow a
   # single pivot table row/column to represent multiple values)
   # atLevel is the number of levels below the current level
   addDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=0 is the current level, 1 = one level below, etc
                                dataName=NULL, dataSortOrder="asc", dataFormat=NULL, onlyCombinationsThatExist=TRUE,
                                explicitListOfValues=NULL, calculationGroupName=NULL,
                                expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total") {
     checkArgument("DataGroup", "addDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("DataGroup", "addDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("DataGroup", "addDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("DataGroup", "addDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none"))
     checkArgument("DataGroup", "addDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
     checkArgument("DataGroup", "addDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("DataGroup", "addDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("DataGroup", "addDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
     checkArgument("DataGroup", "addDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotDataGroup$addDataGroups", "Adding data groups...",
                                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                        onlyCombinationsThatExist=onlyCombinationsThatExist,
                                        explicitListOfValues=explicitListOfValues, calculationGroupName=calculationGroupName,
                                        expandExistingTotals=expandExistingTotals, addTotal=addTotal, visualTotals=visualTotals,
                                        totalPosition=totalPosition, totalCaption=totalCaption))
     private$p_parentPivot$resetCells()
     if(missing(variableName)||is.null(variableName)) stop("PivotDataGroup$addDataGroups(): variableName must be specified", call. = FALSE)
     if(addTotal==TRUE){ private$p_visualTotals <- visualTotals }
     df <- NULL
     topLevelDisinctValues <- NULL
     topLevelCaptions <- NULL
     allValues <- NULL
     if(fromData==TRUE) {
       # check that a data frame has been specified (or that we have a default data frame)
       if(missing(dataName)||is.null(dataName)) {
         if (private$p_parentPivot$data$count < 1) stop("PivotDataGroup$addDataGroups():  No data frames.  Specify data before calling addLeafGroup.", call. = FALSE)
         df <- private$p_parentPivot$data$defaultData
       }
       else {
         df <- private$p_parentPivot$data$getData(dataName)
         if(is.null(df)) stop(paste0("PivotDataGroup$addDataGroups():  No data frame found in PivotTable with name '", dataName, "'."), call. = FALSE)
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
       # build a dplyr query
       # todo: see escaping note 50 or so lines below
       data <- df
       eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
       data <- dplyr::distinct(data)
       if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
       else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", variableName, "))")))
       topLevelDisinctValues <- dplyr::collect(data)[[variableName]]
       if("factor" %in% class(topLevelDisinctValues)) { topLevelDisinctValues <- as.character(topLevelDisinctValues) }
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
     else if(atLevel==0) {
       # immediately below this data group
       parentGroups <- list()
       parentGroups[[1]] <- self
     }
     else {
       # at some number of levels below this group
       parentGroups <- self$getLevelGroups(level=atLevel-1)
       if((is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     # for each group...
     newGroups <- list()
     for(i in 1:length(parentGroups))
     {
       grp <- parentGroups[[i]]
       if((grp$isTotal==TRUE)&&(expandExistingTotals==FALSE)) {
         # add a single group that is an unexpanded total
         newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                     calculationGroupName=calculationGroupName,
                                     isTotal=TRUE, isLevelSubTotal=grp$isLevelSubTotal, isLevelTotal=grp$isLevelTotal)
         index <- length(newGroups) + 1
         newGroups[[index]] <- newGrp
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
         # construct the parent filter settings using "and" filter logic
         rowColFilters <- PivotFilters$new(private$p_parentPivot)
         for(j in length(ancestors):1) {
           acs <- ancestors[[j]]
           filters <- acs$filters
           if(is.null(filters)) next
           if(filters$count==0) next
           for(k in 1:length(filters$filters)) {
             filter <- filters$filters[[k]]
             rowColFilters$setFilter(filter, action="and")
           }
         }
         if(!rowColFilters$isNONE) {
           # build a dplyr query
           data <- df
           # todo: checking the escaping of the variable names and values below
           if((!rowColFilters$isALL)&&(rowColFilters$count > 0))
           {
             filterCmd <- NULL
             filterCount <- 0
             for(j in 1:length(rowColFilters$filters)) {
               filter <- rowColFilters$filters[[j]]
               if(is.null(filter$variableName)) stop("PivotCalculator$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
               if(is.null(filter$values)) next
               if(length(filter$values)==0) next
               if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
               if(length(filter$values)>0) {
                 # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
                 filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% rowColFilters$filters[[", j, "]]$values)")
                 filterCount <- filterCount + 1
               }
             }
             if(filterCount > 0) {
               filterCmd <- paste0("data <- dplyr::filter(data,", filterCmd, ")")
               eval(parse(text=filterCmd))
             }
           }
           # get the distinct values for the current variable
           eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
           data <- dplyr::distinct(data)
           if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
           else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", variableName, "))")))
           distinctValues <- dplyr::collect(data)[[variableName]]
           if("factor" %in% class(distinctValues)) { distinctValues <- as.character(distinctValues) }
           allValues <- union(allValues, distinctValues)
         }
       }
       # todo: potential perf optimisation
       # if the set of distinct values being used in the pivot matches the set of distinct values in the
       # data frame, then the additional filtering being done in the total column is wasted effort.  So,
       # if the generating the row/column groups for a particular variable (E.g. Gender) from the data frame
       # then switch visual totals off (if only M and F in the data, and M and F columns are added to the pivot, then
       # having a total column with a filter of M or F is pointless)

       # check we have some values
       if(is.null(distinctValues)||(length(distinctValues)==0)) {
         # add a blank group
         newGrp <- grp$addChildGroup(variableName=variableName, filterType="NONE", values=NULL, caption="",
                                     calculationGroupName=calculationGroupName,
                                     isTotal=grp$isTotal, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal)
         index <- length(newGroups) + 1
         newGroups[[index]] <- newGrp
       }
       # append the child groups
       else if("list" %in% class(distinctValues)) {
         if((addTotal==TRUE)&&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           caption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) caption <- distinctCaptions[j]
           if(is.null(caption)&&(!is.null(dataFormat))) caption <- private$formatValue(distinctValues[[j]], dataFormat)
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[[j]], caption=caption,
                                       calculationGroupName=calculationGroupName, isTotal=grp$isTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
       }
       else {
         if((addTotal==TRUE)&&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           caption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) caption <- distinctCaptions[j]
           if(is.null(caption)&&(!is.null(dataFormat))) caption <- private$formatValue(distinctValues[j], dataFormat)
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[j], caption=caption,
                                       calculationGroupName=calculationGroupName, isTotal=grp$isTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=NULL, # so that the totals have a reference to the variable
                                       caption=totalCaption, calculationGroupName=calculationGroupName,
                                       isTotal=TRUE, isLevelSubTotal=!grp$isLevelTotal, isLevelTotal=grp$isLevelTotal)
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
         if(descdnt$isTotal) descdnt$filters$setFilter(topLevelFilter, action="and")
       }
     }
     private$p_parentPivot$message("PivotDataGroup$addDataGroups", "Added groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
   },
   sortDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
     checkArgument("DataGroup", "sortDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("DataGroup", "sortDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
     checkArgument("DataGroup", "sortDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
     checkArgument("DataGroup", "sortDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     checkArgument("DataGroup", "sortDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotDataGroup$sortDataGroups", "Sorting data groups...",
                                   list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName))
     private$p_parentPivot$resetCells()
     if(is.null(private$p_groups)) return(invisible())
     if(length(private$p_groups)==0) return(invisible())
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
     if(levelNumber==0) {
       # sort at this level
       if(orderBy=="value") {
         # sorting by value
         groups <- list()
         values <- list()
         j <- 0
         for(i in 1:length(private$p_groups)) {
           grp <- private$p_groups[[i]]
           if(grp$isTotal==TRUE) next
           j <- j + 1
           groups[[j]] <- grp
           values[[j]] <- grp$sortValue
         }
         if(length(values)>0){
           if(sortOrder=="asc") sortIndexes <- order(unlist(values))
           else sortIndexes <- order(unlist(values), decreasing=TRUE)
           j <- 0
           for(i in 1:length(private$p_groups)) {
             if(private$p_groups[[i]]$isTotal==TRUE) next
             j <- j + 1
             private$p_groups[[i]] <- groups[[sortIndexes[j]]]
           }
         }
       }
       else if(orderBy=="caption") {
         # sorting by caption
         groups <- list()
         captions <- list()
         j <- 0
         for(i in 1:length(private$p_groups)) {
           grp <- private$p_groups[[i]]
           if(grp$isTotal==TRUE) next
           j <- j + 1
           groups[[j]] <- grp
           captions[[j]] <- grp$caption
         }
         if(length(captions)>0) {
           if(sortOrder=="asc") sortIndexes <- order(unlist(captions))
           else sortIndexes <- order(unlist(captions), decreasing=TRUE)
           j <- 0
           for(i in 1:length(private$p_groups)) {
             if(private$p_groups[[i]]$isTotal==TRUE) next
             j <- j + 1
             private$p_groups[[i]] <- groups[[sortIndexes[j]]]
           }
         }
       }
       else {
         # sorting by calculation
         groups <- list()
         rawValues <- list()
         pivotCalculator <- PivotCalculator$new(private$p_parentPivot)
         j <- 0
         for(i in 1:length(private$p_groups)) {
           grp <- private$p_groups[[i]]
           if(grp$isTotal==TRUE) next
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
               netFilters$setFilter(filter, action="and")
             }
           }
           # calculate the value
           results <- pivotCalculator$evaluateNamedCalculation2(calculationName=calculationName,
                                                               calculationGroupName=calculationGroupName,
                                                               rowColFilters=netFilters)
           calcResults <- results[[calculationName]]
           rawValues[[j]] <- calcResults$rawValue
         }
         if(length(rawValues)>0) {
           if(sortOrder=="asc") sortIndexes <- order(unlist(rawValues))
           else sortIndexes <- order(unlist(rawValues), decreasing=TRUE)
           j <- 0
           for(i in 1:length(private$p_groups)) {
             if(private$p_groups[[i]]$isTotal==TRUE) next
             j <- j + 1
             private$p_groups[[i]] <- groups[[sortIndexes[j]]]
           }
         }
       }
     }
     else {
       # sort below this level
       for(i in 1:length(private$p_groups)) {
         private$p_groups[[i]]$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                              calculationGroupName=calculationGroupName, calculationName=calculationName)
       }
     }
     private$p_parentPivot$message("PivotDataGroup$sortDataGroups", "Sorted data groups.")
     return(invisible())
   },
   addCalculationGroups = function(calculationGroupName=NULL, atLevel=NULL) {
     checkArgument("PivotDataGroup", "addCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     private$p_parentPivot$message("PivotDataGroup$addCalculationGroups", "Adding calculation groups...")
     private$p_parentPivot$resetCells()
     private$p_parentPivot$calculationsPosition <- private$p_rowOrColumn # will throw an error if trying to add calcs to one axis when already present on the other axis
     if(missing(calculationGroupName)) stop("PivotDataGroup$addCalculationGroups():  calculationGroupName must be specified.", call. = FALSE)
     if(is.null(calculationGroupName)) stop("PivotDataGroup$addCalculationGroups():  calculationGroupName cannot be null.", call. = FALSE)
     if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
       stop(paste0("PivotDataGroup$addCalculationGroups():  There is no Calculation Group named '", calculationGroupName, "' in the Pivot Table."), call. = FALSE)
     # get the calculation group and the calculations to be displayed
     calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(calculationGroup$count==0)
       stop(paste0("PivotDataGroup$addCalculationGroups():  There are no calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- list()
     for (i in 1:calculationGroup$count) {
       calc <- calculationGroup$calculations[[i]]
       if(calc$visible==TRUE) {
         cname <- paste0("calc", calc$displayOrder, "-", i)
         calculations[[cname]] <- calc
       }
     }
     if(length(calculations)==0)
       stop(paste0("PivotDataGroup$addCalculationGroups():  There are no visible calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- calculations[order(names(calculations))]
     # where are the new groups being added?
     if(is.null(atLevel)) {
       # get the current set of leaf groups
       parentGroups <- self$getLeafGroups()
       if((length(private$p_groups)==0)||(is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     else if(atLevel==0) {
       # immediately below this data group
       parentGroups <- list()
       parentGroups[[1]] <- self
     }
     else {
       # at some number of levels below this group
       parentGroups <- self$getLevelGroups(level=atLevel-1)
       if((is.null(parentGroups))||(length(parentGroups)==0)) {
         parentGroups <- list()
         parentGroups[[1]] <- self
       }
     }
     # if there is only one calculation (and this is not the top group), just set the calculation directly on the existing leaf nodes,
     # otherwise add a new row and iterate the calculations
     newGroups <- list()
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
                                         calculationGroupName=calculationGroupName, calculationName=calc$calculationName, isTotal=self$isTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGroup
         }
       }
     }
     private$p_parentPivot$message("PivotDataGroup$addCalculationGroups", "Added calculation groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
   },
   getLevelCount = function(includeCurrentLevel=FALSE) {
     checkArgument("PivotDataGroup", "getLevelCount", includeCurrentLevel, missing(includeCurrentLevel), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     private$p_parentPivot$message("PivotDataGroup$levelCount", "Counting levels...")
     # get the leaf level groups
     leafGroups <- self$getLeafGroups()
     # get the maximum number of parents of each group
     maxParents <- 0
     if(length(leafGroups)==0) return()
     for(i in 1:length(leafGroups)) {
       ancestors <- leafGroups[[i]]$getAncestorGroups(includeCurrentGroup=includeCurrentLevel)
       maxParents <- max(maxParents, length(ancestors))
     }
     private$p_parentPivot$message("PivotDataGroup$levelCount", "Counted levels.")
     return(invisible(maxParents))
   },
   normaliseDataGroup = function() {
     private$p_parentPivot$message("PivotDataGroup$normaliseDataGroup", "Normalising data group...")
     private$p_parentPivot$resetCells()
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
           dg <- dg$addChildGroup(isTotal=self$isTotal)
           groupsAdded <- groupsAdded + 1
         }
       }
     }
     private$p_parentPivot$message("PivotDataGroup$normaliseDataGroup", "Normalised data group.")
     return(invisible())
   },
   getNetFilters = function() { # start at the root and apply the filter criteria that gets set at each lower level
     private$p_parentPivot$message("PivotDataGroup$getNetFilters", "Getting net filters...")
     ancestors <- self$getAncestorGroups(includeCurrentGroup=TRUE)
     netFilters <- PivotFilters$new(private$p_parentPivot)
     for(j in length(ancestors):1) {
       acs <- ancestors[[j]]
       filters <- acs$filters
       if(is.null(filters)) next
       if(filters$count==0) next
       for(k in 1:length(filters$filters)) {
         filter <- filters$filters[[k]]
         netFilters$setFilter(filter, action="and")
       }
     }
     private$p_parentPivot$message("PivotDataGroup$getNetFilters", "Got net filters.")
     return(invisible(netFilters))
   },
   getNetCalculationName = function() { # start at the current node and work upwards until the first calculation is encountered
     private$p_parentPivot$message("PivotDataGroup$getNetCalculationName", "Getting net calculation...")
     grp <- self
     while (!is.null(grp)) {
       if(!is.null(grp$calculationName)) return(invisible(grp$calculationName))
       grp <- grp$parentGroup
     }
     private$p_parentPivot$message("PivotDataGroup$getNetCalculationName", "Got net calculation.")
     return(invisible())
   },
   isFindMatch = function(matchMode="simple", variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL) {
     checkArgument("PivotDataGroup", "isFindMatch", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
     checkArgument("PivotDataGroup", "isFindMatch", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "isFindMatch", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "isFindMatch", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
     checkArgument("PivotDataGroup", "isFindMatch", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotDataGroup$isFindMatch", "Checking if matches criteria...")
     # a) check the filter match
     if((!is.null(variableNames))||(!is.null(variableValues))) {
       if(matchMode=="simple") filters <- private$p_filters
       else filters <- self$getNetFilters()
       if(is.null(filters)) return(invisible(FALSE))
       isMatch <- filters$isFilterMatch(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues)
       if(isMatch==FALSE) return(invisible(FALSE))
     }
     # b) check totals criteria
     if((totals=="exclude")&&(self$isTotal==TRUE)) return(invisible(FALSE))
     if((totals=="only")&&(self$isTotal==FALSE)) return(invisible(FALSE))
     # c) check calculation criteria
     if(!is.null(calculationNames)) {
       if(matchMode=="simple") calcName <- private$p_calculationName
       else calcName <- self$getNetCalculationName()
       if(is.null(calcName)) return(invisible(FALSE))
       if(!(calcName %in% calculationNames)) return(invisible(FALSE))
     }
     private$p_parentPivot$message("PivotDataGroup$isFindMatch", "Checked if matches criteria.")
     return(invisible(TRUE))
   },
   findDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                             totals="include", calculationNames=NULL, includeDescendantGroups=FALSE) {
     checkArgument("PivotDataGroup", "findDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
     checkArgument("PivotDataGroup", "findDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "findDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "findDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
     checkArgument("PivotDataGroup", "findDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "findDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     private$p_parentPivot$message("PivotDataGroup$findDataGroups", "Finding data groups...")
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
                              totals=totals, calculationNames=calculationNames)) {
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
                              totals=totals, calculationNames=calculationNames)) {
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
     private$p_parentPivot$message("PivotDataGroup$findDataGroups", "Found data groups.")
     return(invisible(matches))
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
   parentGroup = function(value) { return(invisible(private$p_parentGroup)) },
   childGroups = function(value) { return(invisible(private$p_groups)) },
   leafGroups = function(value) { return(invisible(self$getLeafGroups())) },
   filters = function(value) { return(invisible(private$p_filters)) },
   calculationGroupName = function(value) {
     if(missing(value)) { return(invisible(private$p_calculationGroupName)) }
     else {
       checkArgument("PivotDataGroup", "calculationGroupName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
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
       checkArgument("PivotDataGroup", "calculationName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
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
   isTotal = function(value) { return(invisible(private$p_isTotal)) },
   isLevelSubTotal = function(value) { return(invisible(private$p_isLevelSubTotal)) },
   isLevelTotal = function(value) { return(invisible(private$p_isLevelTotal)) },
   caption = function(value) {
     if(is.null(private$p_caption)) {
       if((private$p_isTotal)&&(!is.null(private$p_parentGroup))&&
          (length(private$p_parentGroup$childGroups)<2)) return(invisible(""))
       if(is.null(private$p_filters)) return(invisible(""))
       else return(invisible(private$p_filters$asString(includeVariableName=FALSE)))
     }
     else return(invisible(private$p_caption))
   },
   sortValue = function(value) { return(invisible(private$p_sortValue)) },
   rowColumnNumber = function(value) {
     if(missing(value)) { return(invisible(private$p_rowColumnNumber)) }
     else {
       checkArgument("PivotDataGroup", "rowColumnNumber", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "number"))
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
       checkArgument("PivotDataGroup", "baseStyleName", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       private$p_baseStyleName <- value
       return(invisible())
     }
   },
   style = function(value) {
     if(missing(value)) { return(invisible(private$p_style)) }
     else {
       checkArgument("PivotDataGroup", "style", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyle")
       private$p_style <- value
       return(invisible())
     }
   },
   isMatch = function(value) {
     if(missing(value)) { return(invisible(private$p_isMatch)) }
     else {
       checkArgument("PivotDataGroup", "isMatch", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       private$p_isMatch <- value
       return(invisible())
     }
   },
   isRendered = function(value) {
     if(missing(value)) { return(invisible(private$p_isRendered)) }
     else {
       checkArgument("PivotDataGroup", "isRendered", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       private$p_isRendered <- value
       return(invisible())
     }
   },
   isWithinVisibleRange = function(value) {
     if(missing(value)) { return(invisible(private$p_isWithinVisibleRange)) }
     else {
       checkArgument("PivotDataGroup", "isWithinVisibleRange", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
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
   }
  ),
  private = list(
   p_parentGroup = NULL,
   p_parentPivot = NULL,
   p_rowOrColumn = NULL,
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
   p_style = NULL,
   p_isMatch = FALSE, # helper flag used when searching through data groups
   p_isWithinVisibleRange = TRUE, # helper flag to allow a subsection of the pivot table to be output
   p_isRendered = FALSE, # helper flag to keep track of which data groups have already been rendered

   # private functions:
   formatValue = function(value=NULL, format=NULL) {
     checkArgument("PivotDataGroup", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric", "character", "factor", "logical", "Date", "POSIXct", "POSIXlt"))
     checkArgument("PivotDataGroup", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
     private$p_parentPivot$message("PivotDataGroup$formatValue", "Formatting value...")
     if(is.null(value)) return(invisible(NULL))
     if(is.null(format)) return(value)
     clsv <- class(value)
     if(("numeric" %in% clsv)||("integer" %in% clsv)) {
       clsf <- class(format)
       if("character" %in% clsf) value <- sprintf(format, value)
       else if ("list" %in% clsf) {
         args <- format
         args$x <- value
         value <- do.call(base::format, args)
       }
       else if ("function" %in% class(format)) value <- format(value)
     }
     else if(("Date" %in% clsv)||("POSIXct" %in% clsv)||("POSIXlt" %in% clsv)) {
       clsf <- class(format)
       if ("list" %in% clsf) {
         args <- format
         args$x <- value
         value <- do.call(base::format, args)
       }
       else if ("function" %in% class(format)) value <- format(value)
     }
     else if ("factor" %in% clsv) value <- as.character(value)
     else if("logical" %in% clsv) value <- as.character(value)
     private$p_parentPivot$message("PivotDataGroup$formatValue", "Formated value.")
     return(invisible(value))
   }
  )
)
