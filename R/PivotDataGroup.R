PivotDataGroup <- R6::R6Class("PivotDataGroup",
  public = list(
   initialize = function(parentGroup=NULL, parentPivot=NULL, rowOrColumn=NULL,
                         caption=NULL, isTotal=FALSE, # common properties
                         variableName=NULL, values=NULL, # filter properties
                         calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "initialize", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotDataGroup", "initialize", rowOrColumn, missing(rowOrColumn), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
     checkArgument("PivotDataGroup", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "initialize", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "initialize", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("DataGroup$new", "Creating new data group...",
                                   list(rowOrColumn=rowOrColumn, caption=caption, variableName=variableName, values=values,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName))
     if(!(rowOrColumn %in% c("row", "column"))) stop("DataGroup$new(): rowOrColumn must be either row or column", call. = FALSE)
     private$p_parentGroup <- parentGroup
     private$p_rowOrColumn <- rowOrColumn
     private$p_caption <- caption
     private$p_isTotal <- isTotal
     private$p_filters <- PivotFilters$new(parentPivot, variableName, values)
     private$p_groups <- list() # child groups
     private$p_calculationGroupName <- calculationGroupName
     private$p_calculationName <- calculationName
     private$p_parentPivot$message("DataGroup$new", "Created new data group.")
   },
   getAncestorGroups = function(ancestors=NULL, includeCurrentGroup=FALSE) {
     checkArgument("PivotDataGroup", "getAncestorGroups", ancestors, missing(ancestors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getAncestorGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     private$p_parentPivot$message("DataGroup$getAncestorGroups", "Getting ancestors...",
                                   list(ancestorCount=length(ancestors), includeCurrentGroup=includeCurrentGroup))
     acs <- NULL
     if(missing(ancestors)|is.null(ancestors)) {
       acs <- list()
       if(includeCurrentGroup==TRUE) { acs[[1]] <- self }
     }
     else { acs <- ancestors }
     if(!is.null(private$p_parentGroup)) {
       index <- length(acs) + 1
       acs[[index]] <- private$p_parentGroup
       acs <- private$p_parentGroup$getAncestorGroups(acs)
     }
     private$p_parentPivot$message("DataGroup$getAncestorGroups", "Got ancestors.", list(count=length(acs)))
     return(acs) # note the top-most parent will be at the bottom of the return list
   },
   getDescendantGroups = function(descendants=NULL, includeCurrentGroup=FALSE) {
     checkArgument("PivotDataGroup", "getDescendantGroups", descendants, missing(descendants), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getDescendantGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     private$p_parentPivot$message("DataGroup$getDescendantGroups", "Getting descendant groups...")
     dgs <- NULL
     if(missing(descendants)|is.null(descendants)) { dgs <- list() }
     else { dgs <- descendants }
     index <- length(dgs) + 1
     dgs[[index]] <- self
     if(length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         dgs <- private$p_groups[[i]]$getLeafGroups(dgs)
       }
     }
     private$p_parentPivot$message("DataGroup$getDescendantGroups", "Got descendant groups", list(count=length(dgs)))
     return(dgs)
   },
   getLeafGroups = function(leafGroups=NULL) {
     checkArgument("PivotDataGroup", "getLeafGroups", leafGroups, missing(leafGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_parentPivot$message("DataGroup$getLeafGroups", "Getting leaf groups...", list(leafGroupCount=length(leafGroups)))
     lgs <- NULL
     if(missing(leafGroups)|is.null(leafGroups)) { lgs <- list() }
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
     private$p_parentPivot$message("DataGroup$getLeafGroups", "Got leaf groups", list(count=length(lgs)))
     return(lgs)
   },
   getLevelGroups = function(levelGroups=NULL, level=NULL) { #level=1 is the current data group
     checkArgument("PivotDataGroup", "getLevelGroups", levelGroups, missing(levelGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getLevelGroups", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     private$p_parentPivot$message("DataGroup$getLevelGroups", "Getting level groups...",
                                   list(levelGroupCount=length(levelGroups), level=level))
     lgs <- NULL
     if(missing(levelGroups)|is.null(levelGroups)) { lgs <- list() }
     else { lgs <- levelGroups }
     if(level==1) {
       index <- length(lgs) + 1
       lgs[[index]] <- self
     }
     else if (length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         lgs <- private$p_groups[[i]]$getLevelGroups(lgs, level-1)
       }
     }
     private$p_parentPivot$message("DataGroup$getLevelGroups", "Got level groups", list(count=length(lgs)))
     return(lgs)
   },
   addChildGroup = function(variableName=NULL, values=NULL,
                            caption=NULL, isTotal=FALSE,
                            calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "addChildGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "addChildGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotDataGroup", "addChildGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("DataGroup$addChildGroup", "Adding child group...",
                                   list(caption=caption, isTotal=isTotal, variableName=variableName, values=values,
                                   calculationGroupName=calculationGroupName, calculationName=calculationName))
     private$p_parentPivot$resetCells()
     total <- isTotal | self$isTotal
     grp <- PivotDataGroup$new(parentGroup=self, parentPivot=private$p_parentPivot,
                          rowOrColumn=private$p_rowOrColumn, caption=caption, isTotal=total,
                          variableName=variableName, values=values,
                          calculationGroupName=calculationGroupName, calculationName=calculationName)
     index <- length(private$p_groups) + 1
     private$p_groups[[index]] <- grp
     private$p_parentPivot$message("DataGroup$addChildGroup", "Added child group.")
     return(grp)
   },
   # permutations:
   # dataName="...", fromData=TRUE, leafLevelPermutations=TRUE >> generate the list (from the data at leaf level), 1 value per group
   # dataName="...", fromData=TRUE, leafLevelPermutations=FALSE >> generate the list (from the data at top level), 1 value per group
   # fromData=FALSE, explicitListOfValues=TRUE, simply generates the groups from the values passed in
   # explicitListOfValues should be a LIST of values, each element in the list can be single value or a vector of values (to allow a
   # single pivot table row/column to represent multiple values)
   addLeafDataGroup = function(variableName=NULL, dataName=NULL, fromData=TRUE,
                           leafLevelPermutations=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                           expandExistingTotals=FALSE,
                           addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total") {
     checkArgument("DataGroup", "addLeafDataGroup", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("DataGroup", "addLeafDataGroup", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("DataGroup", "addLeafDataGroup", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addLeafDataGroup", leafLevelPermutations, missing(leafLevelPermutations), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addLeafDataGroup", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("DataGroup", "addLeafDataGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("DataGroup", "addLeafDataGroup", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addLeafDataGroup", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addLeafDataGroup", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("DataGroup", "addLeafDataGroup", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
     checkArgument("DataGroup", "addLeafDataGroup", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("DataGroup$addLeafDataGroup", "Adding leaf data group...",
                                   list(variableName=variableName, dataName=dataName, fromData=fromData,
                                        leafLevelPermutations=leafLevelPermutations, explicitListOfValues=explicitListOfValues,
                                        expandExistingTotals=expandExistingTotals, addTotal=addTotal, visualTotals=visualTotals,
                                        totalPosition=totalPosition, totalCaption=totalCaption))
     private$p_parentPivot$resetCells()
     if(missing(variableName)|is.null(variableName)) stop("DataGroup$addLeafDataGroup(): variableName must be specified", call. = FALSE)
     if(addTotal==TRUE){ private$p_visualTotals <- visualTotals }
     df <- NULL
     topLevelDisinctValues <- NULL
     topLevelFilter <- NULL
     if(fromData==TRUE) {
       # check that a data frame has been specified (or that we have a default data frame)
       if(missing(dataName)|is.null(dataName)) {
         if (private$p_parentPivot$data$count < 1) stop("DataGroup$addLeafDataGroup():  No data frames.  Specify data before calling addLeafGroup.", call. = FALSE)
         df <- private$p_parentPivot$data$defaultData
       }
       else {
         df <- private$p_parentPivot$data$getData(dataName)
         if(is.null(df)) stop(paste0("DataGroup$addLeafDataGroup():  No data frame found in PivotTable with name '", dataName, "'."), call. = FALSE)
       }
     }
     else {
       if (missing(explicitListOfValues)) stop("DataGroup$addLeafDataGroup():  An explicitListOfValues must be specified when fromData=FALSE", call. = FALSE)
       if (is.null(explicitListOfValues)) stop("DataGroup$addLeafDataGroup():  explicitListOfValues must not be null when fromData=FALSE", call. = FALSE)
       topLevelDisinctValues <- explicitListOfValues
       fvals <- topLevelDisinctValues
       if("list" %in% class(topLevelDisinctValues)) { fvals <- unlist(topLevelDisinctValues) }
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=fvals)
     }
     # ignore the filters from the other heading groups?
     if((fromData==TRUE)&(leafLevelPermutations==FALSE)) {
       # build a dplyr query
       # todo: see escaping note 50 or so lines below
       data <- df
       eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
       data <- dplyr::distinct(data)
       eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
       topLevelDisinctValues <- dplyr::collect(data)[[variableName]]
       if("factor" %in% class(topLevelDisinctValues)) { topLevelDisinctValues <- as.character(distinctValues) }
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=topLevelDisinctValues)
     }
     # get the current set of leaf groups
     leafGroups <- self$getLeafGroups()
     if((length(private$p_groups)==0)|(is.null(leafGroups))|(length(leafGroups)==0)) {
       leafGroups <- list()
       leafGroups[[1]] <- self
     }
     # for each leaf group...
     newLeafGroups <- list()
     if(is.null(topLevelFilter))
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=NULL)
     for(i in 1:length(leafGroups))
     {
       grp <- leafGroups[[i]]
       if((grp$isTotal==TRUE)&(expandExistingTotals==FALSE)) next

       # use top level groups?
       distinctValues <- NULL
       if(!is.null(topLevelDisinctValues)) { distinctValues <- topLevelDisinctValues }
       else {
         # get the ancestor groups for this group, starting with the current object
         ancestors <- grp$getAncestorGroups(includeCurrentGroup=TRUE)
         # construct the parent filter settings using "replace" filter logic
         rowColFilters <- PivotFilters$new(private$p_parentPivot)
         for(j in length(ancestors):1) {
           acs <- ancestors[[j]]
           filters <- acs$filters
           if(is.null(filters)) next
           if(filters$count==0) next
           for(k in 1:length(filters$filters)) {
             filter <- filters$filters[[k]]
             rowColFilters$setFilter(filter, action="replace")
           }
         }
         # build a dplyr query
         data <- df
         # todo: checking the escaping of the variable names and values below
         if (rowColFilters$count > 0)
         {
           filterCmd <- NULL
           for(j in 1:length(rowColFilters$filters)) {
             filter <- rowColFilters$filters[[j]]
             if(is.null(filter$variableName)) stop("PivotCalculator$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
             if(is.null(filter$values)) next
             if(length(filter$values)==0) next
             if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
             if(length(filter$values)==1) {
               filterCmd <- paste0(filterCmd, "(", filter$variableName, " == rowColFilters$filters[[", j, "]]$values)")
             }
             else if(length(filter$values)>1) {
               filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% rowColFilters$filters[[", j, "]]$values)")
             }
             # using eval repeatedly with the command above is not very efficient
             # but it avoids issues with values as strings, escaping, using stringi::stri_escape_unicode, etc
           }
           filterCmd <- paste0("data <- dplyr::filter(data,", filterCmd, ")")
           eval(parse(text=filterCmd))
         }
         # get the distinct values for the current variable
         eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
         data <- dplyr::distinct(data)
         eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
         distinctValues <- dplyr::collect(data)[[variableName]]
         if("factor" %in% class(distinctValues)) { distinctValues <- as.character(distinctValues) }
         topLevelFilter$union(PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=distinctValues))
       }
       # todo: potential perf optimisation
       # if the set of distinct values being used in the pivot matches the set of distinct values in the
       # data frame, then the additional filtering being done in the total column is wasted effort.  So,
       # if the generating the row/column groups for a particular variable (E.g. Gender) from the data frame
       # then switch visual totals off (if only M and F in the data, and M and F columns are added to the pivot, then
       # having a total column with a filter of M or F is pointless)

       # append the child groups
       if("list" %in% class(distinctValues)) {
         if((addTotal==TRUE)&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[[j]], calculationGroupName=calculationGroupName, isTotal=self$isTotal)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
       }
       else {
         if((addTotal==TRUE)&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[j], calculationGroupName=calculationGroupName, isTotal=self$isTotal)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGrp
         }
       }
     }
     # if visual totals are enabled...
     # get all the totals that are the descendants of this (i.e. self) data group and union in this additional criteria
     if(visualTotals==TRUE) {
       descdnts <- self$getDescendantGroups(includeCurrentGroup=TRUE)
       for(i in 1:length(descdnts)) {
         descdnt <- descdnts[[i]]
         if(descdnt$isTotal) descdnt$filters$setFilter(topLevelFilter, action="union")
       }
     }
     private$p_parentPivot$message("DataGroup$addLeafDataGroup", "Added leaf groups.", list(count=length(newLeafGroups)))
     return(newLeafGroups)
   },
   addLeafCalculationGroup = function(calculationGroupName=NULL) {
     checkArgument("PivotDataGroup", "addLeafCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("DataGroup$addLeafCalculationGroup", "Adding leaf calculation group...")
     private$p_parentPivot$resetCells()
     private$p_parentPivot$calculationsPosition <- private$p_rowOrColumn # will throw an error if trying to add calcs to one axis when already present on the other axis
     if(missing(calculationGroupName)) stop("DataGroup$addLeafCalculationGroup():  calculationGroupName must be specified.", call. = FALSE)
     if(is.null(calculationGroupName)) stop("DataGroup$addLeafCalculationGroup():  calculationGroupName cannot be null.", call. = FALSE)
     if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
       stop(paste0("DataGroup$addLeafCalculationGroup():  There is no Calculation Group named '", calculationGroupName, "' in the Pivot Table."), call. = FALSE)
     # get the calculation group and the calculations to be displayed
     calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(calculationGroup$count==0)
       stop(paste0("DataGroup$addLeafCalculationGroup():  There are no calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- list()
     for (i in 1:calculationGroup$count) {
       calc <- calculationGroup$calculations[[i]]
       if(calc$visible==TRUE) {
         cname <- paste0("calc", calc$displayOrder, "-", i)
         calculations[[cname]] <- calc
       }
     }
     if(length(calculations)==0)
       stop(paste0("DataGroup$addLeafCalculationGroup():  There are no visible calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- calculations[order(names(calculations))]
     # get the current set of leaf groups
     leafGroups <- self$getLeafGroups()
     if((length(private$p_groups)==0)|(is.null(leafGroups))|(length(leafGroups)==0)) {
       leafGroups <- list()
       leafGroups[[1]] <- self
     }
     # if there is only one calculation, just set the calculation directly on the existing leaf nodes,
     # otherwise add a new row and iterate the calculations
     newLeafGroups <- list()
     if (length(calculations)==1) {
       for(i in 1:length(leafGroups)) {
         grp <- leafGroups[[i]]
         grp$calculationGroupName <- calculationGroupName
         grp$calculationName <- calculations[[1]]$calculationName
       }
     }
     else {
       for(i in 1:length(leafGroups)) {
         grp <- leafGroups[[i]]
         for(j in 1:length(calculations)) {
           calc <- calculations[[j]]
           newGroup <- grp$addChildGroup(caption=calc$caption,
                                         calculationGroupName=calculationGroupName, calculationName=calc$calculationName, isTotal=self$isTotal)
           index <- length(newLeafGroups) + 1
           newLeafGroups[[index]] <- newGroup
         }
       }
     }
     private$p_parentPivot$message("DataGroup$addLeafCalculationGroup", "Added leaf calculation group.", list(count=length(newLeafGroups)))
     return(newLeafGroups)
   },
   getLevelCount = function(includeCurrentLevel=FALSE) {
     checkArgument("PivotDataGroup", "getLevelCount", includeCurrentLevel, missing(includeCurrentLevel), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     private$p_parentPivot$message("DataGroup$levelCount", "Counting levels...")
     # get the leaf level groups
     leafGroups <- self$getLeafGroups()
     # get the maximum number of parents of each group
     maxParents <- 0
     if(length(leafGroups)==0) return()
     for(i in 1:length(leafGroups)) {
       ancestors <- leafGroups[[i]]$getAncestorGroups(includeCurrentGroup=includeCurrentLevel)
       maxParents <- max(maxParents, length(ancestors))
     }
     private$p_parentPivot$message("DataGroup$levelCount", "Counted levels.")
     return(maxParents)
   },
   normaliseDataGroup = function() {
     private$p_parentPivot$message("DataGroup$normaliseDataGroup", "Normalising data group...")
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
     private$p_parentPivot$message("DataGroup$normaliseDataGroup", "Normalised data group.")
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
       filters = private$p_filters$asList(),
       calculationGroupName = private$p_calculationGroupName,
       calculationName = private$p_calculationName,
       rowColumnNumber = private$p_rowColumnNumber,
       isRendered = private$p_isRendered,
       groups = grps
     )
     lst <- lst[order(names(lst))]
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) }
  ),
  active = list(
   parentGroup = function(value) { return(private$p_parentGroup) },
   childGroups = function(value) { return(private$p_groups) },
   leafGroups = function(value) { return(self$getLeafGroups()) },
   filters = function(value) { return(private$p_filters) },
   calculationGroupName = function(value) {
     if(missing(value)) { return(private$p_calculationGroupName) }
     else {
       checkArgument("PivotDataGroup", "calculationGroupName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       if(!(private$p_parentPivot$calculationGroups$isExistingCalculationGroup(value))) {
         stop(paste0("DataGroup$calculationGroupName(): The Calculation Group '", value, "' does not exist in the Pivot Table."), call. = FALSE)
       }
       private$p_calculationGroupName <- value
     }
   },
   calculationName = function(value) {
     if(missing(value)) { return(private$p_calculationName) }
     else {
       checkArgument("PivotDataGroup", "calculationName", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       if(is.null(private$p_calculationGroupName))
         stop("DataGroup$calculationName(): Specify the Calculation Group before the Calculation.", call. = FALSE)
       calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(private$p_calculationGroupName)
       if(!(calculationGroup$isExistingCalculation(value))) {
         stop(paste0("DataGroup$calculationName(): The Calculation '", value,
                     "' does not exist in the Calculation Group '", private$p_calculationGroupName, "'"), call. = FALSE)
       }
       private$p_calculationName <- value
     }
   },
   isTotal = function(value) { return(private$p_isTotal) },
   caption = function(value) {
     if(is.null(private$p_caption)) {
       if(is.null(private$p_filters)) return("")
       else return(private$p_filters$asString(includeVariableName=FALSE))
     }
     else return(private$p_caption)
   },
   rowColumnNumber = function(value) {
     if(missing(value)) { return(private$p_rowColumnNumber) }
     else {
       checkArgument("PivotDataGroup", "rowColumnNumber", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "number"))
       if(!is.null(value)) {
         if(!is.integer(value)) stop("DataGroup$rowColumnNumber(): rowColumnNumber must be an integer", call. = FALSE)
       }
       private$p_rowColumnNumber <- value
     }
   },
   isRendered = function(value) {
     if(missing(value)) { return(private$p_isRendered) }
     else {
       checkArgument("PivotDataGroup", "isRendered", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       private$p_isRendered <- value
     }
   }
  ),
  private = list(
   p_parentGroup = NULL,
   p_parentPivot = NULL,
   p_rowOrColumn = NULL,
   p_caption = NULL,
   p_isTotal = NULL,
   p_visualTotals = NULL,
   p_filters = NULL,
   p_groups = NULL,
   p_calculationGroupName = NULL,
   p_calculationName = NULL,
   p_rowColumnNumber = NULL,
   p_isRendered = FALSE # helper flag to keep track of which data groups have already been rendered
  )
)
