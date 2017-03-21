PivotDataGroup <- R6::R6Class("PivotDataGroup",
  public = list(
   initialize = function(parentGroup=NULL, parentPivot=NULL, rowOrColumn=NULL,
                         caption=NULL, isTotal=FALSE, # common properties
                         variableName=NULL, values=NULL, # filter properties
                         calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "initialize", parentGroup, missing(parentGroup), allowMissing=FALSE, allowNull=TRUE, allowedClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotDataGroup", "initialize", rowOrColumn, missing(rowOrColumn), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
     checkArgument("PivotDataGroup", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
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
     private$p_sortValue <- values[1]
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
     private$p_parentPivot$message("DataGroup$getAncestorGroups", "Got ancestors.", list(count=length(acs)))
     return(invisible(acs)) # note the top-most parent will be at the bottom of the return list
   },
   getDescendantGroups = function(descendants=NULL, includeCurrentGroup=FALSE) {
     checkArgument("PivotDataGroup", "getDescendantGroups", descendants, missing(descendants), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     checkArgument("PivotDataGroup", "getDescendantGroups", includeCurrentGroup, missing(includeCurrentGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     private$p_parentPivot$message("DataGroup$getDescendantGroups", "Getting descendant groups...")
     dgs <- NULL
     if(missing(descendants)||is.null(descendants)) { dgs <- list() }
     else { dgs <- descendants }
     index <- length(dgs) + 1
     dgs[[index]] <- self
     if(length(private$p_groups) > 0) {
       for (i in 1:length(private$p_groups)) {
         dgs <- private$p_groups[[i]]$getLeafGroups(dgs)
       }
     }
     private$p_parentPivot$message("DataGroup$getDescendantGroups", "Got descendant groups", list(count=length(dgs)))
     return(invisible(dgs))
   },
   getLeafGroups = function(leafGroups=NULL) {
     checkArgument("PivotDataGroup", "getLeafGroups", leafGroups, missing(leafGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_parentPivot$message("DataGroup$getLeafGroups", "Getting leaf groups...", list(leafGroupCount=length(leafGroups)))
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
     private$p_parentPivot$message("DataGroup$getLeafGroups", "Got leaf groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   getLevelGroups = function(level=NULL, levelGroups=NULL) { #level=0 is the current data group
     checkArgument("PivotDataGroup", "getLevelGroups", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotDataGroup", "getLevelGroups", levelGroups, missing(levelGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotDataGroup")
     private$p_parentPivot$message("DataGroup$getLevelGroups", "Getting level groups...",
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
     private$p_parentPivot$message("DataGroup$getLevelGroups", "Got level groups", list(count=length(lgs)))
     return(invisible(lgs))
   },
   addChildGroup = function(variableName=NULL, values=NULL,
                            caption=NULL, isTotal=FALSE,
                            calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotDataGroup", "addChildGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addChildGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     checkArgument("PivotDataGroup", "addChildGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
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
     private$p_parentPivot$message("DataGroup$addDataGroups", "Adding data groups...",
                                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                        onlyCombinationsThatExist=onlyCombinationsThatExist,
                                        explicitListOfValues=explicitListOfValues, calculationGroupName=calculationGroupName,
                                        expandExistingTotals=expandExistingTotals, addTotal=addTotal, visualTotals=visualTotals,
                                        totalPosition=totalPosition, totalCaption=totalCaption))
     private$p_parentPivot$resetCells()
     if(missing(variableName)||is.null(variableName)) stop("DataGroup$addDataGroups(): variableName must be specified", call. = FALSE)
     if(addTotal==TRUE){ private$p_visualTotals <- visualTotals }
     df <- NULL
     topLevelDisinctValues <- NULL
     topLevelCaptions <- NULL
     topLevelFilter <- NULL
     if(fromData==TRUE) {
       # check that a data frame has been specified (or that we have a default data frame)
       if(missing(dataName)||is.null(dataName)) {
         if (private$p_parentPivot$data$count < 1) stop("DataGroup$addDataGroups():  No data frames.  Specify data before calling addLeafGroup.", call. = FALSE)
         df <- private$p_parentPivot$data$defaultData
       }
       else {
         df <- private$p_parentPivot$data$getData(dataName)
         if(is.null(df)) stop(paste0("DataGroup$addDataGroups():  No data frame found in PivotTable with name '", dataName, "'."), call. = FALSE)
       }
     }
     else {
       if (missing(explicitListOfValues)) stop("DataGroup$addDataGroups():  An explicitListOfValues must be specified when fromData=FALSE", call. = FALSE)
       if (is.null(explicitListOfValues)) stop("DataGroup$addDataGroups():  explicitListOfValues must not be null when fromData=FALSE", call. = FALSE)
       topLevelDisinctValues <- explicitListOfValues
       topLevelCaptions <- names(explicitListOfValues)
       fvals <- topLevelDisinctValues
       if("list" %in% class(topLevelDisinctValues)) { fvals <- unlist(topLevelDisinctValues) }
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=fvals)
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
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=topLevelDisinctValues)
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
     if(is.null(topLevelFilter))
       topLevelFilter <- PivotFilter$new(parentPivot=private$p_parentPivot, variableName=variableName, values=NULL)
     for(i in 1:length(parentGroups))
     {
       grp <- parentGroups[[i]]
       if((grp$isTotal==TRUE)&&(expandExistingTotals==FALSE)) next

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
         if(dataSortOrder=="asc") eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
         else if(dataSortOrder=="desc") eval(parse(text=paste0("data <- dplyr::arrange(data, desc(", variableName, "))")))
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
         if((addTotal==TRUE)&&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           caption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) caption <- distinctCaptions[j]
           if(is.null(caption)&&(!is.null(dataFormat))) caption <- private$formatValue(distinctValues[[j]], dataFormat)
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[[j]], caption=caption,
                                       calculationGroupName=calculationGroupName, isTotal=self$isTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
       }
       else {
         if((addTotal==TRUE)&&(totalPosition=="before")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         for(j in 1:length(distinctValues)) {
           caption <- NULL
           if((!is.null(distinctCaptions))&&(nchar(distinctCaptions[j])>0)) caption <- distinctCaptions[j]
           if(is.null(caption)&&(!is.null(dataFormat))) caption <- private$formatValue(distinctValues[j], dataFormat)
           newGrp <- grp$addChildGroup(variableName=variableName, values=distinctValues[j], caption=caption,
                                       calculationGroupName=calculationGroupName, isTotal=self$isTotal)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
         }
         if((addTotal==TRUE)&&(totalPosition=="after")) {
           newGrp <- grp$addChildGroup(caption=totalCaption, calculationGroupName=calculationGroupName, isTotal=TRUE)
           index <- length(newGroups) + 1
           newGroups[[index]] <- newGrp
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
     private$p_parentPivot$message("DataGroup$addDataGroups", "Added groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
   },
   sortDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
     checkArgument("DataGroup", "sortDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("DataGroup", "sortDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
     checkArgument("DataGroup", "sortDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
     checkArgument("DataGroup", "sortDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
     checkArgument("DataGroup", "sortDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("DataGroup$sortDataGroups", "Sorting data groups...",
                                   list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                                        calculationGroupName=calculationGroupName, calculationName=calculationName))
     private$p_parentPivot$resetCells()
     if(is.null(private$p_groups)) return(invisible())
     if(length(private$p_groups)==0) return(invisible())
     calculationGroup <- NULL
     calculation <- NULL
     if(orderBy=="calculation") {
       if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
         stop(paste0("DataGroup$sortDataGroups():  There is no Calculation Group named '", calculationGroupName, "'"), call. = FALSE)
       calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
       if(is.null(calculationName)) {
         calculationName <- calculationGroup$defaultCalculationName
         if(is.null(calculationName))
           stop(paste0("DataGroup$sortDataGroups():  No calculation has been specified and there is no default calculation."), call. = FALSE)
       }
       if(!calculationGroup$isExistingCalculation(calculationName))
         stop(paste0("DataGroup$sortDataGroups():  There is no Calculation named '", calculationName , "' in group '", calculationGroupName, "'"), call. = FALSE)
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
         if(sortOrder=="asc") sortIndexes <- order(unlist(values))
         else sortIndexes <- order(unlist(values), decreasing=TRUE)
         j <- 0
         for(i in 1:length(private$p_groups)) {
           if(private$p_groups[[i]]$isTotal==TRUE) next
           j <- j + 1
           private$p_groups[[i]] <- groups[[sortIndexes[j]]]
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
         if(sortOrder=="asc") sortIndexes <- order(unlist(captions))
         else sortIndexes <- order(unlist(captions), decreasing=TRUE)
         j <- 0
         for(i in 1:length(private$p_groups)) {
           if(private$p_groups[[i]]$isTotal==TRUE) next
           j <- j + 1
           private$p_groups[[i]] <- groups[[sortIndexes[j]]]
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
               netFilters$setFilter(filter, action="replace")
             }
           }
           # calculate the value
           results <- pivotCalculator$evaluateNamedCalculation(calculationName=calculationName,
                                                               calculationGroupName=calculationGroupName,
                                                               rowColFilters=netFilters, cell=NULL)
           calcResults <- results[[calculationName]]
           rawValues[[j]] <- calcResults$rawValue
         }
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
     else {
       # sort below this level
       for(i in 1:length(private$p_groups)) {
         private$p_groups[[i]]$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                              calculationGroupName=calculationGroupName, calculationName=calculationName)
       }
     }
     private$p_parentPivot$message("DataGroup$sortDataGroups", "Sorted data groups.")
     return(invisible())
   },
   addCalculationGroups = function(calculationGroupName=NULL, atLevel=NULL) {
     checkArgument("PivotDataGroup", "addCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotDataGroup", "addCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     private$p_parentPivot$message("DataGroup$addCalculationGroups", "Adding calculation groups...")
     private$p_parentPivot$resetCells()
     private$p_parentPivot$calculationsPosition <- private$p_rowOrColumn # will throw an error if trying to add calcs to one axis when already present on the other axis
     if(missing(calculationGroupName)) stop("DataGroup$addCalculationGroups():  calculationGroupName must be specified.", call. = FALSE)
     if(is.null(calculationGroupName)) stop("DataGroup$addCalculationGroups():  calculationGroupName cannot be null.", call. = FALSE)
     if(!private$p_parentPivot$calculationGroups$isExistingCalculationGroup(calculationGroupName))
       stop(paste0("DataGroup$addCalculationGroups():  There is no Calculation Group named '", calculationGroupName, "' in the Pivot Table."), call. = FALSE)
     # get the calculation group and the calculations to be displayed
     calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(calculationGroup$count==0)
       stop(paste0("DataGroup$addCalculationGroups():  There are no calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
     calculations <- list()
     for (i in 1:calculationGroup$count) {
       calc <- calculationGroup$calculations[[i]]
       if(calc$visible==TRUE) {
         cname <- paste0("calc", calc$displayOrder, "-", i)
         calculations[[cname]] <- calc
       }
     }
     if(length(calculations)==0)
       stop(paste0("DataGroup$addCalculationGroups():  There are no visible calculations in the calculation group '", calculationGroupName, "'"), call. = FALSE)
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
     private$p_parentPivot$message("DataGroup$addCalculationGroups", "Added calculation groups.", list(count=length(newGroups)))
     return(invisible(newGroups))
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
     return(invisible(maxParents))
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
     return(invisible())
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
       calculationGroupName = private$p_calculationGroupName,
       calculationName = private$p_calculationName,
       rowColumnNumber = private$p_rowColumnNumber,
       isRendered = private$p_isRendered,
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
         stop(paste0("DataGroup$calculationGroupName(): The Calculation Group '", value, "' does not exist in the Pivot Table."), call. = FALSE)
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
         stop("DataGroup$calculationName(): Specify the Calculation Group before the Calculation.", call. = FALSE)
       calculationGroup <- private$p_parentPivot$calculationGroups$getCalculationGroup(private$p_calculationGroupName)
       if(!(calculationGroup$isExistingCalculation(value))) {
         stop(paste0("DataGroup$calculationName(): The Calculation '", value,
                     "' does not exist in the Calculation Group '", private$p_calculationGroupName, "'"), call. = FALSE)
       }
       private$p_calculationName <- value
       return(invisible())
     }
   },
   isTotal = function(value) { return(invisible(private$p_isTotal)) },
   caption = function(value) {
     if(is.null(private$p_caption)) {
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
         if(!is.integer(value)) stop("DataGroup$rowColumnNumber(): rowColumnNumber must be an integer", call. = FALSE)
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
   isRendered = function(value) {
     if(missing(value)) { return(invisible(private$p_isRendered)) }
     else {
       checkArgument("PivotDataGroup", "isRendered", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       private$p_isRendered <- value
       return(invisible())
     }
   }
  ),
  private = list(
   p_parentGroup = NULL,
   p_parentPivot = NULL,
   p_rowOrColumn = NULL,
   p_caption = NULL,
   p_sortValue = NULL,
   p_isTotal = NULL,
   p_visualTotals = NULL,
   p_filters = NULL,
   p_groups = NULL,
   p_calculationGroupName = NULL,
   p_calculationName = NULL,
   p_rowColumnNumber = NULL,
   p_baseStyleName = NULL,
   p_style = NULL,
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
