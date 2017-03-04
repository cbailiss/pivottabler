PivotTable <- R6::R6Class("PivotTable",
  public = list(
    initialize = function(messages=FALSE, messageFile=NULL) {
      checkArgument("PivotTable", "initialize", messages, missing(messages), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "initialize", messageFile, missing(messageFile), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_messages <- messages
      if(messages&(!is.null(messageFile))) {
        private$p_messageFile <- file(messageFile, open="w")
      }
      self$message("PivotTable$new", "Creating new Pivot Table...")
      private$p_data <- PivotData$new(parentPivot=self)
      private$p_styles <- getTheme(parentPivot=self, themeName="default")
      private$p_rowGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="row")
      private$p_columnGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="column")
      private$p_calculationsPosition <- NULL
      private$p_calculationGroups <- PivotCalculationGroups$new(parentPivot=self)
      private$p_renderer <- PivotHtmlRenderer$new(parentPivot=self)
      self$message("PivotTable$new", "Created new Pivot Table.")
      return(invisible())
    },
    addData = function(name, df) {
      checkArgument("PivotTable", "addData", name, missing(name), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addData", df, missing(df), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
      self$message("PivotTable$addData", "Adding data to Pivot Table...")
      private$p_data$addData(name, df)
      self$message("PivotTable$addData", "Added data to Pivot Table.")
      return(invisible(private$p_data))
    },
    getTopColumnGroups = function() {
      self$message("PivotTable$getTopColumnGroups", "Getting top level column groups...")
      grps <- private$p_columnGroup$getChildGroups()
      self$message("PivotTable$getTopColumnGroups", "Got top level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafColumnGroups = function() {
      self$message("PivotTable$getLeafColumnGroups", "Getting leaf level column groups...")
      leafGroups = list()
      grps <- private$p_columnGroup$getLeafGroups(leafGroups)
      self$message("PivotTable$getTopColumnGroups", "Got leaf level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addTopColumnGroup = function(variableName=NULL, values=NULL, caption=NULL) {
      checkArgument("PivotTable", "addTopColumnGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addTopColumnGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      checkArgument("PivotTable", "addTopColumnGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$addTopColumnGroup", "Adding top level column group...",
                   list(variableName=variableName, values=values, caption=caption))
      self$resetCells()
      grps <- private$p_columnGroup$addChildGroup(variableName=variableName, values=values, caption=caption)
      self$message("PivotTable$addTopColumnGroup", "Added top level column group.")
      return(invisible(grps))
    },
    addLeafColumnDataGroup = function(variableName=NULL, dataName=NULL, fromData=TRUE,
                                  leafLevelPermutations=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                                  expandExistingTotals=FALSE,
                                  addTotal=TRUE, visualTotals=FALSE,totalPosition="after", totalCaption="Total") {
      checkArgument("PivotTable", "addLeafColumnDataGroup", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addLeafColumnDataGroup", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addLeafColumnDataGroup", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addLeafColumnDataGroup", leafLevelPermutations, missing(leafLevelPermutations), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addLeafColumnDataGroup", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      checkArgument("PivotTable", "addLeafColumnDataGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addLeafColumnDataGroup", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addLeafColumnDataGroup", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addLeafColumnDataGroup", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addLeafColumnDataGroup", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
      checkArgument("PivotTable", "addLeafColumnDataGroup", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addLeafColumnDataGroup", "Adding leaf level column group...",
                   list(variableName=variableName, dataName=dataName,
                        leafLevelPermutations=leafLevelPermutations, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                        visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption))
      self$resetCells()
      grp <- private$p_columnGroup$addLeafDataGroup(variableName=variableName, dataName=dataName, fromData=fromData,
                                                leafLevelPermutations=leafLevelPermutations, explicitListOfValues=explicitListOfValues,
                                                calculationGroupName=calculationGroupName,
                                                expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                                visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption)
      self$message("PivotTable$addLeafColumnDataGroup", "Added leaf level column group.")
      return(invisible(grp))
    },
    normaliseColumnGroups = function() {
      self$message("PivotTable$normaliseColumnGroups", "Normalising column groups...")
      self$resetCells()
      groupsAdded <- private$p_columnGroup$normaliseDataGroup()
      self$message("PivotTable$normaliseColumnGroups", "Normalised column groups.", list(groupsAdded = groupsAdded))
      return(invisible())
    },
    getTopRowGroups = function() {
      self$message("PivotTable$getTopRowGroups", "Getting top level row groups...")
      grps <- private$p_rowGroup$getChildGroups()
      self$message("PivotTable$getTopRowGroups", "Got top level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafRowGroups = function() {
      self$message("PivotTable$getLeafRowGroups", "Getting leaf level row groups...")
      leafGroups = list()
      grps <- private$p_rowGroup$getLeafGroups(leafGroups)
      self$message("PivotTable$getTopRowGroups", "Got leaf level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addTopRowGroup = function(variableName=NULL, values=NULL, caption=NULL) {
      checkArgument("PivotTable", "addTopRowGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addTopRowGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      checkArgument("PivotTable", "addTopRowGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$resetCells()
      self$message("PivotTable$addTopRowGroup", "Adding top level row group...",
                   list(variableName=variableName, values=values, caption=caption))
      grp <- private$p_rowGroup$addChildGroup(variableName=variableName, values=values, caption=caption)
      self$message("PivotTable$addTopRowGroup", "Added top level row group.")
      return(invisible(grp))
    },
    addLeafRowDataGroup = function(variableName=NULL, dataName=NULL, fromData=TRUE,
                                    leafLevelPermutations=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                                    expandExistingTotals=FALSE,
                                    addTotal=TRUE, visualTotals=FALSE,totalPosition="after", totalCaption="Total") {
     checkArgument("PivotTable", "addLeafRowDataGroup", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotTable", "addLeafRowDataGroup", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotTable", "addLeafRowDataGroup", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addLeafRowDataGroup", leafLevelPermutations, missing(leafLevelPermutations), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addLeafRowDataGroup", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("PivotTable", "addLeafRowDataGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotTable", "addLeafRowDataGroup", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addLeafRowDataGroup", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addLeafRowDataGroup", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addLeafRowDataGroup", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
     checkArgument("PivotTable", "addLeafRowDataGroup", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addLeafRowDataGroup", "Adding leaf level row group...",
                   list(variableName=variableName, dataName=dataName,
                        leafLevelPermutations=leafLevelPermutations, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                        visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption))
      self$resetCells()
      grps <- private$p_rowGroup$addLeafDataGroup(variableName=variableName, dataName=dataName, fromData=fromData,
                                                leafLevelPermutations=leafLevelPermutations, explicitListOfValues=explicitListOfValues,
                                                calculationGroupName=calculationGroupName,
                                                expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                                visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption)
      self$message("PivotTable$addLeafRowDataGroup", "Added leaf level row group.")
      return(invisible(grps))
    },
    normaliseRowGroups = function() {
      self$message("PivotTable$normaliseRowGroups", "Normalising row groups...")
      self$resetCells()
      groupsAdded <- private$p_rowGroup$normaliseDataGroup()
      self$message("PivotTable$normaliseRowGroups", "Normalised row groups.", list(groupsAdded = groupsAdded))
      return(invisible())
    },
    addCalculationGroup = function(calculationGroupName=NULL) {
      checkArgument("PivotTable", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addCalculationGroup", "Adding calculation group...", list(calculationGroupName=calculationGroupName))
      self$resetCells()
      calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      self$message("PivotTable$addCalculationGroup", "Added calculation group.")
      return(invisible(calculationGroup))
    },
    defineCalculation = function(calculationGroupName="default", calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, basedOn=NULL) {
      checkArgument("PivotTable", "defineCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
      checkArgument("PivotTable", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("function", "character"))
      checkArgument("PivotTable", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
      checkArgument("PivotTable", "defineCalculation", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
      checkArgument("PivotTable", "defineCalculation", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      fstr <- NULL
      if(!is.null(filters)) {
        fstr <- filters$asString()
      }
      self$message("PivotTable$defineCalculation", "Defining calculation...",
                   list(calculationGroupName=calculationGroupName, calculationName=calculationName, caption=caption,
                        visible=visible, displayOrder=displayOrder, filters=fstr, format=format, dataName=dataName,
                        type=type, valueName=valueName, summariseExpression=summariseExpression,
                        calculationExpression=calculationExpression, calculationFunction=calculationFunction, basedOn=basedOn))
      self$resetCells()
      calculationGroupExists <- private$p_calculationGroups$isExistingCalculationGroup(calculationGroupName)
      if(calculationGroupExists) {
        calculationGroup <- private$p_calculationGroups$getCalculationGroup(calculationGroupName)
      }
      else {
        calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      }
      calculation <- calculationGroup$defineCalculation(calculationName=calculationName, caption=caption, visible=visible,
                         displayOrder=displayOrder, filters=filters, format=format, dataName=dataName,
                         type=type, valueName=valueName, summariseExpression=summariseExpression,
                         calculationExpression=calculationExpression, calculationFunction=calculationFunction, basedOn=basedOn)
      self$message("PivotTable$defineCalculation", "Defined calculation.")
      return(invisible(calculation))
    },
    addLeafColumnCalculationGroup = function(calculationGroupName="default") {
      checkArgument("PivotTable", "addLeafColumnCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addLeafColumnCalculationGroup", "Adding leaf level column calculation group...",
                   list(calculationGroupName=calculationGroupName))
      self$resetCells()
      grps <- private$p_columnGroup$addLeafCalculationGroup(calculationGroupName=calculationGroupName)
      self$message("PivotTable$addLeafColumnCalculationGroup", "Added leaf level column calculation group.")
      return(invisible(grps))
    },
    addLeafRowCalculationGroup = function(calculationGroupName="default") {
      checkArgument("PivotTable", "addLeafRowCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addLeafRowCalculationGroup", "Adding leaf level row calculation group...",
                   list(calculationGroupName=calculationGroupName))
      self$resetCells()
      grps <- private$p_rowGroup$addLeafCalculationGroup(calculationGroupName=calculationGroupName)
      self$message("PivotTable$addLeafRowCalculationGroup", "Added leaf level row calculation group.")
      return(invisible(grps))
    },
    addStyle = function(styleName=NULL, declarations=NULL) {
      checkArgument("PivotTable", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      self$message("PivotTable$addStyle", "Adding style...", list(styleName=styleName))
      style <- private$p_styles$addStyle(styleName=styleName, declarations=declarations)
      self$message("PivotTable$addStyle", "Added style.")
      return(invisible(style))
    },
    createInlineStyle = function(baseStyleName=NULL, declarations=NULL) {
      checkArgument("PivotTable", "createInlineStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      self$message("PivotTable$createInlineStyle", "Creating inline style...")
      if(is.null(baseStyleName)) {
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=declarations)
      }
      else {
        baseStyle <- private$p_styles$getStyle(styleName=baseStyleName)
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=baseStyle$declarations)
        style$setPropertyValues(declarations=declarations)
      }
      self$message("PivotTable$createInlineStyle", "Created inline style.")
      return(invisible(style))
    },
    generateCellStructure = function() {
      self$message("PivotTable$generateCellStructure", "Generating cell structure...")
      # clear any existing PivotCells
      private$p_cells <- NULL
      # clear rowColumn numbers on both axes
      rowGrps <- private$p_rowGroup$getDescendantGroups(descendants=NULL, includeCurrentGroup=TRUE)
      for(i in 1:length(rowGrps)) {
        rowGrps[[i]]$rowColumnNumber <- NULL
      }
      columnGrps <- private$p_columnGroup$getDescendantGroups(descendants=NULL, includeCurrentGroup=TRUE)
      for(i in 1:length(columnGrps)) {
        columnGrps[[i]]$rowColumnNumber <- NULL
      }
      # get the leaf levels on both axes
      rowGrps <- private$p_rowGroup$getLeafGroups(leafGroups=NULL)
      columnGrps <- private$p_columnGroup$getLeafGroups(leafGroups=NULL)
      rowCount <- length(rowGrps)
      columnCount <- length(columnGrps)
      # determine if there is a default calculation (i.e. if we have a default group with only one visible calculation)
      defaultCalculationFound <- FALSE
      defaultCalculationGroupName <- NULL
      defaultCalculationName <- NULL
      if(!is.null(self$calculationGroups$defaultGroup)) {
        if(self$calculationGroups$defaultGroup$visibleCount==1) {
          defaultCalculationGroupName <- self$calculationGroups$defaultGroup$calculationGroupName
          defaultCalculationName <- self$calculationGroups$defaultGroup$visibleCalculations[[1]]$calculationName
          defaultCalculationFound <- TRUE
        }
      }
      # calculate filters and calculations for each heading group
      # net filters are calculated top-down
      # calculations are selected bottom-up (first one encountered is used)
      # rows...
      rowFilters <- list()
      rowCalculationGroupNames <- list()
      rowCalculationNames <- list()
      for(i in 1:rowCount) {
        # set the rowColumnNumber on the leaf cell
        rowGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- rowGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the parent filter settings using "replace" filter logic
        rowColFilters <- PivotFilters$new(self)
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
        rowFilters[[i]] <- rowColFilters
        # find the calculation
        calcFound <- FALSE
        for(j in 1:length(ancestors)) {
          acs <- ancestors[[j]]
          if(is.null(acs$calculationGroupName)) next
          if(is.null(acs$calculationName)) next
          rowCalculationGroupNames[[i]] <- acs$calculationGroupName
          rowCalculationNames[[i]] <- acs$calculationName
          calcFound <- TRUE
          break
        }
        if(calcFound==FALSE) {
          if(defaultCalculationFound==TRUE) {
            rowCalculationGroupNames[[i]] <- defaultCalculationGroupName
            rowCalculationNames[[i]] <- defaultCalculationName
          }
        }
      }
      # calculate the net filters at each position on the columns
      columnFilters <- list()
      columnCalculationGroupNames <- list()
      columnCalculationNames <- list()
      for(i in 1:columnCount) {
        # set the rowColumnNumber on the leaf cell
        columnGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- columnGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the parent filter settings using "replace" filter logic
        rowColFilters <- PivotFilters$new(self)
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
        columnFilters[[i]] <- rowColFilters
        # find the calculation
        calcFound <- FALSE
        for(j in 1:length(ancestors)) {
          acs <- ancestors[[j]]
          if(is.null(acs$calculationGroupName)) next
          if(is.null(acs$calculationName)) next
          columnCalculationGroupNames[[i]] <- acs$calculationGroupName
          columnCalculationNames[[i]] <- acs$calculationName
          calcFound <- TRUE
          break
        }
        if(calcFound==FALSE) {
          if(defaultCalculationFound==TRUE) {
            columnCalculationGroupNames[[i]] <- defaultCalculationGroupName
            columnCalculationNames[[i]] <- defaultCalculationName
          }
        }
      }
      # get the calculation position
      calculationsPosition <- private$p_calculationsPosition
      if(is.null(calculationsPosition)) { calculationsPosition <- "column" }
      if(!(calculationsPosition %in% c("row", "column")))
        stop("PivotTable$generateCellStructure(): calculationsPosition must be either row or column", call. = FALSE)
      # create and size the new PivotCells
      private$p_cells <- PivotCells$new(self, rowGroups=rowGrps, columnGroups=columnGrps)
      if(rowCount>0) {
        for(r in 1:rowCount) {
          if(columnCount>0) {
            for(c in 1:columnCount) {
              # calculate the net filters
              if(is.null(rowFilters[[r]])) {
                if(!is.null(columnFilters[[c]])) { rowColFilters <- columnFilters[[c]]$getCopy() }
              }
              else {
                rowColFilters <- rowFilters[[r]]$getCopy()
                if (!is.null(columnFilters[[c]])) { rowColFilters$setFilters(columnFilters[[c]]) } # column filters override row filters
              }
              # find the calculation
              calcGrpNme <- NULL
              calcNme <- NULL
              if(calculationsPosition=="row") {
                if(r <= length(rowCalculationGroupNames)) calcGrpNme <- rowCalculationGroupNames[[r]]
                else calcGrpNme <- NULL
                if(r <= length(rowCalculationNames)) calcNme <- rowCalculationNames[[r]]
                else calcNme<- NULL
              }
              else if(calculationsPosition=="column") {
                if(c <= length(columnCalculationGroupNames)) calcGrpNme <- columnCalculationGroupNames[[c]]
                else calcGrpNme<- NULL
                if(c <= length(columnCalculationNames))
                calcNme <- columnCalculationNames[[c]]
                else calcNme<- NULL
              }
              # create the cell
              cell <- PivotCell$new(self, rowNumber=as.integer(r), columnNumber=as.integer(c),
                                    calculationName=calcNme, calculationGroupName=calcGrpNme,
                                    rowColFilters=rowColFilters, rowFilters=rowFilters[[r]], columnFilters=columnFilters[[c]],
                                    rowLeafGroup=rowGrps[[r]], columnLeafGroup=columnGrps[[c]])
              private$p_cells$setCell(r=r, c=c, cell=cell)
            }
          }
        }
      }
      self$message("PivotTable$generateCellStructure", "Generated cell structure.")
      return(invisible(private$cells))
    },
    resetCells = function() {
      self$message("PivotTable$resetCells", "Resetting cells...")
      if(private$p_evaluated==TRUE){
        p_cells <- NULL
        private$p_evaluated <- FALSE
      }
      self$message("PivotTable$resetCells", "Reset cells.")
      return(invisible())
    },
    evaluateCells = function() {
      self$message("PivotTable$evaluateCells", "Evaluating cell values...")
      if(is.null(private$p_cells)) stop("PivotTable$evaluateCells():  No cells exist to calculate.", call. = FALSE)
      calculator <- PivotCalculator$new(self)
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          calculator$evaluateCell(cell)
        }
      }
      private$p_evaluated <- TRUE
      self$message("PivotTable$evaluateCells", "Evaluated cell values.")
      return(invisible())
    },
    evaluatePivot = function() {
      self$message("PivotTable$evaluatePivot", "Evaluating pivot table...")
      self$normaliseColumnGroups()
      self$normaliseRowGroups()
      self$generateCellStructure()
      self$evaluateCells()
      self$message("PivotTable$evaluatePivot", "Evaluated pivot table.")
      return(invisible())
    },
    getCss = function(styleNamePrefix=NULL) {
      checkArgument("PivotTable", "getCss", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$getCss", "Getting Styles...")
      if(is.null(private$p_styles)) return("")
      if(length(private$p_styles$styles)==0) return("")
      styles <- ""
      for(s in 1:length(private$p_styles$styles)) {
        style <- private$p_styles$styles[[s]]
        if(is.null(style)) next
        styles <- paste0(styles, style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix), "\r\n")
      }
      self$message("PivotTable$getCss", "Got Styles.")
      return(invisible(styles))
    },
    getHtml = function(styleNamePrefix=NULL, includeRCFilters=FALSE, includeCalculationFilters=FALSE,
                       includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "getHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "getHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$getHtml", "Getting HTML...")
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_renderer$getTableHtml(styleNamePrefix=styleNamePrefix,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      self$message("PivotTable$getHtml", "Got HTML.")
      return(invisible(htmlTable))
    },
    saveHtml = function(filePath=NULL, fullPageHTML=TRUE, styleNamePrefix=NULL, includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "saveHtml", filePath, missing(filePath), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "saveHtml", fullPageHTML, missing(fullPageHTML), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "saveHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$saveHtml", "Saving HTML...", list(filePath=filePath, fullPageHTML=fullPageHTML, styleNamePrefix=styleNamePrefix,
                                                                 includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                 includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue))
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # todo: enable rendering before cells are calculated so the structure of the pivot can be checked as it is being developed
      if(is.null(private$p_cells)) stop("PivotTable$saveHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_renderer$getTableHtml(styleNamePrefix=styleNamePrefix,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      if (fullPageHTML==FALSE) {
        fileConn <- file(filePath)
        writeLines(as.character(htmlTable), fileConn)
        close(fileConn)
        self$message("PivotTable$saveHtml", "Saved HTML.")
        return(invisible())
      }
      # basic css
      cssStr1 <- "<style>h1 { font: 2.5em arial; font-weight: bold; } p { font: 0.9em arial; }</style>"
      cssStr2 <- paste0("<style>", self$getCss(styleNamePrefix=styleNamePrefix), "</style>")
      #pgHtml <- htmltools::tags$html(htmltools::tags$head(htmltools::tags$title('R Pivot Table')), htmltools::HTML(cssStr),
      pgHtml <- htmltools::tags$html(htmltools::HTML("<head>"), htmltools::tags$title('R Pivot Table'), htmltools::HTML(cssStr1), htmltools::HTML(cssStr2), htmltools::HTML("</head>"),
                 htmltools::tags$body(
                   htmltools::h1("R Pivot Table"),
                   htmlTable,
                   htmltools::tags$br(),
                   htmltools::tags$p(paste0("Generated at ", format(Sys.time(), "%X on %a %d %b %Y")))
                 ))
      fileConn <- file(filePath)
      writeLines(as.character(pgHtml), fileConn)
      close(fileConn)
      self$message("PivotTable$saveHtml", "Saved HTML.")
      return(invisible())
    },
    renderPivot = function(width=NULL, height=NULL, styleNamePrefix=NULL,
                           includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "renderPivot", width, missing(width), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "renderPivot", height, missing(height), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "renderPivot", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "renderPivot", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$renderPivot", "Rendering htmlwidget...", list(width=width, height=height, styleNamePrefix=styleNamePrefix,
                                                                             includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                             includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue))
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # pivottabler(self, width=width, height=height, includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
      #                 includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      settings <- list() # may need this in the future
      widgetData <- list(
        tableCss = pt$getCss(styleNamePrefix=styleNamePrefix),
        tableHtml = as.character(pt$getHtml(styleNamePrefix=styleNamePrefix,
                                            includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                            includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)),
        settings = settings
      )
      # viewer.fill=TRUE and browser.fill=TRUE sound like they would be good things, but they seem to prevent
      # any scroll bars being shown when the HTML tables are larger than the RStudio Viewer or the web browser window size
      sp = htmlwidgets::sizingPolicy(
        viewer.padding=10, viewer.fill=FALSE, viewer.suppress=FALSE,
        browser.padding=10, browser.fill=FALSE
      )
      w <- htmlwidgets::createWidget("pivottabler", widgetData, width=width, height=height, sizingPolicy=sp)
      self$message("PivotTable$renderPivot", "Rendered htmlwidget.")
      return(w)
    },
    message = function(methodName, desc, detailList=NULL) {
      if(!private$p_messages) return()
      stackdepth <- length(sys.calls())
      indent <- strrep(" ", (stackdepth - 1) *2)
      msg <- paste0(indent, methodName, ":  ", desc)
      if(length(detailList)>0) {
        nms <- names(detailList)
        msg <- paste0(msg, ": ")
        for(i in 1:length(detailList)) {
          sep <- ""
          if(i > 1) { sep <- ", " }
          dtl <- NULL
          if("function" %in% class(detailList[[i]])) {
            dtl <- deparse(detailList[[i]])
          }
          else dtl <- detailList[[i]]
          msg <- paste0(msg, sep, nms[i], "=", dtl)
        }
      }
      if(is.null(private$p_messageFile)) { message(msg) }
      else { cat(msg, file=private$p_messageFile, sep="\r\n", append=TRUE)}
    },
    asList = function() {
      self$message("PivotTable$asList", "Getting list...")
      lst <- list(
        type = "pivotTable",
        dataFrames = private$p_data$asList(),
        rowGroup = private$p_rowGroup$asList(),
        columnGroup = private$p_columnGroup$asList(),
        calculationsPosition = private$p_calculationsPosition,
        calculationGroups = private$p_calculationGroups$asList(),
        cells = private$p_cells$asList()
      )
      self$message("PivotTable$asList", "Got list.")
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    viewJSON = function() {
      if (!requireNamespace("listviewer", quietly = TRUE)) {
        stop("PivotTable$asJSON():  The listviewer package is needed to view the internal structure of the PivotTable as JSON.  Please install it.", call. = FALSE)
      }
      listviewer::jsonedit(self$asList(), mode="code")
    },
    finalize = function() {
      if(!is.null(private$p_messageFile)) close(private$p_messageFile)
    }
  ),
  active = list(
    data = function(value) { return(private$p_data) },
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        checkArgument("PivotTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        private$p_styles <- getTheme(parentPivot=self, themeName=value)
        return(invisible())
      }
    },
    styles = function(value) {
      if(missing(value)) return(invisible(private$p_styles))
      else {
        checkArgument("PivotTable", "styles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyles")
        private$p_styles <- value
        return(invisible())
      }
    },
    allowExternalStyles = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(private$p_styles$allowExternalStyles)
      }
      else {
        checkArgument("PivotTable", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        private$p_styles$allowExternalStyles <- value
        return(invisible())
      }
    },
    rowGroup = function(value) { return(invisible(private$p_rowGroup ))},
    columnGroup = function(value) { return(invisible(private$p_columnGroup ))},
    calculationGroups = function(value) { return(invisible(private$p_calculationGroups)) },
    calculationsPosition = function(value) {
      if(missing(value)) { return(invisible(private$p_calculationsPosition)) }
      else {
        if(is.null(private$p_calculationsPosition)) { private$p_calculationsPosition <- value }
        else {
          if(!(value %in% c("row", "column")))
            stop("PivotTable$calculationsPosition(): calculationsPosition must be either row or column", call. = FALSE)
          if(private$p_calculationsPosition != value)
            stop(paste0("PivotTable$calculationsPosition():  Calculations position already set to be '",
                        private$p_calculationsPosition, "' and cannot be changed."), call. = FALSE)
          return(invisible())
        }
      }
    },
    cells = function(value) { return(invisible(private$p_cells ))},
    messages = function(value) {
      if(missing(value)) return(invisible(private$p_messages))
      else {
        if(is.logical(value)) private$p_messages <- value
        else stop("PivotTable$messages: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    }
  ),
  private = list(
    p_data = NULL,
    p_styles = NULL,
    p_rowGroup = NULL,
    p_columnGroup = NULL,
    p_calculationsPosition = NULL,
    p_calculationGroups = NULL,
    p_evaluated = FALSE,
    p_cells = NULL,
    p_renderer = NULL,
    p_messages = FALSE,
    p_messageFile = NULL
  )
)
