# PivotCalculationGroups = all of the calculation definitions in the PivotTable
# PivotCalculationGroup = a set of PivotCalculation objects
#     name - this name is referred to in the data groups
# PivotCalculation...
#     name (not displayed, used to allow one calculation to refer to others)
#     caption (displayed)
#     visible (TRUE/FALSE)
#     filters (to override filters from column headings)
#     format string (tbc)
#     dataName (the data frame the calculation is based on)
#     type = summarise (a dplyr summarise expression), calc (to derive one from others), custom (a custom function, to support ASRs)
#            for custom, define a function with parameters:  PivotData, cellFilters, rowFilters, colFilters, rowGroups, colGroups

# PivotCalculator - a class with helper methods to simplify calculating values
# methods:
# evaluateCalculationGroup(...)
# evaluateSummariseExpression(dataFrame) # don't forget calculation filter
# evaluateCalculationExpression(dataFrame, namedResults) # don't forget calculation filter
# evaluateCalculationFunction(dataFrame)  # don't forget calculation filter
# make copies of the filter objects before passing them to the function (to avoid changes to a filter permeating across the grid)
# evaluateCell(cell)

# this class is designed to be a helper class for users to interact with when running calculations, especially
# inside custom functions

PivotCalculator <- R6::R6Class("PivotCalculator",
  public = list(
   initialize = function(parentPivot=NULL) {
     checkArgument("PivotCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotCalculator$new", "Creating new Pivot Calculator...")
     private$p_parentPivot$message("PivotCalculator$new", "Created new Pivot Calculator.")
   },
   getDataFrame = function(dataName=NULL) {
     checkArgument("PivotCalculator", "getDataFrame", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     df <- private$p_parentPivot$data$getData(dataName)
     private$p_parentPivot$message("PivotCalculator$getDataFrame", "Got data frame.")
     return(df)
   },
   getCalculationGroup = function(calculationGroupName=NULL) {
     checkArgument("PivotCalculator", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getCalculationGroup", "Getting calculation group...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     private$p_parentPivot$message("PivotCalculator$getCalculationGroup", "Got calculation group.")
     return(cg)
   },
   getCalculation = function(calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotCalculator", "getCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "getCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getCalculation", "Getting calculation...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     cn <- cg$getCalculation(calculationName)
     private$p_parentPivot$message("PivotCalculator$getCalculation", "Got calculation.")
     return(cg)
   },
   newFilter = function(variableName=NULL, values=NULL) {
     checkArgument("PivotCalculator", "newFilter", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "newFilter", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$newFilter", "Creating filter...")
     filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, values=values)
     private$p_parentPivot$message("PivotCalculator$newFilter", "Created filter.")
     return(filter)
   },
   newFilters = function(variableName=NULL, values=NULL) {
     checkArgument("PivotCalculator", "newFilters", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "newFilters", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$newFilters", "Creating filters...")
     filters <- PivotFilters$new(private$p_parentPivot, variableName=variableName, values=values)
     private$p_parentPivot$message("PivotCalculator$newFilters", "Created filters.")
     return(filters)
   },
   setFilters = function(filters1=NULL, filters2=NULL, action="replace") { # filters2 overrides filters1
     checkArgument("PivotCalculator", "setFilters", filters1, missing(filters1), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilters", filters2, missing(filters2), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     private$p_parentPivot$message("PivotCalculator$setFilters", "Setting filters...")
     checkArgument("PivotCalculator", "setFilters", filters1, missing(filters1), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilters", filters2, missing(filters2), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     copy <- filters1$getCopy() # always copy, to avoid inadvertant bugs of a change to one filter affecting multiple cells in the Pivot Table
     copy$setFilters(filters=filters2, action=action)
     private$p_parentPivot$message("PivotCalculator$setFilters", "Set filters.")
     return(copy)
   },
   setFilter = function(filters=NULL, filter=NULL, action="replace") {
     checkArgument("PivotCalculator", "setFilter", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     private$p_parentPivot$message("PivotCalculator$setFilter", "Setting filter...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilter(filter=filter, action=action)
     private$p_parentPivot$message("PivotCalculator$setFilter", "Set filter.")
     return(copy)
   },
   setFilterValues = function(filters=NULL, variableName=NULL, values=NULL, action="replace") {
     checkArgument("PivotCalculator", "setFilterValues", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$setFilterValues", "Setting filter values...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilterValues(variableName=variableName, values=values, action=action)
     private$p_parentPivot$message("PivotCalculator$setFilterValues", "Set filter values.")
     return(copy)
   },
   getFilteredDataFrame = function(dataFrame=NULL, filters=NULL) {
     checkArgument("PivotCalculator", "getFilteredDataFrame", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "getFilteredDataFrame", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     private$p_parentPivot$message("PivotCalculator$getFilteredDataFrame", "Getting filtered data frame...")
     # build a dplyr query
     data <- dataFrame
     # todo: checking the escaping of the variable names and values below
     # todo: build up the criteria string to include all variables and values in one go, so only
     # need to invoke one filter call (i.e. loop to create filterCmd, not to execute filter)
     # filterCmd can be e.g. "data <- filter(data, (", filters[1]$variableName, "== filters[1]$values) & (", filters[2]$variableName, "== filters[2]$values)"
     # implement the same optimisation inside addLeafDataGroup function - change that code to call this function
     if (filters$count > 0)
     {
       for(j in 1:length(filters$filters)) {
         filter <- filters$filters[[j]]
         if(is.null(filter$variableName)) stop("PivotCalculator$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
         if(is.null(filter$values)) next
         if(length(filter$values)==0) next
         if(length(filter$values)==1) {
           filterCmd <- paste0("data <- dplyr::filter(data, ", filter$variableName, "== filter$values)")
         }
         else if(length(filter$values)>1) {
           filterCmd <- paste0("data <- dplyr::filter(data, ", filter$variableName, "%in% filter$values)")
         }
         # using eval repeatedly with the command above is not very efficient
         # but it avoids issues with values as strings, escaping, using stringi::stri_escape_unicode, etc
         eval(parse(text=filterCmd))
       }
     }
     private$p_parentPivot$message("PivotCalculator$getFilteredDataFrame", "Got filtered data frame.")
     return(data)
   },
   getDistinctValues = function(dataFrame=NULL, variableName=NULL) {
     checkArgument("PivotCalculator", "getDistinctValues", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "getDistinctValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getDistinctValues", "Getting filtered data frame...")
     # build a dplyr query
     data <- dataFrame
     eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
     data <- dplyr::distinct(data)
     eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
     distinctValues <- dplyr::collect(data)[[variableName]]
     private$p_parentPivot$message("PivotCalculator$getDistinctValues", "Got filtered data frame.")
     return(distinctValues)
   },
   formatValue = function(value=NULL, format=NULL) {
     checkArgument("PivotCalculator", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCalculator", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$formatValue", "Formatting value...")
     if(is.null(value)) return(NULL)
     if(is.null(format)) return(value)
     value <- sprintf(format, value)
     private$p_parentPivot$message("PivotCalculator$formatValue", "Formated value.")
     return(value)
   },
   getSummaryValue = function(dataFrame=NULL, summaryName=NULL, summariseExpression=NULL) {
     checkArgument("PivotCalculator", "getSummaryValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "getSummaryValue", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "getSummaryValue", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getSummaryValue", "Getting summary value...")
     # todo: escaping below
     summaryCmd <- paste0("data <- dplyr::summarise(dataFrame, ", summaryName, " = ", summariseExpression, ")")
     eval(parse(text=summaryCmd))
     if((nrow(data)>1)|(ncol(data)>1))
       stop(paste0("PivotCalculator$getSummaryValue(): Summary expression '", summaryName, "' has resulted in '", nrow(data),
                   " row(s) and ", ncol(data), " columns.  There must be a maximum of 1 row and 1 column in the result."))
     data <- dplyr::collect(data)
     if("tbl_df" %in% class(data)) data <- as.data.frame(data) # workaround of a possible bug in dplyr? collect seems to still sometimes return a tbl_df
     if("tbl_df" %in% class(data)) {
       stop(paste0("PivotCalculator$getSummaryValue(): Unable to coerce the tbl_df back to a data.frame for summary epxression '", summaryName, "'.",
                   "  This has resulted in a value of data type [", class(data), "] with ", nrow(data), " row(s) and ", ncol(data), " columns."))
     }
     value <- data[1, 1]
     private$p_parentPivot$message("PivotCalculator$getSummaryValue", "Got summary value.")
     return(value)
   },
   evaluateSummariseExpression = function(dataFrame=NULL, rowColFilters=NULL, calcFilters=NULL,
                                          summaryName=NULL, summariseExpression=NULL, format=NULL) {
     checkArgument("PivotCalculator", "evaluateSummariseExpression", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$evaluateSummariseExpression", "Evaluating summary expression...")
     data <- dataFrame
     # get the final filter context for this calculation (calcFilters override row/col filters)
     filters <- NULL
     value <- list()
     if(is.null(rowColFilters)) {
       if(!isnull(calcFilters)) filters <- calcFilters$getCopy()
     }
     else {
       filters <- rowColFilters$getCopy()
       if(!is.null(calcFilters)) { filters <- filters$setFilters(filters=calcFilters) }
     }
     # if we have some filters, filter the data frame
     if(!is.null(filters)) {
       value$filters <- filters
       if(filters$count > 0) {
         data <- self$getFilteredDataFrame(dataFrame=data, filters=filters)
       }
     }
     # calculate the value
     rv <- self$getSummaryValue(dataFrame=data, summaryName=summaryName, summariseExpression=summariseExpression)
     value$rawValue <- rv
     value$formattedValue <- self$formatValue(rv, format=format)
     private$p_parentPivot$message("PivotCalculator$evaluateSummariseExpression", "Evaluated summary expression.")
     return(value)
   },
   evaluateCalculationExpression = function(values=NULL, calculationExpression=NULL, format=NULL) {
     checkArgument("PivotCalculator", "evaluateCalculationExpression", values, missing(values), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
     checkArgument("PivotCalculator", "evaluateCalculationExpression", calculationExpression, missing(calculationExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateCalculationExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$evaluateSCalculationExpression", "Evaluating summary expression...")
     rv <- eval(parse(text=calculationExpression))
     value <- list()
     value$rawValue <- rv
     value$formattedValue <- self$formatValue(rv, format=format)
     private$p_parentPivot$message("PivotCalculator$evaluateSCalculationExpression", "Evaluated summary expression.")
     return(value)
   },
   evaluateCalculationGroup = function(calculationGroupName=NULL, rowColFilters=NULL) {
     checkArgument("PivotCalculator", "evaluateCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateCalculationGroup", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     private$p_parentPivot$message("PivotCalculator$evaluateCalculationGroup", "Evaluating calculation group...")
     # get the calculation group
     calcGrp <- self$getCalculationGroup(calculationGroupName)
     calcs <- calcGrp$calculations
     # build a list of the calculations to be computed, in the order of evaluation
     sorter <- function(calc) { return(calc$executionOrder) }
     execOrder <- calcs[order(unlist(lapply(calcs, sorter)))]
     # execute the calculations
     results <- list() # each item in the list is a list of three items (rawValue, formattedValue, filters)
     for(i in 1:length(execOrder)) {
       calc <- execOrder[[i]]
       if(calc$type=="summary") {
         df <- self$getDataFrame(calc$dataName)
         value <- self$evaluateSummariseExpression(dataFrame=df, rowColFilters=rowColFilters, calcFilters=calc$filters,
                                              summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                              format=calc$format)
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="calculation") {
         values <- list()
         for(j in 1:(i-1)) {
           values[[names(results)[[j]]]] <- results[[j]]$rawValue
         }
         value <- self$evaluateCalculationExpression(values=values, calculationExpression=calc$calculationExpression, format=calc$format)
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="function") {
         stop("PivotCalculator$evaluateCalculationGroup():  todo: support type='function'", call. = FALSE)
       }
       else stop(paste0("PivotCalculator$evaluateCalculationGroup():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     private$p_parentPivot$message("PivotCalculator$evaluateCalculationGroup", "Evaluated calculation group.")
     return(results)
   },
   evaluateCell = function(cell) {
     checkArgument("PivotCalculator", "evaluateCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     private$p_parentPivot$message("PivotCalculator$evaluateCell", "Evaluating cell...")
     rowNumber <- cell$rowNumber
     columnNumber <- cell$rowNumber
     calculationGroupName <- cell$calculationGroupName
     calculationName <- cell$calculationName
     rowColFilters <- cell$rowColFilters
     if(is.null(calculationGroupName)) return()
     if(is.null(calculationName)) return()
     results <- self$evaluateCalculationGroup(calculationGroupName=calculationGroupName, rowColFilters=rowColFilters)
     if(!(calculationName %in% names(results)))
       stop(paste0("PivotCalculator$evaluateCell():  calculation result for '", calculationName,
                   "' not found in cell r=", rowNumber, ", c=", columnNumber), call. = FALSE)
     cell$rawValue <- results[[calculationName]]$rawValue
     cell$formattedValue <- results[[calculationName]]$formattedValue
     cell$calculationFilters <- results[[calculationName]]$filters
     private$p_parentPivot$message("PivotCalculator$evaluateCell", "Evaluated cell.")
   }
  ),
  private = list(
    p_parentPivot = NULL
  )
)
