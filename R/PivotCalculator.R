#' A class that computes the value of a cell.
#'
#' The PivotCalculator class has various functions and methods that assist with calculating the value of a cell in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import dplyr
#' @import jsonlite
#' @keywords calculation
#' @return Object of \code{\link{R6Class}} with properties and methods that help calculate the value of a pivot table cell.
#' @format \code{\link{R6Class}} object.
#' @examples
#' This class should only be created by the pivot table.
#' It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot table calculator, specifying the field value documented above.}
#'
#'   \item{\code{getDataFrame(dataName)}}{Gets a data frame with the specified name from the data frames added to the pivot table.}
#'   \item{\code{getCalculationGroup(calculationGroupName)}}{Gets a calculation group with the specified name from the calculation groups added to the pivot table.}
#'   \item{\code{getCalculation(calculationGroupName, calculationName)}}{Gets a calculation with the specified name and group from the calculation groups added to the pivot table.}
#'   \item{\code{newFilter(variableName, values)}}{Creates a new PivotFilter object associated with the specified data frame column name and column values.}
#'   \item{\code{newFilters(variableName, values)}}{Creates a new PivotFilters object associated with the specified data frame column name and column values.}
#'   \item{\code{setFilters(filters1, filters2, action="replace")}}{Combines two PivotFilters objects (e.g. to intersect the filters coming from the row and column headings for a particular cell).}
#'   \item{\code{setFilterValues(filters, variableName, values, action="replace")}}{Updates a PivotFilters object based on a PivotFilter object (e.g. to union the filter criteria arising from multiple row headers).}
#'   \item{\code{getFilteredDataFrame(dataFrame, filters)}}{Applies a PivotFilters object to a data frame, returning a new data frame.}
#'   \item{\code{getDistinctValues(dataFrame, variableName)}}{Gets the distinct values from the specified column of a data frame.}
#'   \item{\code{formatValue(value, format)}}{Formats a numerical value using either an sprintf string, a list of arguments for the base::format() function or using a custom R function.}
#'   \item{\code{getSingleValue(dataFrame, valueName)}}{Gets a single value from a data frame.  Essentially the same as the getSummaryValue() function but with no aggregation.}
#'   \item{\code{getSummaryValue(dataFrame, summaryName, summariseExpression)}}{Aggregates a data frame using the dplyr::summarise() function to calculate and return an aggregate value.}
#'   \item{\code{evaluateSingleValue(dataFrame, rowColFilters, calcFilters, valueName, format, noDataValue, noDataCaption)}}{A wrapper for getSingleValue() which performs filtering and handles edge cases.}
#'   \item{\code{evaluateSummariseExpression(dataFrame, rowColFilters, calcFilters, summaryName, summariseExpression, format, noDataValue, noDataCaption)}}{A wrapper for getSummaryValue() which performs filtering and handles edge cases.}
#'   \item{\code{evaluateCalculationExpression(values, calculationExpression, format, noDataValue, noDataCaption)}}{Evaluates an R expression in order to combine the results of other calculations.}
#'   \item{\code{evaluateCalculateFunction(rowColFilters, calcFilters, calculationFunction, format, baseValues, cell)}}{Invokes a user-provided custom R function to aggregate data and perform calculations.}
#'   \item{\code{evaluateNamedCalculation(calculationName, calculationGroupName, rowColFilters, cell)}}{Invokes the relevant calculation function above based upon the calculation type.}
#'   \item{\code{evaluateCell(cell)}}{Top-level calculation function responsible for calculating the value of a pivot table cell.}
#' }

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
     private$p_parentPivot$message("PivotCalculator$getDataFrame", "Getting data frame...")
     df <- private$p_parentPivot$data$getData(dataName)
     private$p_parentPivot$message("PivotCalculator$getDataFrame", "Got data frame.")
     return(invisible(df))
   },
   getCalculationGroup = function(calculationGroupName=NULL) {
     checkArgument("PivotCalculator", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getCalculationGroup", "Getting calculation group...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     private$p_parentPivot$message("PivotCalculator$getCalculationGroup", "Got calculation group.")
     return(invisible(cg))
   },
   getCalculation = function(calculationGroupName=NULL, calculationName=NULL) {
     checkArgument("PivotCalculator", "getCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "getCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getCalculation", "Getting calculation...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     cn <- cg$getCalculation(calculationName)
     private$p_parentPivot$message("PivotCalculator$getCalculation", "Got calculation.")
     return(invisible(cg))
   },
   newFilter = function(variableName=NULL, values=NULL) {
     checkArgument("PivotCalculator", "newFilter", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "newFilter", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$newFilter", "Creating filter...")
     filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, values=values)
     private$p_parentPivot$message("PivotCalculator$newFilter", "Created filter.")
     return(invisible(filter))
   },
   newFilters = function(variableName=NULL, values=NULL) {
     checkArgument("PivotCalculator", "newFilters", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "newFilters", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$newFilters", "Creating filters...")
     filters <- PivotFilters$new(private$p_parentPivot, variableName=variableName, values=values)
     private$p_parentPivot$message("PivotCalculator$newFilters", "Created filters.")
     return(invisible(filters))
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
     return(invisible(copy))
   },
   setFilter = function(filters=NULL, filter=NULL, action="replace") {
     checkArgument("PivotCalculator", "setFilter", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     private$p_parentPivot$message("PivotCalculator$setFilter", "Setting filter...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilter(filter=filter, action=action)
     private$p_parentPivot$message("PivotCalculator$setFilter", "Set filter.")
     return(invisible(copy))
   },
   setFilterValues = function(filters=NULL, variableName=NULL, values=NULL, action="replace") {
     checkArgument("PivotCalculator", "setFilterValues", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     private$p_parentPivot$message("PivotCalculator$setFilterValues", "Setting filter values...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilterValues(variableName=variableName, values=values, action=action)
     private$p_parentPivot$message("PivotCalculator$setFilterValues", "Set filter values.")
     return(invisible(copy))
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
       filterCmd <- NULL
       for(j in 1:length(filters$filters)) {
         filter <- filters$filters[[j]]
         if(is.null(filter$variableName)) stop("PivotCalculator$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
         if(is.null(filter$values)) next
         if(length(filter$values)==0) next
         if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
         if(length(filter$values)>0) {
           # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
           filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% filters$filters[[", j, "]]$values)")
         }
       }
       filterCmd <- paste0("data <- dplyr::filter(data,", filterCmd, ")")
       eval(parse(text=filterCmd))
     }
     private$p_parentPivot$message("PivotCalculator$getFilteredDataFrame", "Got filtered data frame.")
     return(invisible(data))
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
     return(invisible(distinctValues))
   },
   formatValue = function(value=NULL, format=NULL) {
     # this function is ready to support other data types, once the other PivotTable logic/cells have been modified
     # to work with data types other than numeric/integer
     # , "character", "factor", "logical", "Date", "POSIXct", "POSIXlt"
     checkArgument("PivotCalculator", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCalculator", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
     private$p_parentPivot$message("PivotCalculator$formatValue", "Formatting value...")
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
     # else if(("Date" %in% clsv)||("POSIXct" %in% clsv)||("POSIXlt" %in% clsv)) {
     #   clsf <- class(format)
     #   if ("list" %in% clsf) {
     #     args <- format
     #     args$x <- value
     #     value <- do.call(base::format, args)
     #   }
     #   else if ("function" %in% class(format)) value <- format(value)
     # }
     # else if ("factor" %in% clsv) value <- as.character(value)
     # else if("logical" %in% clsv) value <- as.character(value)
     private$p_parentPivot$message("PivotCalculator$formatValue", "Formated value.")
     return(invisible(value))
   },
   getSingleValue = function(dataFrame=NULL, valueName=NULL) {
     checkArgument("PivotCalculator", "getSingleValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "getSingleValue", valueName, missing(valueName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getSingleValue", "Getting single value...")
     data <- dataFrame
     if(nrow(data)>1)
       stop(paste0("PivotCalculator$getSingleValue(): Value '", valueName, "' has resulted in '", nrow(data),
                   " row(s).  There must be a maximum of 1 row for the value after the filters have been applied."))
     if(nrow(data)==0) return(invisible(NULL))
     data <- dplyr::collect(data)
     # The code below is no longer needed.
     # Even if data is still a tbl_df, tbl_df[[valueName]] will always return a column as a vector
     # if("tbl_df" %in% class(data)) {
     #   data <- as.data.frame(data) # workaround of a possible bug in dplyr? collect seems to still sometimes return a tbl_df
     #   if("tbl_df" %in% class(data)) {
     #     stop(paste0("PivotCalculator$getSingleValue(): Unable to coerce the tbl_df back to a data.frame for summary expression '", summaryName, "'.",
     #               "  This has resulted in a value of data type [", class(data), "] with ", nrow(data), " row(s)."))
     #   }
     # }
     value <- data[[valueName]][1]
     private$p_parentPivot$message("PivotCalculator$getSingleValue", "Got single value.")
     return(invisible(value))
   },
   getSummaryValue = function(dataFrame=NULL, summaryName=NULL, summariseExpression=NULL) {
     checkArgument("PivotCalculator", "getSummaryValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "getSummaryValue", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "getSummaryValue", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$getSummaryValue", "Getting summary value...")
     # todo: escaping below
     if(nrow(dataFrame)==0) return(invisible(NULL))
     summaryCmd <- paste0("data <- dplyr::summarise(dataFrame, ", summaryName, " = ", summariseExpression, ")")
     eval(parse(text=summaryCmd))
     if((nrow(data)>1)||(ncol(data)>1))
       stop(paste0("PivotCalculator$getSummaryValue(): Summary expression '", summaryName, "' has resulted in '", nrow(data),
                   " row(s) and ", ncol(data), " columns.  There must be a maximum of 1 row and 1 column in the result."))
     data <- dplyr::collect(data)
     # The code below is no longer needed.
     # Even if data is still a tbl_df, tbl_df[[colIndex]] will always return a column (as a vector) and tbl_df[[colIndex]][rowIndex] will always return a single value
     # if("tbl_df" %in% class(data)) {
     #   data <- as.data.frame(data) # workaround of a possible bug in dplyr? collect seems to still sometimes return a tbl_df
     #   if("tbl_df" %in% class(data)) {
     #     stop(paste0("PivotCalculator$getSummaryValue(): Unable to coerce the tbl_df back to a data.frame for summary epxression '", summaryName, "'.",
     #               "  This has resulted in a value of data type [", class(data), "] with ", nrow(data), " row(s) and ", ncol(data), " columns."))
     #   }
     # }
     value <- data[[1]][1]
     private$p_parentPivot$message("PivotCalculator$getSummaryValue", "Got summary value.")
     return(invisible(value))
   },
   evaluateSingleValue = function(dataFrame=NULL, rowColFilters=NULL, calcFilters=NULL,
                                  valueName=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     checkArgument("PivotCalculator", "evaluateSingleValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "evaluateSingleValue", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSingleValue", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSingleValue", valueName, missing(valueName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateSingleValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
     checkArgument("PivotCalculator", "evaluateSingleValue", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
     checkArgument("PivotCalculator", "evaluateSingleValue", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$evaluateSingleValue", "Evaluating single value...")
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
     # no data?
     if(nrow(data)==0) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format)
     }
     else {
       # calculate the value
       rv <- self$getSingleValue(dataFrame=data, valueName=valueName)
       value$rawValue <- rv
       value$formattedValue <- self$formatValue(rv, format=format)
     }
     private$p_parentPivot$message("PivotCalculator$evaluateSingleValue", "Evaluated single value.")
     return(invisible(value))
   },
   evaluateSummariseExpression = function(dataFrame=NULL, rowColFilters=NULL, calcFilters=NULL,
                                          summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     checkArgument("PivotCalculator", "evaluateSummariseExpression", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateSummariseExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
     checkArgument("PivotCalculator", "evaluateSummariseExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
     checkArgument("PivotCalculator", "evaluateSummariseExpression", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
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
     # no data?
     if(nrow(data)==0) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format)
     }
     else {
       # calculate the value
       rv <- self$getSummaryValue(dataFrame=data, summaryName=summaryName, summariseExpression=summariseExpression)
       value$rawValue <- rv
       value$formattedValue <- self$formatValue(rv, format=format)
     }
     private$p_parentPivot$message("PivotCalculator$evaluateSummariseExpression", "Evaluated summary expression.")
     return(invisible(value))
   },
   evaluateCalculationExpression = function(values=NULL, calculationExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     checkArgument("PivotCalculator", "evaluateCalculationExpression", values, missing(values), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
     checkArgument("PivotCalculator", "evaluateCalculationExpression", calculationExpression, missing(calculationExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateCalculationExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
     checkArgument("PivotCalculator", "evaluateCalculationExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
     checkArgument("PivotCalculator", "evaluateCalculationExpression", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotCalculator$evaluateSCalculationExpression", "Evaluating summary expression...")
     noData <- FALSE
     if(length(values) > 0) {
       for(i in 1:length(values)) {
         if(is.null(values[[i]])) {
           noData <- TRUE
           break
         }
       }
     }
     # no data?
     if(noData==TRUE) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format)
     }
     else {
       # calculate the value
       rv <- eval(parse(text=calculationExpression))
       value <- list()
       value$rawValue <- rv
       value$formattedValue <- self$formatValue(rv, format=format)
     }
     private$p_parentPivot$message("PivotCalculator$evaluateSCalculationExpression", "Evaluated summary expression.")
     return(invisible(value))
   },
   evaluateCalculateFunction = function(rowColFilters=NULL, calcFilters=NULL,
                                        calculationFunction=NULL, format=NULL, baseValues=NULL, cell=NULL) {
     checkArgument("PivotCalculator", "evaluateCalculateFunction", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateCalculateFunction", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateCalculateFunction", calculationFunction, missing(calculationFunction), allowMissing=FALSE, allowNull=FALSE, allowedClasses="function")
     checkArgument("PivotCalculator", "evaluateCalculateFunction", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
     checkArgument("PivotCalculator", "evaluateCalculateFunction", baseValues, missing(baseValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
     checkArgument("PivotCalculator", "evaluateCalculateFunction", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     private$p_parentPivot$message("PivotCalculator$evaluateCalculateFunction", "Evaluating calculation function...")
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
     # calculate the value by calling the calculation function
     rv <- calculationFunction(pivotCalculator=self, netFilters=filters, format=format, baseValues=baseValues, cell=cell)
     value$rawValue <- rv$rawValue
     value$formattedValue <- rv$formattedValue
     private$p_parentPivot$message("PivotCalculator$evaluateCalculateFunction", "Evaluated calculation function.")
     return(invisible(value))
   },
   evaluateNamedCalculation = function(calculationName=NULL, calculationGroupName=NULL, rowColFilters=NULL, cell=NULL) {
     checkArgument("PivotCalculator", "evaluateNamedCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateNamedCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculator", "evaluateNamedCalculation", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculator", "evaluateNamedCalculation", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     private$p_parentPivot$message("PivotCalculator$evaluateNamedCalculation", "Evaluating named calculation...")
     # get the calculation and calculation group
     calcGrp <- self$getCalculationGroup(calculationGroupName)
     calcs <- calcGrp$calculations
     # execution order
     execOrder <- NULL
     basedOn <- calcs[[calculationName]]$basedOn
     if(!is.null(basedOn)) execOrder <- rep(basedOn)
     execOrder[length(execOrder)+1] <- calculationName
     # execute the calculations
     results <- list() # each item in the list is a list of three items (rawValue, formattedValue, filters)
     for(i in 1:length(execOrder)) {
       calc <- calcs[[execOrder[i]]]
       if(calc$type=="value") {
         df <- self$getDataFrame(calc$dataName)
         if((!is.null(cell))&&(cell$isTotal==TRUE)) {
           if(is.null(calc$summariseExpression)) { value <- private$getNullValue() }
           else {
             value <- self$evaluateSummariseExpression(dataFrame=df, rowColFilters=rowColFilters, calcFilters=calc$filters,
                                                       summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                                       format=calc$format, noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
           }
         }
         else {
           value <- self$evaluateSingleValue(dataFrame=df, rowColFilters=rowColFilters, calcFilters=calc$filters,
                                             valueName=calc$valueName, format=calc$format,
                                             noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         }
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="summary") {
         df <- self$getDataFrame(calc$dataName)
         value <- self$evaluateSummariseExpression(dataFrame=df, rowColFilters=rowColFilters, calcFilters=calc$filters,
                                              summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                              format=calc$format, noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="calculation") {
         values <- list()
         if(i>1) {
           for(j in 1:(i-1)) {
             values[[names(results)[[j]]]] <- results[[j]]$rawValue
           }
         }
         value <- self$evaluateCalculationExpression(values=values, calculationExpression=calc$calculationExpression, format=calc$format,
                                                     noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="function") {
         values <- list()
         if(i>1) {
           for(j in 1:(i-1)) {
             values[[names(results)[[j]]]] <- results[[j]]$rawValue
           }
         }
         value <- self$evaluateCalculateFunction(rowColFilters=rowColFilters, calcFilters=calc$filters,
                                            calculationFunction=calc$calculationFunction, format=calc$format, baseValues=values, cell=cell)
         results[[calc$calculationName]] <- value
       }
       else stop(paste0("PivotCalculator$evaluateNamedCalculation():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     private$p_parentPivot$message("PivotCalculator$evaluateNamedCalculation", "Evaluated named calculation.")
     return(invisible(results))
   },
   evaluateCell = function(cell) {
     checkArgument("PivotCalculator", "evaluateCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     private$p_parentPivot$message("PivotCalculator$evaluateCell", "Evaluating cell...")
     rowNumber <- cell$rowNumber
     columnNumber <- cell$columnNumber
     calculationGroupName <- cell$calculationGroupName
     calculationName <- cell$calculationName
     rowColFilters <- cell$rowColFilters
     if(is.null(calculationGroupName)) return(invisible())
     if(is.null(calculationName)) return(invisible())
     results <- self$evaluateNamedCalculation(calculationName=calculationName, calculationGroupName=calculationGroupName,
                                              rowColFilters=rowColFilters, cell=cell)
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
    p_parentPivot = NULL,
    getNullValue = function() {
     value <- list()
     value$rawValue <- NULL
     value$formattedValue <- ""
     return(value)
    }
  )
)
