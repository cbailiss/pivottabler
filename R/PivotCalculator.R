#' A class that computes the value of a cell.
#'
#' The PivotCalculator class has various functions and methods that assist with
#' calculating the value of a cell in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import dplyr
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   calculate the value of a pivot table cell.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field batchInfo Get a summary of the batch calculations.
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot table calculator, specifying the
#'   field value documented above.}
#'
#'   \item{\code{getDataFrame(dataName)}}{Gets a data frame with the specified
#'   name from the data frames added to the pivot table.}
#'   \item{\code{getCalculationGroup(calculationGroupName)}}{Gets a calculation
#'   group with the specified name from the calculation groups added to the
#'   pivot table.}
#'   \item{\code{getCalculation(calculationGroupName, calculationName)}}{Gets a
#'   calculation with the specified name and group from the calculation groups
#'   added to the pivot table.}
#'   \item{\code{generateBatchesForCellEvaluation()}}{Examines the cells in the
#'   pivot table to generate one or more batch calculations.}
#'   \item{\code{evaluateBatches()}}{Evaluate batch calculations using the batch
#'   calculator.}
#'   \item{\code{newFilter(variableName, values)}}{Creates a new PivotFilter
#'   object associated with the specified data frame column name and column
#'   values.}
#'   \item{\code{newFilters(variableName, values)}}{Creates a new PivotFilters
#'   object associated with the specified data frame column name and column
#'   values.}
#'   \item{\code{setFilters(filters1, filters2, action="replace")}}{Combines two
#'   PivotFilters objects (e.g. to intersect the filters coming from the row and
#'   column headings for a particular cell).}
#'   \item{\code{setFilterValues(filters, variableName, values,
#'   action="replace")}}{Updates a PivotFilters object based on a PivotFilter
#'   object (e.g. to union the filter criteria arising from multiple row
#'   headers).}
#'   \item{\code{getFilteredDataFrame(dataFrame, filters)}}{Applies a
#'   PivotFilters object to a data frame, returning a new data frame.}
#'   \item{\code{getDistinctValues(dataFrame, variableName)}}{Gets the distinct
#'   values from the specified column of a data frame.}
#'   \item{\code{formatValue(value, format)}}{Formats a numerical value using
#'   either an sprintf string, a list of arguments for the base::format()
#'   function or using a custom R function.}
#'   \item{\code{getFiltersForSingleValue = function(rowColFilters=NULL,
#'   calcFilters=NULL)}}{Get the working filters for a single value
#'   calculation.}
#'   \item{\code{getFiltersForSummariseExpression = function(rowColFilters=NULL,
#'   calcFilters=NULL)}}{Get the working filters for a summary calculation.}
#'   \item{\code{getFiltersForCalculateFunction = function(rowColFilters=NULL,
#'   calcFilters=NULL)}}{Get the working filters for a calculation based on
#'   other calculations.}
#'   \item{\code{getFiltersForNamedCalculation = function(calculationName=NULL,
#'   calculationGroupName=NULL, rowColFilters=NULL, cell=NULL)}}{Get the working
#'   filters for a custom calculation.}
#'   \item{\code{setWorkingFilters = function(cell=NULL)}}{Set the working
#'   filters for a cell.}
#'   \item{\code{getSingleValue(dataFrame, valueName)}}{Gets a single value from
#'   a data frame.  Essentially the same as the getSummaryValue() function but
#'   with no aggregation.}
#'   \item{\code{getSummaryValue(dataFrame, summaryName,
#'   summariseExpression)}}{Aggregates a data frame using the dplyr::summarise()
#'   function to calculate and return an aggregate value.}
#'   \item{\code{evaluateSingleValue(dataFrame, workingFilters,
#'   valueName, format, noDataValue, noDataCaption)}}{A wrapper for
#'   getSingleValue() which performs filtering and handles edge cases.}
#'   \item{\code{evaluateSummariseExpression2(dataName=NULL, dataFrame=NULL,
#'   workingFilters=NULL, calculationName=NULL, calculationGroupName=NULL,
#'   summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL,
#'   noDataCaption=NULL)}}{Calculate a summary value, either using a batch or
#'   sequential calculation.}
#'   \item{\code{evaluateSummariseExpression1(dataFrame=NULL,
#'   workingFilters=NULL, summaryName=NULL, summariseExpression=NULL,
#'   format=NULL, noDataValue=NULL, noDataCaption=NULL)}}{A wrapper for
#'   getSummaryValue() which performs filtering and handles edge cases for
#'   sequential evaluation.}
#'   \item{\code{evaluateCalculationExpression(values, calculationExpression,
#'   format, noDataValue, noDataCaption)}}{Evaluates an R expression in order to
#'   combine the results of other calculations.}
#'   \item{\code{evaluateCalculateFunction(workingFilters, calculationFunction,
#'   format, baseValues, cell)}}{Invokes a user-provided custom R function to
#'   aggregate data and perform calculations.}
#'   \item{\code{evaluateNamedCalculation1(calculationName,
#'   calculationGroupName, workingFilters, cell)}}{Invokes the relevant
#'   calculation function above based upon the calculation type.}
#'   \item{\code{evaluateNamedCalculation2(calculationName,
#'   calculationGroupName, rowColFilters)}}{Determines the working filters and
#'   invokes the relevant calculation function above based upon the calculation
#'   type.}
#'   \item{\code{evaluateCell(cell)}}{Top-level calculation function responsible
#'   for calculating the value of a pivot table cell.}
#' }

PivotCalculator <- R6::R6Class("PivotCalculator",
  public = list(
   initialize = function(parentPivot=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     private$p_batchCalculator <- PivotBatchCalculator$new(private$p_parentPivot)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$new", "Creating new Pivot Calculator...")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$new", "Created new Pivot Calculator.")
   },
   getDataFrame = function(dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDataFrame", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDataFrame", "Getting data frame...")
     df <- private$p_parentPivot$data$getData(dataName)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDataFrame", "Got data frame.")
     return(invisible(df))
   },
   getCalculationGroup = function(calculationGroupName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculationGroup", "Getting calculation group...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculationGroup", "Got calculation group.")
     return(invisible(cg))
   },
   getCalculation = function(calculationGroupName=NULL, calculationName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculation", "Getting calculation...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     cn <- cg$getCalculation(calculationName)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculation", "Got calculation.")
     return(invisible(cg))
   },
   generateBatchesForCellEvaluation = function() {
     if(private$p_parentPivot$evaluationMode=="sequential") {
       if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$generateBatchesForCellEvaluation", "Pivot table is using sequential evaluation mode, so not creating batches.")
       return(invisible())
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$generateBatchesForCellEvaluation", "Generating batches for cell evaluation...")
     res <- private$p_batchCalculator$generateBatchesForCellEvaluation()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$generateBatchesForCellEvaluation", "Generated batches for cell evaluation.")
     return(invisible(res))
   },
   evaluateBatches = function() {
     if(private$p_parentPivot$evaluationMode=="sequential") {
       if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateBatches", "Pivot table is using sequential evaluation mode, so not evaluating batches.")
       return(invisible())
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateBatches", "Evaluating batches...")
     res <- private$p_batchCalculator$evaluateBatches()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateBatches", "Evaluated batches.")
     return(invisible(res))
   },
   newFilter = function(variableName=NULL, values=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "newFilter", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "newFilter", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$newFilter", "Creating filter...")
     filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, values=values)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$newFilter", "Created filter.")
     return(invisible(filter))
   },
   newFilters = function(variableName=NULL, values=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "newFilters", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "newFilters", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$newFilters", "Creating filters...")
     filters <- PivotFilters$new(private$p_parentPivot, variableName=variableName, values=values)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$newFilters", "Created filters.")
     return(invisible(filters))
   },
   setFilters = function(filters1=NULL, filters2=NULL, action="replace") { # filters2 overrides filters1
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters1, missing(filters1), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters2, missing(filters2), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilters", "Setting filters...")
     checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters1, missing(filters1), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters2, missing(filters2), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     copy <- filters1$getCopy() # always copy, to avoid inadvertant bugs of a change to one filter affecting multiple cells in the Pivot Table
     copy$setFilters(filters=filters2, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilters", "Set filters.")
     return(invisible(copy))
   },
   setFilter = function(filters=NULL, filter=NULL, action="replace") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilter", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilter", "Setting filter...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilter(filter=filter, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilter", "Set filter.")
     return(invisible(copy))
   },
   setFilterValues = function(filters=NULL, variableName=NULL, values=NULL, action="replace") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilterValues", "Setting filter values...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilterValues(variableName=variableName, values=values, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilterValues", "Set filter values.")
     return(invisible(copy))
   },
   getFilteredDataFrame = function(dataFrame=NULL, filters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getFilteredDataFrame", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getFilteredDataFrame", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFilteredDataFrame", "Getting filtered data frame...")
     data <- filters$getFilteredDataFrame(dataFrame)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFilteredDataFrame", "Got filtered data frame.")
     return(invisible(data))
   },
   getDistinctValues = function(dataFrame=NULL, variableName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDistinctValues", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDistinctValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDistinctValues", "Getting filtered data frame...")
     # build a dplyr query
     data <- dataFrame
     eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
     data <- dplyr::distinct(data)
     eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
     distinctValues <- dplyr::collect(data)[[variableName]]
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDistinctValues", "Got filtered data frame.")
     return(invisible(distinctValues))
   },
   formatValue = function(value=NULL, format=NULL) {
     # this function is ready to support other data types, once the other PivotTable logic/cells have been modified
     # to work with data types other than numeric/integer
     # , "character", "factor", "logical", "Date", "POSIXct", "POSIXlt"
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$formatValue", "Formatting value...")
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
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$formatValue", "Formated value.")
     return(invisible(value))
   },
   getFiltersForSingleValue = function(rowColFilters=NULL, calcFilters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForSingleValue", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForSingleValue", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForSingleValue", "Getting filters for single value...")
     # get the final filter context for this calculation (calcFilters override row/col filters)
     rf <- list()
     rf$calculationFilters <- calcFilters
     filters <- NULL
     if(is.null(rowColFilters)) {
       if(!isnull(calcFilters)) filters <- calcFilters$getCopy()
     }
     else {
       filters <- rowColFilters$getCopy()
       if(!is.null(calcFilters)) filters$setFilters(filters=calcFilters, action="and")
     }
     rf$workingFilters <- filters
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForSingleValue", "Got filters for single value.")
     return(invisible(rf))
   },
   getFiltersForSummariseExpression = function(rowColFilters=NULL, calcFilters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForSummariseExpression", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForSummariseExpression", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForSummariseExpression", "Getting filters for summary expression...")
     # get the final filter context for this calculation (calcFilters override row/col filters)
     rf <- list()
     rf$calculationFilters <- calcFilters
     filters <- NULL
     if(is.null(rowColFilters)) {
       if(!isnull(calcFilters)) filters <- calcFilters$getCopy()
     }
     else {
       filters <- rowColFilters$getCopy()
       if(!is.null(calcFilters)) filters$setFilters(filters=calcFilters, action="and")
     }
     rf$workingFilters <- filters
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForSummariseExpression", "Got filters for summary expression.")
     return(invisible(rf))
   },
   getFiltersForCalculateFunction = function(rowColFilters=NULL, calcFilters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForCalculateFunction", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForCalculateFunction", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForCalculateFunction", "Getting filters for calculation function...")
     # get the final filter context for this calculation (calcFilters override row/col filters)
     rf <- list()
     rf$calculationFilters <- calcFilters
     filters <- NULL
     if(is.null(rowColFilters)) {
       if(!isnull(calcFilters)) filters <- calcFilters$getCopy()
     }
     else {
       filters <- rowColFilters$getCopy()
       if(!is.null(calcFilters)) filters$setFilters(filters=calcFilters, action="and")
     }
     rf$workingFilters <- filters
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForCalculateFunction", "Got filters for calculation function.")
     return(invisible(rf))
   },
   getFiltersForNamedCalculation = function(calculationName=NULL, calculationGroupName=NULL, rowColFilters=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForNamedCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForNamedCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForNamedCalculation", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getFiltersForNamedCalculation", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForNamedCalculation", "Getting filters for named calculation...")
     # get the calculation and calculation group
     calcGrp <- self$getCalculationGroup(calculationGroupName)
     calcs <- calcGrp$calculations
     # execution order
     execOrder <- NULL
     basedOn <- calcs[[calculationName]]$basedOn
     if(!is.null(basedOn)) execOrder <- rep(basedOn)
     execOrder[length(execOrder)+1] <- calculationName
     # execute the calculations
     filters <- list() # each item in the list is a list of two items (calculationFilters, workingFilters)
     for(i in 1:length(execOrder)) {
       calc <- calcs[[execOrder[i]]]
       if(calc$type=="value") {
         if((!is.null(cell))&&(cell$isTotal==TRUE)) {
           if(is.null(calc$summariseExpression)) { rf <- list() }
           else {
             rf <- self$getFiltersForSummariseExpression(rowColFilters=rowColFilters, calcFilters=calc$filters)
           }
         }
         else {
           rf <- self$getFiltersForSingleValue(rowColFilters=rowColFilters, calcFilters=calc$filters)
         }
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="summary") {
         rf <- self$getFiltersForSummariseExpression(rowColFilters=rowColFilters, calcFilters=calc$filters)
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="calculation") {
         rf <- list()
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="function") {
         rf <- self$getFiltersForCalculateFunction(rowColFilters=rowColFilters, calcFilters=calc$filters)
         filters[[calc$calculationName]] <- rf
       }
       else stop(paste0("PivotCalculator$getFiltersForNamedCalculation():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForNamedCalculation", "Got filters for named calculation.")
     return(invisible(filters))
   },
   setWorkingFilters = function(cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setWorkingFilters", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setWorkingFilters", "Setting working filters for cell...")
     rowNumber <- cell$rowNumber
     columnNumber <- cell$columnNumber
     calculationGroupName <- cell$calculationGroupName
     calculationName <- cell$calculationName
     rowColFilters <- cell$rowColFilters
     if(is.null(calculationGroupName)) return(invisible())
     if(is.null(calculationName)) return(invisible())
     filters <- self$getFiltersForNamedCalculation(calculationName=calculationName, calculationGroupName=calculationGroupName,
                                                   rowColFilters=rowColFilters, cell=cell)
     if(is.null(filters)) {
       cell$calculationFilters <- NULL
       cell$workingFilters <- list()
     }
     else if(length(filters)==0) {
       cell$calculationFilters <- NULL
       cell$workingFilters <- list()
     }
     else {
       cell$calculationFilters <- filters[[calculationName]]$calculationFilters
       # for type=calculation, need the working filters for the base calculations as well
       lst <- lapply(filters, function(x) { return(x$workingFilters) })
       if(is.null(lst)) cell$workingFilters <- list()
       else if(length(lst)==0) cell$workingFilters <- list()
       else cell$workingFilters <- lst
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setWorkingFilters", "Set working filters for cell.")
   },
   getSingleValue = function(dataFrame=NULL, valueName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getSingleValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getSingleValue", valueName, missing(valueName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getSingleValue", "Getting single value...")
     data <- dataFrame
     if(nrow(data)>1)
       stop(paste0("PivotCalculator$getSingleValue(): Value '", valueName, "' has resulted in '", nrow(data),
                   " row(s).  There must be a maximum of 1 row for the value after the filters have been applied."), call. = FALSE)
     if(nrow(data)==0) return(invisible(NULL))
     data <- dplyr::collect(data)
     value <- data[[valueName]][1] # data[[valueName]] will always return a column as a vector (even if data is still a tbl_df)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getSingleValue", "Got single value.")
     return(invisible(value))
   },
   getSummaryValue = function(dataFrame=NULL, summaryName=NULL, summariseExpression=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getSummaryValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getSummaryValue", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getSummaryValue", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getSummaryValue", "Getting summary value...")
     # todo: escaping below
     if(nrow(dataFrame)==0) return(invisible(NULL))
     summaryCmd <- paste0("data <- dplyr::summarise(dataFrame, ", summaryName, " = ", summariseExpression, ")")
     eval(parse(text=summaryCmd))
     if((nrow(data)>1)||(ncol(data)>1))
       stop(paste0("PivotCalculator$getSummaryValue(): Summary expression '", summaryName, "' has resulted in '", nrow(data),
                   " row(s) and ", ncol(data), " columns.  There must be a maximum of 1 row and 1 column in the result."), call. = FALSE)
     data <- dplyr::collect(data)
     value <- data[[1]][1] # data[[colIndex]] will always return a column as a vector (even if data is still a tbl_df)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getSummaryValue", "Got summary value.")
     return(invisible(value))
   },
   evaluateSingleValue = function(dataFrame=NULL, workingFilters=NULL, valueName=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", valueName, missing(valueName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSingleValue", "Evaluating single value...")
     data <- dataFrame
     value <- list()
     # if we have some filters, filter the data frame
     if(!is.null(workingFilters)) {
       value$evaluationFilters <- workingFilters
       if(workingFilters$count > 0) {
         data <- self$getFilteredDataFrame(dataFrame=data, filters=workingFilters)
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
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSingleValue", "Evaluated single value.")
     return(invisible(value))
   },
   evaluateSummariseExpression2 = function(dataName=NULL, dataFrame=NULL, workingFilters=NULL,
                                          calculationName=NULL, calculationGroupName=NULL,
                                          summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression2", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression2", "Evaluating summary expression...")
     # top level vars
     value <- list()
     valFromBatch <- FALSE
     # if in batch mode, try and get the value from a batch calculation
     if(private$p_parentPivot$evaluationMode=="batch") {
       batchValue <- private$p_batchCalculator$getSummaryValueFromBatch(dataName=dataName, calculationName=calculationName,
                                                                        calculationGroupName=calculationGroupName, workingFilters=workingFilters)
       if(batchValue$isBatchCompatible==FALSE) {
         # fall through to sequential evaluation
       }
       else if(batchValue$batchEvaluated==FALSE) {
         # fall through to sequential evaluation
       }
       else {
         # no data?
         if(is.null(batchValue$value)) {
           value$rawValue <- noDataValue
           if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
           else value$formattedValue <- self$formatValue(noDataValue, format=format)
         }
         else {
           # format the value
           value$rawValue <- batchValue$value
           value$formattedValue <- self$formatValue(batchValue$value, format=format)
         }
         valFromBatch <- TRUE
       }
     }
     # if not in batch mode, or was unable to get the value from a batch calculation, then calculate the old fashioned way
     if(valFromBatch==FALSE) {
       value <- self$evaluateSummariseExpression1(dataFrame=dataFrame, workingFilters=workingFilters,
                                                  summaryName=summaryName, summariseExpression=summariseExpression,
                                                  format=format, noDataValue=noDataValue, noDataCaption=noDataCaption)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression2", "Evaluated summary expression.")
     return(invisible(value))
   },
   evaluateSummariseExpression1 = function(dataFrame=NULL, workingFilters=NULL, summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     # no need to check all these again
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
     # checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression1", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression1", "Evaluating summary expression (sequential)...")
     data <- dataFrame
     value <- list()
     # if we have some filters, filter the data frame
     if(!is.null(workingFilters)) {
       value$evaluationFilters <- workingFilters
       if(workingFilters$count > 0) {
         data <- self$getFilteredDataFrame(dataFrame=data, filters=workingFilters)
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
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression1", "Evaluated summary expression (sequential).")
     return(invisible(value))
   },
   evaluateCalculationExpression = function(values=NULL, calculationExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", values, missing(values), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", calculationExpression, missing(calculationExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSCalculationExpression", "Evaluating summary expression...")
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
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSCalculationExpression", "Evaluated summary expression.")
     return(invisible(value))
   },
   evaluateCalculateFunction = function(workingFilters=NULL, calculationFunction=NULL, format=NULL, baseValues=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", calculationFunction, missing(calculationFunction), allowMissing=FALSE, allowNull=FALSE, allowedClasses="function")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", baseValues, missing(baseValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCalculateFunction", "Evaluating calculation function...")
     value <- list()
     value$evaluationFilters <- workingFilters
     # calculate the value by calling the calculation function
     rv <- calculationFunction(pivotCalculator=self, netFilters=workingFilters, format=format, baseValues=baseValues, cell=cell)
     value$evaluationFilters <- rv$filters # the calculationfunction may have updated the filters
     value$rawValue <- rv$rawValue
     value$formattedValue <- rv$formattedValue
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCalculateFunction", "Evaluated calculation function.")
     return(invisible(value))
   },
   evaluateNamedCalculation1 = function(calculationName=NULL, calculationGroupName=NULL, workingFilters=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation1", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation1", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation1", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation1", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation1", "Evaluating named calculation...")
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
       filters <- workingFilters[[calc$calculationName]]
       if(calc$type=="value") {
         df <- self$getDataFrame(calc$dataName)
         if((!is.null(cell))&&(cell$isTotal==TRUE)) {
           if(is.null(calc$summariseExpression)) { value <- private$getNullValue() }
           else {
             value <- self$evaluateSummariseExpression2(dataName=calc$dataName, dataFrame=df, workingFilters=filters,
                                                        calculationName=calc$calculationName, calculationGroupName=calculationGroupName,
                                                        summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                                        format=calc$format, noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
           }
         }
         else {
           value <- self$evaluateSingleValue(dataFrame=df, workingFilters=filters,
                                             valueName=calc$valueName, format=calc$format,
                                             noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         }
         results[[calc$calculationName]] <- value
       }
       else if(calc$type=="summary") {
         df <- self$getDataFrame(calc$dataName)
         value <- self$evaluateSummariseExpression2(dataName=calc$dataName, dataFrame=df, workingFilters=filters,
                                                    calculationName=calc$calculationName, calculationGroupName=calculationGroupName,
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
         value <- self$evaluateCalculateFunction(workingFilters=filters,
                                            calculationFunction=calc$calculationFunction, format=calc$format, baseValues=values, cell=cell)
         results[[calc$calculationName]] <- value
       }
       else stop(paste0("PivotCalculator$evaluateNamedCalculation1():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation1", "Evaluated named calculation.")
     return(invisible(results))
   },
   # this variation of evaluateNamedCalculation gets the working filters from a simple row/column context and directly evaluates
   evaluateNamedCalculation2 = function(calculationName=NULL, calculationGroupName=NULL, rowColFilters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation2", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation2", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation2", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation2", "Evaluating named calculation...")
     # get the set of working filters for the calculation
     filters <- self$getFiltersForNamedCalculation(calculationName=calculationName,
                                                         calculationGroupName=calculationGroupName,
                                                         rowColFilters=rowColFilters, cell=NULL)
     # get the working filters from filters (code taken from PivotCalculator$setWorkingFilters)
     workingFilters <- NULL
     if(is.null(filters)) workingFilters <- list()
     else if(length(filters)==0) workingFilters <- list()
     else {
       # for type=calculation, need the working filters for the base calculations as well, so could have more than one
       lst <- lapply(filters, function(x) { return(x$workingFilters) })
       if(is.null(lst)) workingFilters <- list()
       else if(length(lst)==0) workingFilters <- list()
       else workingFilters <- lst
     }
     # calculate the value
     results <- self$evaluateNamedCalculation1(calculationName=calculationName,
                                               calculationGroupName=calculationGroupName,
                                               workingFilters=workingFilters, cell=NULL)
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation1", "Evaluated named calculation.")
     return(invisible(results))
   },
   evaluateCell = function(cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCell", "Evaluating cell...")
     rowNumber <- cell$rowNumber
     columnNumber <- cell$columnNumber
     calculationGroupName <- cell$calculationGroupName
     calculationName <- cell$calculationName
     if(is.null(calculationGroupName)) return(invisible())
     if(is.null(calculationName)) return(invisible())
     workingFilters <- cell$workingFilters
     results <- self$evaluateNamedCalculation1(calculationName=calculationName, calculationGroupName=calculationGroupName,
                                              workingFilters=workingFilters, cell=cell)
     if(!(calculationName %in% names(results)))
       stop(paste0("PivotCalculator$evaluateCell():  calculation result for '", calculationName,
                   "' not found in cell r=", rowNumber, ", c=", columnNumber), call. = FALSE)
     cell$rawValue <- results[[calculationName]]$rawValue
     cell$formattedValue <- results[[calculationName]]$formattedValue
     cell$evaluationFilters <- results[[calculationName]]$evaluationFilters
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCell", "Evaluated cell.")
   }
  ),
  active = list(
    batchInfo = function(value) {
      cstr <- paste0("BATCH INFO:\n\n", private$p_batchCalculator$batchSummary, "\n\nCALC SUMMARY:\n\n", private$p_batchCalculator$calculationSummary)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_batchCalculator = NULL,
    getNullValue = function() {
     value <- list()
     value$rawValue <- NULL
     value$formattedValue <- ""
     return(value)
    }
  )
)
