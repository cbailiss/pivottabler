#' A class that computes the value of a cell.
#'
#' The PivotCalculator class has various functions and methods that assist with
#' calculating the value of a cell in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
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
#'   \item{\code{setWorkingData = function(cell=NULL)}}{Set the working
#'   filters for a cell.}
#'   \item{\code{evaluateSingleValue(dataFrame, workingFilters, valueName,
#'   format, noDataValue, noDataCaption)}}{Get a single value from a data
#'   frame.}
#'   \item{\code{evaluateSummariseExpression(dataName=NULL, dataFrame=NULL,
#'   workingFilters=NULL, calculationName=NULL, calculationGroupName=NULL,
#'   summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL,
#'   noDataCaption=NULL)}}{Calculate a summary value, either using a batch or
#'   sequential calculation.}
#'   \item{\code{evaluateCalculationExpression(values, calculationExpression,
#'   format, noDataValue, noDataCaption)}}{Evaluates an R expression in order to
#'   combine the results of other calculations.}
#'   \item{\code{evaluateCalculateFunction(workingFilters, calculationFunction,
#'   format, baseValues, cell)}}{Invokes a user-provided custom R function to
#'   aggregate data and perform calculations.}
#'   \item{\code{evaluateNamedCalculationWD(calculationName,
#'   calculationGroupName, workingData, cell)}}{Invokes the relevant
#'   calculation function based upon the calculation type.}
#'   \item{\code{evaluateNamedCalculation(calculationName,
#'   calculationGroupName, rowColFilters)}}{Determines the working filters and
#'   invokes the relevant calculation function based upon the calculation
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
     if(private$p_parentPivot$processingLibrary=="dplyr") {
       eval(parse(text=paste0("data <- dplyr::select(data, ", variableName, ")")))
       data <- dplyr::distinct(data)
       eval(parse(text=paste0("data <- dplyr::arrange(data, ", variableName, ")")))
       distinctValues <- dplyr::collect(data)[[variableName]]
       if(is.factor(distinctValues)) distinctValues <- as.character(distinctValues)
     }
     else if (private$p_parentPivot$processingLibrary=="data.table") {
       # check is a data table
       if(private$p_parentPivot$argumentCheckMode == 4) {
         if(!data.table::is.data.table(data))
           stop(paste0("PivotCalculator$getDistinctValues(): A data.table was expected but the following was encountered: ",
                       paste(class(data), sep="", collapse=", ")), call. = FALSE)
       }
       # seem to need a dummy row count in order to get the distinct values
       rcName <- "rc"
       if(variableName==rcName) rcName <- "rowCount"
       eval(parse(text=paste0("distinctValues <- data[order(", variableName, "), .(", rcName, "=.N), by=.(", variableName, ")][, ", variableName, "]")))
       if(is.factor(distinctValues)) distinctValues <- as.character(distinctValues)
     }
     else stop(paste0("PivotCalculator$getDistinctValues(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
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
   setWorkingData = function(cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setWorkingData", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setWorkingData", "Setting working data for cell...")
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
       cell$workingData <- list()
     }
     else if(length(filters)==0) {
       cell$calculationFilters <- NULL
       cell$workingData <- list()
     }
     else {
       cell$calculationFilters <- filters[[calculationName]]$calculationFilters
       # workingData is a list().  There is an element in the list for each calculation in the calculation group.
       # i.e. for type=calculation, there is a set of working data for each of the base calculations (can be more than 1 base calc.)
       # Each list element in workingData is itself a list, where the elements are the workingFilters (and later batchName) for each calculation.
       workingData <- lapply(filters, function(x) { return(list(workingFilters=x$workingFilters)) })
       if(is.null(workingData)) cell$workingData <- list()
       else if(length(workingData)==0) cell$workingData <- list()
       else cell$workingData <- workingData
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setWorkingData", "Set working data for cell.")
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
     if(workingFilters$isNONE) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format)
     }
     else {
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
         if(nrow(data)>1)
           stop(paste0("PivotCalculator$evaluateSingleValue(): Value '", valueName, "' has resulted in '", nrow(data),
                       " row(s).  There must be a maximum of 1 row for the value after the filters have been applied."), call. = FALSE)
         if(nrow(data)==0) return(invisible(NULL))
         data <- dplyr::collect(data)
         rv <- data[[valueName]][1] # data[[valueName]] will always return a column as a vector (even if data is still a tbl_df)
         value$rawValue <- rv
         value$formattedValue <- self$formatValue(rv, format=format)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSingleValue", "Evaluated single value.")
     return(invisible(value))
   },
   evaluateSummariseExpression = function(dataName=NULL, dataFrame=NULL, workingFilters=NULL, batchName=NULL,
                                          calculationName=NULL, calculationGroupName=NULL,
                                          summaryName=NULL, summariseExpression=NULL, format=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", batchName, missing(batchName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", summaryName, missing(summaryName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", summariseExpression, missing(summariseExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression", "Evaluating summary expression...")
     # top level vars
     value <- list()
     valFromBatch <- FALSE
     # if in batch mode, try and get the value from a batch calculation
     if(workingFilters$isNONE) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format)
     }
     else {
       if(private$p_parentPivot$evaluationMode=="batch") {
         if(is.null(batchName)) {
           # fall through to sequential evaluation
         }
         else {
           # try to get the value from a batch calculation
           batchValue <- private$p_batchCalculator$getSummaryValueFromBatch(batchName=batchName, calculationName=calculationName,
                                                                            calculationGroupName=calculationGroupName, workingFilters=workingFilters)
           # was the batch evaluated?
           if(!batchValue$batchEvaluated) {
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
       }
       # if not in batch mode, or was unable to get the value from a batch calculation, then calculate using the original (sequential) method
       if(valFromBatch==FALSE) {
         data <- dataFrame
         value <- list()
         if(private$p_parentPivot$processingLibrary=="dplyr") {
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
             # todo: escaping below
             if(nrow(data)==0) return(invisible(NULL))
             summaryCmd <- paste0("data <- dplyr::summarise(data, ", summaryName, " = ", summariseExpression, ")")
             eval(parse(text=summaryCmd))
             if((nrow(data)>1)||(ncol(data)>1))
               stop(paste0("PivotCalculator$evaluateSummariseExpression(): Summary expression '", summaryName, "' has resulted in '", nrow(data),
                           " row(s) and ", ncol(data), " columns.  There must be a maximum of 1 row and 1 column in the result."), call. = FALSE)
             data <- dplyr::collect(data)
             rv <- data[[1]][1] # data[[colIndex]] will always return a column as a vector (even if data is still a tbl_df)
             value$rawValue <- rv
             value$formattedValue <- self$formatValue(rv, format=format)
           }
         }
         else if(private$p_parentPivot$processingLibrary=="data.table") {
           # check is a data table
           if(private$p_parentPivot$argumentCheckMode == 4) {
             if(!data.table::is.data.table(data))
               stop(paste0("PivotCalculator$evaluateSummariseExpression(): A data.table was expected but the following was encountered: ",
                           paste(class(data), sep="", collapse=", ")), call. = FALSE)
           }
           # filters
           filterCmd <- NULL
           filterCount <- 0
           if(length(workingFilters$filters) > 0)
           {
             for(j in 1:length(workingFilters$filters)) {
               filter <- workingFilters$filters[[j]]
               if(is.null(filter$variableName))
                 stop("PivotCalculator$evaluateSummariseExpression(): filter$variableName must not be null", call. = FALSE)
               if(is.null(filter$values)) next
               if(length(filter$values)==0) next
               if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
               if(length(filter$values)>0) {
                 # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
                 filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% workingFilters$filters[[", j, "]]$values)")
                 filterCount <- filterCount + 1
               }
             }
           }
           # first check we have some data - otherwise aggregate functions like min() and max() produce warnings like the following:
           # Warning message:
           #   In max(SchedSpeedMPH, na.rm = TRUE) :
           #   no non-missing arguments to max; returning -Inf
           # another option here would be to change the code as highlighted in the ("opt") comments below
           # this would avoid the effort of filtering the data table twice, but would result in duplicates being created, which could be very large
           # this option would probaby be faster but could use a lot more memory.
           # since data.table is intended for usage on very large data frames, don't use this option
           dtqry <- paste0("checkCount <- data[", filterCmd, ", .N]")  # opt:  dtqry <- paste0("data <- data[", filterCmd, "]")
           eval(parse(text=dtqry))
           if(checkCount==0) {                                         # opt: if(nrow(data)==0) {
             value$rawValue <- noDataValue
             if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
             else value$formattedValue <- self$formatValue(noDataValue, format=format)
           }
           else {
             # data.table query 2
             dtqry <- paste0("rv <- data[", filterCmd, ", ", summariseExpression, "]")    # opt: dtqry <- paste0("rv <- data[, ", summariseExpression, "]")
             eval(parse(text=dtqry))
             value$rawValue <- rv
             value$formattedValue <- self$formatValue(rv, format=format)
           }
         }
         else stop(paste0("PivotBatch$evaluateBatch(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression", "Evaluated summary expression.")
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
   # The WD here indicates this variation of evaluateNamedCalculation works with the workingData (the version of evaluateNamedCalculation below starts at a higher level)
   evaluateNamedCalculationWD = function(calculationName=NULL, calculationGroupName=NULL, workingData=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculationWD", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculationWD", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculationWD", workingData, missing(workingData), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       if(private$p_parentPivot$argumentCheckMode==4) private$p_batchCalculator$checkValidWorkingData(workingData=workingData)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculationWD", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculationWD", "Evaluating named calculation...")
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
       filters <- workingData[[calc$calculationName]]$workingFilters
       if(calc$type=="value") {
         df <- self$getDataFrame(calc$dataName)
         if((!is.null(cell))&&(cell$isTotal==TRUE)) {
           if(is.null(calc$summariseExpression)) { value <- private$getNullValue() }
           else {
             batchName <- workingData[[calc$calculationName]]$batchName
             value <- self$evaluateSummariseExpression(dataName=calc$dataName, dataFrame=df, workingFilters=filters, batchName=batchName,
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
         batchName <- workingData[[calc$calculationName]]$batchName
         value <- self$evaluateSummariseExpression(dataName=calc$dataName, dataFrame=df, workingFilters=filters, batchName=batchName,
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
       else stop(paste0("PivotCalculator$evaluateNamedCalculationWD():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculationWD", "Evaluated named calculation.")
     return(invisible(results))
   },
   # This variation of evaluateNamedCalculation gets the working filters from a simple row/column context, computes the workingData then calls evaluateNamedCalculationWD.
   # This variation is named without any suffix, since this is the version end-users may invoke, e.g. from within a custom calculation function.
   evaluateNamedCalculation = function(calculationName=NULL, calculationGroupName=NULL, rowColFilters=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateNamedCalculation", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation", "Evaluating named calculation...")
     # get the set of working filters for the calculation
     filters <- self$getFiltersForNamedCalculation(calculationName=calculationName,
                                                         calculationGroupName=calculationGroupName,
                                                         rowColFilters=rowColFilters, cell=NULL)
     # get the working filters from filters (code taken from PivotCalculator$setWorkingFilters)
     workingFilters <- NULL
     if(is.null(filters)) workingData <- list()
     else if(length(filters)==0) workingData <- list()
     else {
       # for type=calculation, need the working filters for the base calculations as well, so could have more than one
       workingData <- lapply(filters, function(x) { return(list(workingFilters=x$workingFilters)) })
       if(is.null(workingData)) workingData <- list()
       else if(length(workingData)==0) workingData <- list()
     }
     # calculate the value
     results <- self$evaluateNamedCalculationWD(calculationName=calculationName,
                                               calculationGroupName=calculationGroupName,
                                               workingData=workingData, cell=NULL)
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculation", "Evaluated named calculation.")
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
     results <- self$evaluateNamedCalculationWD(calculationName=calculationName, calculationGroupName=calculationGroupName,
                                               workingData=cell$workingData, cell=cell)
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
