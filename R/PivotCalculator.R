
#' R6 class that computes the value of a cell or cells.
#'
#' @description
#' The `PivotCalculator` class has various functions and methods that assist with
#' calculating the value of a cell or cells in a pivot table.
#' @details
#' This class contains all of the logic necessary for evaluating calculations.
#' For batch mode calculations, it makes use of the `PivotBatchCalculator` class
#' to carry out the calculation batches, then retrieves the results from the
#' relevant batch for each calculation.
#' For sequential mode calculations, this class carries out the calculations.
#' Where a pivot table contains some cells that can be evaluated in batch mode
#' and some that cannot, this class contains the appropriate logic to use the
#' relevant calculation mode in each case, preferring to use batch mode where
#' possible, unless this has been disabled in the pivot table settings.
#' There are many utility methods in this class that are thin wrappers around
#' methods in other classes.  This simplifies calling these other methods as well
#' as providing a more unified way to change in the future how these common
#' operations are performed.
#' Custom calculation functions are passed an instance of the `PivotCalculator`
#' class, thereby also providing the authors of custom calculation functions an
#' easy way for custom calculation functions to carry out common operations.
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @import dplyr
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCalculator <- R6::R6Class("PivotCalculator",
  public = list(

     #' @description
     #' Create a new `PivotCalculator` object.
     #' @param parentPivot The pivot table that this `PivotCalculator`
     #' instance belongs to.
     #' @return A new `PivotCalculator` object.
   initialize = function(parentPivot=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     private$p_batchCalculator <- PivotBatchCalculator$new(private$p_parentPivot)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$new", "Creating new Pivot Calculator...")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$new", "Created new Pivot Calculator.")
   },

   #' @description
   #' Retrieve a data frame that was added to the pivot table.
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) to retrieve.
   #' @return The data frame with the specified name.
   getDataFrame = function(dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDataFrame", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDataFrame", "Getting data frame...")
     df <- private$p_parentPivot$data$getData(dataName)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDataFrame", "Got data frame.")
     return(invisible(df))
   },

   #' @description
   #' Count the number of "totals" data frames that have been
   #' added to the pivot table.
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) that the "totals" data frames are associated with.
   #' @return The number of "totals" data frames associated with the specified name.
   countTotalData = function(dataName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "countTotalData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$countTotalData", "Counting total data frames...")
      df <- private$p_parentPivot$data$countTotalData(dataName=dataName)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$countTotalData", "Counted total data frames.")
      return(invisible(df))
   },

   #' @description
   #' Retrieve a "totals" data frame that was added to the
   #' pivot table.
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) that the "totals" data frame is associated with.
   #' @param variableNames The names of the variables that the totals are grouped
   #' by in the "totals" data frame (i.e. the dimensionality).
   #' @return The "totals" data frame.
   getTotalDataFrame = function(dataName=NULL, variableNames=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getTotalDataFrame", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
         checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getTotalDataFrame", variableNames, missing(variableNames), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getTotalDataFrame", "Getting total data frame...")
      df <- private$p_parentPivot$data$getTotalData(dataName=dataName, variableNames=variableNames)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getTotalDataFrame", "Got total data frame.")
      return(invisible(df))
   },

   #' @description
   #' Retrieve a calculation group in the pivot table.
   #' @param calculationGroupName The name of the calculation group to retrieve.
   #' @return The calculation group with the specified name.
   getCalculationGroup = function(calculationGroupName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculationGroup", "Getting calculation group...")
     cg <- private$p_parentPivot$calculationGroups$getCalculationGroup(calculationGroupName)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCalculationGroup", "Got calculation group.")
     return(invisible(cg))
   },

   #' @description
   #' Retrieve a calculation in the pivot table.
   #' @param calculationGroupName The name of the calculation group to retrieve.
   #' @param calculationName The name of the calculation to retrieve.
   #' @return The calculation with the specified name in the specified group.
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

   #' @description
   #' Examine the data groups and cells in a pivot table to
   #' generate the structure of the batches in preparation for evaluating the
   #' pivot table.
   #' @return The batches that exist in the pivot table.
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

   #' @description
   #' Execute the batch calculations as part of evaluating the pivot table.
   #' @return The number of batches that were evaluated.
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

   #' @description
   #' Create a new `PivotFilter` object associated with the
   #' specified data frame column name and column values.  The new filter is
   #' conceptually of the form `variableName %in% values`.
   #' @param variableName The data frame column name the filter is associated with.
   #' @param values The filter values for the filter.
   #' @return The new `PivotFilter` object.
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

   #' @description
   #' Create a new `PivotFilters` object associated with the
   #' specified data frame column name and column values.  The new filter is
   #' conceptually of the form `variableName %in% values`.
   #' @details
   #' A `PivotFilters` object is a collection of `PivotFilter` objects, therefore
   #' the return value of this method is suitable for use where other filters will
   #' subsequently be needed/applied.
   #' @param variableName The data frame column name the filter is associated with.
   #' @param values The filter values for the filter.
   #' @return The new `PivotFilter` object.
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

   #' @description
   #' Combines two `PivotFilters` objects, e.g. to intersect the filters coming
   #' from the row and column headings for a particular cell.
   #' @param filters1 A `PivotFilters` object.
   #' @param filters2 A `PivotFilters` object.
   #' @param action A character value specifying how to combine the two filters.
   #' Must be one of "intersect", "replace", "union".
   #' @return A new `PivotFilters` object.
   setFilters = function(filters1=NULL, filters2=NULL, action="replace") { # filters2 overrides filters1
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters1, missing(filters1), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", filters2, missing(filters2), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilters", action, missing(action), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilters", "Setting filters...")
     copy <- filters1$getCopy() # always copy, to avoid inadvertant bugs of a change to one filter affecting multiple cells in the Pivot Table
     copy$setFilters(filters=filters2, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilters", "Set filters.")
     return(invisible(copy))
   },

   #' @description
   #' Combines a `PivotFilters` object with a `PivotFilter` object.
   #' @param filters A `PivotFilters` object.
   #' @param filter A `PivotFilters` object.
   #' @param action A character value specifying how to combine the two filters.
   #' Must be one of "intersect", "replace", "union".
   #' @return A new `PivotFilters` object.
   setFilter = function(filters=NULL, filter=NULL, action="replace") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilter", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilter", action, missing(action), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilter", "Setting filter...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilter(filter=filter, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilter", "Set filter.")
     return(invisible(copy))
   },

   #' @description
   #' Combines a `PivotFilters` object with additional filter criteria.
   #' @param filters A `PivotFilters` object.
   #' @param variableName The name of the variable (i.e. column) in the
   #' data frame that the criteria relates to.
   #' @param values The values that the specified variable will be
   #' filtered to.
   #' @param action A character value specifying how to combine the
   #' existing filters and new filter criteria.
   #' Must be one of "intersect", "replace", "union".
   #' @return A new `PivotFilters` object.
   setFilterValues = function(filters=NULL, variableName=NULL, values=NULL, action="replace") {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "setFilterValues", action, missing(action), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilterValues", "Setting filter values...")
     copy <- filters$getCopy() # always copy, to avoid inadvertant bugs of changing one filter affecting multiple cells
     copy$setFilterValues(variableName=variableName, values=values, action=action)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$setFilterValues", "Set filter values.")
     return(invisible(copy))
   },

   #' @description
   #' Apply a set of filters to a data frame and return the filtered results.
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) to be filtered.
   #' @param dataFrame The data frame to filter.
   #' @param filters A `PivotFilters` object containing the filter criteria.
   #' @return A filtered data frame.
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

   #' @description
   #' Get the distinct values from a specified column in a data frame.
   #' @param dataFrame The data frame.
   #' @param variableName The name of the variable to get the distinct values for.
   #' @return A vector containing the distinct values.
   getDistinctValues = function(dataFrame=NULL, variableName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDistinctValues", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "getDistinctValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDistinctValues", "Getting filtered data frame...")
     safeVariableName <- processIdentifier(variableName)
     # build a dplyr query
     data <- dataFrame
     if(private$p_parentPivot$processingLibrary=="dplyr") {
       eval(parse(text=paste0("data <- dplyr::select(data, ", safeVariableName, ")")))
       data <- dplyr::distinct(data)
       eval(parse(text=paste0("data <- dplyr::arrange(data, ", safeVariableName, ")")))
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
       eval(parse(text=paste0("distinctValues <- data[order(", safeVariableName, "), .(", rcName, "=.N), by=.(", safeVariableName, ")][, ", safeVariableName, "]")))
       if(is.factor(distinctValues)) distinctValues <- as.character(distinctValues)
     }
     else stop(paste0("PivotCalculator$getDistinctValues(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getDistinctValues", "Got filtered data frame.")
     return(invisible(distinctValues))
   },

   #' @description
   #' Format a value using a variety of different methods.
   #' @param value The value to format.
   #' @param format Either a character format string to be used with `sprintf()`,
   #' a list of arguments to be used with `base::format()` or a custom R function
   #' which will be invoked once per value to be formatted.
   #' @param fmtFuncArgs If `format` is a custom R function, then `fmtFuncArgs`
   #' specifies any additional arguments (in the form of a list) that will be
   #' passed to the custom function.
   #' @return The formatted value if `format` is specified, otherwise the `value`
   #' converted to a character value.
   formatValue = function(value=NULL, format=NULL, fmtFuncArgs=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotCalculator", "formatValue", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$formatValue", "Formatting value...")
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
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
             args <- fmtFuncArgs
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
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
             args <- fmtFuncArgs
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
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
             args <- fmtFuncArgs
             args$x <- value
             value <- do.call(format, args)
          }
       }
       else value <- base::as.character(value)
     }
     else value <- base::as.character(value)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$formatValue", "Formated value.")
     return(invisible(value))
   },

   #' @description
   #' Get the working filters for a calculation by combining
   #' row-column filters and calculation filters.
   #' @param rowColFilters A `PivotFilters` object containing the combined
   #'  filters from the row data groups and column data groups.
   #' @param calcFilters Either `PivotFilters` object or a `PivotFilterOverrides`
   #'  object containing filers defined as part of the calculation.
   #' @param cell A `PivotCell` object that is the cell for which the working
   #' data filters are being calculated.
   #' @return A list of filters, element names:  calculationFilters and
   #' workingFilters.  The working filters are the row-column filters
   #' combined with the calculation filters.
   getCombinedFilters = function(rowColFilters=NULL, calcFilters=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCombinedFilters", rowColFilters, missing(rowColFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCombinedFilters", calcFilters, missing(calcFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotFilters", "PivotFilterOverrides"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "getCombinedFilters", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCombinedFilters", "Getting filters for calculation...")
     # get the final filter context for this calculation (calcFilters override row/col filters)
     # see the comments above PivotFilter$setFilters() for rules/details on how the filters are combined
     rf <- list()
     rf$calculationFilters <- calcFilters
     filters <- NULL
     if(is.null(rowColFilters)) {
       if(!isnull(calcFilters)) {
         if("PivotFilters" %in% class(calcFilters)) filters <- calcFilters$getCopy()
         else if("PivotFilterOverrides" %in% class(calcFilters)) {
           filters <- PivotFilters$new(private$p_parentPivot)
           calcFilters$apply(filters, cell)
           if(filters$isALL) filters <- NULL #i.e. if none of the calculation filters amounted to anything, then skip keeping the filters
         }
       }
     }
     else {
       filters <- rowColFilters$getCopy()
       if(!is.null(calcFilters)) {
         if("PivotFilters" %in% class(calcFilters)) filters$setFilters(filters=calcFilters, action="intersect")
         else if("PivotFilterOverrides" %in% class(calcFilters)) calcFilters$apply(filters, cell)
       }
     }
     rf$workingFilters <- filters
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getCombinedFilters", "Got filters for calculation....")
     return(invisible(rf))
   },

   #' @description
   #' Get the working filters for a named calculation by calling
   #' `getCombinedFilters()` as needed, depending on the calculation type.
   #' @param calculationName The name of the calculation.
   #' @param calculationGroupName The name of the calculation group.
   #' @param rowColFilters A `PivotFilters` object containing the combined
   #'  filters from the row data groups and column data groups.
   #' @param cell A `PivotCell` object that is the cell for which the working
   #' data filters are being calculated.
   #' @return A list of filters, where the element names are calculation names.
   #' Reminder:  Evaluating a named calculation, if `calc$type="calculation"`,
   #' can involve computing multiple named calculations, which is why this
   #' return value is a list.
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
         rf <- self$getCombinedFilters(rowColFilters=rowColFilters, calcFilters=calc$filters, cell)
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="summary") {
         rf <- self$getCombinedFilters(rowColFilters=rowColFilters, calcFilters=calc$filters, cell)
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="calculation") {
         rf <- list()
         filters[[calc$calculationName]] <- rf
       }
       else if(calc$type=="function") {
         rf <- self$getCombinedFilters(rowColFilters=rowColFilters, calcFilters=calc$filters, cell)
         filters[[calc$calculationName]] <- rf
       }
       else stop(paste0("PivotCalculator$getFiltersForNamedCalculation():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$getFiltersForNamedCalculation", "Got filters for named calculation.")
     return(invisible(filters))
   },

   #' @description
   #' Set the working data filters for a cell in the pivot table.
   #' @details
   #' The working data for a cell is a list of `PivotFilters` objects - one per
   #' named calculation.  Most cells only relate to one calculation, but
   #' calculations of type `calc$type="calculation"` can relate to multiple
   #' calculations, hence the working data is a list where the element name
   #' is the calculation name.
   #' This method calls `getFiltersForNamedCalculation()` internally to generate
   #' the filters for the working data.
   #' @param cell The cell to generate the working data for.
   #' @return No return value.
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

   #' @description
   #' Get a single value from a data frame, as part of evaluating a calculation
   #' where the calculation is of type `calc$type="value"`.
   #' @param dataFrame The data frame to retrieve the value from.
   #' @param workingFilters The relevant working data for the calculation.
   #' @param valueName The name of the variable to retrieve from the data frame.
   #' @param format The formatting to apply to the value.
   #' See `formatValue()` for details.
   #' @param fmtFuncArgs Additional arguments for a custom format function.
   #' See `formatValue()` for details.
   #' @param noDataValue A replacement raw value to use if the value is NULL.
   #' @param noDataCaption A replacement formatted value to use if the value is NULL.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
   evaluateSingleValue = function(dataFrame=NULL, workingFilters=NULL, valueName=NULL, format=NULL, fmtFuncArgs=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", valueName, missing(valueName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSingleValue", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSingleValue", "Evaluating single value...")
     data <- dataFrame
     value <- list()
     if(workingFilters$isNONE) {
       value$rawValue <- noDataValue
       if(!is.null(noDataCaption)) value$formattedValue <- noDataCaption
       else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
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
         else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
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
         value$formattedValue <- self$formatValue(rv, format=format, fmtFuncArgs=fmtFuncArgs)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSingleValue", "Evaluated single value.")
     return(invisible(value))
   },

   #' @description
   #' Get a summary value from a data frame, as part of evaluating a calculation
   #' where the calculation is of type `calc$type="summary"`.
   #' @details
   #' Where batch evaluation is used, the value is retrieved from the
   #' pre-calculated batch, otherwise dplyr/data.table is used to
   #' calculate the value (i.e. reverting to sequential evaluation mode which
   #' performs calculations cell-by-cell, one cell at a time).
   #' @param dataName The name of the data frame (as specified in
   #' `pt$addData()`) containing the data.
   #' @param dataFrame The data frame to retrieve the value from.
   #' @param workingFilters The relevant working data for the calculation.
   #' @param batchName The name of the batch that contains the results of the
   #' calculation (if batch evaluation is in use and possible for this cell and
   #' calculation).
   #' @param calculationName The name of the calculation.
   #' @param calculationGroupName The name of the calculation group.
   #' @param summaryName The name of the summary (typically also the calculation
   #' name).
   #' @param summariseExpression The dplyr or data.table expression to aggregate
   #' and summarise the data.
   #' @param format The formatting to apply to the value.
   #' See `formatValue()` for details.
   #' @param fmtFuncArgs Additional arguments for a custom format function.
   #' See `formatValue()` for details.
   #' @param noDataValue A replacement raw value to use if the value is NULL.
   #' @param noDataCaption A replacement formatted value to use if the value is NULL.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
   evaluateSummariseExpression = function(dataName=NULL, dataFrame=NULL, workingFilters=NULL, batchName=NULL,
                                          calculationName=NULL, calculationGroupName=NULL,
                                          summaryName=NULL, summariseExpression=NULL,
                                          format=NULL, fmtFuncArgs=NULL, noDataValue=NULL, noDataCaption=NULL) {
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
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateSummariseExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct"))
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
       else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
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
               else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
             }
             else {
               # format the value
               value$rawValue <- batchValue$value
               value$formattedValue <- self$formatValue(batchValue$value, format=format, fmtFuncArgs=fmtFuncArgs)
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
             else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
           }
           else {
             # calculate the value
             # todo: escaping below
             if(nrow(data)==0) return(invisible(NULL))
             summaryCmd <- paste0("data <- dplyr::summarise(data, ", processIdentifier(summaryName), " = ", summariseExpression, ")")
             eval(parse(text=summaryCmd))
             if((nrow(data)>1)||(ncol(data)>1))
               stop(paste0("PivotCalculator$evaluateSummariseExpression(): Summary expression '", summaryName, "' has resulted in '", nrow(data),
                           " row(s) and ", ncol(data), " columns.  There must be a maximum of 1 row and 1 column in the result."), call. = FALSE)
             data <- dplyr::collect(data)
             rv <- data[[1]][1] # data[[colIndex]] will always return a column as a vector (even if data is still a tbl_df)
             value$rawValue <- rv
             value$formattedValue <- self$formatValue(rv, format=format, fmtFuncArgs=fmtFuncArgs)
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
                 filterCmd <- paste0(filterCmd, "(", filter$safeVariableName, " %in% workingFilters$filters[[", j, "]]$values)")
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
             else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
           }
           else {
             # data.table query 2
             dtqry <- paste0("rv <- data[", filterCmd, ", ", summariseExpression, "]")    # opt: dtqry <- paste0("rv <- data[, ", summariseExpression, "]")
             eval(parse(text=dtqry))
             value$rawValue <- rv
             value$formattedValue <- self$formatValue(rv, format=format, fmtFuncArgs=fmtFuncArgs)
           }
         }
         else stop(paste0("PivotBatch$evaluateBatch(): Unknown processingLibrary encountered: ", private$p_parentPivot$processingLibrary), call. = FALSE)
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSummariseExpression", "Evaluated summary expression.")
     return(invisible(value))
   },

   #' @description
   #' Evaluates an R expression in order to combine the results of
   #' other calculations, as part of evaluating a calculation
   #' where the calculation is of type `calc$type="calculation"`.
   #' @details
   #' A calculation, where `calc$type="calculation"`, combines the
   #' results of other calculations using a simple R expression.
   #' @param values The results of other calculations, passed in the form
   #' of a list where the element names are the names of those other
   #' calculations.
   #' @param calculationExpression A character expression to be evaluated,
   #' e.g. "values$TotalIncome/values$SaleCount".
   #' @param format The formatting to apply to the value.
   #' See `formatValue()` for details.
   #' @param fmtFuncArgs Additional arguments for a custom format function.
   #' See `formatValue()` for details.
   #' @param noDataValue A replacement raw value to use if the value is NULL.
   #' @param noDataCaption A replacement formatted value to use if the value is NULL.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
   evaluateCalculationExpression = function(values=NULL, calculationExpression=NULL, format=NULL, fmtFuncArgs=NULL, noDataValue=NULL, noDataCaption=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", values, missing(values), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", calculationExpression, missing(calculationExpression), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculationExpression", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct"))
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
       else value$formattedValue <- self$formatValue(noDataValue, format=format, fmtFuncArgs=fmtFuncArgs)
     }
     else {
       # calculate the value
       rv <- eval(parse(text=calculationExpression))
       value <- list()
       value$rawValue <- rv
       value$formattedValue <- self$formatValue(rv, format=format, fmtFuncArgs=fmtFuncArgs)
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateSCalculationExpression", "Evaluated summary expression.")
     return(invisible(value))
   },

   #' @description
   #' Invokes a user-provided custom R function to aggregate data and
   #' perform calculations, as part of evaluating a calculation
   #' where the calculation is of type `calc$type="function"`.
   #' @details
   #' A calculation, where `calc$type="function"`, invokes a user provided
   #' R function on a cell-by-cell basis.
   #' @param workingFilters The relevant working data for the calculation.
   #' @param calculationFunction The custom R function to invoke.
   #' @param calcFuncArgs Specifies any additional arguments (in the form
   #' of a list) that will be passed to the custom calculation function.
   #' @param format The formatting to apply to the value.
   #' See `formatValue()` for details.
   #' @param fmtFuncArgs Additional arguments for a custom format function.
   #' See `formatValue()` for details.
   #' @param baseValues The results of other calculations, passed in the form
   #' of a list where the element names are the names of those other
   #' calculations.
   #' @param cell A `PivotCell` object representing the cell being calculated.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
   evaluateCalculateFunction = function(workingFilters=NULL, calculationFunction=NULL, calcFuncArgs=NULL, format=NULL, fmtFuncArgs=NULL, baseValues=NULL, cell=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", workingFilters, missing(workingFilters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", calculationFunction, missing(calculationFunction), allowMissing=FALSE, allowNull=FALSE, allowedClasses="function")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", calcFuncArgs, missing(calcFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", baseValues, missing(baseValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric", "character", "logical", "date", "Date", "POSIXct"))
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculator", "evaluateCalculateFunction", cell, missing(cell), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotCell")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCalculateFunction", "Evaluating calculation function...")
     value <- list()
     value$evaluationFilters <- workingFilters
     # calculate the value by calling the calculation function
     # first check mandatory parameters are supplied
     argNames <- methods::formalArgs(calculationFunction)
     if(!("pivotCalculator" %in% argNames)) stop("Custom calculation function missing 'pivotCalculator' argument.", call. = FALSE)
     if(!("netFilters" %in% argNames)) stop("Custom calculation function missing 'netFilters' argument.", call. = FALSE)
     if(!("format" %in% argNames)) stop("Custom calculation function missing 'format' argument.", call. = FALSE)
     if(!("baseValues" %in% argNames)) stop("Custom calculation function missing 'baseValues' argument.", call. = FALSE)
     if(!("cell" %in% argNames)) stop("Custom calculation function missing 'cell' argument.", call. = FALSE)
     # if users have older code that does not support the fmtFuncArgs or calcFuncArgs arguments, then call the custom function without those, otherwise invoke as normal
     # could perhaps use base::do.call as a generic way of constructing the call to the custom function
     if("fmtFuncArgs" %in% argNames) {
        if("calcFuncArgs" %in% argNames) {
           rv <- calculationFunction(pivotCalculator=self, netFilters=workingFilters, calcFuncArgs=calcFuncArgs, format=format, fmtFuncArgs=fmtFuncArgs, baseValues=baseValues, cell=cell)
        }
        else {
           rv <- calculationFunction(pivotCalculator=self, netFilters=workingFilters, format=format, fmtFuncArgs=fmtFuncArgs, baseValues=baseValues, cell=cell)
        }
     }
     else {
        rv <- calculationFunction(pivotCalculator=self, netFilters=workingFilters, format=format, baseValues=baseValues, cell=cell)
     }
     # build return value
     value$evaluationFilters <- rv$filters # the calculationfunction may have updated the filters
     value$rawValue <- rv$rawValue
     value$formattedValue <- rv$formattedValue
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateCalculateFunction", "Evaluated calculation function.")
     return(invisible(value))
   },

   #' @description
   #' Invokes the relevant calculation function based upon the calculation
   #' type.
   #' @details
   #' This function examines the `calc$type` property then invokes either
   #' `evaluateSingleValue()`, `evaluateSummariseExpression()`,
   #' `evaluateCalculationExpression()` or `evaluateCalculateFunction()`.
   #' Sometimes, more than one of the these functions is invoked, since
   #' calculation type "calculation" and "function" can/do make use of
   #' values from other calculations, which must be evaluated first.
   #' @param calculationName The name of the calculation to execute.
   #' @param calculationGroupName The calculation group that the
   #' calculation belongs to.
   #' @param workingData The relevant working data for the calculation.
   #' @param cell A `PivotCell` object representing the cell being calculated.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
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
       # CALCULATION TYPE: VALUE = pre-calculated value
       if(calc$type=="value") {
         df <- self$getDataFrame(calc$dataName)
         # is this a total cell?
         if((!is.null(cell))&&(cell$isTotal==TRUE)) {
           # is aggregate data in the pivot table?
           if(self$countTotalData(calc$dataName)>0) {
              variableNames <- filters$filteredVariables
              totalsDf <- self$getTotalDataFrame(dataName=calc$dataName, variableNames=variableNames)
              if(is.null(totalsDf)) {
                 value <- private$getNullValue()
              }
              else {
                 value <- self$evaluateSingleValue(dataFrame=totalsDf, workingFilters=filters,
                                                   valueName=calc$valueName, format=calc$format, fmtFuncArgs=calc$fmtFuncArgs,
                                                   noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
              }
           }
           # otherwise use a summarise expression if present
           else if(!is.null(calc$summariseExpression)) {
             batchName <- workingData[[calc$calculationName]]$batchName
             value <- self$evaluateSummariseExpression(dataName=calc$dataName, dataFrame=df, workingFilters=filters, batchName=batchName,
                                                        calculationName=calc$calculationName, calculationGroupName=calculationGroupName,
                                                        summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                                        format=calc$format, fmtFuncArgs=calc$fmtFuncArgs,
                                                        noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
           }
           # otherwise null
           else { value <- private$getNullValue() }
         }
         else {
           # not a total, so can calculate from the line level data
           value <- self$evaluateSingleValue(dataFrame=df, workingFilters=filters,
                                             valueName=calc$valueName, format=calc$format, fmtFuncArgs=calc$fmtFuncArgs,
                                             noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         }
         results[[calc$calculationName]] <- value
       }
       # CALCULATION TYPE: SUMMARY = dplyr/data.table expression
       else if(calc$type=="summary") {
         df <- self$getDataFrame(calc$dataName)
         batchName <- workingData[[calc$calculationName]]$batchName
         value <- self$evaluateSummariseExpression(dataName=calc$dataName, dataFrame=df, workingFilters=filters, batchName=batchName,
                                                    calculationName=calc$calculationName, calculationGroupName=calculationGroupName,
                                                    summaryName=calc$calculationName, summariseExpression=calc$summariseExpression,
                                                    format=calc$format, fmtFuncArgs=calc$fmtFuncArgs,
                                                    noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         results[[calc$calculationName]] <- value
       }
       # CALCULATION TYPE: CALCULATION = simple derived calculation expression
       else if(calc$type=="calculation") {
         values <- list()
         if(i>1) {
           for(j in 1:(i-1)) {
             values[[names(results)[[j]]]] <- results[[j]]$rawValue
           }
         }
         value <- self$evaluateCalculationExpression(values=values, calculationExpression=calc$calculationExpression,
                                                     format=calc$format, fmtFuncArgs=calc$fmtFuncArgs,
                                                     noDataValue=calc$noDataValue, noDataCaption=calc$noDataCaption)
         results[[calc$calculationName]] <- value
       }
       # CALCULATION TYPE: FUNCTION = custom calculation function
       else if(calc$type=="function") {
         values <- list()
         if(i>1) {
           for(j in 1:(i-1)) {
             values[[names(results)[[j]]]] <- results[[j]]$rawValue
           }
         }
         value <- self$evaluateCalculateFunction(workingFilters=filters, calculationFunction=calc$calculationFunction, calcFuncArgs=calc$calcFuncArgs,
                                                 format=calc$format, fmtFuncArgs=calc$fmtFuncArgs, baseValues=values, cell=cell)
         results[[calc$calculationName]] <- value
       }
       # CALCULATION TYPE: UNKNOWN/ERROR
       else stop(paste0("PivotCalculator$evaluateNamedCalculationWD():  Unknown calculation type encountered '", calc$type,
                        "' for calculaton name ", calc$calculationName, "' in calculation group '", calculationGroupName, "'"), call. = FALSE)
     }
     # returns a list of named results
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculator$evaluateNamedCalculationWD", "Evaluated named calculation.")
     return(invisible(results))
   },

   #' @description
   #' Invokes the relevant calculation function based upon the calculation
   #' type.
   #' @details
   #' This function is a higher-level wrapper around
   #' `evaluateNamedCalculationWD()`.  This version incorporates
   #' logic to convert the filters from the row and column data groups
   #' into the working data filters, then calls
   #' `evaluateNamedCalculationWD()`.  This version has no suffix in the
   #' name, since this is the version users are more likely to invoke,
   #' e.g. from within a custom calculation function.
   #' @param calculationName The name of the calculation to execute.
   #' @param calculationGroupName The calculation group that the
   #' calculation belongs to.
   #' @param rowColFilters The filters arising from the row and column
   #' groups.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
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

   #' @description
   #' Evaluate calculations to compute the value of a cell in a pivot table.
   #' @param cell A `PivotCell` object representing the cell to calculate.
   #' @return A list containing two elements: rawValue (typically numeric) and
   #' formattedValue (typically a character value).
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

    #' @field batchInfo A summary of the batches used in evaluating the pivot table.
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
