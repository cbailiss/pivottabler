
#' R6 class that contains named data frames and associated totals.
#'
#' @description
#' The PivotData class stores all of the data frames associated with a pivot
#' table.
#' Each data frame can have a set of associated "totals" data frames,
#' which are used to enable the "value" calculation type.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotData <- R6::R6Class("PivotData",
  public = list(

   #' @description
   #' Create a new `PivotData` object.
   #' @param parentPivot The pivot table that this `PivotData`
   #' instance belongs to.
   #' @return A new `PivotData` object.
   initialize = function(parentPivot=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotData", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$new", "Creating new Pivot Data...")
     private$p_data <- list()
     private$p_totalData <- list()
     private$p_defaultData <- NULL
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$new", "Created new Pivot Data.")
   },

   #' @description
   #' Add a data frame to the pivot table, specifying a name that can be used
   #' later to easily retrieve it or refer to it.
   #' @param dataFrame The data frame to add to the pivot table.
   #' @param dataName The name to assign to this data frame in the pivot table.
   #' If no name is specified, then the name of the data frame variable will
   #' be used.
   #' @return No return value.
   addData = function(dataFrame=NULL, dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "addData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "addData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$addData", "Adding data...", list(dataName=dataName, df=private$getDfStr(dataFrame)))
     if(private$p_parentPivot$processingLibrary=="data.table") {
       if(data.table::is.data.table(dataFrame)) data <- dataFrame
       else data <- data.table::as.data.table(dataFrame)
     }
     else {
       if(is.data.frame(dataFrame)) data <- dataFrame
       else stop("PivotData$addData():  The specified data is not a data frame.", call. = FALSE)
     }
     dn <- dataName
     if(is.null(dn)) dn <- deparse(substitute(dataFrame))
     if(is.null(dn)) stop("PivotData$addData(): Please specify a name for the data frame.", call. = FALSE)
     if(length(dn)==0) stop("PivotData$addData(): Please specify a name for the data frame.", call. = FALSE)
     if(is.null(private$p_defaultData)) {
       private$p_defaultData <- data
       private$p_defaultName <- dn
     }
     private$p_data[[dn]] <- data
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$addData", "Added data.")
     return(invisible())
   },

   #' @description
   #' Retrieve the data frame with the specified name.
   #' @param dataName The name that was assigned to the data frame when it was
   #' added to the pivot table.
   #' @return A data frame.
   getData = function(dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "getData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$getData", "Getting data...", list(dataName=dataName))
     if(!(dataName %in% names(private$p_data))) stop(paste0("PivotData$getData(): dataName '", dataName, "' not found."), call. = FALSE)
     data <- private$p_data[[dataName]]
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$addData", "Got data.")
     return(invisible(data))
   },

   #' @description
   #' Check if a data frame exists with the specified name.
   #' @param dataName The name that was assigned to the data frame when it was
   #' added to the pivot table.
   #' @return `TRUE` if a data frame exists with the specified name,
   #' `FALSE` otherwise.
   isKnownData = function(dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "isKnownData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$isKnownData", "Checking dataName...", list(dataName=dataName))
     if (!(dataName %in% names(private$p_data))) return(invisible(FALSE))
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$isKnownData", "Checked dataName.")
     return(invisible(TRUE))
   },

   #' @description
   #' Add pre-calculated totals/aggregate data to the pivot table.
   #' @param dataFrame The data frame to add to the pivot table.
   #' @param dataName The name of the associated data frame in the
   #' pivot table which these totals relate to.
   #' @param variableNames A character vector specifying the names
   #' of the variables which these totals are grouped by.
   #' @return No return value.
   addTotalData = function(dataFrame=NULL, dataName=NULL, variableNames=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "addTotalData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "addTotalData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "addTotalData", variableNames, missing(variableNames), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$addTotalData", "Adding totals data...", list(dataName=dataName, df=private$getDfStr(dataFrame)))
     if(private$p_parentPivot$processingLibrary=="data.table") {
       if(data.table::is.data.table(dataFrame)) data <- dataFrame
       else data <- data.table::as.data.table(dataFrame)
     }
     else {
       if(is.data.frame(dataFrame)) data <- dataFrame
       else stop("PivotData$addTotalData():  The specified data is not a data frame.", call. = FALSE)
     }
     dn <- dataName
     if(is.null(dn)) dn <- private$p_defaultName
     if(is.null(dn)) stop("PivotData$addTotalData(): Please specify the data frame name that these totals/aggregate data relate to.", call. = FALSE)
     if(length(dn)==0) stop("PivotData$addTotalData(): Please specify the data frame name that these totals/aggregate data relate to.", call. = FALSE)
     if(!(dn %in% names(private$p_totalData))) {
       private$p_totalData[[dn]] <- list() # this list will contain N sets of totals data, each of which is a list(varNames=variableNames, df<-data)
     }
     newIndex <- length(private$p_totalData$dn)+1
     private$p_totalData$dn[[newIndex]] <- list()
     private$p_totalData$dn[[newIndex]]$varNames <- variableNames
     private$p_totalData$dn[[newIndex]]$df <- data
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$addTotalData", "Added totals data.")
     return(invisible())
   },

   #' @description
   #' Count the number of data frames containing total/aggregate data that
   #' exist in the pivot table associated with a specific named data frame.
   #' @param dataName The name of the associated data frame in the
   #' pivot table which these totals relate to.
   #' @return The number of total/aggregate data frames that exist in the
   #' pivot table associated with the specified data frame name.
   countTotalData = function(dataName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "countTotalData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$countTotalData", "Counting total data...", list(dataName=dataName))
     dn <- dataName
     if(is.null(dn)) dn <- private$p_defaultName
     if(is.null(dn)) stop("PivotData$countTotalData(): Please specify the data frame name.", call. = FALSE)
     if(length(dn)==0) stop("PivotData$countTotalData(): Please specify the data frame name.", call. = FALSE)
     if(!(dn %in% names(private$p_totalData))) return(invisible(0))
     if(length(private$p_totalData$dn)==0) return(invisible(0))
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$countTotalData", "Counted total data.", list(data=data))
     return(invisible(length(private$p_totalData$dn)))
   },

   #' @description
   #' Retrieve pre-calculated totals/aggregate data from the pivot table.
   #' @param dataName The name of the associated data frame in the
   #' pivot table which these totals relate to.
   #' @param variableNames A character vector specifying the names
   #' of the variables which the totals are grouped by.
   #' @return A data frame.
   getTotalData = function(dataName=NULL, variableNames=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "getTotalData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotData", "getTotalData", variableNames, missing(variableNames), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$getTotalData", "Getting total data...", list(dataName=dataName))
     dn <- dataName
     if(is.null(dn)) dn <- private$p_defaultName
     if(is.null(dn)) stop("PivotData$getTotalData(): Please specify the data frame name.", call. = FALSE)
     if(length(dn)==0) stop("PivotData$getTotalData(): Please specify the data frame name.", call. = FALSE)
     if(!(dn %in% names(private$p_totalData))) return(invisible(NULL))
     if(length(private$p_totalData$dn)==0) return(invisible(NULL))
     data <- NULL
     for(i in 1:length(private$p_totalData$dn)) {
       vd <- private$p_totalData$dn[[i]]
       if(is.null(vd)) next
       varNames <- vd$varNames
       if(length(varNames)!=length(variableNames)) next
       if(is.null(varNames)&&is.null(variableNames)) {
         data <- vd$df
         break
       }
       if(length(intersect(varNames, variableNames))==length(varNames)) {
         data <- vd$df
         break
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotData$getTotalData", "Got total data.", list(data=data))
     return(invisible(data))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list()
     if(length(private$p_data) > 0) {
       for (i in 1:length(private$p_data)) {
         dataname <- names(private$p_data)[i]
         df <- private$p_data[[dataname]]
         dlst <- private$getDfDesc(df)
         lst[[dataname]] = dlst
       }
       lst <- lst[order(names(lst))]
     }
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

    #' @field count The number of named data frames in the pivot table
    #' (excluding totals/aggregate data frames).
    count = function(value) { return(invisible(length(private$p_data))) },

    #' @field defaultData The default data frame in the pivot table.
    defaultData = function(value) { return(invisible(private$p_defaultData)) },

    #' @field defaultName The name of the default data frame in the pivot table.
    defaultName = function(value) { return(invisible(private$p_defaultName)) }
  ),
  private = list(
    getDfDesc = function(df) {
      if(missing(df)||is.null(df)) return("")
      return(list(
           rows=nrow(df),
           cols=ncol(df),
           size=paste0(round(object.size(df)/1024/1024, 3), " MB"),
           colNames=names(df)
      ))
    },
    getDfStr = function(df) {
      if(missing(df)||is.null(df)) return("")
      lst <- private$getDfDesc(df)
      dfStr <- paste0(lst$rows, " rows, ", lst$cols, " cols, ", lst$size, ", col names: ", paste(lst$colNames, collapse=", "))
      return(dfStr)
    },
    p_parentPivot = NULL,
    p_defaultData = NULL,
    p_defaultName = NULL,
    p_data = NULL,
    p_totalData = NULL
  )
)
