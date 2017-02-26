PivotData <- R6::R6Class("PivotData",
  public = list(
   initialize = function(parentPivot=NULL) {
     checkArgument("PivotData", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotData$new", "Creating new Pivot Data...")
     private$p_data <- list()
     private$p_defaultData <- NULL
     private$p_parentPivot$message("PivotData$new", "Created new Pivot Data.")
   },
   addData = function(dataName, df) {
     checkArgument("PivotData", "addData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotData", "addData", df, missing(df), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
     private$p_parentPivot$message("PivotData$addData", "Adding data...", list(dataName=dataName, df=private$getDfStr(df)))
     if(is.null(private$p_defaultData)) private$p_defaultData <- df
     private$p_data[[dataName]] <- df
     private$p_parentPivot$message("PivotData$addData", "Added data.")
   },
   getData = function(dataName) {
     checkArgument("PivotData", "getData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotData$getData", "Getting data...", list(dataName=dataName))
     if (!(dataName %in% names(private$p_data))) stop(paste0("PivotData$getData(): dataName '", dataName, "' not found."), call. = FALSE)
     data <- private$p_data[[dataName]]
     private$p_parentPivot$message("PivotData$addData", "Got data.")
     return(data)
   },
   isKnownData = function(dataName) {
     checkArgument("PivotData", "isKnownData", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotData$isKnownData", "Checking dataName...", list(dataName=dataName))
     if (!(dataName %in% names(private$p_data))) return(FALSE)
     private$p_parentPivot$message("PivotData$isKnownData", "Checked dataName.")
     return(TRUE)
   },
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
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) }
  ),
  active = list(
    count = function(value) { return(length(private$p_data)) },
    defaultData = function(value) { return(private$p_defaultData) }
  ),
  private = list(
    getDfDesc = function(df) {
      if(missing(df)|is.null(df)) return("")
      return(list(
           rows=nrow(df),
           cols=ncol(df),
           size=paste0(round(object.size(df)/1024/1024, 3), " MB"),
           colNames=names(df)
      ))
    },
    getDfStr = function(df) {
      if(missing(df)|is.null(df)) return("")
      lst <- private$getDfDesc(df)
      dfStr <- paste0(lst$rows, " rows, ", lst$cols, " cols, ", lst$size, ", col names: ", paste(lst$colNames, collapse=", "))
      return(dfStr)
    },
    p_parentPivot = NULL,
    p_defaultData = NULL,
    p_data = NULL
  )
)
