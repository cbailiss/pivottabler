PivotCalculation <- R6::R6Class("PivotCalculation",
  public = list(
   initialize = function(parentPivot, calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, basedOn=NULL) {
     checkArgument("PivotCalculation", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotCalculation", "initialize", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", visible, missing(visible), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     checkArgument("PivotCalculation", "initialize", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCalculation", "initialize", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculation", "initialize", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
     checkArgument("PivotCalculation", "initialize", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculation", "initialize", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
     checkArgument("PivotCalculation", "initialize", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot <- parentPivot
     fstr <- NULL
     if(!is.null(filters)) {
       if (!("PivotFilters" %in% class(filters))) stop("PivotCalculation$new(): filters must be of type PivotFilters", call. = FALSE)
       fstr <- filters$asString()
     }
     private$p_parentPivot$message("PivotCalculation$new", "Creating new Pivot Calculation...",
                                   list(calculationName=calculationName, caption=caption, visible=visible,
                                   displayOrder=displayOrder, filters=fstr, format=format, dataName=dataName,
                                   valueName=valueName, summariseExpression=summariseExpression,
                                   calculationExpression=calculationExpression,
                                   calculationFunctionIsNull=is.null(calculationFunction), basedOn=basedOn))
     if(missing(caption)|is.null(caption)) caption <- calculationName
     if((!(missing(dataName)))&(!is.null(dataName))) {
       if(!private$p_parentPivot$data$isKnownData(dataName))
         stop(paste0("PivotCalculation$new():  Data Frame with name '", dataName, "' not found in the Pivot Data."), call. = FALSE)
     }
     if(type=="value") {
       if (missing(valueName)|is.null(valueName))
         stop("PivotCalculation$new():  For type=value, a valueName must be specified.", call. = FALSE)
       if((missing(dataName))|(is.null(dataName)))
         stop(paste0("PivotCalculation$new():  For type=value, a dataName must be specified."), call. = FALSE)
     }
     if(type=="summary") {
       if(missing(summariseExpression)|is.null(summariseExpression))
         stop("PivotCalculation$new():  For type=summary, a summariseExpression must be specified.", call. = FALSE)
       if((missing(dataName))|(is.null(dataName)))
         stop(paste0("PivotCalculation$new():  For type=summary, a dataName must be specified."), call. = FALSE)
     }
     if((type=="calculation")&(missing(calculationExpression)|is.null(calculationExpression))) {
       stop("PivotCalculation$new():  For type=calculation, a calculationExpression must be specified.", call. = FALSE)
     }
     if((type=="function")&(missing(calculationFunction)|is.null(calculationFunction))) {
       stop("PivotCalculation$new():  For type=function, a calculationFunction must be specified.", call. = FALSE)
     }

     private$p_name <- calculationName
     private$p_caption <- caption
     private$p_visible <- visible
     private$p_displayOrder <- displayOrder
     private$p_filters <- filters
     private$p_format <- format
     private$p_dataName <- dataName
     private$p_type <- type
     private$p_valueName <- valueName
     private$p_summariseExpression <- summariseExpression
     private$p_calculationExpression <- calculationExpression
     private$p_calculationFunction <- calculationFunction
     private$p_basedOn <- basedOn
     private$p_parentPivot$message("PivotCalculation$new", "Created new Pivot Calculation")
   },
   asList = function() {
     lst <- list(
       name = private$p_name,
       caption = private$p_caption,
       visible = private$p_visible,
       displayOrder = private$p_displayOrder,
       filters = private$p_filters,
       format = private$p_format,
       dataName = private$p_dataName,
       type = private$p_type,
       valueName = private$p_valueName,
       summariseExpression = private$p_summariseExpression,
       calculationExpression = private$p_calculationExpression,
       calculationFunction = private$p_calculationFunction,
       basedOn = private$p_basedOn
     )
     return(lst)
   },
   asJSON = function() { return(jsonlite::toJSON(self$asList())) },
   asString = function() {
     cstr <- NULL
     fstr <- NULL
     if(!is.null(private$p_filters)) fstr <- paste0(" with filters: ", private$p_filters$asString())
     cstr <- paste0(name, " = ", type, ": ", ifelse(is.null(private$p_dataName), "", paste0(p_dataName, ": ")),
                    switch("type", "value"=private$p_valueName, "summary"=private$p_summariseExpression,
                           "calculation"=private$p_calculationExpression,
                           "function"=private$p_calculationFunction, "unknown"), fstr)
     return(cstr)
   }
  ),
  active = list(
    calculationName = function(value) { return(private$p_name) },
    caption = function(value) { return(private$p_caption) },
    visible = function(value) { return(private$p_visible) },
    displayOrder = function(value) { return(private$p_displayOrder) },
    filters = function(value) { return(private$p_filters) },
    format = function(value) { return(private$p_format) },
    dataName = function(value) { return(private$p_dataName) },
    type = function(value) { return(private$p_type) },
    valueName = function(value) { return(private$p_valueName) },
    summariseExpression = function(value) { return(private$p_summariseExpression) },
    calculationExpression = function(value) { return(private$p_calculationExpression) },
    calculationFunction = function(value) { return(private$p_calculationFunction) },
    basedOn = function(value) { return(private$p_basedOn) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_caption = NULL,
    p_visible = NULL,
    p_displayOrder = NULL,
    p_filters = NULL,
    p_format = NULL,
    p_dataName = NULL,
    p_type = NULL,
    p_valueName = NULL,
    p_summariseExpression = NULL,
    p_calculationExpression = NULL,
    p_calculationFunction = NULL,
    p_basedOn = NULL
  )
)
