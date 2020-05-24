
#' R6 class that defines a calculation.
#'
#' @description
#' The `PivotCalculation` class defines one calculation in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCalculation <- R6::R6Class("PivotCalculation",
  public = list(

    #' @description
    #' Create a new `PivotCalculation` object.
    #' @param parentPivot The pivot table that this `PivotCalculation`
    #' instance belongs to.
    #' @param calculationName Calculation unique name.
    #' @param caption Calculation display name
    #' @param visible `TRUE` to show the calculation in the pivot table or `FALSE`
    #' to hide it.  Hidden calculations are typically used as base values for
    #' other calculations.
    #' @param displayOrder The order the calculations are displayed in the
    #' pivot table.
    #' @param filters Any additional data filters specific to this calculation.
    #' This can be a `PivotFilters` object that further restricts the data for the
    #' calculation or a list of individual `PivotFilter` objects that provide more
    #' flexibility (and/or/replace).  See the Calculations vignette for details.
    #' @param format A character, list or custom function to format the calculation
    #' result.
    #' @param fmtFuncArgs A list that specifies any additional arguments to pass to
    #' a custom format function.
    #' @param dataName Specifies which data frame in the pivot table is used for
    #' this calculation (as specified in `pt$addData()`).
    #' @param type The calculation type:  "summary", "calculation", "function" or
    #' "value".
    #' @param valueName For type="value", the name of the column containing the
    #' value to display in the pivot table.
    #' @param summariseExpression For type="summary", either the dplyr expression to
    #' use with dplyr::summarise() or a data.table calculation expression.
    #' @param calculationExpression For type="calculation", an expression to combine
    #' aggregate values.
    #' @param calculationFunction For type="function", a reference to a custom R
    #' function that will carry out the calculation.
    #' @param calcFuncArgs For type="function", a list that specifies additional
    #' arguments to pass to calculationFunction.
    #' @param basedOn A character vector specifying the names of one or more
    #' calculations that this calculation depends on.
    #' @param noDataValue An integer or numeric value specifying the value to use if
    #' no data exists for a particular cell.
    #' @param noDataCaption A character value that will be displayed by the pivot
    #' table if no  data exists for a particular cell.
    #' @param headingBaseStyleName The name of a style defined in the pivot table
    #' to use as the base styling for the data group heading.
    #' @param headingStyleDeclarations A list of CSS style declarations (e.g.
    #' `list("font-weight"="bold")`) to override the base style.
    #' @param cellBaseStyleName The name of a style defined in the pivot table to
    #' use as the base styling for the cells related to this calculation.
    #' @param cellStyleDeclarations A list of CSS style declarations (e.g.
    #' `list("font-weight"="bold")`) to override the base style.
    #' @return A new `PivotCalculation` object.
   initialize = function(parentPivot, calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, fmtFuncArgs=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, calcFuncArgs=NULL,
                         basedOn=NULL, noDataValue=NULL, noDataCaption=NULL,
                         headingBaseStyleName=NULL, headingStyleDeclarations=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", visible, missing(visible), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotFilters", "PivotFilterOverrides"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calcFuncArgs, missing(calcFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", headingBaseStyleName, missing(headingBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", headingStyleDeclarations, missing(headingStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     private$p_parentPivot <- parentPivot
     fstr <- NULL
     if(!is.null(filters)) fstr <- filters$asString()
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculation$new", "Creating new Pivot Calculation...",
                                                                             list(calculationName=calculationName, caption=caption, visible=visible,
                                                                             displayOrder=displayOrder, filters=fstr,
                                                                             format=format, fmtFuncArgs=fmtFuncArgs, dataName=dataName,
                                                                             valueName=valueName, summariseExpression=summariseExpression,
                                                                             calculationExpression=calculationExpression,
                                                                             calculationFunctionIsNull=is.null(calculationFunction), calcFuncArgs=calcFuncArgs,
                                                                             basedOn=basedOn, noDataValue=noDataValue, noDataCaption=noDataCaption,
                                                                             headingBaseStyleName=headingBaseStyleName, headingStyleDeclarations=headingStyleDeclarations,
                                                                             cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations))
     if(grepl("`", calculationName)==TRUE)
       stop("PivotCalculation$new():  calculationName must not contain any back-tick characters.", call. = FALSE)
     if(missing(caption)||is.null(caption)) caption <- calculationName
     if((!(missing(dataName)))&&(!is.null(dataName))) {
       if(!private$p_parentPivot$data$isKnownData(dataName))
         stop(paste0("PivotCalculation$new():  Data Frame with name '", dataName, "' not found in the Pivot Data."), call. = FALSE)
     }
     if(type=="value") {
       if (missing(valueName)||is.null(valueName))
         stop("PivotCalculation$new():  For type=value, a valueName must be specified.", call. = FALSE)
       if((missing(dataName))||(is.null(dataName))) {
         if (private$p_parentPivot$data$count < 1)
           stop(paste0("PivotCalculation$new():  For type=value, a dataName must be specified."), call. = FALSE)
         dataName <- private$p_parentPivot$data$defaultName
       }
     }
     if(type=="summary") {
       if(missing(summariseExpression)||is.null(summariseExpression))
         stop("PivotCalculation$new():  For type=summary, a summariseExpression must be specified.", call. = FALSE)
       if((missing(dataName))||(is.null(dataName))) {
         if (private$p_parentPivot$data$count < 1)
           stop(paste0("PivotCalculation$new():  For type=summary, a dataName must be specified."), call. = FALSE)
         dataName <- private$p_parentPivot$data$defaultName
       }
     }
     if((type=="calculation")&&(missing(calculationExpression)||is.null(calculationExpression))) {
       stop("PivotCalculation$new():  For type=calculation, a calculationExpression must be specified.", call. = FALSE)
     }
     if((type=="function")&&(missing(calculationFunction)||is.null(calculationFunction))) {
       stop("PivotCalculation$new():  For type=function, a calculationFunction must be specified.", call. = FALSE)
     }

     private$p_name <- calculationName
     private$p_caption <- caption
     private$p_visible <- visible
     private$p_displayOrder <- displayOrder
     private$p_filters <- filters
     private$p_format <- format
     private$p_fmtFuncArgs <- fmtFuncArgs
     private$p_dataName <- dataName
     private$p_type <- type
     private$p_valueName <- valueName
     private$p_summariseExpression <- summariseExpression
     private$p_calculationExpression <- calculationExpression
     private$p_calculationFunction <- calculationFunction
     private$p_calcFuncArgs <- calcFuncArgs
     private$p_basedOn <- basedOn
     private$p_noDataValue <- noDataValue
     private$p_noDataCaption <- noDataCaption
     private$p_headingBaseStyleName <- headingBaseStyleName
     private$p_headingStyleDeclarations <- headingStyleDeclarations
     private$p_cellBaseStyleName <- cellBaseStyleName
     private$p_cellStyleDeclarations <- cellStyleDeclarations
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculation$new", "Created new Pivot Calculation")
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       name = private$p_name,
       caption = private$p_caption,
       visible = private$p_visible,
       displayOrder = private$p_displayOrder,
       filters = private$p_filters,
       format = private$p_format,
       fmtFuncArgs = private$p_fmtFuncArgs,
       dataName = private$p_dataName,
       type = private$p_type,
       valueName = private$p_valueName,
       summariseExpression = private$p_summariseExpression,
       calculationExpression = private$p_calculationExpression,
       calculationFunction = private$p_calculationFunction,
       calcFuncArgs = private$p_calcFuncArgs,
       basedOn = private$p_basedOn,
       noDataValue = private$p_noDataValue,
       noDataCaption = private$p_noDataCaption,
       headingBaseStyleName = private$p_headingBaseStyleName,
       headingStyleDeclarations = private$p_headingStyleDeclarations,
       cellBaseStyleName = private$p_cellBaseStyleName,
       cellStyleDeclarations = private$p_cellStyleDeclarations
     )
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
   },

   #' @description
   #' Return a representation of this object as a character value.
   #' @return A character summary of various object properties.
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

    #' @field calculationName Calculation unique name.
    calculationName = function(value) { return(invisible(private$p_name)) },

    #' @field caption Calculation display name
    caption = function(value) { return(invisible(private$p_caption)) },

    #' @field visible `TRUE` to show the calculation in the pivot table or `FALSE`
    #' to hide it.  Hidden calculations are typically used as base values for
    #' other calculations.
    visible = function(value) { return(invisible(private$p_visible)) },

    #' @field displayOrder The order the calculations are displayed in the
    #' pivot table.
    displayOrder = function(value) { return(invisible(private$p_displayOrder)) },

    #' @field filters Any additional data filters specific to this calculation.
    #' This can be a `PivotFilters` object that further restricts the data for the
    #' calculation or a list of individual `PivotFilter` objects that provide more
    #' flexibility (and/or/replace).  See the Calculations vignette for details.
    filters = function(value) { return(invisible(private$p_filters)) },

    #' @field format A character, list or custom function to format the calculation
    #' result.
    format = function(value) { return(invisible(private$p_format)) },

    #' @field fmtFuncArgs A list that specifies any additional arguments to pass to
    #' a custom format function.
    fmtFuncArgs = function(value) { return(invisible(private$p_fmtFuncArgs)) },

    #' @field dataName Specifies which data frame in the pivot table is used for
    #' this calculation (as specified in `pt$addData()`).
    dataName = function(value) { return(invisible(private$p_dataName)) },

    #' @field type The calculation type:  "summary", "calculation", "function" or
    #' "value".
    type = function(value) { return(invisible(private$p_type)) },

    #' @field valueName For type="value", the name of the column containing the
    #' value to display in the pivot table.
    valueName = function(value) { return(invisible(private$p_valueName)) },

    #' @field summariseExpression For type="summary", either the dplyr expression to
    #' use with dplyr::summarise() or a data.table calculation expression.
    summariseExpression = function(value) { return(invisible(private$p_summariseExpression)) },

    #' @field calculationExpression For type="calculation", an expression to combine
    #' aggregate values.
    calculationExpression = function(value) { return(invisible(private$p_calculationExpression)) },

    #' @field calculationFunction For type="function", a reference to a custom R
    #' function that will carry out the calculation.
    calculationFunction = function(value) { return(invisible(private$p_calculationFunction)) },

    #' @field calcFuncArgs For type="function", a list that specifies additional
    #' arguments to pass to calculationFunction.
    calcFuncArgs = function(value) { return(invisible(private$p_calcFuncArgs)) },

    #' @field basedOn A character vector specifying the names of one or more
    #' calculations that this calculation depends on.
    basedOn = function(value) { return(invisible(private$p_basedOn)) },

    #' @field noDataValue An integer or numeric value specifying the value to use if
    #' no data exists for a particular cell.
    noDataValue = function(value) { return(invisible(private$p_noDataValue)) },

    #' @field noDataCaption A character value that will be displayed by the pivot
    #' table if no  data exists for a particular cell.
    noDataCaption = function(value) { return(invisible(private$p_noDataCaption)) },

    #' @field headingBaseStyleName The name of a style defined in the pivot table
    #' to use as the base styling for the data group heading.
    headingBaseStyleName = function(value) { return(invisible(private$p_headingBaseStyleName)) },

    #' @field headingStyleDeclarations A list of CSS style declarations (e.g.
    #' `list("font-weight"="bold")`) to override the base style.
    headingStyleDeclarations = function(value) { return(invisible(private$p_headingStyleDeclarations)) },

    #' @field cellBaseStyleName The name of a style defined in the pivot table to
    #' use as the base styling for the cells related to this calculation.
    cellBaseStyleName = function(value) { return(invisible(private$p_cellBaseStyleName)) },

    #' @field cellStyleDeclarations A list of CSS style declarations (e.g.
    #' `list("font-weight"="bold")`) to override the base style.
    cellStyleDeclarations = function(value) { return(invisible(private$p_cellStyleDeclarations)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_caption = NULL,
    p_visible = NULL,
    p_displayOrder = NULL,
    p_filters = NULL,
    p_format = NULL,
    p_fmtFuncArgs = NULL,
    p_dataName = NULL,
    p_type = NULL,
    p_valueName = NULL,
    p_summariseExpression = NULL,
    p_calculationExpression = NULL,
    p_calculationFunction = NULL,
    p_calcFuncArgs = NULL,
    p_basedOn = NULL,
    p_noDataValue = NULL,
    p_noDataCaption = NULL,
    p_headingBaseStyleName=NULL,
    p_headingStyleDeclarations=NULL,
    p_cellBaseStyleName=NULL,
    p_cellStyleDeclarations=NULL
  )
)
