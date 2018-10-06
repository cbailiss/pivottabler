#' A class that defines a calculation.
#'
#' The PivotCalculation class defines one calculation in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a single pivot table calculation.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field calculationName Calculation unique name.
#' @field safeName The name of the calculation, surrounded by back-ticks, if
#'   the calculationName is not legal.
#' @field caption Calculation display name - i.e. the name shown in the pivot
#'   table.
#' @field visible Show or hide the calculation.  Hidden calculations are
#'   typically used as base values for other calculations.
#' @field displayOrder The order the calculations are displayed in the pivot
#'   table.
#' @field filters Any data filters specific to this calculation.  A PivotFilters
#'   object.
#' @field format A character, list or custom function to format the calculation
#'   result.
#' @field dataName Specifies which data frame in the pivot table is used for
#'   this calculation.
#' @field type The calculation type:  "summary", "calculation", "function" or
#'   "value".
#' @field valueName For type="value", the name of the column containing the
#'   value to display in the pivot table.
#' @field summariseExpression For type="summary", either the dplyr expression to
#'   use with dplyr::summarise() or a data.table calculation expression.
#' @field calculationExpression For type="calculation", an expression to combine
#'   aggregate values.
#' @field calculationFunction For type="function", a reference to a custom R
#'   function that will carry out the calculation.
#' @field basedOn A character vector specifying the names of one or more
#'   calculations that this calculation depends on.
#' @field noDataValue An integer or numeric value specifying the value to use if
#'   no data exists for a particular cell.
#' @field noDataCaption A character value that will be displayed by the pivot
#'   table if no  data exists for a particular cell.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot calculation, specifying the field
#'   values documented above.}
#'
#'   \item{\code{asList()}}{Get a list representation of this calculation.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this calculation.}
#'   \item{\code{asString()}}{Get a text representation of this calculation.}
#' }

PivotCalculation <- R6::R6Class("PivotCalculation",
  public = list(
   initialize = function(parentPivot, calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, basedOn=NULL,
                         noDataValue=NULL, noDataCaption=NULL) {
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", visible, missing(visible), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculation", "initialize", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     private$p_parentPivot <- parentPivot
     fstr <- NULL
     if(!is.null(filters)) {
       if (!("PivotFilters" %in% class(filters))) stop("PivotCalculation$new(): filters must be of type PivotFilters", call. = FALSE)
       fstr <- filters$asString()
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculation$new", "Creating new Pivot Calculation...",
                                                                             list(calculationName=calculationName, caption=caption, visible=visible,
                                                                             displayOrder=displayOrder, filters=fstr, format=format, dataName=dataName,
                                                                             valueName=valueName, summariseExpression=summariseExpression,
                                                                             calculationExpression=calculationExpression,
                                                                             calculationFunctionIsNull=is.null(calculationFunction), basedOn=basedOn,
                                                                             noDataValue=noDataValue, noDataCaption=noDataCaption))
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
     private$p_safeName <- processIdentifier(calculationName)
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
     private$p_noDataValue <- noDataValue
     private$p_noDataCaption <- noDataCaption
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculation$new", "Created new Pivot Calculation")
   },
   asList = function() {
     lst <- list(
       name = private$p_name,
       safeName = private$p_safeName,
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
       basedOn = private$p_basedOn,
       noDataValue = private$p_noDataValue,
       noDataCaption = private$p_noDataCaption
     )
     return(invisible(lst))
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
    calculationName = function(value) { return(invisible(private$p_name)) },
    calculationSafeName = function(value) { return(invisible(private$p_safeName)) },
    caption = function(value) { return(invisible(private$p_caption)) },
    visible = function(value) { return(invisible(private$p_visible)) },
    displayOrder = function(value) { return(invisible(private$p_displayOrder)) },
    filters = function(value) { return(invisible(private$p_filters)) },
    format = function(value) { return(invisible(private$p_format)) },
    dataName = function(value) { return(invisible(private$p_dataName)) },
    type = function(value) { return(invisible(private$p_type)) },
    valueName = function(value) { return(invisible(private$p_valueName)) },
    summariseExpression = function(value) { return(invisible(private$p_summariseExpression)) },
    calculationExpression = function(value) { return(invisible(private$p_calculationExpression)) },
    calculationFunction = function(value) { return(invisible(private$p_calculationFunction)) },
    basedOn = function(value) { return(invisible(private$p_basedOn)) },
    noDataValue = function(value) { return(invisible(private$p_noDataValue)) },
    noDataCaption = function(value) { return(invisible(private$p_noDataCaption)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_safeName = NULL,
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
    p_basedOn = NULL,
    p_noDataValue = NULL,
    p_noDataCaption = NULL
  )
)
