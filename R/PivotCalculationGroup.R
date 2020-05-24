
#' R6 class that defines a group of calculations.
#'
#' @description
#' The `PivotCalculationGroup` class is a container for multiple
#' `PivotCalculation` objects.  Every pivot table has at least
#' one pivot calculation group and this is sufficient for all
#' regular pivot tables.  Additional calculation groups are
#' typically only created for irregular/custom pivot tables.
#' See the "Irregular Layout" vignette for an example.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotCalculationGroup <- R6::R6Class("PivotCalculationGroup",
  public = list(

    #' @description
    #' Create a new `PivotCalculationGroup` object.
    #' @param parentPivot The pivot table that this `PivotCalculationGroup`
    #' instance belongs to.
    #' @param calculationGroupName Calculation group unique name.
    #' Recommendation: Do not have spaces in this name.
    #' @return A new `PivotCalculationGroup` object.
    initialize = function(parentPivot, calculationGroupName=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$new", "Creating new Pivot Calculation Group...")
      private$p_name <- calculationGroupName
      private$p_calculations <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$new", "Created new Pivot Calculation Group.")
    },

    #' @description
    #' Check whether a calculation already exists in this calculation group.
    #' @param calculationName group unique name.
    #' @return `TRUE` if a calculation with the specified name exists in
    #' this calculation group object, `FALSE` otherwise.
    isExistingCalculation = function(calculationName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "isExistingCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$isExistingCalculation", "Checking calculation exists...",
                                    list(calculationName=calculationName))
      calcExists <- calculationName %in% names(private$p_calculations)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$isExistingCalculation", "Checked calculation exists.")
      return(invisible(calcExists))
    },

    #' @description
    #' Retrieve a calculation by index.
    #' @param index An integer specifying the calculation to retrieve.
    #' @return The calculation that exists at the specified index.
    item = function(index) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "item", index, missing(index), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$item", "Getting calculation...")
      if(index<1) {
        stop(paste0("PivotCalculationGroups$index(): index must be greater than 0."), call. = FALSE)
      }
      if(index>length(private$p_calculations)) {
        stop(paste0("PivotCalculationGroups$index(): index must be less than or equal to ", length(private$p_calculations), "."), call. = FALSE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$item", "Got calculation.")
      return(invisible(private$p_calculations[[index]]))
    },

    #' @description
    #' Retrieve a calculation by name.
    #' @param calculationName The name of the calculation to retrieve.
    #' @return The calculation with the specified name.
    getCalculation = function(calculationName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "getCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$getCalculation", "Getting calculation...",
                                    list(calculationName=calculationName))
      calculation <- private$p_calculations[[calculationName]]
      if(is.null(calculation)) {
        stop(paste0("PivotCalculationGroups$getCalculation(): No calculation exists with the name '", calculationName, "'"), call. = FALSE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$getCalculation", "Got calculation.")
      return(invisible(calculation))
    },

    #' @description
    #' Create a new `PivotCalculation` object.
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
    defineCalculation = function(calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, fmtFuncArgs=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, calcFuncArgs=NULL,
                         basedOn=NULL, noDataValue=NULL, noDataCaption=NULL,
                         headingBaseStyleName=NULL, headingStyleDeclarations=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotFilters", "PivotFilterOverrides"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character","list","function"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", calcFuncArgs, missing(calcFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", headingBaseStyleName, missing(headingBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", headingStyleDeclarations, missing(headingStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "defineCalculation", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$defineCalculation", "Defining calculation...")
      if(calculationName %in% names(private$p_calculations)) {
        stop(paste0("PivotCalculationGroup$defineCalculation():  A Calculation already exists",
                    " in the Calculation Group with the name '", calculationName, "'.  calculationName must unique."), call. = FALSE)
      }
      if(is.null(displayOrder)) displayOrder <- length(private$p_calculations) + 1
      calculation <- PivotCalculation$new(private$p_parentPivot, calculationName=calculationName, caption=caption,
                                          visible=visible, displayOrder=displayOrder, filters=filters, format=format, fmtFuncArgs=fmtFuncArgs,
                                          dataName=dataName, type=type, valueName=valueName, summariseExpression=summariseExpression,
                                          calculationExpression=calculationExpression, calculationFunction=calculationFunction, calcFuncArgs=calcFuncArgs,
                                          basedOn=basedOn, noDataValue=noDataValue, noDataCaption=noDataCaption,
                                          headingBaseStyleName=headingBaseStyleName, headingStyleDeclarations=headingStyleDeclarations,
                                          cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations)
      private$p_calculations[[calculationName]] <- calculation
      if(is.null(private$p_defaultCalculation)) private$p_defaultCalculation <- calculationName
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotCalculationGroup$defineCalculation", "Defined calculation.")
      return(invisible(calculation))
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      if(length(private$p_calculations) > 0) {
        calcNames <- names(private$p_calculations)
        for (i in 1:length(private$p_calculations)) {
          calcName <- calcNames[i]
          lst[[calcName]] = private$p_calculations[[calcName]]$asList()
        }
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
    },

    #' @description
    #' Return a representation of this object as a character value.
    #' @param seperator A character value used when concatenating
    #' the text representations of different calculations.
    #' @return A character summary of various object properties.
    asString = function(seperator=", ") {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotCalculationGroup", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       cstr <- ""
       if(length(private$p_calculations)>0) {
         for(i in 1:length(private$p_calculations)) {
           calc <- private$p_calculations[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           cstr <- paste0(cstr, sep, calc$asString())
         }
       }
       return(cstr)
    }
  ),
  active = list(

    #' @field calculationGroupName The name of the calculation group.
    calculationGroupName = function(value) { return(invisible(private$p_name)) },

    #' @field defaultCalculationName The name of the default calculation in this
    #' calculation group.
    defaultCalculationName = function(value) { return(private$p_defaultCalculation) },

    #' @field count The number of calculations in this calculation group.
    count = function(value) { return(invisible(length(private$p_calculations))) },

    #' @field calculations A list containing the calculations in this group.
    calculations = function(value) { return(invisible(private$p_calculations)) },

    #' @field visibleCount The number of visible calculations in this calculation
    #' group.
    visibleCount = function(value) {
      cnt <- 0
      for(i in 1:length(private$p_calculations)) {
        if(private$p_calculations[[i]]$visible==TRUE) cnt<- cnt + 1
      }
      return(cnt)
    },

    #' @field visibleCalculations A list containing the visible calculations in
    #' this group.
    visibleCalculations = function(value) {
      visibleCalcs <- list()
      for(i in 1:length(private$p_calculations)) {
        if(private$p_calculations[[i]]$visible==TRUE) {
          index <- length(visibleCalcs) + 1
          visibleCalcs[[index]] <- private$p_calculations[[i]]
        }
      }
      return(invisible(visibleCalcs))
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_calculations = NULL,
    p_defaultCalculation = NULL
  )
)
