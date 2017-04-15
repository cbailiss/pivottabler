#' A class that defines a pivot table.
#'
#' The PivotTable class represents a pivot table and is the primary class for
#' constructing and interacting with the pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmlwidgets
#' @import htmltools
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a pivot table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # The package vignettes have many more examples of working with the
#' # PivotTable class.
#' library(pivottabler)
#' pt <- PivotTable$new()
#' pt$addData(bhmtrains)
#' pt$addColumnDataGroups("TrainCategory")
#' pt$addRowDataGroups("TOC")
#' pt$defineCalculation(calculationName="TotalTrains",
#' summariseExpression="n()")
#' pt$renderPivot()
#' @field data A PivotData object containing the data frames used to populate
#'   the pivot table.
#' @field rowGroup The top PivotDataGroup in the parent-child hierarchy of row
#'   data groups.
#' @field columnGroup The top PivotDataGroup in the parent-child hierarchy of
#'   column data groups.
#' @field calculationGroups A PivotCalculationGroups object containing all of
#'   the pivot calculations in the pivot table.
#' @field calculationsPosition "row" or "column" indicating where the
#'   calculation names will appear (only if multiple calculations are defined
#'   and visible in the pivot table).
#' @field cells A PivotCells object containing all of the cells in the body of
#'   the pivot table.
#' @field theme The name of the theme currently applied to the pivot table.
#' @field styles A PivotStyles object containing the styles used to theme the
#'   pivot table.
#' @field allowExternalStyles Enable support for external styles, when producing
#'   content for external systems.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(messages=FALSE, messageFile=NULL)}}{Create a new pivot
#'   table, including optionally enabling debug logging.}
#'
#'   \item{\code{addData(df, dataName)}}{Add a data frame with the specified
#'   name to the pivot table.}
#'   \item{\code{getTopColumnGroups()}}{Get the very top column PivotDataGroup
#'   that sits at the top of the parent-child hierarchy.}
#'   \item{\code{getLeafColumnGroups()}}{Get the PivotDataGroups at the bottom
#'   of the column heading parent-child hierarchy.}
#'   \item{\code{addColumnDataGroups(variableName, atLevel, fromData=TRUE,
#'   dataName, dataSortOrder="asc", dataFormat, onlyCombinationsThatExist=TRUE,
#'   explicitListOfValues, calculationGroupName, expandExistingTotals=FALSE,
#'   addTotal=TRUE, visualTotals=FALSE, totalPosition="after",
#'   totalCaption="Total")}}{Generate new column heading data groups based on
#'   the distinct values in a data frame or using explicitly specified data
#'   values.}
#'   \item{\code{normaliseColumnGroups() }}{Normalise the column heading data
#'   group hierachy so that all branches have the same number of levels -
#'   accomplished by adding empty child data groups where needed.}
#'   \item{\code{sortColumnDataGroups(levelNumber=1, orderBy="calculation",
#'   sortOrder="desc", calculationGroupName="default", calculationName)}}{Sort
#'   the column heading data groups either by the data group data value, caption
#'   or based on calculation result values.}
#'   \item{\code{getTopRowGroups()}}{Get the left-most row PivotDataGroup that
#'   sits at the top of the parent-child hierarchy.}
#'   \item{\code{getLeafRowGroups()}}{Get the PivotDataGroups at the bottom of
#'   the row heading parent-child hierarchy.}
#'   \item{\code{addRowDataGroups(variableName, atLevel, fromData=TRUE,
#'   dataName, dataSortOrder="asc", dataFormat, onlyCombinationsThatExist=TRUE,
#'   explicitListOfValues, calculationGroupName, expandExistingTotals=FALSE,
#'   addTotal=TRUE, visualTotals=FALSE, totalPosition="after",
#'   totalCaption="Total")}}{Generate new row heading data groups based on the
#'   distinct values in a data frame or using explicitly specified data values.}
#'   \item{\code{normaliseRowGroups()}}{Normalise the row heading data group
#'   hierachy so that all branches have the same number of levels - accomplished
#'   by adding empty child data groups where needed.}
#'   \item{\code{sortRowDataGroups(levelNumber=1, orderBy="calculation",
#'   sortOrder="desc", calculationGroupName="default", calculationName)}}{Sort
#'   the row heading data groups either by the data group data value, caption or
#'   based on calculation result values.}
#'   \item{\code{addCalculationGroup(calculationGroupName)}}{Create a new
#'   calculation group (rarely needed since the default group is sufficient for
#'   almost all scenarios).}
#'   \item{\code{defineCalculation(calculationGroupName="default",
#'   calculationName, caption, visible=TRUE, displayOrder, filters, format,
#'   dataName, type="summary", valueName, summariseExpression,
#'   calculationExpression, calculationFunction, basedOn, noDataValue,
#'   noDataCaption)}}{Define a new calculation.  See the PivotCalculation class
#'   for details.}
#'   \item{\code{addColumnCalculationGroups(calculationGroupName="default",
#'   atLevel)}}{Add calculation names on columns (if more than one calculation
#'   is defined and visible, then the calculation names will appear as column
#'   headings).}
#'   \item{\code{addRowCalculationGroups(calculationGroupName="default",
#'   atLevel)}}{Add calculation names on rows (if more than one calculation is
#'   defined and visible, then the calculation names will appear as row
#'   headings).}
#'   \item{\code{addStyle(styleName, declarations)}}{Define a new PivotStyle and
#'   add it to the PivotStyles collection.}
#'   \item{\code{createInlineStyle(baseStyleName, declarations)}}{Create a
#'   PivotStyle object that can be used to style individual cell in the pivot
#'   table.}
#'   \item{\code{generateCellStructure()}}{Generate the empty pivot table cells
#'   (after the row/column headings have been defined).}
#'   \item{\code{resetCells()}}{Clear the cells of the pivot table (should be
#'   done automatically after structural changes have been made to the pivot
#'   table).}
#'   \item{\code{evaluateCells()}}{Calculate the values of the cells in the body
#'   of the pivot table.}
#'   \item{\code{evaluatePivot()}}{A wrapper for calling
#'   normaliseColumnGroups(), normaliseRowGroups(), generateCellStructure() and
#'   evaluateCells() in sequence.}
#'   \item{\code{asMatrix(includeHeaders=TRUE, repeatHeaders=FALSE,
#'   rawValue=FALSE)}}{Gets the pivot table as a matrix, with or without
#'   headings.}
#'   \item{\code{asDataFrame(separator=" ")}}{Gets the pivot table as a data
#'   frame, combining multiple levels of headings with the specified separator.}
#'   \item{\code{getCss(styleNamePrefix)}}{Get the CSS declarations for the
#'   entire pivot table.}
#'   \item{\code{getHtml(styleNamePrefix, includeHeaderValues=FALSE,
#'   includeRCFilters=FALSE, includeCalculationFilters=FALSE,
#'   includeCalculationNames=FALSE, includeRawValue=FALSE)}}{Get the HTML
#'   representation of the pivot table, specifying the CSS style name prefix to
#'   use and whether additional debug information should be included in the
#'   pivot table.}
#'   \item{\code{saveHtml(filePath, fullPageHTML=TRUE, styleNamePrefix,
#'   includeHeaderValues=FALSE, includeRCFilters=FALSE,
#'   includeCalculationFilters=FALSE, includeCalculationNames=FALSE,
#'   includeRawValue=FALSE)}}{Save the HTML representation of the pivot table to
#'   a file.}
#'   \item{\code{renderPivot(width, height, styleNamePrefix,
#'   includeHeaderValues=FALSE, includeRCFilters=FALSE,
#'   includeCalculationFilters=FALSE, includeCalculationNames=FALSE,
#'   includeRawValue=FALSE)}}{Render the pivot table as a htmlwidget.}
#'   \item{\code{getLatex(caption=NULL, label=NULL, fromRow=NULL, toRow=NULL,
#'   fromColumn=NULL, toColumn=NULL, boldHeadings=FALSE,
#'   italicHeadings=FALSE)}}{Get the Latex representation of the pivot table,
#'   specifying the caption to appear above the table, the label to use when
#'   referring to the table elsewhere in the document and how headings should be
#'   styled.}
#'   \item{\code{asList()}}{Get a list representation of the pivot table.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the pivot table.}
#'   \item{\code{viewJSON()}}{View the JSON representation of the pivot table.}
#' }

PivotTable <- R6::R6Class("PivotTable",
  public = list(
    initialize = function(messages=FALSE, messageFile=NULL) {
      checkArgument("PivotTable", "initialize", messages, missing(messages), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "initialize", messageFile, missing(messageFile), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_messages <- messages
      if(messages&(!is.null(messageFile))) {
        private$p_messageFile <- file(messageFile, open="w")
      }
      self$message("PivotTable$new", "Creating new Pivot Table...")
      private$p_data <- PivotData$new(parentPivot=self)
      private$p_styles <- getTheme(parentPivot=self, themeName="default")
      private$p_rowGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="row")
      private$p_columnGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="column")
      private$p_calculationsPosition <- NULL
      private$p_calculationGroups <- PivotCalculationGroups$new(parentPivot=self)
      private$p_htmlRenderer <- PivotHtmlRenderer$new(parentPivot=self)
      private$p_latexRenderer <- PivotLatexRenderer$new(parentPivot=self)
      self$message("PivotTable$new", "Created new Pivot Table.")
      return(invisible())
    },
    addData = function(df=NULL, dataName=NULL) {
      checkArgument("PivotTable", "addData", df, missing(df), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
      checkArgument("PivotTable", "addData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$addData", "Adding data to Pivot Table...")
      private$p_data$addData(df, dataName=dataName)
      self$message("PivotTable$addData", "Added data to Pivot Table.")
      return(invisible(private$p_data))
    },
    getTopColumnGroups = function() {
      self$message("PivotTable$getTopColumnGroups", "Getting top level column groups...")
      grps <- private$p_columnGroup$getChildGroups()
      self$message("PivotTable$getTopColumnGroups", "Got top level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafColumnGroups = function() {
      self$message("PivotTable$getLeafColumnGroups", "Getting leaf level column groups...")
      leafGroups = list()
      grps <- private$p_columnGroup$getLeafGroups(leafGroups)
      self$message("PivotTable$getTopColumnGroups", "Got leaf level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addColumnDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                   dataName=NULL, dataSortOrder="asc", dataFormat=NULL, onlyCombinationsThatExist=TRUE,
                                   explicitListOfValues=NULL, calculationGroupName=NULL,
                                   expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total") {
      checkArgument("PivotTable", "addColumnDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addColumnDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "addColumnDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addColumnDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addColumnDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none"))
      checkArgument("PivotTable", "addColumnDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
      checkArgument("PivotTable", "addColumnDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addColumnDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      checkArgument("PivotTable", "addColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "addColumnDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addColumnDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addColumnDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "addColumnDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
      checkArgument("PivotTable", "addColumnDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addColumnDataGroups", "Adding column groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                        onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                        addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grp <- private$p_columnGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                                 dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                                 onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                                 calculationGroupName=calculationGroupName,
                                                 expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                                 visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption)
      self$message("PivotTable$addColumnDataGroups", "Added column groups.")
      return(invisible(grp))
    },
    normaliseColumnGroups = function() {
      self$message("PivotTable$normaliseColumnGroups", "Normalising column groups...")
      self$resetCells()
      groupsAdded <- private$p_columnGroup$normaliseDataGroup()
      self$message("PivotTable$normaliseColumnGroups", "Normalised column groups.", list(groupsAdded = groupsAdded))
      return(invisible())
    },
    sortColumnDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
      checkArgument("PivotTable", "sortColumnDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "sortColumnDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
      checkArgument("PivotTable", "sortColumnDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
      checkArgument("PivotTable", "sortColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "sortColumnDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$sortColumnDataGroups", "Sorting column data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName))
      if(levelNumber<1) stop("PivotTable$sortColumnDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_columnGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName)
      self$message("PivotTable$sortColumnDataGroups", "Sorted column data groups.")
      return(invisible())
    },
    getTopRowGroups = function() {
      self$message("PivotTable$getTopRowGroups", "Getting top level row groups...")
      grps <- private$p_rowGroup$getChildGroups()
      self$message("PivotTable$getTopRowGroups", "Got top level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafRowGroups = function() {
      self$message("PivotTable$getLeafRowGroups", "Getting leaf level row groups...")
      leafGroups = list()
      grps <- private$p_rowGroup$getLeafGroups(leafGroups)
      self$message("PivotTable$getTopRowGroups", "Got leaf level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addRowDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                   dataName=NULL, dataSortOrder="asc", dataFormat=NULL, onlyCombinationsThatExist=TRUE,
                                   explicitListOfValues=NULL, calculationGroupName=NULL,
                                   expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total") {
     checkArgument("PivotTable", "addRowDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotTable", "addRowDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotTable", "addRowDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addRowDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotTable", "addRowDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none"))
     checkArgument("PivotTable", "addRowDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
     checkArgument("PivotTable", "addRowDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addRowDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
     checkArgument("PivotTable", "addRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotTable", "addRowDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addRowDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addRowDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     checkArgument("PivotTable", "addRowDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
     checkArgument("PivotTable", "addRowDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addRowDataGroups", "Adding row groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                        onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                        addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_rowGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                               dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                               onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                               calculationGroupName=calculationGroupName,
                                               expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                               visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption)
      self$message("PivotTable$addRowDataGroups", "Added row groups.")
      return(invisible(grps))
    },
    normaliseRowGroups = function() {
      self$message("PivotTable$normaliseRowGroups", "Normalising row groups...")
      self$resetCells()
      groupsAdded <- private$p_rowGroup$normaliseDataGroup()
      self$message("PivotTable$normaliseRowGroups", "Normalised row groups.", list(groupsAdded = groupsAdded))
      return(invisible())
    },
    sortRowDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
      checkArgument("PivotTable", "sortRowDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "sortRowDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
      checkArgument("PivotTable", "sortRowDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
      checkArgument("PivotTable", "sortRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "sortRowDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$sortRowDataGroups", "Sorting row data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName))
      if(levelNumber<1) stop("PivotTable$sortRowDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_rowGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName)
      self$message("PivotTable$sortRowDataGroups", "Sorted row data groups.")
      return(invisible())
    },
    addCalculationGroup = function(calculationGroupName=NULL) {
      checkArgument("PivotTable", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$addCalculationGroup", "Adding calculation group...", list(calculationGroupName=calculationGroupName))
      self$resetCells()
      calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      self$message("PivotTable$addCalculationGroup", "Added calculation group.")
      return(invisible(calculationGroup))
    },
    defineCalculation = function(calculationGroupName="default", calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, basedOn=NULL,
                         noDataValue=NULL, noDataCaption=NULL) {
      checkArgument("PivotTable", "defineCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
      checkArgument("PivotTable", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("function", "list", "character"))
      checkArgument("PivotTable", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
      checkArgument("PivotTable", "defineCalculation", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
      checkArgument("PivotTable", "defineCalculation", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "defineCalculation", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
      checkArgument("PivotTable", "defineCalculation", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      fstr <- NULL
      if(!is.null(filters)) {
        fstr <- filters$asString()
      }
      self$message("PivotTable$defineCalculation", "Defining calculation...",
                   list(calculationGroupName=calculationGroupName, calculationName=calculationName, caption=caption,
                        visible=visible, displayOrder=displayOrder, filters=fstr, format=format, dataName=dataName,
                        type=type, valueName=valueName, summariseExpression=summariseExpression,
                        calculationExpression=calculationExpression, calculationFunction=calculationFunction, basedOn=basedOn,
                        noDataValue=noDataValue, noDataCaption=noDataCaption))
      self$resetCells()
      calculationGroupExists <- private$p_calculationGroups$isExistingCalculationGroup(calculationGroupName)
      if(calculationGroupExists) {
        calculationGroup <- private$p_calculationGroups$getCalculationGroup(calculationGroupName)
      }
      else {
        calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      }
      calculation <- calculationGroup$defineCalculation(calculationName=calculationName, caption=caption, visible=visible,
                         displayOrder=displayOrder, filters=filters, format=format, dataName=dataName,
                         type=type, valueName=valueName, summariseExpression=summariseExpression,
                         calculationExpression=calculationExpression, calculationFunction=calculationFunction, basedOn=basedOn,
                         noDataValue=noDataValue, noDataCaption=noDataCaption)
      self$message("PivotTable$defineCalculation", "Defined calculation.")
      return(invisible(calculation))
    },
    addColumnCalculationGroups = function(calculationGroupName="default", atLevel=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      checkArgument("PivotTable", "addColumnCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addColumnCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     self$message("PivotTable$addColumnCalculationGroups", "Adding column calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_columnGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow)
      self$message("PivotTable$addColumnCalculationGroups", "Added column calculation groups.")
      return(invisible(grps))
    },
    addRowCalculationGroups = function(calculationGroupName="default", atLevel=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      checkArgument("PivotTable", "addRowCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addRowCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     self$message("PivotTable$addRowCalculationGroups", "Adding row calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_rowGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow)
      self$message("PivotTable$addRowCalculationGroups", "Added row calculation groups.")
      return(invisible(grps))
    },
    addStyle = function(styleName=NULL, declarations=NULL) {
      checkArgument("PivotTable", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      self$message("PivotTable$addStyle", "Adding style...", list(styleName=styleName))
      style <- private$p_styles$addStyle(styleName=styleName, declarations=declarations)
      self$message("PivotTable$addStyle", "Added style.")
      return(invisible(style))
    },
    createInlineStyle = function(baseStyleName=NULL, declarations=NULL) {
      checkArgument("PivotTable", "createInlineStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      self$message("PivotTable$createInlineStyle", "Creating inline style...")
      if(is.null(baseStyleName)) {
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=declarations)
      }
      else {
        baseStyle <- private$p_styles$getStyle(styleName=baseStyleName)
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=baseStyle$declarations)
        style$setPropertyValues(declarations=declarations)
      }
      self$message("PivotTable$createInlineStyle", "Created inline style.")
      return(invisible(style))
    },
    generateCellStructure = function() {
      self$message("PivotTable$generateCellStructure", "Generating cell structure...")
      # clear any existing PivotCells
      private$p_cells <- NULL
      # clear rowColumn numbers on both axes
      rowGrps <- private$p_rowGroup$getDescendantGroups(descendants=NULL, includeCurrentGroup=TRUE)
      for(i in 1:length(rowGrps)) {
        rowGrps[[i]]$rowColumnNumber <- NULL
      }
      columnGrps <- private$p_columnGroup$getDescendantGroups(descendants=NULL, includeCurrentGroup=TRUE)
      for(i in 1:length(columnGrps)) {
        columnGrps[[i]]$rowColumnNumber <- NULL
      }
      # set the calculations on columns, if not present
      if((is.null(private$p_calculationsPosition))&&(!is.null(private$p_calculationGroups))&&
         (!is.null(private$p_calculationGroups$defaultGroup))&&(private$p_calculationGroups$defaultGroup$visibleCount>0)) {
        self$addColumnCalculationGroups()
      }
      # get the leaf levels on both axes
      rowGrps <- private$p_rowGroup$getLeafGroups(leafGroups=NULL)
      columnGrps <- private$p_columnGroup$getLeafGroups(leafGroups=NULL)
      rowCount <- length(rowGrps)
      columnCount <- length(columnGrps)
      # calculate filters and calculations for each heading group
      # net filters are calculated top-down
      # calculations are selected bottom-up (first one encountered is used)
      # rows...
      rowFilters <- list()
      rowCalculationGroupNames <- list()
      rowCalculationNames <- list()
      for(i in 1:rowCount) {
        # set the rowColumnNumber on the leaf cell
        rowGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- rowGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the parent filter settings using "replace" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="replace")
          }
        }
        rowFilters[[i]] <- rowColFilters
        # find the calculation
        for(j in 1:length(ancestors)) {
          acs <- ancestors[[j]]
          if(is.null(acs$calculationGroupName)) next
          if(is.null(acs$calculationName)) next
          rowCalculationGroupNames[[i]] <- acs$calculationGroupName
          rowCalculationNames[[i]] <- acs$calculationName
          break
        }
      }
      # calculate the net filters at each position on the columns
      columnFilters <- list()
      columnCalculationGroupNames <- list()
      columnCalculationNames <- list()
      for(i in 1:columnCount) {
        # set the rowColumnNumber on the leaf cell
        columnGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- columnGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the parent filter settings using "replace" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="replace")
          }
        }
        columnFilters[[i]] <- rowColFilters
        # find the calculation
        for(j in 1:length(ancestors)) {
          acs <- ancestors[[j]]
          if(is.null(acs$calculationGroupName)) next
          if(is.null(acs$calculationName)) next
          columnCalculationGroupNames[[i]] <- acs$calculationGroupName
          columnCalculationNames[[i]] <- acs$calculationName
          break
        }
      }
      # get the calculation position
      calculationsPosition <- private$p_calculationsPosition
      if(is.null(calculationsPosition)) { calculationsPosition <- "column" }
      if(!(calculationsPosition %in% c("row", "column")))
        stop("PivotTable$generateCellStructure(): calculationsPosition must be either row or column", call. = FALSE)
      # create and size the new PivotCells
      private$p_cells <- PivotCells$new(self, rowGroups=rowGrps, columnGroups=columnGrps)
      if(rowCount>0) {
        for(r in 1:rowCount) {
          if(columnCount>0) {
            for(c in 1:columnCount) {
              # calculate the net filters
              if(is.null(rowFilters[[r]])) {
                if(!is.null(columnFilters[[c]])) { rowColFilters <- columnFilters[[c]]$getCopy() }
              }
              else {
                rowColFilters <- rowFilters[[r]]$getCopy()
                if (!is.null(columnFilters[[c]])) { rowColFilters$setFilters(columnFilters[[c]]) } # column filters override row filters
              }
              # find the calculation
              calcGrpNme <- NULL
              calcNme <- NULL
              if(calculationsPosition=="row") {
                if(r <= length(rowCalculationGroupNames)) calcGrpNme <- rowCalculationGroupNames[[r]]
                else calcGrpNme <- NULL
                if(r <= length(rowCalculationNames)) calcNme <- rowCalculationNames[[r]]
                else calcNme<- NULL
              }
              else if(calculationsPosition=="column") {
                if(c <= length(columnCalculationGroupNames)) calcGrpNme <- columnCalculationGroupNames[[c]]
                else calcGrpNme<- NULL
                if(c <= length(columnCalculationNames)) calcNme <- columnCalculationNames[[c]]
                else calcNme<- NULL
              }
              # create the cell
              cell <- PivotCell$new(self, rowNumber=as.integer(r), columnNumber=as.integer(c),
                                    calculationName=calcNme, calculationGroupName=calcGrpNme,
                                    rowColFilters=rowColFilters, rowFilters=rowFilters[[r]], columnFilters=columnFilters[[c]],
                                    rowLeafGroup=rowGrps[[r]], columnLeafGroup=columnGrps[[c]])
              private$p_cells$setCell(r=r, c=c, cell=cell)
            }
          }
        }
      }
      self$message("PivotTable$generateCellStructure", "Generated cell structure.")
      return(invisible(private$cells))
    },
    resetCells = function() {
      self$message("PivotTable$resetCells", "Resetting cells...")
      if(private$p_evaluated==TRUE){
        p_cells <- NULL
        private$p_evaluated <- FALSE
        private$p_latextRenderer$resetVisibleRange()
      }
      self$message("PivotTable$resetCells", "Reset cells.")
      return(invisible())
    },
    evaluateCells = function() {
      self$message("PivotTable$evaluateCells", "Evaluating cell values...")
      if(is.null(private$p_cells)) stop("PivotTable$evaluateCells():  No cells exist to calculate.", call. = FALSE)
      calculator <- PivotCalculator$new(self)
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          calculator$evaluateCell(cell)
        }
      }
      private$p_evaluated <- TRUE
      self$message("PivotTable$evaluateCells", "Evaluated cell values.")
      return(invisible())
    },
    evaluatePivot = function() {
      self$message("PivotTable$evaluatePivot", "Evaluating pivot table...")
      self$normaliseColumnGroups()
      self$normaliseRowGroups()
      self$generateCellStructure()
      self$evaluateCells()
      self$message("PivotTable$evaluatePivot", "Evaluated pivot table.")
      return(invisible())
    },
    asMatrix = function(includeHeaders=TRUE, repeatHeaders=FALSE, rawValue=FALSE) {
      checkArgument("PivotTable", "asMatrix", includeHeaders, missing(includeHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$asMatrix", "Getting pivot table as a matrix...",
                    list(includeHeaders=includeHeaders, repeatHeaders=repeatHeaders, rawValue=rawValue))
      if(!private$p_evaluated) stop("PivotTable$asMatrix():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asMatrix():  No cells exist to retrieve.", call. = FALSE)
      if(includeHeaders==FALSE) {
        return(private$p_cells$asMatrix(rawValue=rawValue))
      }
      if(repeatHeaders==FALSE) {
        private$clearIsRenderedFlags()
      }
      # size the matrix
      rowHeaderLevelCount <- private$p_rowGroup$getLevelCount()
      columnHeaderLevelCount <- private$p_columnGroup$getLevelCount()
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      m <- matrix(data=NA, nrow=columnHeaderLevelCount+rowCount, ncol=rowHeaderLevelCount+columnCount)
      # set the root cells
      for(r in 1:columnHeaderLevelCount) {
        for(c in 1:rowHeaderLevelCount) {
          m[r, c] <- ""
        }
      }
      # set the column headers
      colHeaderLeafGroups <- private$p_columnGroup$getLeafGroups()
      for(c in 1:length(colHeaderLeafGroups)) {
        leafGroup <- colHeaderLeafGroups[[c]]
        grps <- leafGroup$getAncestorGroups(includeCurrentGroup=TRUE)
        for(r in (length(grps)-1):1) {
          grp <- grps[[length(grps) - r]]
          if((repeatHeaders==FALSE) && (grp$isRendered==TRUE)) {
            m[r, c + rowHeaderLevelCount] <- ""
            next
          }
          m[r, c + rowHeaderLevelCount] <- grp$caption
          grp$isRendered <- TRUE
        }
      }
      # set the row headers
      rowHeaderLeafGroups <- private$p_rowGroup$getLeafGroups()
      for(r in 1:length(rowHeaderLeafGroups)) {
        leafGroup <- rowHeaderLeafGroups[[r]]
        grps <- leafGroup$getAncestorGroups(includeCurrentGroup=TRUE)
        for(c in (length(grps)-1):1) {
          grp <- grps[[length(grps) - c]]
          if((repeatHeaders==FALSE) && (grp$isRendered==TRUE)) {
            m[r + columnHeaderLevelCount, c] <- ""
            next
          }
          m[r + columnHeaderLevelCount, c] <- grp$caption
          grp$isRendered <- TRUE
        }
      }
      # set the cell values
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          if(rawValue==TRUE) {
            v <- cell$rawValue
            if(!(("integer" %in% class(v))||("numeric" %in% class(v)))) v <- NA
          }
          else v <- cell$formattedValue
          if(is.null(v)) m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- ""
          else if(is.na(v)) m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- ""
          else m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- v
        }
      }
      self$message("PivotTable$asMatrix", "Got pivot table as a matrix.")
      return(m)
    },
    asDataFrame = function(separator=" ") {
      checkArgument("PivotTable", "asDataFrame", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      self$message("PivotTable$asDataFrame", "Getting pivot table as a data frame...",
                    list(dataGroupSeparatordataGroupSeparator))
      if(!private$p_evaluated) stop("PivotTable$asDataFrame():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asDataFrame():  No cells exist to retrieve.", call. = FALSE)
      # sizing
      rowHeaderLevelCount <- private$p_rowGroup$getLevelCount()
      columnHeaderLevelCount <- private$p_columnGroup$getLevelCount()
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      rowHeaders <- list()
      columnHeaders <- list()
      # set the column headers
      colHeaderLeafGroups <- private$p_columnGroup$getLeafGroups()
      for(c in 1:length(colHeaderLeafGroups)) {
        leafGroup <- colHeaderLeafGroups[[c]]
        grps <- leafGroup$getAncestorGroups(includeCurrentGroup=TRUE)
        headerValue <- ""
        for(r in (length(grps)-1):1) {
          grp <- grps[[r]]
          if(nchar(headerValue) == 0) headerValue <- grp$caption
          else headerValue <- paste0(headerValue, separator, grp$caption)
        }
        columnHeaders[[length(columnHeaders) + 1]] <- headerValue
      }
      # set the row headers
      rowHeaderLeafGroups <- private$p_rowGroup$getLeafGroups()
      for(r in 1:length(rowHeaderLeafGroups)) {
        leafGroup <- rowHeaderLeafGroups[[r]]
        grps <- leafGroup$getAncestorGroups(includeCurrentGroup=TRUE)
        headerValue <- ""
        for(c in (length(grps)-1):1) {
          grp <- grps[[c]]
          if(nchar(headerValue) == 0) headerValue <- grp$caption
          else headerValue <- paste0(headerValue, separator, grp$caption)
        }
        rowHeaders[[length(rowHeaders) + 1]] <- headerValue
      }
      # get the value vectors to form the data frame
      dfColumns <- list()
      for(c in 1:columnCount) {
        columnValues <- NA
        for(r in 1:rowCount) {
          cell <- private$p_cells$getCell(r, c)
          v <- cell$rawValue
          if(!(("integer" %in% class(v))||("numeric" %in% class(v)))) v <- NA
          if(is.null(v)) v <- NA
          columnValues[r] <- v
        }
        dfColumns[[c]] <- columnValues
      }
      df <- as.data.frame(dfColumns)
      colnames(df) <- columnHeaders
      rownames(df) <- rowHeaders
      self$message("PivotTable$asDataFrame", "Got pivot table as a data frame.")
      return(df)
    },
    getCss = function(styleNamePrefix=NULL) {
      checkArgument("PivotTable", "getCss", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      self$message("PivotTable$getCss", "Getting Styles...")
      if(is.null(private$p_styles)) return("")
      if(length(private$p_styles$styles)==0) return("")
      styles <- ""
      for(s in 1:length(private$p_styles$styles)) {
        style <- private$p_styles$styles[[s]]
        if(is.null(style)) next
        styles <- paste0(styles, style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix), "\r\n")
      }
      self$message("PivotTable$getCss", "Got Styles.")
      return(invisible(styles))
    },
    getHtml = function(styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE, includeCalculationFilters=FALSE,
                       includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "getHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "getHtml", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "getHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$getHtml", "Getting HTML...")
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      self$message("PivotTable$getHtml", "Got HTML.")
      return(invisible(htmlTable))
    },
    saveHtml = function(filePath=NULL, fullPageHTML=TRUE, styleNamePrefix=NULL, includeHeaderValues=FALSE,
                        includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "saveHtml", filePath, missing(filePath), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotTable", "saveHtml", fullPageHTML, missing(fullPageHTML), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "saveHtml", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "saveHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$saveHtml", "Saving HTML...", list(filePath=filePath, fullPageHTML=fullPageHTML, styleNamePrefix=styleNamePrefix,
                                                                 includeHeaderValues=includeHeaderValues,
                                                                 includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                 includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue))
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # todo: enable rendering before cells are calculated so the structure of the pivot can be checked as it is being developed
      if(is.null(private$p_cells)) stop("PivotTable$saveHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      if (fullPageHTML==FALSE) {
        fileConn <- file(filePath)
        writeLines(as.character(htmlTable), fileConn)
        close(fileConn)
        self$message("PivotTable$saveHtml", "Saved HTML.")
        return(invisible())
      }
      # basic css
      cssStr1 <- "<style>h1 { font: 2.5em arial; font-weight: bold; } p { font: 0.9em arial; }</style>"
      cssStr2 <- paste0("<style>", self$getCss(styleNamePrefix=styleNamePrefix), "</style>")
      #pgHtml <- htmltools::tags$html(htmltools::tags$head(htmltools::tags$title('R Pivot Table')), htmltools::HTML(cssStr),
      pgHtml <- htmltools::tags$html(htmltools::HTML("<head>"), htmltools::tags$title('R Pivot Table'), htmltools::HTML(cssStr1), htmltools::HTML(cssStr2), htmltools::HTML("</head>"),
                 htmltools::tags$body(
                   htmltools::h1("R Pivot Table"),
                   htmlTable,
                   htmltools::tags$br(),
                   htmltools::tags$p(paste0("Generated at ", format(Sys.time(), "%X on %a %d %b %Y")))
                 ))
      fileConn <- file(filePath)
      writeLines(as.character(pgHtml), fileConn)
      close(fileConn)
      self$message("PivotTable$saveHtml", "Saved HTML.")
      return(invisible())
    },
    renderPivot = function(width=NULL, height=NULL, styleNamePrefix=NULL, includeHeaderValues=FALSE,
                           includeRCFilters=FALSE, includeCalculationFilters=FALSE, includeCalculationNames=FALSE, includeRawValue=FALSE) {
      checkArgument("PivotTable", "renderPivot", width, missing(width), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "renderPivot", height, missing(height), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "renderPivot", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "renderPivot", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument("PivotTable", "renderPivot", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      self$message("PivotTable$renderPivot", "Rendering htmlwidget...", list(width=width, height=height, styleNamePrefix=styleNamePrefix,
                                                                             includeHeaderValues=includeHeaderValues,
                                                                             includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                             includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue))
      if(!private$p_evaluated) self$evaluatePivot()
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # pivottabler(self, width=width, height=height, includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
      #                 includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      settings <- list() # may need this in the future
      widgetData <- list(
        tableCss = self$getCss(styleNamePrefix=styleNamePrefix),
        tableHtml = as.character(self$getHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                            includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                            includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)),
        settings = settings
      )
      # viewer.fill=TRUE and browser.fill=TRUE sound like they would be good things, but they seem to prevent
      # any scroll bars being shown when the HTML tables are larger than the RStudio Viewer or the web browser window size
      sp = htmlwidgets::sizingPolicy(
        viewer.padding=10, viewer.fill=FALSE, viewer.suppress=FALSE,
        browser.padding=10, browser.fill=FALSE,
        knitr.defaultWidth="auto", knitr.defaultHeight="auto", knitr.figure = FALSE
      )
      w <- htmlwidgets::createWidget("pivottabler", widgetData, width=width, height=height, sizingPolicy=sp)
      self$message("PivotTable$renderPivot", "Rendered htmlwidget.")
      return(w)
    },
    getLatex = function(caption=NULL, label=NULL, fromRow=NULL, toRow=NULL, fromColumn=NULL, toColumn=NULL,
                        boldHeadings=FALSE, italicHeadings=FALSE) {
      checkArgument("PivotTable", "getLatex", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "getLatex", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotTable", "getLatex", fromRow, missing(fromRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "getLatex", toRow, missing(toRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "getLatex", fromColumn, missing(fromColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "getLatex", toColumn, missing(toColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      checkArgument("PivotTable", "getLatex", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      checkArgument("PivotTable", "getLatex", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      self$message("PivotTable$getLatex", "Getting Latex...", list(caption=caption, label=label,
                                                                   fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn,
                                                                   boldHeadings=boldHeadings, italicHeadings=italicHeadings))
      if(!private$p_evaluated) self$evaluatePivot()
      if(!private$p_evaluated) stop("PivotTable$getLatex():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getLatex():  No cells exist to render.", call. = FALSE)
      private$p_latexRenderer$setVisibleRange(fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn)
      ltx <- private$p_latexRenderer$getTableLatex(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings)
      self$message("PivotTable$getLatex", "Got Latex.")
      return(ltx)
    },
    message = function(methodName, desc, detailList=NULL) {
      if(!private$p_messages) return()
      stackdepth <- length(sys.calls())
      indent <- strrep(" ", (stackdepth - 1) *2)
      msg <- paste0(indent, methodName, ":  ", desc)
      if(length(detailList)>0) {
        nms <- names(detailList)
        msg <- paste0(msg, ": ")
        for(i in 1:length(detailList)) {
          sep <- ""
          if(i > 1) { sep <- ", " }
          dtl <- NULL
          if("function" %in% class(detailList[[i]])) {
            dtl <- deparse(detailList[[i]])
          }
          else dtl <- detailList[[i]]
          msg <- paste0(msg, sep, nms[i], "=", dtl)
        }
      }
      if(is.null(private$p_messageFile)) { message(msg) }
      else { cat(msg, file=private$p_messageFile, sep="\r\n", append=TRUE)}
    },
    asList = function() {
      self$message("PivotTable$asList", "Getting list...")
      lst <- list(
        type = "pivotTable",
        dataFrames = private$p_data$asList(),
        rowGroup = private$p_rowGroup$asList(),
        columnGroup = private$p_columnGroup$asList(),
        calculationsPosition = private$p_calculationsPosition,
        calculationGroups = private$p_calculationGroups$asList()
      )
      if(!is.null(private$p_cells)) lst["cells"] <- private$p_cells$asList()
      self$message("PivotTable$asList", "Got list.")
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    viewJSON = function() {
      if (!requireNamespace("listviewer", quietly = TRUE)) {
        stop("PivotTable$asJSON():  The listviewer package is needed to view the internal structure of the PivotTable as JSON.  Please install it.", call. = FALSE)
      }
      listviewer::jsonedit(self$asList(), mode="code")
    },
    finalize = function() {
      if(!is.null(private$p_messageFile)) close(private$p_messageFile)
    }
  ),
  active = list(
    data = function(value) { return(private$p_data) },
    rowGroup = function(value) { return(invisible(private$p_rowGroup ))},
    columnGroup = function(value) { return(invisible(private$p_columnGroup ))},
    calculationGroups = function(value) { return(invisible(private$p_calculationGroups)) },
    calculationsPosition = function(value) {
      if(missing(value)) { return(invisible(private$p_calculationsPosition)) }
      else {
        if(is.null(private$p_calculationsPosition)) { private$p_calculationsPosition <- value }
        else {
          if(!(value %in% c("row", "column")))
            stop("PivotTable$calculationsPosition(): calculationsPosition must be either row or column", call. = FALSE)
          if(private$p_calculationsPosition != value)
            stop(paste0("PivotTable$calculationsPosition():  Calculations position already set to be '",
                        private$p_calculationsPosition, "' and cannot be changed."), call. = FALSE)
          return(invisible())
        }
      }
    },
    cells = function(value) { return(invisible(private$p_cells ))},
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        checkArgument("PivotTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "PivotStyles"))
        if("character" %in% class(value)) private$p_styles <- getTheme(parentPivot=self, themeName=value)
        else if("PivotStyles" %in% class(value)) private$p_styles <- value
        return(invisible())
      }
    },
    styles = function(value) {
      if(missing(value)) return(invisible(private$p_styles))
      else {
        checkArgument("PivotTable", "styles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyles")
        private$p_styles <- value
        return(invisible())
      }
    },
    allowExternalStyles = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(private$p_styles$allowExternalStyles)
      }
      else {
        checkArgument("PivotTable", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        private$p_styles$allowExternalStyles <- value
        return(invisible())
      }
    },
    messages = function(value) {
      if(missing(value)) return(invisible(private$p_messages))
      else {
        if(is.logical(value)) private$p_messages <- value
        else stop("PivotTable$messages: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    }
  ),
  private = list(
    p_data = NULL,
    p_styles = NULL,
    p_rowGroup = NULL,
    p_columnGroup = NULL,
    p_calculationsPosition = NULL,
    p_calculationGroups = NULL,
    p_evaluated = FALSE,
    p_cells = NULL,
    p_htmlRenderer = NULL,
    p_latexRenderer = NULL,
    p_messages = FALSE,
    p_messageFile = NULL,
    clearIsRenderedFlags = function() {
      self$message("PivotTable$clearIsRenderedFlags", "Clearing isRendered flags...")
      clearFlags <- function(dg) {
        grp <- dg
        while(!is.null(grp)) {
          grp$isRendered <- FALSE
          grp <- grp$parentGroup
        }
      }
      rowGroups <- self$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(rowGroups, clearFlags)
      columnGroups <- self$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(columnGroups, clearFlags)
      self$message("PivotTable$clearIsRenderedFlags", "Cleared isRendered flags...")
      return(invisible())
    }
  )
)
