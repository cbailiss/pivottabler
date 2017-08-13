#' A class that defines a pivot table.
#'
#' The PivotTable class represents a pivot table and is the primary class for
#' constructing and interacting with the pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
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
#' @field argumentCheckMode A number (0-4 meaning none, minimal, basic,
#'   balanced, full) indicating the argument checking level.
#' @field processingLibrary A character value indicating the processing library
#'   being used (base, dplyr, data.table).
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
#' @field evaluationMode Either "sequential" or "batch" to specify how summary
#'   calculations (i.e. where type="summary") are evaluated.
#' @field batchInfo Get a text summary of the batch calculations from the last
#'   evaluation of this pivot table.
#' @field cells A PivotCells object containing all of the cells in the body of
#'   the pivot table.
#' @field theme The name of the theme currently applied to the pivot table.
#' @field styles A PivotStyles object containing the styles used to theme the
#'   pivot table.
#' @field allowExternalStyles Enable support for external styles, when producing
#'   content for external systems.
#' @field allTimings The time taken for various activities related to
#'   constructing the pivot table.
#' @field significantTimings The time taken for various activities related to
#'   constructing the pivot table, where the elapsed time > 0.1 seconds.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(processingLibrary="auto", traceEnabled=FALSE,
#'   traceFile=NULL, argumentCheckMode="auto")}}{Create a new pivot table,
#'   including optionally enabling debug logging.}
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
#'   totalCaption="Total", preGroupData=TRUE)}}{Generate new column heading data
#'   groups based on the distinct values in a data frame or using explicitly
#'   specified data values.}
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
#'   totalCaption="Total", preGroupData=TRUE)}}{Generate new row heading data
#'   groups based on the distinct values in a data frame or using explicitly
#'   specified data values.}
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
#'   \item{\code{findRowDataGroups(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL,
#'   includeDescendantGroups=FALSE)}}{Find row data groups matching the
#'   specified criteria.}
#'   \item{\code{findColumnDataGroups(matchMode="simple", variableNames=NULL,
#'   variableValues=NULL, totals="include", calculationNames=NULL,
#'   includeDescendantGroups=FALSE)}}{Find column data groups matching the
#'   specified criteria.}
#'   \item{\code{getCells(specifyCellsAsList=FALSE, rowNumbers=NULL,
#'   columnNumbers=NULL, cellCoordinates=NULL)}}{Retrieve cells by a combination
#'   of row and/or column numbers.}
#'   \item{\code{findCells(variableNames=NULL, variableValues=NULL,
#'   totals="include", calculationNames=NULL, minValue=NULL, maxValue=NULL,
#'   exactValues=NULL, includeNull=TRUE, includeNA=TRUE)}}{Find cells in the
#'   body of the pivot table matching the specified criteria.}
#'   \item{\code{print(asCharacter=FALSE)}}{Either print the pivot table to the
#'   console or retrieve it as a character value.}
#'   \item{\code{asMatrix(includeHeaders=TRUE, repeatHeaders=FALSE,
#'   rawValue=FALSE)}}{Gets the pivot table as a matrix, with or without
#'   headings.}
#'   \item{\code{asDataFrame(separator=" ")}}{Gets the pivot table as a data
#'   frame, combining multiple levels of headings with the specified separator.}
#'   \item{\code{asTidyDataFrame(includeGroupCaptions=TRUE,
#'   includeGroupValues=TRUE, separator=" ")}}{Gets the pivot table as a tidy
#'   data frame, where each cell in the body of the pivot table becomes one row
#'   in the data frame.}
#'   \item{\code{getCss(styleNamePrefix)}}{Get the CSS declarations for the
#'   entire pivot table.}
#'   \item{\code{getHtml(styleNamePrefix, includeHeaderValues=FALSE,
#'   includeRCFilters=FALSE, includeCalculationFilters=FALSE,
#'   includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
#'   includeCalculationNames=FALSE, includeRawValue=FALSE,
#'   includeTotalInfo=FALSE)}}{Get the HTML representation of the pivot table,
#'   specifying the CSS style name prefix to use and whether additional debug
#'   information should be included in the pivot table.}
#'   \item{\code{saveHtml(filePath, fullPageHTML=TRUE, styleNamePrefix,
#'   includeHeaderValues=FALSE, includeRCFilters=FALSE,
#'   includeCalculationFilters=FALSE, includeWorkingData=FALSE,
#'   includeEvaluationFilters=FALSE, includeCalculationNames=FALSE,
#'   includeRawValue=FALSE, includeTotalInfo=FALSE)}}{Save the HTML
#'   representation of the pivot table to a file.}
#'   \item{\code{renderPivot(width, height, styleNamePrefix,
#'   includeHeaderValues=FALSE, includeRCFilters=FALSE,
#'   includeCalculationFilters=FALSE, includeWorkingData=FALSE,
#'   includeEvaluationFilters=FALSE, includeCalculationNames=FALSE,
#'   includeRawValue=FALSE, includeTotalInfo=FALSE)}}{Render the pivot table as
#'   a htmlwidget.}
#'   \item{\code{getLatex(caption=NULL, label=NULL, fromRow=NULL, toRow=NULL,
#'   fromColumn=NULL, toColumn=NULL, boldHeadings=FALSE,
#'   italicHeadings=FALSE)}}{Get the Latex representation of the pivot table,
#'   specifying the caption to appear above the table, the label to use when
#'   referring to the table elsewhere in the document and how headings should be
#'   styled.}
#'   \item{\code{showBatchInfo()}}{Show a text summary of the batch calculations
#'   from the last evaluation of this pivot table.}
#'   \item{\code{asList()}}{Get a list representation of the pivot table.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the pivot table.}
#'   \item{\code{viewJSON()}}{View the JSON representation of the pivot table.}
#' }

PivotTable <- R6::R6Class("PivotTable",
  public = list(
    initialize = function(processingLibrary="auto", evaluationMode="batch", argumentCheckMode="auto", traceEnabled=FALSE, traceFile=NULL) {
      checkArgument(4, TRUE, "PivotTable", "initialize", processingLibrary, missing(processingLibrary), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "dplyr", "data.table"))
      checkArgument(4, TRUE, "PivotTable", "initialize", evaluationMode, missing(evaluationMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("batch", "sequential"))
      checkArgument(4, TRUE, "PivotTable", "initialize", argumentCheckMode, missing(argumentCheckMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "none", "minimal", "basic", "balanced", "full"))
      checkArgument(4, TRUE, "PivotTable", "initialize", traceEnabled, missing(traceEnabled), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument(4, TRUE, "PivotTable", "initialize", traceFile, missing(traceFile), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      if(argumentCheckMode=="auto") {
        if (length(strsplit(packageDescription("pivottabler")$Version, "\\.")[[1]]) > 3) {
          message("Development version of pivottabler detected: Using argumentCheckMode=full.\nThis may reduce performance. To override, specify the argumentCheckMode explicitly.\nargumentCheckMode values: none, minimal, basic, balanced (the normal default), full.")
          private$p_argumentCheckMode <- 4
        }
        else private$p_argumentCheckMode <- 3
      }
      else if(argumentCheckMode=="none") private$p_argumentCheckMode <- 0
      else if(argumentCheckMode=="minimal") private$p_argumentCheckMode <- 1
      else if(argumentCheckMode=="basic") private$p_argumentCheckMode <- 2
      else if(argumentCheckMode=="balanced") private$p_argumentCheckMode <- 3
      else if(argumentCheckMode=="full") private$p_argumentCheckMode <- 4
      else stop("PivotTable$initialize():  Unknown argumentCheckMode encountered.", call. = FALSE)
      private$p_traceEnabled <- traceEnabled
      if(private$p_traceEnabled&(!is.null(traceFile))) {
        private$p_traceFile <- file(traceFile, open="w")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$new", "Creating new Pivot Table...")
      if(processingLibrary=="auto") {
        if(requireNamespace(package="dplyr", quietly=TRUE)==TRUE) private$p_processingLibrary <- "dplyr"
        else if(requireNamespace(package="data.table", quietly=TRUE)==TRUE) private$p_processingLibrary <- "data.table"
        else stop("PivotTable$initialize():  No suitable processing library found.  Please install either the dplyr package or the data.table package.", call. = FALSE)
      }
      else if(processingLibrary=="data.table") {
        if(requireNamespace(package="data.table", quietly=TRUE)==FALSE) {
          stop("PivotTable$initialize():  data.table package cannot be loaded.  Please check the package is installed and working.", call. = FALSE)
        }
        private$p_processingLibrary <- "data.table"
      }
      else if(processingLibrary=="dplyr") {
        if(requireNamespace(package="dplyr", quietly=TRUE)==FALSE) {
          stop("PivotTable$initialize():  dplyr package cannot be loaded.  Please check the package is installed and working.", call. = FALSE)
        }
        private$p_processingLibrary <- "dplyr"
      }
      else stop("PivotTable$initialize():  Unknown processingLibrary encountered.", call. = FALSE)
      private$p_evaluationMode <- evaluationMode
      # Create the basic parts of the pivot table
      private$p_data <- PivotData$new(parentPivot=self)
      private$p_styles <- getTheme(parentPivot=self, themeName="default")
      private$p_rowGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="row", isLevelTotal=TRUE)
      private$p_columnGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="column", isLevelTotal=TRUE)
      private$p_calculationsPosition <- NULL
      private$p_calculationGroups <- PivotCalculationGroups$new(parentPivot=self)
      private$p_cells <- PivotCells$new(self)
      private$p_htmlRenderer <- PivotHtmlRenderer$new(parentPivot=self)
      private$p_latexRenderer <- PivotLatexRenderer$new(parentPivot=self)
      private$p_openxlsxRenderer <- PivotOpenXlsxRenderer$new(parentPivot=self)
      private$p_timings <- list()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$new", "Created new Pivot Table.")
      return(invisible())
    },
    addData = function(dataFrame=NULL, dataName=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addData", "Adding data to Pivot Table...")
      dn <- dataName
      if(is.null(dn)) dn <- deparse(substitute(dataFrame))
      private$p_data$addData(dataFrame, dataName=dn)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addData", "Added data to Pivot Table.")
      private$addTiming(paste0("addData(", dn, ")"), timeStart)
      return(invisible(private$p_data))
    },
    getTopColumnGroups = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopColumnGroups", "Getting top level column groups...")
      grps <- private$p_columnGroup$getChildGroups()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopColumnGroups", "Got top level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafColumnGroups = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafColumnGroups", "Getting leaf level column groups...")
      leafGroups = list()
      grps <- private$p_columnGroup$getLeafGroups(leafGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopColumnGroups", "Got leaf level column groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addColumnDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                   dataName=NULL, dataSortOrder="asc", dataFormat=NULL, onlyCombinationsThatExist=TRUE,
                                   explicitListOfValues=NULL, calculationGroupName=NULL,
                                   expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                                   preGroupData=TRUE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", preGroupData, missing(preGroupData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnDataGroups", "Adding column groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                        onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                        addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                        preGroupData=preGroupData))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grp <- private$p_columnGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                                 dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                                 onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                                 calculationGroupName=calculationGroupName,
                                                 expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                                 visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                                                 preGroupData=preGroupData)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnDataGroups", "Added column groups.")
      private$addTiming(paste0("addColumnDataGroups(", variableName, ")"), timeStart)
      return(invisible(grp))
    },
    normaliseColumnGroups = function() {
      timeStart <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseColumnGroups", "Normalising column groups...")
      self$resetCells()
      groupsAdded <- private$p_columnGroup$normaliseDataGroup()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseColumnGroups", "Normalised column groups.", list(groupsAdded = groupsAdded))
      private$addTiming("normaliseColumnGroups", timeStart)
      return(invisible())
    },
    sortColumnDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortColumnDataGroups", "Sorting column data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName))
      if(levelNumber<1) stop("PivotTable$sortColumnDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_columnGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortColumnDataGroups", "Sorted column data groups.")
      private$addTiming("sortColumnDataGroups", timeStart)
      return(invisible())
    },
    getTopRowGroups = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Getting top level row groups...")
      grps <- private$p_rowGroup$getChildGroups()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Got top level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    getLeafRowGroups = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafRowGroups", "Getting leaf level row groups...")
      leafGroups = list()
      grps <- private$p_rowGroup$getLeafGroups(leafGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Got leaf level row groups", list(count = length(grps)))
      return(invisible(grps))
    },
    addRowDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                dataName=NULL, dataSortOrder="asc", dataFormat=NULL, onlyCombinationsThatExist=TRUE,
                                explicitListOfValues=NULL, calculationGroupName=NULL,
                                expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                                preGroupData=TRUE) {
     timeStart <- proc.time()
     if(private$p_argumentCheckMode > 0) {
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "none"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", preGroupData, missing(preGroupData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowDataGroups", "Adding row groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                        dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                        onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                        addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                        preGroupData=preGroupData))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_rowGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                               dataName=dataName, dataSortOrder=dataSortOrder, dataFormat=dataFormat,
                                               onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                               calculationGroupName=calculationGroupName,
                                               expandExistingTotals=expandExistingTotals, addTotal=addTotal,
                                               visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                                               preGroupData=preGroupData)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowDataGroups", "Added row groups.")
      private$addTiming(paste0("addRowDataGroups(", variableName, ")"), timeStart)
      return(invisible(grps))
    },
    normaliseRowGroups = function() {
      timeStart <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseRowGroups", "Normalising row groups...")
      self$resetCells()
      groupsAdded <- private$p_rowGroup$normaliseDataGroup()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseRowGroups", "Normalised row groups.", list(groupsAdded = groupsAdded))
      private$addTiming("normaliseRowGroups", timeStart)
      return(invisible())
    },
    sortRowDataGroups = function(levelNumber=1, orderBy="calculation", sortOrder="desc", calculationGroupName="default", calculationName=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortRowDataGroups", "Sorting row data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName))
      if(levelNumber<1) stop("PivotTable$sortRowDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_rowGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortRowDataGroups", "Sorted row data groups.")
      private$addTiming("sortRowDataGroups", timeStart)
      return(invisible())
    },
    addCalculationGroup = function(calculationGroupName=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addCalculationGroup", "Adding calculation group...", list(calculationGroupName=calculationGroupName))
      self$resetCells()
      calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addCalculationGroup", "Added calculation group.")
      private$addTiming("addCalculationGroup", timeStart)
      return(invisible(calculationGroup))
    },
    defineCalculation = function(calculationGroupName="default", calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, basedOn=NULL,
                         noDataValue=NULL, noDataCaption=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("function", "list", "character"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      fstr <- NULL
      if(!is.null(filters)) {
        fstr <- filters$asString()
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$defineCalculation", "Defining calculation...",
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$defineCalculation", "Defined calculation.")
      private$addTiming(paste0("defineCalculation(", calculationGroupName, ":", calculationName, ")"), timeStart)
      return(invisible(calculation))
    },
    addColumnCalculationGroups = function(calculationGroupName="default", atLevel=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnCalculationGroups", "Adding column calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_columnGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnCalculationGroups", "Added column calculation groups.")
      return(invisible(grps))
    },
    addRowCalculationGroups = function(calculationGroupName="default", atLevel=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowCalculationGroups", "Adding row calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel))
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_rowGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowCalculationGroups", "Added row calculation groups.")
      return(invisible(grps))
    },
    addStyle = function(styleName=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addStyle", "Adding style...", list(styleName=styleName))
      style <- private$p_styles$addStyle(styleName=styleName, declarations=declarations)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addStyle", "Added style.")
      return(invisible(style))
    },
    createInlineStyle = function(baseStyleName=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "createInlineStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$createInlineStyle", "Creating inline style...")
      if(is.null(baseStyleName)) {
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=declarations)
      }
      else {
        baseStyle <- private$p_styles$getStyle(styleName=baseStyleName)
        style <- PivotStyle$new(parentPivot=self, styleName="", declarations=baseStyle$declarations)
        style$setPropertyValues(declarations=declarations)
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$createInlineStyle", "Created inline style.")
      return(invisible(style))
    },
    generateCellStructure = function() {
      timeStart <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$generateCellStructure", "Generating cell structure...")
      # clear any existing PivotCells
      private$p_cells$reset()
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
        # construct the parent filter settings using "and" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="and")
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
        # construct the parent filter settings using "and" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="and")
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
      private$p_cells$setGroups(rowGroups=rowGrps, columnGroups=columnGrps)
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
                if (!is.null(columnFilters[[c]])) { rowColFilters$setFilters(columnFilters[[c]], action="and") }
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$generateCellStructure", "Generated cell structure.")
      private$addTiming("generateCellStructure", timeStart)
      return(invisible(private$cells))
    },
    resetCells = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$resetCells", "Resetting cells...")
      if(private$p_evaluated==TRUE){
        timeStart <- proc.time()
        private$p_cells$reset()
        private$p_evaluated <- FALSE
        private$p_latexRenderer$resetVisibleRange()
        private$p_fixedWidthSized <- FALSE
        private$addTiming("resetCells", timeStart)
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$resetCells", "Reset cells.")
      return(invisible())
    },
    evaluateCells = function() {
      timeStartT <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluateCells", "Evaluating cell values...")
      if(is.null(private$p_cells)) stop("PivotTable$evaluateCells():  No cells exist to calculate.", call. = FALSE)
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      calculator <- PivotCalculator$new(self)
      timeStart1 <- proc.time()
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          calculator$setWorkingData(cell)
        }
      }
      private$addTiming("evaluateCells:setWorkingData", timeStart1)
      if(private$p_evaluationMode=="batch") {
        timeStart1 <- proc.time()
        calculator$generateBatchesForCellEvaluation()
        private$addTiming("evaluateCells:generateBatchesForCellEvaluation", timeStart1)
        timeStart1 <- proc.time()
        calculator$evaluateBatches()
        private$addTiming("evaluateCells:evaluateBatches", timeStart1)
        private$p_lastCellBatchInfo <- calculator$batchInfo
      }
      timeStart1 <- proc.time()
      for(r in 1:rowCount) {
        for(c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          calculator$evaluateCell(cell)
        }
      }
      private$addTiming("evaluateCells:evaluateCell", timeStart1)
      private$p_evaluated <- TRUE
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluateCells", "Evaluated cell values.")
      private$addTiming("evaluateCells:total", timeStartT)
      return(invisible())
    },
    evaluatePivot = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluatePivot", "Evaluating pivot table...")
      self$normaliseColumnGroups()
      self$normaliseRowGroups()
      self$generateCellStructure()
      self$evaluateCells()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluatePivot", "Evaluated pivot table.")
      return(invisible())
    },
    findRowDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                                 totals="include", calculationNames=NULL, includeDescendantGroups=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findRowDataGroups", "Finding row data groups...")
      grps <- private$p_rowGroup$findDataGroups(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues,
                                                totals=totals, calculationNames=calculationNames, includeDescendantGroups=includeDescendantGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findRowDataGroups", "Found row data groups.")
      return(invisible(grps))
    },
    findColumnDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                                    totals="include", calculationNames=NULL, includeDescendantGroups=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findColumnDataGroups", "Finding column data groups...")
      grps <- private$p_columnGroup$findDataGroups(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues,
                                                   totals=totals, calculationNames=calculationNames, includeDescendantGroups=includeDescendantGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findColumnDataGroups", "Found column data groups.")
      return(invisible(grps))
    },
    getCells = function(specifyCellsAsList=FALSE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCells", "Getting cells...")
      if(!private$p_evaluated) stop("PivotTable$getCells():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getCells():  No cells exist to retrieve.", call. = FALSE)
      # need to miss the specifyCellsAsList argument out if it is missing here, so the warning message is generated
      if(missing(specifyCellsAsList)) {
        cells <- private$p_cells$getCells(rowNumbers=rowNumbers, columnNumber=columnNumbers, cellCoordinates=cellCoordinates)
      }
      else {
        cells <- private$p_cells$getCells(specifyCellsAsList=specifyCellsAsList, rowNumbers=rowNumbers, columnNumber=columnNumbers, cellCoordinates=cellCoordinates)
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCells", "Got cells.")
      return(invisible(cells))
    },
    findCells = function(variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL,
                         minValue=NULL, maxValue=NULL, exactValues=NULL, includeNull=TRUE, includeNA=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findCells", "Finding cells...")
      if(!private$p_evaluated) stop("PivotTable$findCells():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$findCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$findCells(variableNames=variableNames, variableValues=variableValues, totals=totals, calculationNames=calculationNames,
                                         minValue=minValue, maxValue=maxValue, exactValues=exactValues, includeNull=includeNull, includeNA=includeNA)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findCells", "Found cells.")
      return(invisible(cells))
    },
    print = function(asCharacter=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "print", asCharacter, missing(asCharacter), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$print", "Printing matrix...")
      lineIndex <- 0
      if(asCharacter==TRUE) returnLines <- vector("list", self$rowCount + private$p_columnGroup$getLevelCount())
      else returnLines <- NULL
      # constant
      columnPadding <- 2 # characters
      # clearing rendered flags
      private$clearIsRenderedFlags()
      # get the leaf groups
      columnLeafGroups <- private$p_columnGroup$getLeafGroups()
      if(!is.null(columnLeafGroups)) {
        if(length(columnLeafGroups)>0)
          for(c in 1:length(columnLeafGroups))
            columnLeafGroups[[c]]$fixedWidthSize <- 0
      }
      # get the data widths
      columnWidths <- NULL
      if(private$p_evaluated) {
        if(!is.null(private$p_cells)) {
          columnWidths <- private$p_cells$getColumnWidths()
          if(length(columnWidths)!=length(columnLeafGroups)) stop("PivotTable$print():  Column count - column widths mismatch!", call. = FALSE)
          for(c in 1:length(columnWidths)) {
            columnLeafGroups[[c]]$fixedWidthSize <- columnWidths[[c]] + columnPadding
          }
        }
      }
      # work up from the leaf groups setting the data group widths
      columnLevelCount <- private$p_columnGroup$getLevelCount()
      if(columnLevelCount>0) {
        for(l in columnLevelCount:1) {
          levelGroups <- private$p_columnGroup$getLevelGroups(l)
          for(c in 1:length(levelGroups)) {
            grp <- levelGroups[[c]]
            if(is.null(grp$caption)) captionWidth <- columnPadding
            else captionWidth <- nchar(grp$caption) + columnPadding
            if(length(grp$childGroups)==0) {
              # at the leaf level
              # group width is the wider of the caption width of data width
              if(captionWidth > grp$fixedWidthSize) grp$fixedWidthSize <- captionWidth
            }
            else if(length(grp$childGroups)>0) {
              # not at the leaf level (somewhere above)
              # group width is either the caption width or the sum of the child widths - whichever is larger
              totalChildWidth <- 0
              for(cg in 1:length(grp$childGroups)) {
                cgrp <- grp$childGroups[[cg]]
                totalChildWidth <- totalChildWidth + cgrp$fixedWidthSize
              }
              if(totalChildWidth>=captionWidth) grp$fixedWidthSize <- totalChildWidth
              else {
                # pad the child widths to match the caption width
                grp$fixedWidthSize <- captionWidth
                while (totalChildWidth < captionWidth) {
                  for(cg in 1:length(grp$childGroups)) {
                    cgrp <- grp$childGroups[[cg]]
                    cgrp$fixedWidthSize <- cgrp$fixedWidthSize + 1
                    totalChildWidth <- totalChildWidth + 1
                    if(totalChildWidth==captionWidth) break
                  }
                }
              }
            }
          }
        }
      }
      # calculate the widths of the row headings
      rowLevelCount <- private$p_rowGroup$getLevelCount()
      rowLevelWidths <- vector("integer", rowLevelCount)
      if(rowLevelCount>0) {
        for(l in 1:rowLevelCount) {
          levelGroups <- private$p_rowGroup$getLevelGroups(l)
          maxWidth <- 0
          for(r in 1:length(levelGroups)) {
            grp <- levelGroups[[r]]
            maxWidth <- max(maxWidth, nchar(grp$caption))
          }
          rowLevelWidths[l] <- maxWidth + columnPadding
        }
      }
      # quick functions
      repStr <- function(string, times) {
        return(paste(rep(string, times), collapse = ""))
      }
      padStr <- function(str, len, side){
        if(is.null(str)) return(repStr(" ", len))
        thisLength <- nchar(str)
        padLength <- len - thisLength
        if(side=="left") return(paste0(repStr(" ", padLength), str))
        else return(paste0(str, repStr(" ", padLength)))
      }
      # print the output string
      if(columnLevelCount>0) {
        # column headings
        for(cl in 1:columnLevelCount) {
          currentLine <- NULL
          # empty cells at top left
          if(rowLevelCount==0) currentLine <- paste0(currentLine, "  ")
          else {
            for(rl in 1:rowLevelCount) {
              currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
            }
          }
          # column headings at this level
          levelGroups <- private$p_columnGroup$getLevelGroups(cl)
          for(cg in 1:length(levelGroups)) {
            cgrp <- levelGroups[[cg]]
            if(is.null(cgrp$caption)) currentLine <- paste0(currentLine, repStr(" ", cgrp$fixedWidthSize))
            if(is.na(cgrp$caption)) currentLine <- paste0(currentLine, repStr(" ", cgrp$fixedWidthSize))
            else currentLine <- paste0(currentLine, cgrp$caption, repStr(" ", cgrp$fixedWidthSize - nchar(cgrp$caption)))
          }
          # print this line
          if(asCharacter==TRUE) {
            lineIndex <- lineIndex + 1
            returnLines[[lineIndex]] <- currentLine
          }
          else cat(paste0(currentLine, "\n"))
        }
      }
      # row headings and data
      rowLeafGroups <- private$p_rowGroup$getLeafGroups()
      columnWidths <- sapply(columnLeafGroups, function(grp) { return(grp$fixedWidthSize) })
      for(r in 1:length(rowLeafGroups)) {
        currentLine <- NULL
        # row headings
        rgrp <- rowLeafGroups[[r]]
        rancs <- rgrp$getAncestorGroups(includeCurrentGroup=TRUE)
        if(rowLevelCount != (length(rancs)-1)) stop("PivotTable$print():  Row level count - leaf group ancestor count mismatch!", call. = FALSE)
        if(rowLevelCount==0) {
          currentLine <- paste0(currentLine, "  ")
        }
        else if(rowLevelCount>0) {
          for(rl in 1:rowLevelCount) {
            rg <- rancs[[rowLevelCount-rl+1]]
            if(rg$isRendered==TRUE) {
              currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
            }
            else {
              if(is.null(rg$caption)) currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
              if(is.na(rg$caption)) currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
              else currentLine <- paste0(currentLine, rg$caption, repStr(" ", rowLevelWidths[rl] - nchar(rg$caption)))
              rg$isRendered <- TRUE
            }
          }
        }
        # data
        if(private$p_evaluated) {
          if(!is.null(private$p_cells)) {
            for(c in 1:self$columnCount) {
              cell <- private$p_cells$getCell(r, c)
              if(is.null(cell$formattedValue)) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else if(is.na(cell$formattedValue)) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else currentLine <- paste0(currentLine, repStr(" ", columnWidths[c] - 2 - nchar(cell$formattedValue)), cell$formattedValue, "  ")
            }
          }
        }
        # print this line
        if(asCharacter==TRUE) {
          lineIndex <- lineIndex + 1
          returnLines[[lineIndex]] <- currentLine
        }
        else cat(paste0(currentLine, "\n"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$print", "Printed matrix.")
      return(invisible(paste0(returnLines, sep="", collapse="\n")))
    },
    asMatrix = function(includeHeaders=TRUE, repeatHeaders=FALSE, rawValue=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asMatrix", includeHeaders, missing(includeHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asMatrix", "Getting pivot table as a matrix...",
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asMatrix", "Got pivot table as a matrix.")
      return(m)
    },
    asDataFrame = function(separator=" ") {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataFrame", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataFrame", "Getting pivot table as a data frame...", list(separator=separator))
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataFrame", "Got pivot table as a data frame.")
      return(df)
    },
    asTidyDataFrame = function(includeGroupCaptions=TRUE, includeGroupValues=TRUE, separator=" ") {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", includeGroupCaptions, missing(includeGroupCaptions), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", includeGroupValues, missing(includeGroupValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asTidyDataFrame", "Getting pivot table as a tidy data frame...",
                   list(includeGroupCaptions=includeGroupCaptions, includeGroupValues=includeGroupValues, separator=separator))
      if(!private$p_evaluated) stop("PivotTable$asDataFrame():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asDataFrame():  No cells exist to retrieve.", call. = FALSE)
      df <- list()
      vals <- list()
      # basic information
      df$rowNumber[1] <- NA
      df$columnNumber[1] <- NA
      df$isTotal[1] <- NA
      vals$calculationName[1] <- NA
      vals$calculationGroupName[1] <- NA
      vals$rawValue[1] <- NA
      vals$formattedValue[1] <- NA
      # other standard information
      if(includeGroupCaptions==TRUE) {
        # add in the level headings
        rowLevelCount <- private$p_rowGroup$getLevelCount()
        for(r in 1:rowLevelCount) {
          df[[paste0("RowLevel", sprintf("%02d", r))]][1] <- NA
        }
        columnLevelCount <- private$p_columnGroup$getLevelCount()
        for(c in 1:columnLevelCount) {
          df[[paste0("ColumnLevel", sprintf("%02d", c))]][1] <- NA
        }
      }
      # iterate the cells
      cellNumber <- 0
      if(private$p_cells$rowCount > 0) {
        for(r in 1:private$p_cells$rowCount) {
          if(private$p_cells$columnCount > 0) {
            for(c in 1:private$p_cells$columnCount) {
              # get the cell
              cell <- private$p_cells$getCell(r, c)
              if(is.null(cell)) next
              cellNumber <- cellNumber + 1
              # basic info
              df$rowNumber[cellNumber] <- r
              df$columnNumber[cellNumber] <- c
              df$isTotal[cellNumber] <- cell$isTotal
              vals$calculationName[cellNumber] <- cell$calculationName
              vals$calculationGroupName[cellNumber] <- cell$calculationGroupName
              if(!is.null(cell$rawValue)) {
                if(length(cell$rawValue)>0) {
                  if(!is.na(cell$rawValue)) vals$rawValue[cellNumber] <- cell$rawValue
                }
              }
              if(!is.null(cell$formattedValue)) {
                if(length(cell$formattedValue)>0) {
                  if(!is.na(cell$formattedValue)) vals$formattedValue[cellNumber] <- cell$formattedValue
                }
              }
              # captions
              if(includeGroupCaptions==TRUE) {
                # row heading captions
                rg <- cell$rowLeafGroup
                while(!is.null(rg)) {
                  levelNumber <- rg$getLevelNumber()
                  if(levelNumber==0) break
                  df[[paste0("RowLevel", sprintf("%02d", levelNumber))]][cellNumber] <- rg$caption
                  rg <- rg$parentGroup
                }
                # column heading captions
                cg <- cell$columnLeafGroup
                while(!is.null(cg)) {
                  levelNumber <- cg$getLevelNumber()
                  if(levelNumber==0) break
                  df[[paste0("ColumnLevel", sprintf("%02d", levelNumber))]][cellNumber] <- cg$caption
                  cg <- cg$parentGroup
                }
              }
              # filter values
              if(includeGroupValues==TRUE) {
                filters <- cell$rowColFilters
                if(!is.null(filters)) {
                  if(filters$count>0) {
                    for(i in 1:filters$count) {
                      filter <- filters$filters[[i]]
                      if(is.null(filter)) next
                      values <- filter$values
                      if(is.null(values)) values <- NA
                      if(length(values>1)) values <- paste(values, sep=separator, collapse=separator)
                      df[[filter$variableName]][cellNumber] <- values
                    }
                  }
                }
              }
            }
          }
        }
      }
      if(cellNumber==0) return(invisible())
      # append the values to the end of the data frame
      df$calculationName <- vals$calculationName
      df$calculationGroupName <- vals$calculationGroupName
      df$rawValue <- vals$rawValue
      df$formattedValue <- vals$formattedValue
      # check all of the to-be columns are of the same length and adjust if not
      maxLength <- 0
      for(i in 1:length(df)) {
        maxLength <- max(maxLength, length(df[[i]]))
      }
      for(i in 1:length(df)) {
        if(length(df[[i]]) < maxLength) df[[i]][maxLength] <- NA
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asTidyDataFrame", "Got pivot table as a tidy data frame.")
      return(invisible(as.data.frame(df)))
    },
    getCss = function(styleNamePrefix=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCss", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCss", "Getting Styles...")
      if(is.null(private$p_styles)) return("")
      if(length(private$p_styles$styles)==0) return("")
      styles <- ""
      for(s in 1:length(private$p_styles$styles)) {
        style <- private$p_styles$styles[[s]]
        if(is.null(style)) next
        styles <- paste0(styles, style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix), "\r\n")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCss", "Got Styles.")
      private$addTiming("getCss", timeStart)
      return(invisible(styles))
    },
    getHtml = function(styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                       includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                       includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeWorkingData, missing(includeWorkingData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeEvaluationFilters, missing(includeEvaluationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", includeTotalInfo, missing(includeTotalInfo), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getHtml", "Getting HTML...")
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeWorkingData=includeWorkingData,
                                                   includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                   includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getHtml", "Got HTML.")
      private$addTiming("getHtml", timeStart)
      return(invisible(htmlTable))
    },
    saveHtml = function(filePath=NULL, fullPageHTML=TRUE, styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                        includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                        includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", filePath, missing(filePath), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", fullPageHTML, missing(fullPageHTML), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeWorkingData, missing(includeWorkingData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeEvaluationFilters, missing(includeEvaluationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", includeTotalInfo, missing(includeTotalInfo), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$saveHtml", "Saving HTML...", list(filePath=filePath, fullPageHTML=fullPageHTML))
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # todo: enable rendering before cells are calculated so the structure of the pivot can be checked as it is being developed
      if(is.null(private$p_cells)) stop("PivotTable$saveHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeWorkingData=includeWorkingData,
                                                   includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                   includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo)
      if (fullPageHTML==FALSE) {
        fileConn <- file(filePath)
        writeLines(as.character(htmlTable), fileConn)
        close(fileConn)
        if(private$p_traceEnabled==TRUE) self$trace("PivotTable$saveHtml", "Saved HTML.")
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$saveHtml", "Saved HTML.")
      return(invisible())
    },
    renderPivot = function(width=NULL, height=NULL, styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                           includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                           includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", width, missing(width), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", height, missing(height), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeHeaderValues, missing(includeHeaderValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeRCFilters, missing(includeRCFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeCalculationFilters, missing(includeCalculationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeWorkingData, missing(includeWorkingData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeEvaluationFilters, missing(includeEvaluationFilters), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeCalculationNames, missing(includeCalculationNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeRawValue, missing(includeRawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", includeTotalInfo, missing(includeTotalInfo), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$renderPivot", "Rendering htmlwidget...", list(width=width, height=height, styleNamePrefix=styleNamePrefix,
                                                                             includeHeaderValues=includeHeaderValues,
                                                                             includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                             includeWorkingData=includeWorkingData,
                                                                             includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                                             includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo))
      if(!private$p_evaluated) self$evaluatePivot()
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # pivottabler(self, width=width, height=height, includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
      #                 includeCalculationNames=includeCalculationNames, includeRawValue=includeRawValue)
      settings <- list() # may need this in the future
      widgetData <- list(
        tableCss = self$getCss(styleNamePrefix=styleNamePrefix),
        tableHtml = as.character(self$getHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                            includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                            includeWorkingData=includeWorkingData,
                                            includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                            includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo)),
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
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$renderPivot", "Rendered htmlwidget.")
      return(w)
    },
    getLatex = function(caption=NULL, label=NULL, fromRow=NULL, toRow=NULL, fromColumn=NULL, toColumn=NULL,
                        boldHeadings=FALSE, italicHeadings=FALSE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", label, missing(label), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", fromRow, missing(fromRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", toRow, missing(toRow), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", fromColumn, missing(fromColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", toColumn, missing(toColumn), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", boldHeadings, missing(boldHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", italicHeadings, missing(italicHeadings), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLatex", "Getting Latex...", list(caption=caption, label=label,
                                                                                        fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn,
                                                                                        boldHeadings=boldHeadings, italicHeadings=italicHeadings))
      if(!private$p_evaluated) self$evaluatePivot()
      if(!private$p_evaluated) stop("PivotTable$getLatex():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getLatex():  No cells exist to render.", call. = FALSE)
      private$p_latexRenderer$setVisibleRange(fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn)
      ltx <- private$p_latexRenderer$getTableLatex(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLatex", "Got Latex.")
      private$addTiming("getLatex", timeStart)
      return(ltx)
    },
    trace = function(methodName, desc, detailList=NULL) {
      if(!private$p_traceEnabled) return()
      stackdepth <- length(sys.calls())
      repStr <- function(string, times) {
        return(paste(rep(string, times), collapse = ""))
      }
      indent <- repStr(" ", (stackdepth - 1) *2)
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
      if(is.null(private$p_traceFile)) { message(msg) }
      else { cat(msg, file=private$p_traceFile, sep="\r\n", append=TRUE)}
    },
    showBatchInfo = function() {
      message(self$batchInfo)
    },
    asList = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asList", "Getting list...")
      lst <- list(
        type = "pivotTable",
        dataFrames = private$p_data$asList(),
        rowGroup = private$p_rowGroup$asList(),
        columnGroup = private$p_columnGroup$asList(),
        calculationsPosition = private$p_calculationsPosition,
        calculationGroups = private$p_calculationGroups$asList()
      )
      if(!is.null(private$p_cells)) lst$cells <- private$p_cells$asList()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asList", "Got list.")
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
      if(!is.null(private$p_traceFile)) close(private$p_traceFile)
    }
  ),
  active = list(
    argumentCheckMode = function(value) { return(private$p_argumentCheckMode) },
    traceEnabled = function(value){
      if(missing(value)) return(invisible(private$p_traceEnabled))
      else {
        if(is.logical(value)) private$p_traceEnabled <- value
        else stop("PivotTable$traceEnabled: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },
    processingLibrary = function(value) { return(private$p_processingLibrary) },
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
    evaluationMode = function(value) { return(invisible(private$p_evaluationMode)) },
    batchInfo = function(value) { return(invisible(private$p_lastCellBatchInfo)) },
    cells = function(value) { return(invisible(private$p_cells)) },
    rowCount = function(value) { return(invisible(private$p_cells$rowCount)) },
    columnCount = function(value) { return(invisible(private$p_cells$columnCount)) },
    fixedWidthSized = function(value) {
      if(missing(value)) return(invisible(private$p_fixedWidthSized))
      else {
        if(is.logical(value)) private$p_fixedWidthSized <- value
        else stop("PivotTable$fixedWidthSized: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },
    asCharacter = function() { return(self$print(asCharacter=TRUE)) },
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "PivotStyles"))
        }
        if("character" %in% class(value)) private$p_styles <- getTheme(parentPivot=self, themeName=value)
        else if("PivotStyles" %in% class(value)) private$p_styles <- value
        return(invisible())
      }
    },
    styles = function(value) {
      if(missing(value)) return(invisible(private$p_styles))
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "styles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotStyles")
        }
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
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        }
        private$p_styles$allowExternalStyles <- value
        return(invisible())
      }
    },
    allTimings = function(value) {
      descriptions <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$desc), NA, x$desc)) })
      user <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["user.self"]), NA, x$time["user.self"])) })
      system <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["sys.self"]), NA, x$time["sys.self"])) })
      elapsed <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["elapsed"]), NA, x$time["elapsed"])) })
      return(data.frame(action=descriptions, user=user, system=system, elapsed=elapsed))
    },
    significantTimings = function(value) {
      df <- self$allTimings
      df <- df[df$elapsed>0.1, ]
      return(df)
    }
  ),
  private = list(
    p_argumentCheckMode = 4,
    p_traceEnabled = FALSE,
    p_processingLibrary = NULL,
    p_data = NULL,
    p_styles = NULL,
    p_rowGroup = NULL,
    p_columnGroup = NULL,
    p_calculationsPosition = NULL,
    p_calculationGroups = NULL,
    p_evaluationMode = "batch",
    p_evaluated = FALSE,
    p_cells = NULL,
    p_lastCellBatchInfo = NULL,
    p_fixedWidthSized = FALSE,
    p_htmlRenderer = NULL,
    p_latexRenderer = NULL,
    p_openxlsxRenderer = NULL,
    p_traceFile = NULL,
    p_timings = NULL,
    clearIsRenderedFlags = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$clearIsRenderedFlags", "Clearing isRendered flags...")
      rowGroups <- self$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(rowGroups, function(grp) { grp$isRendered <- FALSE })
      columnGroups <- self$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(columnGroups, function(grp) { grp$isRendered <- FALSE })
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$clearIsRenderedFlags", "Cleared isRendered flags...")
      return(invisible())
    },
    # simple mechanism to track the activities/time taken to construct the pivot table
    # should only be tracking top-level actions (i.e. methods on the pivot table) not functions which return a value (as these could be called multiple times)
    addTiming = function(descr, timeStart) {
      timeEnd <- proc.time()
      private$p_timings[[length(private$p_timings)+1]] <- list(descr=descr, time=timeEnd-timeStart)
    }
  )
)
