
#' R6 class that represents a pivot table.
#'
#' @description
#' The `PivotTable` class is the primary class for
#' constructing and interacting with a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @import htmlwidgets
#' @import htmltools
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' # The package vignettes include extensive examples of working with the
#' # PivotTable class.
#' library(pivottabler)
#' pt <- PivotTable$new()
#' pt$addData(bhmtrains)
#' pt$addColumnDataGroups("TrainCategory")
#' pt$addRowDataGroups("TOC")
#' pt$defineCalculation(calculationName="TotalTrains",
#' summariseExpression="n()")
#' pt$renderPivot()

PivotTable <- R6::R6Class("PivotTable",
  public = list(

    #' @description
    #' Create a new `PivotTable` object.
    #' @param processingLibrary The package to use when processing data.
    #' Must be one of "auto" (which today is dplyr), "dplyr" or "data.table".
    #' @param evaluationMode Either "batch" (default) or "sequential" (legacy).
    #' @param argumentCheckMode The level of argument checking to perform.
    #' Must be one of "auto", "none", "minimal", "basic", "balanced" (default)
    #' or "full".
    #' @param theme A theme to use to style the pivot table. Either:\cr
    #' (1) The name of a built in theme, or\cr
    #' (2) A list of simple style settings, or\cr
    #' (3) A `PivotStyles` object containing a full set of styles.\cr
    #' See the "Styling" vignette for many examples.
    #' @param replaceExistingStyles Default `FALSE` to retain existing styles in
    #' the styles collection and add specified styles as new custom styles.
    #' Specify `TRUE` to update the definitions of existing styles.
    #' @param tableStyle Styling to apply to the table.  Either:\cr
    #' (1) The name of a built in style, or\cr
    #' (2) A list of CSS style declarations, e.g.\cr
    #' `list("font-weight"="bold", "color"="#0000FF")`, or\cr
    #' (3) A `PivotStyle` object.
    #' @param headingStyle Styling to apply to the headings.
    #' See the `tableStyle` argument for details.
    #' @param cellStyle Styling to apply to the normal cells.
    #' See the `tableStyle` argument for details.
    #' @param totalStyle Styling to apply to the total cells.
    #' See the `tableStyle` argument for details.
    #' @param compatibility A list containing compatibility options to force
    #' legacy behaviours.  See the NEWS file for details.
    #' @param traceEnabled Default `FALSE`.  Specify `TRUE` to generate a trace
    #' for debugging purposes.
    #' @param traceFile If tracing is enabled, the location to generate the trace file.
    #' @return A new `PivotTable` object.
    initialize = function(processingLibrary="auto", evaluationMode="batch", argumentCheckMode="auto",
                          theme=NULL, replaceExistingStyles=FALSE,
                          tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL,
                          compatibility=NULL, traceEnabled=FALSE, traceFile=NULL) {
      checkArgument(4, TRUE, "PivotTable", "initialize", processingLibrary, missing(processingLibrary), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "dplyr", "data.table"))
      checkArgument(4, TRUE, "PivotTable", "initialize", evaluationMode, missing(evaluationMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("batch", "sequential"))
      checkArgument(4, TRUE, "PivotTable", "initialize", argumentCheckMode, missing(argumentCheckMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "none", "minimal", "basic", "balanced", "full"))
      checkArgument(4, TRUE, "PivotTable", "initialize", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyles"), allowedListElementClasses="character")
      checkArgument(4, TRUE, "PivotTable", "initialize", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument(4, TRUE, "PivotTable", "initialize", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
      checkArgument(4, TRUE, "PivotTable", "initialize", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
      checkArgument(4, TRUE, "PivotTable", "initialize", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
      checkArgument(4, TRUE, "PivotTable", "initialize", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "PivotStyle"))
      checkArgument(4, TRUE, "PivotTable", "initialize", compatibility, missing(compatibility), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric", "logical"))
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
      private$p_compatibility <- compatibility
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
      private$p_defaults <- list()
      private$p_lastInstanceId <- 0
      # Create the basic parts of the pivot table
      private$p_data <- PivotData$new(parentPivot=self)
      private$p_rowGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="row", isLevelTotal=TRUE)
      private$p_columnGroup <- PivotDataGroup$new(parentPivot=self, parentGroup=NULL, rowOrColumn="column", isLevelTotal=TRUE)
      private$p_rowGrpHeaders <- list()
      private$p_calculationsPosition <- NULL
      private$p_calculationGroups <- PivotCalculationGroups$new(parentPivot=self)
      private$p_cells <- PivotCells$new(self)
      private$p_htmlRenderer <- PivotHtmlRenderer$new(parentPivot=self)
      private$p_latexRenderer <- PivotLatexRenderer$new(parentPivot=self)
      private$p_openxlsxRenderer <- PivotOpenXlsxRenderer$new(parentPivot=self)
      private$p_timings <- list()
      # apply theming and styles
      if(is.null(theme)) {
        private$p_styles <- getTheme(parentPivot=self, themeName="default")
      }
      else {
        if("PivotStyles" %in% class(theme)) { private$p_styles <- theme }
        else if("list" %in% class(theme)) {
          private$p_styles <- getSimpleColoredTheme(parentPivot=self, themeName="coloredTheme", theme=theme)
        }
        else if("character" %in% class(theme)) {
          if(tolower(trimws(theme))=="none") { theme <- "blank" }
          private$p_styles <- getTheme(parentPivot=self, themeName=theme)
        }
      }
      if(!is.null(tableStyle)) {
        if("PivotStyle" %in% class(tableStyle)) { tableStyle <- tableStyle$declarations }
        if("list" %in% class(tableStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$tableStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$tableStyle)$setPropertyValues(declarations=tableStyle)
            tableStyle <- private$p_styles$tableStyle
          }
          else {
            private$p_styles$addStyle(styleName="customTableStyle", declarations=tableStyle)
            tableStyle <- "customTableStyle"
          }
        }
        if("character" %in% class(tableStyle)) { private$p_styles$tableStyle <- tableStyle }
      }
      if(!is.null(headingStyle)) {
        if("PivotStyle" %in% class(headingStyle)) { headingStyle <- headingStyle$declarations }
        # root style
        rootStyle <- headingStyle
        if("list" %in% class(rootStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$rootStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$rootStyle)$setPropertyValues(declarations=rootStyle)
            rootStyle <- private$p_styles$rootStyle
          }
          else {
            private$p_styles$addStyle(styleName="customRootStyle", declarations=rootStyle)
            rootStyle <- "customRootStyle"
          }
        }
        if("character" %in% class(rootStyle)) { private$p_styles$rootStyle <- rootStyle }
        # row heading style
        rowHeaderStyle <- headingStyle
        if("list" %in% class(rowHeaderStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$rowHeaderStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$rowHeaderStyle)$setPropertyValues(declarations=rowHeaderStyle)
            rowHeaderStyle <- private$p_styles$rowHeaderStyle
          }
          else {
            private$p_styles$addStyle(styleName="customRowHeadingStyle", declarations=rowHeaderStyle)
            rowHeaderStyle <- "customRowHeadingStyle"
          }
        }
        if("character" %in% class(rowHeaderStyle)) { private$p_styles$rowHeaderStyle <- rowHeaderStyle }
        # column heading style
        colHeaderStyle <- headingStyle
        if("list" %in% class(colHeaderStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$colHeaderStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$colHeaderStyle)$setPropertyValues(declarations=colHeaderStyle)
            colHeaderStyle <- private$p_styles$colHeaderStyle
          }
          else {
            private$p_styles$addStyle(styleName="customColHeadingStyle", declarations=colHeaderStyle)
            colHeaderStyle <- "customColHeadingStyle"
          }
        }
        if("character" %in% class(colHeaderStyle)) { private$p_styles$colHeaderStyle <- colHeaderStyle }
      }
      if(!is.null(cellStyle)) {
        if("PivotStyle" %in% class(cellStyle)) { cellStyle <- cellStyle$declarations }
        if("list" %in% class(cellStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$cellStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$cellStyle)$setPropertyValues(declarations=cellStyle)
            cellStyle <- private$p_styles$cellStyle
          }
          else {
            private$p_styles$addStyle(styleName="customCellStyle", declarations=cellStyle)
            cellStyle <- "customCellStyle"
          }
        }
        if("character" %in% class(cellStyle)) { private$p_styles$cellStyle <- cellStyle }
      }
      if(!is.null(totalStyle)) {
        if("PivotStyle" %in% class(totalStyle)) { totalStyle <- totalStyle$declarations }
        if("list" %in% class(totalStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$totalStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$totalStyle)$setPropertyValues(declarations=totalStyle)
            totalStyle <- private$p_styles$totalStyle
          }
          else {
            private$p_styles$addStyle(styleName="customTotalStyle", declarations=totalStyle)
            totalStyle <- "customTotalStyle"
          }
        }
        if("character" %in% class(totalStyle)) { private$p_styles$totalStyle <- totalStyle }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$new", "Created new Pivot Table.")
      return(invisible())
    },

    #' @description
    #' Specify default values for some function arguments.
    #' @param ... Default values to specify.  See details.
    #' @details
    #' Defaults can be set for the following arguments of
    #' `pt$addRowDataGroups()` and `pt$addColumnDataGroups()`:
    #' `logical` values: `addTotal`, `expandExistingTotals`, `visualTotals`.
    #' `character` values:  `totalPosition`, `totalCaption`.
    #' `list` or `logical` values:  `outlineBefore`, `outlineAfter`, `outlineTotal`.\cr
    #' Errors are generated for default values that could not be set.\cr
    #' Warnings are generated for attempts to set defaults that aren't supported.\cr
    #' See the "A1. Appendix" vignette for more details.
    #' @return No return value.
    setDefault = function(...) {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setDefault", "Setting defaults...")
      # get the defaults
      args <- list(...)
      # process each of the defaults...
      invaligArgs <- vector("character", 0)
      for(argName in names(args)) {
        validArg <- FALSE
        # skip defaults with no name
        if(is.null(argName)||(nchar(argName)==0)) {
          stop("PivotTable$setDefault():  Default with no name encountered.  Arguments must be specified in the form name=value.", call. = FALSE)
        }
        # get value
        argValue <- args[[argName]]
        # logical defaults
        if(argName %in% c("addTotal", "expandExistingTotals", "visualTotals")) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", paste0("setDefault(", argName, ")"), argValue, FALSE, allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
          private$p_defaults[[argName]] <- isTRUE(argValue)
          validArg <- TRUE
        }
        # character defaults
        if(argName %in% c("totalPosition", "totalCaption")) {
          allowedValues <- NULL
          if(argName=="totalPosition") allowedValues <- c("before", "after")
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", paste0("setDefault(", argName, ")"), argValue, FALSE, allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=allowedValues)
          private$p_defaults[[argName]] <- argValue
          validArg <- TRUE
        }
        # outline settings
        if(argName %in% c("outlineBefore", "outlineAfter", "outlineTotal")) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", paste0("setDefault(", argName, ")"), argValue, FALSE, allowMissing=FALSE, allowNull=FALSE, allowedClasses="list")
          cleanOutline <- NULL
          if(argName=="outlineBefore") cleanOutline <- cleanOutlineArg(self, argValue)
          else if(argName=="outlineAfter") cleanOutline <- cleanOutlineArg(self, argValue, defaultCaption="")
          else if(argName=="outlineTotal") cleanOutline <- cleanOutlineArg(self, argValue, defaultCaption="Total", defaultIsEmpty=FALSE)
          if(!is.null(cleanOutline)) private$p_defaults[[argName]] <- cleanOutline
          validArg <- TRUE
        }
        # was it a valid argument?
        if(validArg==FALSE) {
          invaligArgs[length(invaligArgs)+1] <- argName
        }
      }
      # warn about any unknown arguments
      if(length(invaligArgs)>0) {
        warning(paste0("PivotTable$setDefault():  Unknown default(s) encountered: ", paste(invaligArgs,collapse=", ")), call. = FALSE)
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setDefault", "Set defaults.")
    },

    #' @description
    #' Get the default value of an argument.
    #' @param argValue The name and value of the argument.
    #' @param useDefault Specify `TRUE` to use the default.
    #' @details
    #' Both the argument name and argument value are taken from the `argValue`
    #' argument.  The name is obtained from `as.character(substitute(argValue))`.
    #' This function is designed to easily slot into existing code, e.g.
    #' `getDefault1(addTotal, missing(addTotal))`.
    #' @return The current value of the argument or the default value.
    getDefault1 = function(argValue=NULL, useDefault=NULL) {
      argName <- as.character(substitute(argValue))
      return(invisible(self$getDefault2(argName=argName, argValue=argValue, useDefault=useDefault)))
    },

    #' @description
    #' Get the default value of an argument.
    #' @param argName The name of the argument.
    #' @param argValue The current value of the argument.
    #' @param useDefault Specify `TRUE` to use the default.
    #' @return The current value of the argument or the default value.
    getDefault2 = function(argName=NULL, argValue=NULL, useDefault=NULL) {
      if(!isTRUE(useDefault)) return(invisible(argValue))
      defaultNames <- names(private$p_defaults)
      if(argName %in% defaultNames) return(invisible(private$p_defaults[[argName]]))
      else return(invisible(argValue))
    },

    #' @description
    #' Get the default value of an argument.
    #' @param argName The name of the argument.
    #' @return The default value.
    getDefault3 = function(argName) {
      defaultNames <- names(private$p_defaults)
      if(argName %in% defaultNames) return(invisible(private$p_defaults[[argName]]))
      else return(invisible(NULL))
    },

    #' @description
    #' Get the next unique object instance identifier.
    #' @details
    #' R6 classes cannot be easily compared to check if two variables are both
    #' referring to the same object instance.  Instance ids are a mechanism to
    #' work around this problem.  Each data group and cell is assigned an
    #' instance id during object creation, which enables reliable reference
    #' comparisons.
    #' @return An integer instance id.
    getNextInstanceId = function() { # used for reliable object instance comparisons (since R6 cannot easily compare object instances)
      private$p_lastInstanceId <- private$p_lastInstanceId + 1
      return(invisible(private$p_lastInstanceId))
    },

    #' @description
    #' Add a data frame with the specified name to the pivot table.
    #' @param dataFrame The data frame to add.
    #' @param dataName The name to be used to refer to the data frame.
    #' If no name is specified, the data frame variable name from the
    #' calling code is used, retrieved via `deparse(substitute(dataFrame))`.
    #' @details
    #' The name is used to refer to the data frame when generating data groups
    #' or defining calculations.  The pivot table tracks the first data frame
    #' added as the default data frame, so if only a single data frame is used,
    #' it is typically not necessary to ever explicitly refer to the name.
    #' Pivot tables are typically based on a single data frame, however it
    #' is possible to build a pivot table that uses data from multiple data
    #' frames.
    #' @return The `PivotData` object managing the data frames for the pivot table.
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

    #' @description
    #' Add a data frame containing totals data with the specified name and
    #' variables to the pivot table.
    #' @param dataFrame The data frame to add.
    #' @param dataName The name of the data frame to associate these totals with.
    #' @param variableNames A vector specifying how the aggregate data/totals in
    #' the data frame are grouped.
    #' @details
    #' When generating pivot tables, the package typically calculates cell values.
    #' However, the package can also use provided values (i.e. carry out no
    #' calculations).  This presents a challenge in that the sub-totals and totals
    #' in a pivot table display values at a higher aggregation level than the
    #' normal cells in the body of the pivot table.  This method allows further
    #' data frames to be specified that contain aggregated versions of the data.
    #' See the "Calculations" vignette for details and an example.
    #' @return No return value.
    addTotalData = function(dataFrame=NULL, dataName=NULL, variableNames=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addTotalData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addTotalData", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addTotalData", variableNames, missing(variableNames), allowMissing=FALSE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addTotalData", "Adding totals data to Pivot Table...")
      private$p_data$addTotalData(dataFrame, dataName=dataName, variableNames=variableNames)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addTotalData", "Added totals data to Pivot Table.")
      private$addTiming("addTotalData()", timeStart)
      return(invisible())
    },

    #' @description
    #' Retrieve the data groups at the specified level or levels in the column
    #' groups hierarchy.
    #' @param level An integer value or vector specifying one or more level numbers.
    #' Level 1 represents the first visible level of data groups.
    #' @param collapse A logical value specifying whether the return value should be
    #' simplified.  See details.
    #' @details
    #' If `level` is a vector:  If `collapse` is `FALSE`, then a list of lists is
    #' returned, if `collapse` is `TRUE`, then a single combined list is returned.
    #' @return A list containing `PivotDataGroup` objects.
    getColumnGroupsByLevel = function(level=NULL, collapse=FALSE) {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getColumnGroupsByLevel", "Getting level column groups...")
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getColumnGroupsByLevel", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getColumnGroupsByLevel", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple levels
      if(length(level)>1) {
        fx <- function(x) { return(self$getColumnGroupsByLevel(level=x)) }
        if(isTRUE(collapse)) { return(invisible(unlist(lapply(level, fx)))) }
        else { return(invisible(lapply(level, fx))) }
      }
      # single level
      if(level<1) stop("PivotTable$getColumnGroupsByLevel():  level must be greater than or equal to one.", call. = FALSE)
      levelCount <- self$columnGroupLevelCount
      if(level>levelCount) stop(paste0("PivotTable$getColumnGroupsByLevel():  level must be less than or equal to ", levelCount, "."), call. = FALSE)
      grps <- private$p_columnGroup$getLevelGroups(level=level)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getColumnGroupsByLevel", "Got level column groups.", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' [Deprecated: Use topColumnGroups instead]
    #' Retrieve the first level of column data groups.
    #' @return A list containing `PivotDataGroup` objects.
    getTopColumnGroups = function() {
      .Deprecated(new="topColumnGroups")
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopColumnGroups", "Getting top level column groups...")
      grps <- private$p_columnGroup$childGroups
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopColumnGroups", "Got top level column groups.", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' [Deprecated: Use leafColumnGroups instead]
    #' Retrieve the bottom level of column data groups.
    #' @return A list containing `PivotDataGroup` objects.
    getLeafColumnGroups = function() {
      .Deprecated(new="leafColumnGroups")
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafColumnGroups", "Getting leaf level column groups...")
      leafGroups = list()
      grps <- private$p_columnGroup$getLeafGroups(leafGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafColumnGroups", "Got leaf level column groups.", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' Retrieve the leaf-level data group associated with a specific column or
    #' columns.
    #' @param c An integer column number or an integer vector of column numbers.
    #' @return A `PivotDataGroup` object or a list of `PivotDataGroup` objects.
    getLeafColumnGroup = function(c=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLeafColumnGroup", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      # multiple columns
      if(length(c)>1) {
        return(invisible(lapply(c, self$getLeafColumnGroup)))
      }
      # single column
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafColumnGroup", "Getting leaf level column group...")
      if((private$p_evaluated)&&(!is.null(private$p_cells))) {
        # retrieve directly from the cells instance
        grp <- private$p_cells$getColumnGroup(c=c)
      }
      else {
        # fall back to the slower method (requires navigating through the hierarchy)
        grps <- self$leafColumnGroups
        if(length(grps)>c) grp <- grps[[c]]
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafColumnGroup", "Got leaf level column group.")
      return(invisible(grp))
    },

    #' @description
    #' Add a new column data group at the top level of the column group
    #' hierarchy.  The new group is added as the last child unless an index
    #' is specified.
    #' @details See the "Irregular Layout" vignette for details and examples.
    #' @param variableName A character value that specifies the name of the
    #' variable in the data frame that the group relates to and will filter.
    #' @param filterType Must be one of "ALL", "VALUES", or "NONE" to specify
    #' the filter type:\cr
    #' ALL means no filtering is applied.\cr
    #' VALUEs is the typical value used to specify that `variableName` is
    #' filtered to only `values`.\cr
    #' NONE means no data will match this data group.
    #' @param values A vector that specifies the filter values applied to
    #' `variableName` to select the data to match this row/column in the pivot
    #' table.
    #' @param doNotExpand Default value `FALSE` - specify `TRUE` to
    #' prevent the high-level methods such as `addDataGroups()` from adding
    #' child groups.
    #' @param isEmpty Default value `FALSE`, specify `TRUE` to mark that
    #' this group contains no data (e.g. if
    #' it is part of a header or outline row)
    #' @param isOutline Default value `FALSE` - specify `TRUE` to mark
    #' that this data group is an outline group.
    #' @param styleAsOutline Default value `FALSE` - specify `TRUE` to style
    #' this data group as an outline group.  Only applicable when
    #' `isOutline` is `TRUE`.
    #' @param captionTemplate A character value that specifies the template
    #' for the data group caption, default "{values}".
    #' @param caption Effectively a hard-coded caption that overrides the
    #' built-in logic for generating a caption.
    #' @param isTotal Default `FALSE` - specify `TRUE` to mark that this
    #' data group is a total.
    #' @param isLevelSubTotal Default `FALSE` - specify `TRUE` to mark that
    #' this data group is a sub-total within a level.
    #' @param isLevelTotal Default `FALSE` - specify `TRUE` to mark that
    #' this data group is level total.
    #' @param calculationGroupName For calculation groups, this character value
    #' specifies the calculation group that `calculationName` belongs to.
    #' @param calculationName For calculation groups, this character value
    #' specifies the name of the calculation.
    #' @param baseStyleName The style name for the data group.
    #' @param styleDeclarations A list of CSS style declarations to overlay
    #' on top of the base style.
    #' @param insertAtIndex An integer that specifies the index in the list
    #' of child groups where the new group should be inserted.
    #' @param insertBeforeGroup Specifies an existing group that the new group
    #'  should be inserted before.
    #' @param insertAfterGroup Specifies an existing group that the new group
    #'  should be inserted after
    #' @param mergeEmptySpace A character value that specifies how empty space
    #' should be merged. This is typically only used with outline groups
    #' (so applies to row groups only, not column groups).  Must be one of
    #' "doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1" or
    #' "dataGroupsAndCellsAs2".  See the "Regular Layout" vignette
    #' for more information.
    #' @param cellBaseStyleName The style name for cells related to this data
    #' group.
    #' @param cellStyleDeclarations A list of CSS style declarations to overlay
    #' on top of the base style for cells related to this data group
    #' @param sortAnchor Used to specify sort behaviour for outline groups, must
    #' be one of "fixed", "next" or "previous".
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return The new `PivotDataGroup` object.
    addColumnGroup = function(variableName=NULL, filterType="ALL", values=NULL,
                             doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, styleAsOutline=FALSE,
                             captionTemplate="{value}", caption=NULL,
                             isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
                             calculationGroupName=NULL, calculationName=NULL,
                             baseStyleName=NULL, styleDeclarations=NULL,
                             insertAtIndex=NULL, insertBeforeGroup=NULL, insertAfterGroup=NULL,
                             mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                             sortAnchor=NULL, resetCells=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", styleAsOutline, missing(styleAsOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", captionTemplate, missing(captionTemplate), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", insertAtIndex, missing(insertAtIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", insertBeforeGroup, missing(insertBeforeGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", insertAfterGroup, missing(insertAfterGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", mergeEmptySpace, missing(mergeEmptySpace), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_argumentCheckMode==TRUE) private$p_traceEnabled("PivotTable$addColumnGroup", "Adding column group...",
                                                                   list(captionTemplate=captionTemplate, caption=caption,
                                                                        doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                                                                        isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                                                        variableName=variableName, values=values,
                                                                        calculationGroupName=calculationGroupName, calculationName=calculationName,
                                                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                                        mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName,
                                                                        cellStyleDeclarations=cellStyleDeclarations, sortAnchor=sortAnchor,
                                                                        resetCells=resetCells))
      # default to style as outline if not specified
      if(isOutline && missing(styleAsOutline)) styleAsOutline <- TRUE
      # add the group
      grp <- private$p_columnGroup$addChildGroup(variableName=variableName, filterType=filterType, values=values,
                                                 doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                                                 captionTemplate=captionTemplate, caption=caption,
                                                 isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                                 calculationGroupName=calculationGroupName, calculationName=calculationName,
                                                 baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                 insertAtIndex=insertAtIndex, insertBeforeGroup=insertBeforeGroup, insertAfterGroup=insertAfterGroup,
                                                 mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations,
                                                 sortAnchor=sortAnchor, resetCells=resetCells)
      if(private$p_argumentCheckMode==TRUE) private$p_traceEnabled("PivotTable$addColumnGroup", "Added column group.")
      return(invisible(grp))
    },

    #' @description
    #' Add multiple new data groups to the column group hierarchy
    #' based on the distinct values in a data frame
    #' column or using explicitly specified data values.
    #' See the "Data Groups" vignette for example usage.
    #' @details
    #' There are broadly three different ways to call `addColumnDataGroups()`:\cr
    #' (1) dataName=name, fromData=TRUE, onlyCombinationsThatExist=TRUE - which
    #' considers the ancestors of each existing data group to generate only those
    #' combinations of values that exist in the data frame.\cr
    #' (2) dataName=name, fromData=TRUE, onlyCombinationsThatExist=FALSE - which
    #' ignores the ancestors of each existing data group and simply adds every
    #' distinct value of the specified variable under every existing data group,
    #' which can result in combinations of values in the pivot table that don't
    #' exist in the data frame (i.e. blank rows/columns in the pivot table).\cr
    #' (3) fromData=FALSE, explicitListOfValues=list(...) - simply adds every
    #' value from the specified list under every existing data group.
    #' @param variableName The name of the related column in the data frame(s) of
    #'   the pivot table.
    #' @param atLevel The level number that specifies where to add the new
    #' groups.  Level 1 = on the first visible level of the hierarchy.
    #' `NULL` = create a new level at the bottom of the hierarchy for the new
    #' groups.
    #' @param fromData Default `TRUE` to generate the new data groups based on the
    #' data values that exist in the `variableName` column in the named data frame.
    #' If `FALSE`, then `explicitListOfValues` must be specified.
    #' @param dataName The name of the data frame (as specified in
    #' `pt$addData()`) to read the data group values from.
    #' @param dataSortOrder Must be one of "asc", "desc", "custom" or "none".
    #' @param customSortOrder A vector values sorted into the desired order.
    #' @param caption The template of data group captions to generate,
    #' default "{value}".
    #' @param dataFormat A character, list or custom function to format the
    #' data value.
    #' @param dataFmtFuncArgs A list that specifies any additional arguments to
    #' pass to a custom format function.
    #' @param onlyCombinationsThatExist Default `TRUE` to generate only
    #' combinations of data groups that exist in the data frame.
    #' @param explicitListOfValues A list of explicit values to create data
    #' groups from.  A data group is created for each element of the list.
    #' If a list element is vector of values (with length greater than 1),
    #' then a data group is created for multiple values instead of just
    #' a single value.
    #' @param calculationGroupName The calculation group that the new data groups
    #' are related to.
    #' @param expandExistingTotals Default `FALSE`, which means totals are not
    #' broken down in multi-level hierarchies.
    #' @param addTotal Default `TRUE`, which means sub-total and total data groups
    #' are automatically added.
    #' @param visualTotals Default `FALSE`, which means visual totals are disabled.
    #' See the "Data Groups" vignette for more details about visual totals.
    #' @param totalPosition Either "before" or "after" to specify where total groups
    #' are created, default "after".
    #' @param totalCaption The caption to display on total groups, default "Total".
    #' @param onlyAddGroupIf A filter expression that can be used to more finely
    #' control whether data groups are created at different locations in the
    #' hierarchy.  There must be at least one row that matches this filter and the
    #' filters from the ancestor groups in order that the child group is created.
    #' E.g. `MaxDisplayLevel>5`.
    #' @param preGroupData Default `TRUE`, which means that the pivot table
    #' pre-calculates the distinct combinations of variable values to reduce the CPU
    #' time and elapsed time required to generate data groups.
    #' Cannot be used in conjunction with the
    #' @param baseStyleName The name of the style applied to this data group (i.e.
    #'   this row/column heading).  The style must exist in the `PivotStyles` object
    #'   associated with the PivotTable.
    #' @param styleDeclarations CSS style declarations that can override the base
    #' style, expressed as a list, e.g. `list("font-weight"=bold")`.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    addColumnDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                   dataName=NULL, dataSortOrder="asc", customSortOrder=NULL, caption="{value}", dataFormat=NULL, dataFmtFuncArgs=NULL,
                                   onlyCombinationsThatExist=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                                   expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                                   onlyAddGroupIf=NULL, preGroupData=TRUE, baseStyleName=NULL, styleDeclarations=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "custom", "none"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", customSortOrder, missing(customSortOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", caption, missing(caption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", dataFmtFuncArgs, missing(dataFmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", onlyAddGroupIf, missing(onlyAddGroupIf), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", preGroupData, missing(preGroupData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnDataGroups", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnDataGroups", "Adding column groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                        dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                        caption=caption, dataFormat=dataFormat, dataFmtFuncArgs=dataFmtFuncArgs,
                        onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                        calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                        addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                        onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations))
      if((!is.null(styleDeclarations))&&(length(styleDeclarations)!=length(names(styleDeclarations))))
        stop("PivotTable$addColumnDataGroups(): One or more style declarations are missing a name.", call. = FALSE)
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grp <- private$p_columnGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                                 dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                                                 caption=caption, dataFormat=dataFormat, dataFmtFuncArgs=dataFmtFuncArgs,
                                                 onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                                 calculationGroupName=calculationGroupName,
                                                 expandExistingTotals=self$getDefault1(expandExistingTotals, missing(expandExistingTotals)),
                                                 addTotal=self$getDefault1(addTotal, missing(addTotal)),
                                                 visualTotals=self$getDefault1(visualTotals, missing(visualTotals)),
                                                 totalPosition=self$getDefault1(totalPosition, missing(totalPosition)),
                                                 totalCaption=self$getDefault1(totalCaption, missing(totalCaption)),
                                                 onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnDataGroups", "Added column groups.")
      private$addTiming(paste0("addColumnDataGroups(", variableName, ")"), timeStart)
      return(invisible(grp))
    },

    #' @description
    #' Normalise the column data group hierarchy so that all branches have the
    #' same number of levels - accomplished by adding empty child data groups
    #' where needed.
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    normaliseColumnGroups = function(resetCells=TRUE) {
      timeStart <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseColumnGroups", "Normalising column groups...")
      groupsAdded <- private$p_columnGroup$normaliseDataGroup(resetCells)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseColumnGroups", "Normalised column groups.", list(groupsAdded = groupsAdded))
      private$addTiming("normaliseColumnGroups", timeStart)
      return(invisible())
    },

    #' @description
    #' Sort column data groups either by the data group data value, caption,
    #' a custom order or based on calculation result values.
    #' @param levelNumber The level number to sort the data groups, e.g.
    #' level 1 (default) sorts the data groups at level 1 of the hierarchy
    #' (which is the first visible level of data groups).
    #' @param orderBy Must be either "value", "caption", "calculation",
    #' "customByValue" or "customByCaption".\cr
    #' "value" sorts by the raw (i.e. unformatted) group value.\cr
    #' "caption" sorts by the formatted character group caption.\cr
    #' "calculation" sorts using one of the calculations defined in the pivot table.\cr
    #' "customValue" sorts by the raw (i.e. unformatted) group value according to
    #' the specified custom sort order.\cr
    #' "customCaption" sorts by the formatted character group caption according to
    #' the specified custom sort order.
    #' @param customOrder A vector values sorted into the desired order.
    #' @param sortOrder Must be either "asc" or "desc".
    #' @param calculationGroupName If sorting using a calculation, the name of the
    #' calculation group containing the specified calculation.
    #' @param calculationName If sorting using a calculation, the name of the
    #' calculation.
    #' @param fromIndex A boundary to limit the sort operation.
    #' @param toIndex A boundary to limit the sort operation.
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return No return value.
    sortColumnDataGroups = function(levelNumber=1, orderBy="calculation", customOrder=NULL, sortOrder="desc",
                                    calculationGroupName="default", calculationName=NULL,
                                    fromIndex=NULL, toIndex=NULL, resetCells=TRUE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation","customByValue","customByCaption"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", customOrder, missing(customOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", fromIndex, missing(fromIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", toIndex, missing(toIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortColumnDataGroups", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortColumnDataGroups", "Sorting column data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, customOrder=customOrder, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName,
                         fromIndex=fromIndex, toIndex=toIndex, resetCells=resetCells))
      if(levelNumber<1) stop("PivotTable$sortColumnDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_columnGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, customOrder=customOrder, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName,
                                           fromIndex=fromIndex, toIndex=toIndex, resetCells=resetCells)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortColumnDataGroups", "Sorted column data groups.")
      private$addTiming("sortColumnDataGroups", timeStart)
      return(invisible())
    },

    #' @description
    #' Retrieve the data groups at the specified level or levels in the row groups
    #' hierarchy.
    #' @param level An integer value or vector specifying one or more level numbers.
    #' Level 1 represents the first visible level of data groups.
    #' @param collapse A logical value specifying whether the return value should be
    #' simplified.  See details.
    #' @details
    #' If `level` is a vector:  If `collapse` is `FALSE`, then a list of lists is
    #' returned, if `collapse` is `TRUE`, then a single combined list is returned.
    #' @return A list containing `PivotDataGroup` objects.
    getRowGroupsByLevel = function(level=NULL, collapse=FALSE) {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getRowGroupsByLevel", "Getting level row groups...")
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getRowGroupsByLevel", level, missing(level), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getRowGroupsByLevel", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple levels
      if(length(level)>1) {
        fx <- function(x) { return(self$getRowGroupsByLevel(level=x)) }
        if(isTRUE(collapse)) { return(invisible(unlist(lapply(level, fx)))) }
        else { return(invisible(lapply(level, fx))) }
      }
      # single level
      if(level<1) stop("PivotTable$getRowGroupsByLevel():  level must be greater than or equal to one.", call. = FALSE)
      levelCount <- self$rowGroupLevelCount
      if(level>levelCount) stop(paste0("PivotTable$getRowGroupsByLevel():  level must be less than or equal to ", levelCount, "."), call. = FALSE)
      grps <- private$p_rowGroup$getLevelGroups(level=level)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getRowGroupsByLevel", "Got level row groups", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' [Deprecated: Use topRowGroups instead]
    #' Retrieve the first level of row data groups.
    #' @return A list containing `PivotDataGroup` objects.
    getTopRowGroups = function() {
      .Deprecated(new="topRowGroups")
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Getting top level row groups...")
      grps <- private$p_rowGroup$childGroups
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Got top level row groups", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' [Deprecated: Use leafRowGroups instead]
    #' Retrieve the bottom level of row data groups.
    #' @return A list containing `PivotDataGroup` objects.
    getLeafRowGroups = function() {
      .Deprecated(new="leafRowGroups")
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafRowGroups", "Getting leaf level row groups...")
      leafGroups = list()
      grps <- private$p_rowGroup$getLeafGroups(leafGroups)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getTopRowGroups", "Got leaf level row groups", list(count = length(grps)))
      return(invisible(grps))
    },

    #' @description
    #' Retrieve the leaf-level data group associated with a specific row or rows.
    #' @param r An integer row number or an integer vector of row numbers.
    #' @return A `PivotDataGroup` object or a list of `PivotDataGroup` objects.
    getLeafRowGroup = function(r=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLeafRowGroup", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      # multiple rows
      if(length(r)>1) {
        return(invisible(lapply(r, self$getLeafRowGroup)))
      }
      # single row
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafRowGroup", "Getting leaf level row group...")
      if((private$p_evaluated)&&(!is.null(private$p_cells))) {
        # retrieve directly from the cells instance
        grp <- private$p_cells$getRowGroup(r=r)
      }
      else {
        # fall back to the slower method (requires navigating through the hierarchy)
        grps <- self$leafRowGroups
        if(length(grps)>r) grp <- grps[[r]]
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLeafRowGroup", "Got leaf level row group.")
      return(invisible(grp))
    },

    #' @description
    #' Add a new column data group at the top level of the row group
    #' hierarchy.  The new group is added as the last child unless an index
    #' is specified.
    #' @details See the "Irregular Layout" vignette for details and examples.
    #' @param variableName A character value that specifies the name of the
    #' variable in the data frame that the group relates to and will filter.
    #' @param filterType Must be one of "ALL", "VALUES", or "NONE" to specify
    #' the filter type:\cr
    #' ALL means no filtering is applied.\cr
    #' VALUEs is the typical value used to specify that `variableName` is
    #' filtered to only `values`.\cr
    #' NONE means no data will match this data group.
    #' @param values A vector that specifies the filter values applied to
    #' `variableName` to select the data to match this row/column in the pivot
    #' table.
    #' @param doNotExpand Default value `FALSE` - specify `TRUE` to
    #' prevent the high-level methods such as `addDataGroups()` from adding
    #' child groups.
    #' @param isEmpty Default value `FALSE`, specify `TRUE` to mark that
    #' this group contains no data (e.g. if
    #' it is part of a header or outline row)
    #' @param isOutline Default value `FALSE` - specify `TRUE` to mark
    #' that this data group is an outline group.
    #' @param styleAsOutline Default value `FALSE` - specify `TRUE` to style
    #' this data group as an outline group.  Only applicable when
    #' `isOutline` is `TRUE`.
    #' @param captionTemplate A character value that specifies the template
    #' for the data group caption, default "{values}".
    #' @param caption Effectively a hard-coded caption that overrides the
    #' built-in logic for generating a caption.
    #' @param isTotal Default `FALSE` - specify `TRUE` to mark that this
    #' data group is a total.
    #' @param isLevelSubTotal Default `FALSE` - specify `TRUE` to mark that
    #' this data group is a sub-total within a level.
    #' @param isLevelTotal Default `FALSE` - specify `TRUE` to mark that
    #' this data group is level total.
    #' @param calculationGroupName For calculation groups, this character value
    #' specifies the calculation group that `calculationName` belongs to.
    #' @param calculationName For calculation groups, this character value
    #' specifies the name of the calculation.
    #' @param baseStyleName The style name for the data group.
    #' @param styleDeclarations A list of CSS style declarations to overlay
    #' on top of the base style.
    #' @param insertAtIndex An integer that specifies the index in the list
    #' of child groups where the new group should be inserted.
    #' @param insertBeforeGroup Specifies an existing group that the new group
    #'  should be inserted before.
    #' @param insertAfterGroup Specifies an existing group that the new group
    #'  should be inserted after
    #' @param mergeEmptySpace A character value that specifies how empty space
    #' should be merged. This is typically only used with outline groups
    #' (so applies to row groups only, not column groups).  Must be one of
    #' "doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1" or
    #' "dataGroupsAndCellsAs2".  See the "Regular Layout" vignette
    #' for more information.
    #' @param cellBaseStyleName The style name for cells related to this data
    #' group.
    #' @param cellStyleDeclarations A list of CSS style declarations to overlay
    #' on top of the base style for cells related to this data group
    #' @param sortAnchor Used to specify sort behaviour for outline groups, must
    #' be one of "fixed", "next" or "previous".
    #' @param outlineLinkedGroupId Used to link an outline group to the value
    #' data group which has the child data groups.
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return The new `PivotDataGroup` object.
    addRowGroup = function(variableName=NULL, filterType="ALL", values=NULL,
                           doNotExpand=FALSE, isEmpty=FALSE, isOutline=FALSE, styleAsOutline=FALSE,
                           captionTemplate="{value}", caption=NULL,
                           isTotal=FALSE, isLevelSubTotal=FALSE, isLevelTotal=FALSE,
                           calculationGroupName=NULL, calculationName=NULL,
                           baseStyleName=NULL, styleDeclarations=NULL,
                           insertAtIndex=NULL, insertBeforeGroup=NULL, insertAfterGroup=NULL,
                           mergeEmptySpace=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                           sortAnchor=NULL, resetCells=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", filterType, missing(filterType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", doNotExpand, missing(doNotExpand), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", isEmpty, missing(isEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", isOutline, missing(isOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", styleAsOutline, missing(styleAsOutline), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", captionTemplate, missing(captionTemplate), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", isTotal, missing(isTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", isLevelSubTotal, missing(isLevelSubTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", isLevelTotal, missing(isLevelTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", insertAtIndex, missing(insertAtIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", insertBeforeGroup, missing(insertBeforeGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", insertAfterGroup, missing(insertAfterGroup), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", mergeEmptySpace, missing(mergeEmptySpace), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", sortAnchor, missing(sortAnchor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("fixed", "next", "previous"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowGroup", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_argumentCheckMode==TRUE) private$p_traceEnabled("PivotTable$addRowGroup", "Adding row group...",
                                                                   list(captionTemplate=captionTemplate, caption=caption,
                                                                        doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                                                                        isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                                                        variableName=variableName, values=values,
                                                                        calculationGroupName=calculationGroupName, calculationName=calculationName,
                                                                        baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                                                        mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName,
                                                                        cellStyleDeclarations=cellStyleDeclarations, sortAnchor=sortAnchor,
                                                                        resetCells=resetCells))
      # default to style as outline if not specified
      if(isOutline && missing(styleAsOutline)) styleAsOutline <- TRUE
      # add the group
      grp <- private$p_rowGroup$addChildGroup(variableName=variableName, filterType=filterType, values=values,
                                              doNotExpand=doNotExpand, isEmpty=isEmpty, isOutline=isOutline, styleAsOutline=styleAsOutline,
                                              captionTemplate=captionTemplate, caption=caption,
                                              isTotal=isTotal, isLevelSubTotal=isLevelSubTotal, isLevelTotal=isLevelTotal,
                                              calculationGroupName=calculationGroupName, calculationName=calculationName,
                                              baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                              insertAtIndex=insertAtIndex, insertBeforeGroup=insertBeforeGroup, insertAfterGroup=insertAfterGroup,
                                              mergeEmptySpace=mergeEmptySpace, cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations,
                                              sortAnchor=sortAnchor, resetCells=resetCells)
      if(private$p_argumentCheckMode==TRUE) private$p_traceEnabled("PivotTable$addRowGroup", "Added row group.")
      return(invisible(grp))
    },

    #' @description
    #' Add multiple new data groups to the row group hierarchy
    #' based on the distinct values in a data frame
    #' column or using explicitly specified data values.
    #' See the "Data Groups" vignette for example usage.
    #' @details
    #' There are broadly three different ways to call `addRowDataGroups()`:\cr
    #' (1) dataName=name, fromData=TRUE, onlyCombinationsThatExist=TRUE - which
    #' considers the ancestors of each existing data group to generate only those
    #' combinations of values that exist in the data frame.\cr
    #' (2) dataName=name, fromData=TRUE, onlyCombinationsThatExist=FALSE - which
    #' ignores the ancestors of each existing data group and simply adds every
    #' distinct value of the specified variable under every existing data group,
    #' which can result in combinations of values in the pivot table that don't
    #' exist in the data frame (i.e. blank rows/columns in the pivot table).\cr
    #' (3) fromData=FALSE, explicitListOfValues=list(...) - simply adds every
    #' value from the specified list under every existing data group.
    #' @param variableName The name of the related column in the data frame(s) of
    #'   the pivot table.
    #' @param atLevel The level number that specifies where to add the new
    #' groups.  Level 1 = on the first visible level of the hierarchy.
    #' `NULL` = create a new level at the bottom of the hierarchy for the new
    #' groups.
    #' @param fromData Default `TRUE` to generate the new data groups based on the
    #' data values that exist in the `variableName` column in the named data frame.
    #' If `FALSE`, then `explicitListOfValues` must be specified.
    #' @param dataName The name of the data frame (as specified in
    #' `pt$addData()`) to read the data group values from.
    #' @param dataSortOrder Must be one of "asc", "desc", "custom" or "none".
    #' @param customSortOrder A vector values sorted into the desired order.
    #' @param caption The template of data group captions to generate,
    #' default "{value}".
    #' @param dataFormat A character, list or custom function to format the
    #' data value.
    #' @param dataFmtFuncArgs A list that specifies any additional arguments to
    #' pass to a custom format function.
    #' @param onlyCombinationsThatExist Default `TRUE` to generate only
    #' combinations of data groups that exist in the data frame.
    #' @param explicitListOfValues A list of explicit values to create data
    #' groups from.  A data group is created for each element of the list.
    #' If a list element is vector of values (with length greater than 1),
    #' then a data group is created for multiple values instead of just
    #' a single value.
    #' @param calculationGroupName The calculation group that the new data groups
    #' are related to.
    #' @param expandExistingTotals Default `FALSE`, which means totals are not
    #' broken down in multi-level hierarchies.
    #' @param addTotal Default `TRUE`, which means sub-total and total data groups
    #' are automatically added.
    #' @param visualTotals Default `FALSE`, which means visual totals are disabled.
    #' See the "Data Groups" vignette for more details about visual totals.
    #' @param totalPosition Either "before" or "after" to specify where total groups
    #' are created, default "after".
    #' @param totalCaption The caption to display on total groups, default "Total".
    #' @param onlyAddGroupIf A filter expression that can be used to more finely
    #' control whether data groups are created at different locations in the
    #' hierarchy.  There must be at least one row that matches this filter and the
    #' filters from the ancestor groups in order that the child group is created.
    #' E.g. `MaxDisplayLevel>5`.
    #' @param preGroupData Default `TRUE`, which means that the pivot table
    #' pre-calculates the distinct combinations of variable values to reduce the CPU
    #' time and elapsed time required to generate data groups.
    #' Cannot be used in conjunction with the
    #' @param baseStyleName The name of the style applied to this data group (i.e.
    #'   this row/column heading).  The style must exist in the `PivotStyles` object
    #'   associated with the PivotTable.
    #' @param styleDeclarations CSS style declarations that can override the base
    #' style, expressed as a list, e.g. `list("font-weight"=bold")`.
    #' @param header A character value used as the row-group column caption
    #' when row group headers are rendered.
    #' @param outlineBefore Default `FALSE` to disable the creation of outline header
    #' groups.  Specify either `TRUE` or a list of outline group settings to create
    #' outline header groups.  See the "Regular Layout" vignette for details.
    #' @param outlineAfter Default `FALSE` to disable the creation of outline footer
    #' groups.  Specify either `TRUE` or a list of outline group settings to create
    #' outline footer groups.  See the "Regular Layout" vignette for details.
    #' @param outlineTotal  Default `FALSE` to disable the creation of outline
    #' totals.  Specify either `TRUE` or a list of outline group settings to create
    #' outline totals.  See the "Regular Layout" vignette for details.
    #' @param onlyAddOutlineChildGroupIf A filter expression that can be used to
    #' more finely control whether outline child groups are created at different
    #' locations in the hierarchy.  There must be at least one row that matches
    #' this filter and the filters from the ancestor groups in order that the
    #' outline child group is created.  E.g. `MaxDisplayLevel>5`.
    #' See the "Regular Layout" vignette for an example.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    addRowDataGroups = function(variableName=NULL, atLevel=NULL, fromData=TRUE, # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
                                dataName=NULL, dataSortOrder="asc", customSortOrder=NULL, caption="{value}", dataFormat=NULL, dataFmtFuncArgs=NULL,
                                onlyCombinationsThatExist=TRUE, explicitListOfValues=NULL, calculationGroupName=NULL,
                                expandExistingTotals=FALSE, addTotal=TRUE, visualTotals=FALSE, totalPosition="after", totalCaption="Total",
                                onlyAddGroupIf=NULL, preGroupData=TRUE, baseStyleName=NULL, styleDeclarations=NULL, header=NULL,
                                outlineBefore=NULL, outlineAfter=NULL, outlineTotal=NULL, onlyAddOutlineChildGroupIf=NULL) {
     timeStart <- proc.time()
     if(private$p_argumentCheckMode > 0) {
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", fromData, missing(fromData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataSortOrder, missing(dataSortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc", "desc", "custom", "none"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", customSortOrder, missing(customSortOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", caption, missing(caption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataFormat, missing(dataFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", dataFmtFuncArgs, missing(dataFmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", onlyCombinationsThatExist, missing(onlyCombinationsThatExist), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", explicitListOfValues, missing(explicitListOfValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", expandExistingTotals, missing(expandExistingTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", addTotal, missing(addTotal), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", visualTotals, missing(visualTotals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", totalPosition, missing(totalPosition), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("before", "after"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", totalCaption, missing(totalCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", onlyAddGroupIf, missing(onlyAddGroupIf), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", preGroupData, missing(preGroupData), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", header, missing(header), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", outlineBefore, missing(outlineBefore), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", outlineAfter, missing(outlineAfter), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", outlineTotal, missing(outlineTotal), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
       checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowDataGroups", onlyAddOutlineChildGroupIf, missing(onlyAddOutlineChildGroupIf), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical","character"))
     }
     if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowDataGroups", "Adding row groups...",
                   list(variableName=variableName, atLevel=atLevel, fromData=fromData,
                   dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                   caption=caption, dataFormat=dataFormat, dataFmtFuncArgs=dataFmtFuncArgs,
                   onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                   calculationGroupName=calculationGroupName, expandExistingTotals=expandExistingTotals,
                   addTotal=addTotal, visualTotals=visualTotals, totalPosition=totalPosition, totalCaption=totalCaption,
                   onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData,
                   baseStyleName=baseStyleName, styleDeclarations=styleDeclarations), header=header,
                   outlineBefore=outlineBefore, outlineAfter=outlineAfter, outlineTotal=outlineTotal,
                   onlyAddOutlineChildGroupIf=onlyAddOutlineChildGroupIf)
     if((!is.null(styleDeclarations))&&(length(styleDeclarations)!=length(names(styleDeclarations))))
       stop("PivotTable$addRowDataGroups(): One or more style declarations are missing a name.", call. = FALSE)
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      # helper function:  if outline==TRUE, then use the default outline setting if we have one
      getDefaultOutline <- function(outline, outlineDefault) {
        if((isTRUE(outline))&&(!is.null(outlineDefault))) return(invisible(outlineDefault))
        return(invisible(outline))
      }
      # add the groups
      grps <- private$p_rowGroup$addDataGroups(variableName=variableName, atLevel=levelsBelow, fromData=fromData,
                                               dataName=dataName, dataSortOrder=dataSortOrder, customSortOrder=customSortOrder,
                                               caption=caption, dataFormat=dataFormat, dataFmtFuncArgs=dataFmtFuncArgs,
                                               onlyCombinationsThatExist=onlyCombinationsThatExist, explicitListOfValues=explicitListOfValues,
                                               calculationGroupName=calculationGroupName,
                                               expandExistingTotals=self$getDefault1(expandExistingTotals, missing(expandExistingTotals)),
                                               addTotal=self$getDefault1(addTotal, missing(addTotal)),
                                               visualTotals=self$getDefault1(visualTotals, missing(visualTotals)),
                                               totalPosition=self$getDefault1(totalPosition, missing(totalPosition)),
                                               totalCaption=self$getDefault1(totalCaption, missing(totalCaption)),
                                               onlyAddGroupIf=onlyAddGroupIf, preGroupData=preGroupData, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations,
                                               outlineBefore=getDefaultOutline(outlineBefore, self$getDefault3("outlineBefore")),
                                               outlineAfter=getDefaultOutline(outlineAfter, self$getDefault3("outlineAfter")),
                                               outlineTotal=getDefaultOutline(outlineTotal, self$getDefault3("outlineTotal")),
                                               onlyAddOutlineChildGroupIf=onlyAddOutlineChildGroupIf)
      if(is.null(header)) {
        header <- variableName
      }
      if (!is.null(header)) {
        if (length(grps)>0) {
          levelNumber <- grps[[1]]$levelNumber
          private$p_rowGrpHeaders[[levelNumber]] <- header
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowDataGroups", "Added row groups.")
      private$addTiming(paste0("addRowDataGroups(", variableName, ")"), timeStart)
      return(invisible(grps))
    },

    #' @description
    #' Normalise the row data group hierarchy so that all branches have the
    #' same number of levels - accomplished by adding empty child data groups
    #' where needed.
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    normaliseRowGroups = function(resetCells=TRUE) {
      timeStart <- proc.time()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseRowGroups", "Normalising row groups...")
      self$resetCells()
      groupsAdded <- private$p_rowGroup$normaliseDataGroup(resetCells=resetCells)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$normaliseRowGroups", "Normalised row groups.", list(groupsAdded = groupsAdded))
      private$addTiming("normaliseRowGroups", timeStart)
      return(invisible())
    },

    #' @description
    #' Sort row data groups either by the data group data value, caption,
    #' a custom order or based on calculation result values.
    #' @param levelNumber The level number to sort the data groups, e.g.
    #' level 1 (default) sorts the data groups at level 1 of the hierarchy
    #' (which is the first visible level of data groups).
    #' @param orderBy Must be either "value", "caption", "calculation",
    #' "customByValue" or "customByCaption".\cr
    #' "value" sorts by the raw (i.e. unformatted) group value.\cr
    #' "caption" sorts by the formatted character group caption.\cr
    #' "calculation" sorts using one of the calculations defined in the pivot table.
    #' "customValue" sorts by the raw (i.e. unformatted) group value according to
    #' the specified custom sort order.\cr
    #' "customCaption" sorts by the formatted character group caption according to
    #' the specified custom sort order.
    #' @param customOrder A vector values sorted into the desired order.
    #' @param sortOrder Must be either "asc" or "desc".
    #' @param calculationGroupName If sorting using a calculation, the name of the
    #' calculation group containing the specified calculation.
    #' @param calculationName If sorting using a calculation, the name of the
    #' calculation.
    #' @param fromIndex A boundary to limit the sort operation.
    #' @param toIndex A boundary to limit the sort operation.
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return No return value.
    sortRowDataGroups = function(levelNumber=1, orderBy="calculation", customOrder=NULL, sortOrder="desc",
                                 calculationGroupName="default", calculationName=NULL,
                                 fromIndex=NULL, toIndex=NULL, resetCells=TRUE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", levelNumber, missing(levelNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", orderBy, missing(orderBy), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value","caption","calculation","customByValue","customByCaption"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", customOrder, missing(customOrder), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", sortOrder, missing(sortOrder), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("asc","desc"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", calculationName, missing(calculationName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", fromIndex, missing(fromIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", toIndex, missing(toIndex), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "sortRowDataGroups", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortRowDataGroups", "Sorting row data groups...",
                    list(levelNumber=levelNumber, orderBy=orderBy, customOrder=customOrder, sortOrder=sortOrder,
                         calculationGroupName=calculationGroupName, calculationName=calculationName,
                         fromIndex=fromIndex, toIndex=toIndex, resetCells=resetCells))
      if(levelNumber<1) stop("PivotTable$sortRowDataGroups():  levelNumber must be 1 or above.", call. = FALSE)
      private$p_rowGroup$sortDataGroups(levelNumber=levelNumber-1, orderBy=orderBy, customOrder=customOrder, sortOrder=sortOrder,
                                           calculationGroupName=calculationGroupName, calculationName=calculationName,
                                        fromIndex=fromIndex, toIndex=toIndex, resetCells=resetCells)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$sortRowDataGroups", "Sorted row data groups.")
      private$addTiming("sortRowDataGroups", timeStart)
      return(invisible())
    },

    #' @description
    #' Set the row group header associated with a level of the row data group
    #' hierarchy.
    #' @param levelNumber An integer specifying the level number.
    #' @param header A character value specifying the caption.
    #' @details
    #' By default, the row data groups (i.e. row headings) at the left of the pivot
    #' table have no column headings.  This method can specify the headings, which
    #' can be rendered by specifying the `showRowGroupHeaders=TRUE` in the render
    #' methods.
    #' @return No return value.
    setRowDataGroupHeader = function(levelNumber=NULL, header=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setRowDataGroupHeader", levelNumber, missing(levelNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setRowDataGroupHeader", header, missing(header), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setRowDataGroupHeader", "Setting row group header...")
      private$p_rowGrpHeaders[[levelNumber]] <- header
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setRowDataGroupHeader", "Set row group header.")
    },

    #' @description
    #' Create a new calculation group.  This is rarely needed since the
    #' default group is sufficient for all regular pivot tables.
    #' @param calculationGroupName The name of the new calculation group to create.
    #' @return A `PivotCalculationGroup` object.
    addCalculationGroup = function(calculationGroupName=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addCalculationGroup", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addCalculationGroup", "Adding calculation group...", list(calculationGroupName=calculationGroupName))
      if(private$p_calculationsSet) stop("PivotTable$defineCalculation(): Calculations cannot be moved/added after either addColumnCalculationGroups() or addRowCalculationGroups() has been executed.", call. = FALSE)
      self$resetCells()
      calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addCalculationGroup", "Added calculation group.")
      private$addTiming("addCalculationGroup", timeStart)
      return(invisible(calculationGroup))
    },

    #' @description
    #' Create a new `PivotCalculation` object.
    #' @param calculationGroupName The name of the calculation group this
    #' calculation will belong to.  The default calculation group will be
    #' used if this parameter is not specified (this is sufficient for all
    #' regular pivot tables).
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
    #' @param resetCells Default `TRUE` to reset any cells that currently exist
    #' in the pivot table and trigger a recalculation of the pivot table when
    #' it is next rendered.
    #' @return A new `PivotCalculation` object.
    defineCalculation = function(calculationGroupName="default", calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, fmtFuncArgs=NULL, dataName=NULL, type="summary",
                         valueName=NULL, summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL, calcFuncArgs=NULL,
                         basedOn=NULL, noDataValue=NULL, noDataCaption=NULL,
                         headingBaseStyleName=NULL, headingStyleDeclarations=NULL, cellBaseStyleName=NULL, cellStyleDeclarations=NULL,
                         resetCells=TRUE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotFilters", "PivotFilterOverrides"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("function", "list", "character"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "summary", "calculation", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", valueName, missing(valueName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", calcFuncArgs, missing(calcFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", basedOn, missing(basedOn), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", noDataValue, missing(noDataValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", noDataCaption, missing(noDataCaption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", headingBaseStyleName, missing(headingBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", headingStyleDeclarations, missing(headingStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", cellBaseStyleName, missing(cellBaseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", cellStyleDeclarations, missing(cellStyleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "defineCalculation", resetCells, missing(resetCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_calculationsSet) stop("PivotTable$defineCalculation(): Calculations cannot be moved/added after either addColumnCalculationGroups() or addRowCalculationGroups() has been executed.", call. = FALSE)
      fstr <- NULL
      if(!is.null(filters)) fstr <- filters$asString()
      if((!is.null(headingStyleDeclarations))&&(length(headingStyleDeclarations)!=length(names(headingStyleDeclarations))))
        stop("PivotTable$defineCalculation(): One or more heading style declarations are missing a name.", call. = FALSE)
      if((!is.null(cellStyleDeclarations))&&(length(cellStyleDeclarations)!=length(names(cellStyleDeclarations))))
        stop("PivotTable$defineCalculation(): One or more cell style declarations are missing a name.", call. = FALSE)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$defineCalculation", "Defining calculation...",
                   list(calculationGroupName=calculationGroupName, calculationName=calculationName, caption=caption,
                        visible=visible, displayOrder=displayOrder, filters=fstr, format=format, fmtFuncArgs=fmtFuncArgs,
                        dataName=dataName, type=type, valueName=valueName, summariseExpression=summariseExpression,
                        calculationExpression=calculationExpression, calculationFunction=calculationFunction, calcFuncArgs=calcFuncArgs,
                        basedOn=basedOn, noDataValue=noDataValue, noDataCaption=noDataCaption,
                        headingBaseStyleName=headingBaseStyleName, headingStyleDeclarations=headingStyleDeclarations,
                        cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations, resetCells=resetCells))
      if(resetCells) self$resetCells()
      calculationGroupExists <- private$p_calculationGroups$isExistingCalculationGroup(calculationGroupName)
      if(calculationGroupExists) {
        calculationGroup <- private$p_calculationGroups$getCalculationGroup(calculationGroupName)
      }
      else {
        calculationGroup <- private$p_calculationGroups$addCalculationGroup(calculationGroupName)
      }
      calculation <- calculationGroup$defineCalculation(calculationName=calculationName, caption=caption, visible=visible,
                         displayOrder=displayOrder, filters=filters, format=format, fmtFuncArgs=fmtFuncArgs, dataName=dataName,
                         type=type, valueName=valueName, summariseExpression=summariseExpression,
                         calculationExpression=calculationExpression, calculationFunction=calculationFunction, calcFuncArgs=calcFuncArgs,
                         basedOn=basedOn, noDataValue=noDataValue, noDataCaption=noDataCaption,
                         headingBaseStyleName=headingBaseStyleName, headingStyleDeclarations=headingStyleDeclarations,
                         cellBaseStyleName=cellBaseStyleName, cellStyleDeclarations=cellStyleDeclarations)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$defineCalculation", "Defined calculation.")
      private$addTiming(paste0("defineCalculation(", calculationGroupName, ":", calculationName, ")"), timeStart)
      return(invisible(calculation))
    },

    #' @description
    #' Set calculations on existing data groups or add multiple new groups
    #' to the column data group hierarchy to represent calculations.
    #' @details
    #' If only one calculation is defined in the pivot table, then the calculation
    #' is set onto the existing column data groups (and no new groups are generated).
    #' If multiple calculations are defined, then a new level of data groups is
    #' added, e.g. if two calculations are defined, then two new data groups will
    #' be created under each existing leaf-level column data group.
    #' @param calculationGroupName The name of the calculation group to add
    #' into the data group hierarchy.
    #' @param atLevel The level number that specifies where to add the new
    #' groups.  Level 1 = on the first visible level of the hierarchy.
    #' `NULL` = create a new level at the bottom of the hierarchy for the new
    #' groups.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    addColumnCalculationGroups = function(calculationGroupName="default", atLevel=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addColumnCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnCalculationGroups", "Adding column calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel))
      if(private$p_calculationsSet) stop("PivotTable$addColumnCalculationGroups(): Calculations cannot be moved/added after either addColumnCalculationGroups() or addRowCalculationGroups() has been executed.", call. = FALSE)
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      grps <- private$p_columnGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow)
      private$p_calculationsSet <- TRUE
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addColumnCalculationGroups", "Added column calculation groups.")
      return(invisible(grps))
    },

    #' @description
    #' Set calculations on existing data groups or add multiple new groups
    #' to the row data group hierarchy to represent calculations.
    #' @details
    #' If only one calculation is defined in the pivot table, then the calculation
    #' is set onto the existing row data groups (and no new groups are generated).
    #' If multiple calculations are defined, then a new level of data groups is
    #' added, e.g. if two calculations are defined, then two new data groups will
    #' be created under each existing leaf-level row data group.
    #' @param calculationGroupName The name of the calculation group to add
    #' into the data group hierarchy.
    #' @param atLevel The level number that specifies where to add the new
    #' groups.  Level 1 = on the first visible level of the hierarchy.
    #' `NULL` = create a new level at the bottom of the hierarchy for the new
    #' groups.
    #' @param outlineBefore Default `FALSE` to disable the creation of outline header
    #' groups.  Specify either `TRUE` or a list of outline group settings to create
    #' outline header groups.  See the "Regular Layout" vignette for details.
    #' @param outlineAfter Default `FALSE` to disable the creation of outline footer
    #' groups.  Specify either `TRUE` or a list of outline group settings to create
    #' outline footer groups.  See the "Regular Layout" vignette for details.
    #' @return A list of new `PivotDataGroup` objects that have been added.
    addRowCalculationGroups = function(calculationGroupName="default", atLevel=NULL, outlineBefore=NULL, outlineAfter=NULL) { # atLevel=1 is the top level, (since 1 is the top level as visible to the user)
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", calculationGroupName, missing(calculationGroupName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", atLevel, missing(atLevel), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", outlineBefore, missing(outlineBefore), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "addRowCalculationGroups", outlineAfter, missing(outlineAfter), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "list"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowCalculationGroups", "Adding row calculation groups...",
                   list(calculationGroupName=calculationGroupName, atLevel=atLevel, outlineBefore=outlineBefore, outlineAfter=outlineAfter))
      if(private$p_calculationsSet) stop("PivotTable$addRowCalculationGroups(): Calculations cannot be moved/added after either addColumnCalculationGroups() or addRowCalculationGroups() has been executed.", call. = FALSE)
      self$resetCells()
      levelsBelow <- NULL
      if((!is.null(atLevel))&&(atLevel>0)) levelsBelow <- atLevel - 1
      # helper function:  if outline==TRUE, then use the default outline setting if we have one
      getDefaultOutline <- function(outline, outlineDefault) {
        if((isTRUE(outline))&&(!is.null(outlineDefault))) return(invisible(outlineDefault))
        return(invisible(outline))
      }
      # add groups
      grps <- private$p_rowGroup$addCalculationGroups(calculationGroupName=calculationGroupName, atLevel=levelsBelow,
                                                      outlineBefore=getDefaultOutline(outlineBefore, self$getDefault3("outlineBefore")),
                                                      outlineAfter=getDefaultOutline(outlineAfter, self$getDefault3("outlineAfter")))
      private$p_calculationsSet <- TRUE
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$addRowCalculationGroups", "Added row calculation groups.")
      return(invisible(grps))
    },

    #' @description
    #' Add a new named style to the pivot table.
    #' @param styleName The name of the new style.
    #' @param declarations CSS style declarations in the form of a list, e.g.
    #' `list("font-weight"="bold", "color"="#0000FF")`
    #' @return The newly created `PivotStyle` object.
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

    #' @description
    #' Create an inline style that can be used to override a base style.
    #' For general use cases, the `setStyling()` method provides a simpler
    #' and more direct way of styling specific parts of a pivot table.
    #' @details
    #' Inline styles are typically used to override the style of some specific
    #' cells in a pivot table.  Inline styles have no name.
    #' In HTML, they are rendered as 'style' attributes on specific table cells,
    #' where as named styles are linked to cells using the 'class' attribute.
    #' @param baseStyleName The name of an existing style to base the new style on.
    #' @param declarations CSS style declarations in the form of a list, e.g.
    #' `list("font-weight"="bold", "color"="#0000FF")`
    #' @return The newly created `PivotStyle` object.
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

    #' @description
    #' Apply styling to a set of data groups or cells in a pivot table.
    #' @details
    #' There are five ways to specify the part(s) of a pivot table to apply
    #' styling to:\cr
    #' (1) By specifying a list of data groups using the `groups` argument.\cr
    #' (2) By specifying a list of cells using the `cells` argument.\cr
    #' (3) By specifying a single cell using the `rFrom` and `cFrom` arguments.\cr
    #' (4) By specifying a rectangular cell range using the `rFrom`, `cFrom`,
    #' `rTo` and `cTo` arguments.\cr
    #' (5) By specifying a vector of rowNumbers and/or columnNumbers.  If both
    #' rowNumbers and columnNumbers are specified, then the cells at the
    #' intersection of the specified row numbers and column numbers are styled.\cr
    #' If both rFrom/rTo and rowNumbers are specified, then rFrom/rTo constrain
    #' the row numbers specified in rowNumbers.\cr
    #' If both cFrom/cTo and columnNumbers are specified, then cFrom/cTo constrain
    #' the column numbers specified in columnNumbers.\cr
    #' See the "Styling" and "Finding and Formatting" vignettes for more
    #' information and many examples.
    #' @param rFrom An integer row number that specifies the start row for the
    #' styling changes.
    #' @param cFrom An integer column number that specifies the start column for the
    #' styling changes.
    #' @param rTo An integer row number that specifies the end row for the styling
    #' changes.
    #' @param cTo An integer column number that specifies the end column for the
    #' styling changes.
    #' @param rowNumbers An integer vector that specifies the row numbers for the
    #' styling changes.
    #' @param columnNumbers An integer vector that specifies the column numbers for
    #' the styling changes.
    #' @param groups A list containing `PivotDataGroup` objects.
    #' @param cells A list containing `PivotCell` objects.
    #' @param baseStyleName The name of a style to apply.
    #' @param style A `PivotStyle` object to apply.
    #' @param declarations CSS style declarations to apply in the form of a list,
    #' e.g. `list("font-weight"="bold", "color"="#0000FF")`
    #' @return No return value.
    setStyling = function(rFrom=NULL, cFrom=NULL, rTo=NULL, cTo=NULL, rowNumbers=NULL, columnNumbers=NULL,
                          groups=NULL, cells=NULL, baseStyleName=NULL, style=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", rFrom, missing(rFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", cFrom, missing(cFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", groups, missing(groups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "PivotDataGroup"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "PivotCell"), allowedListElementClasses="PivotCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotStyle")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "setStyling", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setStyling", "Setting styling...")
      if(missing(baseStyleName)&&missing(style)&&missing(declarations)) { stop("PivotTable$setStyling():  Please specify at least one of baseStyleName, style or declarations.", call. = FALSE) }
      # style a group or list of groups
      if(!is.null(groups)) {
        if("PivotDataGroup" %in% class(groups)) {
          groups <- list(groups)
        }
        if(length(groups)>0) {
          for(i in 1:length(groups)) {
            grp <- groups[[i]]
            if(!is.null(grp)) {
              if(!missing(baseStyleName)) { grp$baseStyleName <- baseStyleName }
              if(!missing(style)) { grp$style <- ifelse(is.null(style), NULL, style$getCopy()) }
              if((!missing(declarations))&&(!is.null(declarations))) {
                if (is.null(grp$style)) { grp$style <- PivotStyle$new(parentPivot=self, declarations=declarations) }
                else { grp$style$setPropertyValues(declarations) }
              }
            }
          }
        }
      }
      # style a cell or list of cells
      if(!is.null(cells)) {
        if("PivotCell" %in% class(cells)) {
          cells <- list(cells)
        }
        if(length(cells)>0) {
          for(i in 1:length(cells)) {
            cell <- cells[[i]]
            if(!is.null(cell)) {
              if(!missing(baseStyleName)) { cell$baseStyleName <- baseStyleName }
              if(!missing(style)) { cell$style <- ifelse(is.null(style), NULL, style$getCopy()) }
              if((!missing(declarations))&&(!is.null(declarations))) {
                if (is.null(cell$style)) { cell$style <- PivotStyle$new(parentPivot=self, declarations=declarations) }
                else { cell$style$setPropertyValues(declarations) }
              }
            }
          }
        }
      }
      # styling cells by coordinates...
      styleCells <- FALSE
      rowCount <- self$rowCount
      columnCount <- self$columnCount
      ## switch to use the option legacy coordinates (this ignores the rowNumbers and columnNumbers parameters)
      if(isTRUE(private$p_compatibility$legacySetStylingRowColumnNumbers)) {
        if((!is.null(rFrom))&&(!is.null(cFrom))) {
          if(is.null(rTo)) rTo <- rFrom
          if(is.null(cTo)) cTo <- cFrom
          if(rTo<rFrom) { stop("PivotTable$setStyling():  rTo must be greater than or equal to rFrom.", call. = FALSE) }
          if(cTo<cFrom) { stop("PivotTable$setStyling():  cTo must be greater than or equal to cFrom.", call. = FALSE) }
          rowNumbers <- rFrom:rTo
          columnNumbers <- cFrom:cTo
          styleCells <- TRUE
        }
      }
      else {
        ## check if a single cell has been specified
        if((length(rowNumbers)==0)&&(length(columnNumbers)==0)&&
           (!is.null(rFrom))&&(!is.null(cFrom))&&(is.null(rTo))&&(is.null(cTo))) {
          rowNumbers <- rFrom
          columnNumbers <- cFrom
        }
        else
        {
          # specifying a range of cells
          if((length(rowNumbers)>0)||(!is.null(rFrom))||(!is.null(rTo))) {
            if(length(rowNumbers)==0) rowNumbers <- 1:max(rowCount, 1)
            if(!is.null(rFrom)) rowNumbers <- rowNumbers[rowNumbers >= min(rFrom)]
            if(!is.null(rTo)) rowNumbers <- rowNumbers[rowNumbers <= max(rTo)]
          }
          if((length(columnNumbers)>0)||(!is.null(cFrom))||(!is.null(cTo))) {
            if(length(columnNumbers)==0) columnNumbers <- 1:max(columnCount, 1)
            if(!is.null(cFrom)) columnNumbers <- columnNumbers[columnNumbers >= min(cFrom)]
            if(!is.null(cTo)) columnNumbers <- columnNumbers[columnNumbers <= max(cTo)]
          }
        }
        styleCells <- (length(rowNumbers)>0)||(length(columnNumbers)>0)
      }
      if(styleCells==TRUE) {
        if(!private$p_evaluated) stop("PivotTable$setStyling():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
        if(is.null(private$p_cells)) stop("PivotTable$setStyling():  No cells exist in the pivot table.", call. = FALSE)
        # set defaults for other axes
        if((length(rowNumbers)==0)&&(length(columnNumbers)>0)) rowNumbers <- 1:rowCount
        if((length(rowNumbers)>0)&&(length(columnNumbers)==0)) columnNumbers <- 1:columnCount
        # silently remove invalid row/column numbers
        if(min(rowNumbers)<1) rowNumbers <- rowNumbers[rowNumbers >= 1]
        if(max(rowNumbers)>rowCount) rowNumbers <- rowNumbers[rowNumbers <= rowCount]
        if(min(columnNumbers)<1) columnNumbers <- columnNumbers[columnNumbers >= 1]
        if(max(columnNumbers)>columnCount) columnNumbers <- columnNumbers[columnNumbers <= columnCount]
        # style cells
        for(r in rowNumbers) {
          for(c in columnNumbers) {
            cell <- self$cells$getCell(r, c)
            if(!is.null(cell)) {
              if(!missing(baseStyleName)) { cell$baseStyleName <- baseStyleName }
              if(!missing(style)) { cell$style <- ifelse(is.null(style), NULL, style$getCopy()) }
              if((!missing(declarations))&&(!is.null(declarations))) {
                if (is.null(cell$style)) { cell$style <- PivotStyle$new(parentPivot=self, declarations=declarations) }
                else { cell$style$setPropertyValues(declarations) }
              }
            }
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$setStyling", "Set styling.")
    },

    # mapType=value only allows fixed "from" values (either just a number or v==number): it maps discrete "from" values to discrete "to" values
    # mapType=range allows "from" as a number or as a range specified with "v": it matches values into "from" ranges, the "to" values are discrete
    # mapType=continuous only allows fixed "from" values (just a number or v==number): it rescales "from" numbers into continuous "to" values
    # allowed usage:
    # valueType=text, mapType=value:        0, "normal", 1, "bold", etc.
    # valueType=text, mapType=logic:        v==0, "normal", 1<v<=2, "bold", etc.
    # valueType=text, mapType=range:        0, "normal", 1, "bold", etc                styleLowerValues=FALSE, styleHigherValues=TRUE
    # valueType=number: supports the above, plus:
    # valueType=number, mapType=continuous: 0, 10, 1, 20, etc                          styleLowerValues=FALSE, styleHigherValues=TRUE
    # valueType=color:  supports the above, but the continuous option looks like:
    # valueType=color, mapType=continuous:  0, red, 1, yellow, etc                     styleLowerValues=FALSE, styleHigherValues=TRUE
    # Note in documentation that these methods are primarily for numerical data.  Parts may work for dates (e.g. mapType=value and range) ...
    # ... but other bits won't, e.g. v>=as.Date("2020-05-22"), or variables, e.g. v>=x where x is a local variable outside of the pivot table (but the name could collide with a local variable inside the pivot table function).

    #' @description
    #' Apply styling to pivot table cells based on the value of each cell.
    #' @details
    #' `mapStyling()` is typically used to conditionally apply styling to cells
    #' based on the value of each individual cell, e.g. cells with values less
    #' than a specified number could be coloured red.\cr
    #' mapType="logic" maps values matching specified logical criteria to
    #' specific "to" values.  The logical criteria can be any of the following
    #' forms (the first matching mapping is used):\cr
    #' (1) a specific value, e.g. 12.\cr
    #' (2) a specific value equality condition, e.g. "v==12", where v
    #' represents the cell value.\cr
    #' (3) a value range expression using the following abbreviated form:
    #' "value1<=v<value2", e.g. "10<=v<15".  Only "<" or "<=" can be used
    #' in these value range expressions.\cr
    #' (4) a standard R logical expression, e.g.
    #' "10<=v && v<15".\cr
    #' Basic R functions that test the value can also be
    #' used, e.g. is.na(v).\cr
    #' See the "Styling" and Finding and Formatting" vignettes for more
    #' information and many examples.
    #' @param styleProperty The name of the style property to set on the specified
    #' cells, e.g. background-color.
    #' @param cells A list containing `PivotCell` objects.
    #' @param valueType The type of style value to be set.  Must be one of:
    #' "text", "character", "number", "numeric", "color" or "colour".\cr
    #' "text" and "character" are equivalent.  "number" and "numeric" are equivalent.
    #' "color" and "colour" are equivalent.
    #' @param mapType The type of mapping to be performed.  The following mapping
    #' types are supported:\cr
    #' (1) "value" = a 1:1 mapping which maps each specified "from" value to the
    #' corresponding "to" value, e.g. 100 -> "green".\cr
    #' (2) "logic" = each from value is logical criteria.  See details.\cr
    #' (3) "range" = values between each pair of "from" values are mapped to the
    #' corresponding "to" value, e.g. values in the range 80-100 -> "green" (more
    #' specifically values greater than or equal to 80 and less than 100).\cr
    #' (4) "continuous" = rescales values between each pair of "from" values into
    #' the range of the corresponding pair of "to" values, e.g. if the "from" range
    #' is 80-100 and the corresponding "to" range is 0.8-1, then 90 -> 0.9.\cr
    #' "continuous" cannot be used with valueType="text"/"character".
    #' @param mappings The mappings to be applied, specified in one of the following
    #' three forms:\cr
    #' (1) a list containing pairs of values, e.g.
    #' `list(0, "red", 0.4, "yellow", 0.8, "green")`.\cr
    #' (2) a list containing "from" and "to" vectors/lists, e.g.
    #' `list(from=c(0, 0.4, 0.8), to=c("red", "yellow", "green"))`.\cr
    #' (3) a custom mapping function that will be invoked once per cell, e.g.
    #' `function(v, cell) { if(isTRUE(v>0.8)) return("green") }`.\cr
    #' Mappings must be specified in ascending order when valueType="range" or
    #' valueType="continuous".\cr
    #' If a custom mapping function is specified, then the valueType and mapType
    #' parameters are ignored.
    #' @param styleLowerValues A logical value, default `FALSE`, that specifies
    #' whether values less than the lowest specified "from" value should be styled
    #' using the style specified for the lowest "from" value.  Only applies when
    #' valueType="range" or valueType="continuous".
    #' @param styleHigherValues A logical value, default `TRUE`, that specifies
    #' whether values greater than the highest specified "from" value should be styled
    #' using the style specified for the highest "from" value.  Only applies when
    #' valueType="range" or valueType="continuous".
    #' @return No return value.
    mapStyling = function(styleProperty=NULL, cells=NULL, valueType="text", mapType="range", mappings=NULL, styleLowerValues=FALSE, styleHigherValues=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", styleProperty, missing(styleProperty), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", cells, missing(cells), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotCell", "list"), allowedListElementClasses="PivotCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", valueType, missing(valueType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("character", "text", "numeric", "number", "color", "colour"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", mapType, missing(mapType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "range", "logic", "continuous"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", mappings, missing(mappings), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("character", "list", "function"), allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", styleLowerValues, missing(styleLowerValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mapStyling", styleHigherValues, missing(styleHigherValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$mapStyling", "Mapping styling...")
      # prep parameters
      if("PivotCell" %in% class(cells)) cells <- list(cells)
      if(mapType=="numeric") mapType <- "number"
      if(mapType=="character") mapType <- "text"
      if("character" %in% class(mappings)) mappings <- as.list(mappings)
      if(valueType=="colour") valueType <- "color"
      # special case of a mapping function - this ignores the valueType and mapType arguments
      if("function" %in% class(mappings)) {
        for(cell in cells) {
          value <- cell$rawValue
          mappedValue <- mappings(value, cell)
          if((length(mappedValue)>0)&&(!is.na(mappedValue))) {
            declarations <- list()
            declarations[[styleProperty]] <- mappedValue
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
        if(private$p_traceEnabled==TRUE) self$trace("PivotTable$mapStyling", "Mapped styling.")
        return(invisible())
      }
      # general or special case?
      if((length(mappings)==2)&&(length(intersect(names(mappings), c("from", "to")))==2)) {
        # special case of a two element list (from, to)
        if(length(intersect(class(mappings$from), c("character", "integer", "numeric", "list")))==0) {
          stop("PivotTable$mapStyling():  The 'from' values must be either character, integer, numeric or a list.", call. = FALSE)
        }
        # build the maps list
        maps <- list()
        mapCount <- min(length(mappings$from), length(mappings$to))
        m <- 1
        mMax <- mapCount
        for(m in 1:mMax) {
          if("list" %in% class(mappings$from)) vre <- mappings$from[[m]]
          else vre <- mappings$from[m]
          if("list" %in% class(mappings$to)) value <- mappings$to[[m]]
          else value <- mappings$to[m]
          nextVre <- NULL
          nextValue <- NULL
          if((m+1)<=mMax){
            if("list" %in% class(mappings$from)) nextVre <- mappings$from[[m+1]]
            else nextVre <- mappings$from[m+1]
            if("list" %in% class(mappings$to)) nextValue <- mappings$to[[m+1]]
            else nextValue <- mappings$to[m+1]
          }
          maps[[length(maps)+1]] <- list(vre=vre, value=value, nextVre=nextVre, nextValue=nextValue)
        }
      }
      else {
        # general case of a longer list of mappings
        # check some mappings, return if none
        if(length(mappings)<2) {
          if(private$p_traceEnabled==TRUE) self$trace("PivotTable$mapStyling", "Mapped styling.")
          return(invisible())
        }
        # process the mappings into value pairs
        # every map has two values: vre (value range expression) = the "from" value, value = the "to" value
        # "from" here means the raw value (or range of raw values) that we are mapping from, i.e. the input of the mapping
        # "to" here means the result value, i.e. the output of the mapping
        # mapType=range and mapType=continuous also have: rangeStart, rangeEnd, where vre->rangeStart and nextVre->rangeEnd
        # valueType=color with mapType=continuous also has:
        maps <- list()
        mapCount <- length(mappings)
        m <- 1
        mMax <- mapCount
        while(m<mMax) {
          vre <- mappings[[m]]
          value <- NULL
          if((m+1)<=length(mappings)) value <- mappings[[m+1]]
          nextVre <- NULL
          if((m+2)<=length(mappings)) nextVre <- mappings[[m+2]]
          nextValue <- NULL
          if((m+3)<=length(mappings)) nextValue <- mappings[[m+3]]
          maps[[length(maps)+1]] <- list(vre=vre, value=value, nextVre=nextVre, nextValue=nextValue)
          # message("vre=", vre, " value=", value, " nextVre=", nextVre, " nextValue=", nextValue)
          m <- m+2
        }
      }
      # check/prepare maps
      if(mapType=="value") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          if((is.null(map$vre))||(length(map$vre)==0)) stop("PivotTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(vreIsSingleValue(map$vre)) {
            # write the single value back into the mapping to minimise repeated parsing
            maps[[m]]$vre <- vreGetSingleValue(map$vre)
          }
          else {
            stop(paste0("PivotTable$mapStyling():  The 'from' value for a mapping must be a single numerical value - invalid value ", map$vre), call. = FALSE)
          }
          if((is.null(map$value))||(length(map$value)==0)) stop("PivotTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
        }
      }
      else if(mapType=="logic") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          if((is.null(map$vre))||(length(map$vre)==0)) stop("PivotTable$mapStyling():  The 'from' criteria for a mapping cannot be null or blank.", call. = FALSE)
          if((is.null(map$value))||(length(map$value)==0)) stop("PivotTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
          testResult <- vreIsMatch(map$vre, 0, testOnly=TRUE)
        }
      }
      else if(mapType=="range") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          # basic map checks
          if((is.null(map$vre))||(length(map$vre)==0)) stop("PivotTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(!vreIsSingleValue(map$vre)) stop("PivotTable$mapStyling():  Only single-value 'from' values can be be used with a range mapping.", call. = FALSE)
          if(((is.null(map$value))||(length(map$value)==0))&&(m<length(maps))) {
            # the last value of a range mapping can be null, e.g. mappings=list(0, "red", 1000, "orange", 15000), note there is no colour for 15000
            stop("PivotTable$mapStyling():  The 'to' value for a mapping cannot be null or blank (except for the last specified 'from' value).", call. = FALSE)
          }
          # store the numerical range as part of the mapping so they don't need to be parsed repeatedly
          map$rangeStart <- vreGetSingleValue(map$vre)
          if((!is.null(map$nextVre))&&(vreIsSingleValue(map$nextVre))) map$rangeEnd <- vreGetSingleValue(map$nextVre)
          # check the "to" values
          if(valueType=="number") {
            if(!is.numeric(map$value)) stop(paste0("PivotTable$mapStyling():  The 'to' value '", map$value, "' must be numeric since valueType=number/numeric has been specified."), call. = FALSE)
          }
          else if(valueType=="color") {
            testColor <- parseColor(map$value)
            if(is.null(testColor)) stop(paste0("PivotTable$mapStyling():  The 'to' value '", map$value, "' must be a valid color/colour since valueType=color has been specified."), call. = FALSE)
          }
          # message(paste0("vre=", map$vre, " nextVre=", map$nextVre, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", map$value, " nextValue=", map$nextValue))
          maps[[m]] <- map
        }
        # check the order of the mappings
        fx <- function(x) { x$rangeStart }
        if(!isTRUE(all.equal(1:length(maps),order(sapply(maps, fx))))) {
          stop("PivotTable$mapStyling(): The 'from' values for range mappings must be specified in ascending order (lowest to highest).", call. = FALSE)
        }
      }
      else if(mapType=="continuous") {
        if(valueType=="text") stop("PivotTable$mapStyling():  Continuous mappings are not supported with text values.", call. = FALSE)
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          # basic map checks
          if((is.null(map$vre))||(length(map$vre)==0)) stop("PivotTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(!vreIsSingleValue(map$vre)) stop("PivotTable$mapStyling():  Only single-value 'from' values can be be used with a continuous mapping.", call. = FALSE)
          if((is.null(map$value))||(length(map$value)==0)) stop("PivotTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
          # store the numerical range as part of the mapping so they don't need to be parsed repeatedly
          map$rangeStart <- vreGetSingleValue(map$vre)
          if((!is.null(map$nextVre))&&(vreIsSingleValue(map$nextVre))) map$rangeEnd <- vreGetSingleValue(map$nextVre)
          # check the "to" values
          if(valueType=="number") {
            if(!is.numeric(map$value)) stop(paste0("PivotTable$mapStyling():  The 'to' value '", map$value, "' must be numeric since valueType=number/numeric has been specified."), call. = FALSE)
          }
          else if(valueType=="color") {
            map$startColorHex <- parseColor(map$value)
            if(is.null(map$startColorHex)) stop(paste0("PivotTable$mapStyling():  The 'to' value '", map$value, "' must be a valid color/colour since valueType=color/colour has been specified."), call. = FALSE)
            map$startColorList <- vreHexToClr(map$startColorHex)
            if(length(map$nextValue)>0) {
              map$endColorHex <- parseColor(map$nextValue)
              if(is.null(map$endColorHex)) stop(paste0("PivotTable$mapStyling():  The 'to' value '", map$nextValue, "' must be a valid color/colour since valueType=color/colour has been specified."), call. = FALSE)
              map$endColorList <- vreHexToClr(map$endColorHex)
            }
          }
          # message(paste0("vre=", map$vre, " nextVre=", map$nextVre, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", map$value, " nextValue=", map$nextValue))
          maps[[m]] <- map
        }
        # check the order of the mappings
        fx <- function(x) { x$rangeStart }
        if(!isTRUE(all.equal(1:length(maps),order(sapply(maps, fx))))) {
          stop("PivotTable$mapStyling(): The 'from' values for continuous mappings must be specified in ascending order (lowest to highest).", call. = FALSE)
        }
      }
      # map values
      mMax <- length(maps)
      if(mapType=="value") {
        for(cell in cells) {
          value <- cell$rawValue
          for(map in maps) {
            if(vreIsEqual(map$vre, value)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              break # jump to next cell
            }
          }
        }
      }
      else if(mapType=="logic") {
        for(cell in cells) {
          value <- cell$rawValue
          for(map in maps) {
            if(vreIsMatch(map$vre, value)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              break # jump to next cell
            }
          }
        }
      }
      else if(mapType=="range") {
        for(cell in cells) {
          cellStyleSet <- FALSE
          value <- cell$rawValue
          for(map in maps) {
            if(length(map$rangeStart)==0) next
            if(length(map$rangeEnd)==0) next
            if(length(map$value)==0) next
            if(isTRUE(map$rangeStart <= value && value < map$rangeEnd)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              cellStyleSet <- TRUE
              break # jump to next cell
            }
          }
          # if this cell has been processed, jump to the next cell
          if(cellStyleSet==TRUE) next
          # none of the mappings have matched...
          # ...style the same as the first mapping?
          map <- maps[[1]]
          if(styleLowerValues && (length(map$rangeStart)>0) && isTRUE(value < map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$value
            self$setStyling(cells=cell, declarations=declarations)
          }
          # ...style the same as the last mapping?
          map <- maps[[mMax]]
          if(styleHigherValues && (length(map$rangeStart)>0) && isTRUE(value > map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$value
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
      }
      else if(mapType=="continuous") {
        for(cell in cells) {
          cellStyleSet <- FALSE
          value <- cell$rawValue
          for(map in maps) {
            if(length(map$rangeStart)==0) next
            if(length(map$rangeEnd)==0) next
            if(length(map$value)==0) next
            if(length(map$nextValue)==0) next
            if(isTRUE(map$rangeStart <= value && value < map$rangeEnd)) {
              if(valueType=="number") {
                value <- vreScaleNumber(map$value, map$nextValue, map$rangeStart, map$rangeEnd, value)
                cellStyleSet <- TRUE
              }
              else if(valueType=="color") {
                # message(paste0("startColorHex=", map$startColorHex, " endColorHex=", map$endColorHex, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", value))
                value <- vreScale2Colours(map$startColorList, map$endColorList, map$rangeStart, map$rangeEnd, value)
                cellStyleSet <- TRUE
              }
              if((length(value)>0)&&(!is.na(value))) {
                declarations <- list()
                declarations[[styleProperty]] <- value
                self$setStyling(cells=cell, declarations=declarations)
                break # jump to next cell
              }
            }
          }
          # if this cell has been processed, jump to the next cell
          if(cellStyleSet==TRUE) next
          # none of the mappings have matched...
          # ...style the same as the first mapping?
          map <- maps[[1]]
          if(styleLowerValues && (length(map$rangeStart)>0) && isTRUE(value < map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$startColorHex
            self$setStyling(cells=cell, declarations=declarations)
          }
          # ...style the same as the last mapping?
          map <- maps[[mMax]]
          if(styleHigherValues && (length(map$rangeStart)>0) && isTRUE(value > map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$startColorHex
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$mapStyling", "Mapped styling.")
      return(invisible())
    },

    #' @description
    #' Generate the cells that will form the body of the pivot table.
    #' @details
    #' This method rarely needs to be called explicitly, since other methods will
    #' invoke it if needed.
    #' @return No return value.
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
      if((!private$p_calculationsSet)&&(is.null(private$p_calculationsPosition))&&(!is.null(private$p_calculationGroups))&&
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
      rowIsEmptyFlags <- list()
      for(i in 1:rowCount) {
        # set the rowColumnNumber on the leaf cell
        rowGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- rowGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the isEmpty flag using OR
        rowIsEmptyFlag <- FALSE
        # construct the parent filter settings using "intersect" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          if(!is.null(acs$isEmpty)) {
            rowIsEmptyFlag <- rowIsEmptyFlag||acs$isEmpty
          }
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="intersect")
          }
        }
        rowIsEmptyFlags[[i]] <- rowIsEmptyFlag
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
      columnIsEmptyFlags <- list()
      for(i in 1:columnCount) {
        # set the rowColumnNumber on the leaf cell
        columnGrps[[i]]$rowColumnNumber <- as.integer(i)
        # get the ancestor groups for this group, starting with the current object
        ancestors <- columnGrps[[i]]$getAncestorGroups(includeCurrentGroup=TRUE)
        # construct the isEmpty flag using OR
        columnIsEmptyFlag <- FALSE
        # construct the parent filter settings using "intersect" filter logic
        rowColFilters <- PivotFilters$new(self)
        for(j in length(ancestors):1) {
          acs <- ancestors[[j]]
          if(!is.null(acs$isEmpty)) {
            columnIsEmptyFlag <- columnIsEmptyFlag||acs$isEmpty
          }
          filters <- acs$filters
          if(is.null(filters)) next
          if(filters$count==0) next
          for(k in 1:length(filters$filters)) {
            filter <- filters$filters[[k]]
            rowColFilters$setFilter(filter, action="intersect")
          }
        }
        columnIsEmptyFlags[[i]] <- columnIsEmptyFlag
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
      # build a calculation lookup
      calcGrpsLookup <- list()
      if(private$p_calculationGroups$count>0) {
        for(cg in 1:private$p_calculationGroups$count) {
          calcGrp <- private$p_calculationGroups$item(cg)
          cgCalcs <- list()
          if(calcGrp$count>0) {
            for(cn in 1:calcGrp$count) {
              calcn <- calcGrp$item(cn)
              cgCalcs[[calcn$calculationName]]<-calcn
            }
          }
          calcGrpsLookup[[calcGrp$calculationGroupName]] <- cgCalcs
        }
      }
      # create and size the new PivotCells
      private$p_cells$setGroups(rowGroups=rowGrps, columnGroups=columnGrps)
      if(rowCount>0) {
        for(r in 1:rowCount) {
          if(columnCount>0) {
            for(c in 1:columnCount) {
              # determine if this cell is empty
              isEmpty <- rowIsEmptyFlags[[r]]||columnIsEmptyFlags[[c]]
              # calculate the net filters
              if(is.null(rowFilters[[r]])) {
                if(!is.null(columnFilters[[c]])) { rowColFilters <- columnFilters[[c]]$getCopy() }
              }
              else {
                rowColFilters <- rowFilters[[r]]$getCopy()
                if (!is.null(columnFilters[[c]])) { rowColFilters$setFilters(columnFilters[[c]], action="intersect") }
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
              cell <- PivotCell$new(self, rowNumber=as.integer(r), columnNumber=as.integer(c), isEmpty=isEmpty,
                                    calculationName=calcNme, calculationGroupName=calcGrpNme,
                                    rowColFilters=rowColFilters, rowFilters=rowFilters[[r]], columnFilters=columnFilters[[c]],
                                    rowLeafGroup=rowGrps[[r]], columnLeafGroup=columnGrps[[c]])
              # set the style for the cell
              # note:  the logic for inherited styles from the data groups is taken care of in the renderers
              #        i.e. the cell only stores the additional/overriding styles for the cell (not all of the inherited styles)
              if((!is.null(calcGrpNme))&&(!is.null(calcNme))) {
                calcn <- calcGrpsLookup[[calcGrpNme]][[calcNme]]
                if(!is.null(calcn)) {
                  if(!is.null(calcn$cellBaseStyleName)) cell$baseStyleName <- calcn$cellBaseStyleName
                  if(!is.null(calcn$cellStyleDeclarations))
                    cell$style <- self$createInlineStyle(baseStyleName=calcn$cellBaseStyleName, declarations=calcn$cellStyleDeclarations)
                }
              }
              # add the cell to the pivot table
              private$p_cells$setCell(r=r, c=c, cell=cell)
            }
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$generateCellStructure", "Generated cell structure.")
      private$addTiming("generateCellStructure", timeStart)
      return(invisible(private$cells))
    },

    #' @description
    #' Clear the cells of the pivot table.
    #' @details
    #' The cells are reset automatically when structural changes are made to the
    #' pivot table, so this method rarely needs to be called explicitly.
    #' @return No return value.
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

    #' @description
    #' Calculate the cell values in the body of the pivot table.
    #' @details
    #' This method rarely needs to be called explicitly, since other methods will
    #' invoke it if needed.
    #' @return No return value.
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
          # ignore empty cells
          if(cell$isEmpty) next
          # calculate cell
          calculator$evaluateCell(cell)
        }
      }
      private$addTiming("evaluateCells:evaluateCell", timeStart1)
      private$p_evaluated <- TRUE
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluateCells", "Evaluated cell values.")
      private$addTiming("evaluateCells:total", timeStartT)
      return(invisible())
    },

    #' @description
    #' Calculate the cell values in the body of the pivot table.
    #' @details
    #' This generally only needs to be called explicitly if specific pivot cells
    #' need to be further processed (e.g. formatted) before the pivot table is
    #' rendered.\cr
    #' This method is a wrapper for calling `normaliseColumnGroups()`,
    #' `normaliseRowGroups()`, `generateCellStructure()` and `evaluateCells()`
    #' in sequence.
    #' @return No return value.
    evaluatePivot = function() {
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluatePivot", "Evaluating pivot table...")
      self$normaliseColumnGroups()
      self$normaliseRowGroups()
      self$generateCellStructure()
      self$evaluateCells()
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$evaluatePivot", "Evaluated pivot table.")
      return(invisible())
    },

    #' @description
    #' Find row data groups that match specified criteria.
    #' @param matchMode Either "simple" (default) or "combinations".\cr
    #' "simple" is used when matching only one variable-value, multiple
    #' variable-value combinations are effectively logical "OR".\cr
    #' "combinations" is used when matching for combinations of variable
    #' values, multiple variable-value combinations are effectively
    #' logical "AND".  A child group is viewed as having the variable-value
    #' filters of itself and it's parent/ancestors, e.g.\cr
    #' `list("TrainCategory"="Express Passenger", "PowerType"="DMU")`,
    #' would return the "DMU" data group underneath "Express Passenger".\cr
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @param variableNames A character vector specifying the name/names of the
    #' variables to find.  This is useful generally only in pivot tables with
    #' irregular layouts, since in regular pivot tables every cell is related
    #' to every variable.
    #' @param variableValues A list specifying the variable names and values to find,
    #' e.g. `variableValues=list("PowerType"=c("DMU", "HST"))`.\cr
    #' Specify "**" as the variable value to match totals for the specified variable.\cr
    #' Specify "!*" as the variable value to match non-totals for the specified variable.\cr
    #' NB: The totals/non-totals criteria above won’t work when visual totals are used.\cr
    #' @param totals A word that specifies how totals are matched (overrides the finer
    #' settings above) - must be one of "include" (default), "exclude" or "only".
    #' @param calculationNames A character vector specifying the name/names of the
    #' calculations to find.
    #' @param atLevels An integer vector constraining the levels in the hierarchy to
    #' search.
    #' @param minChildCount Match only data groups with this minimum number of children.
    #' @param maxChildCount Match only data groups with this maximum number of children.
    #' @param emptyGroups A word that specifies how empty groups are matched -
    #' must be one of "include", "exclude" (default) or "only".
    #' @param outlineGroups A word that specifies how outline cells are matched -
    #' must be one of "include", "exclude" (default) or "only".
    #' @param outlineLinkedGroupExists `TRUE` to match only groups where the related
    #' outline child group still exists.  `FALSE` to match only groups where the related
    #' outline child group no longer exists.
    #' @param includeDescendantGroups Default `FALSE`.  Specify true to also return
    #' all descendants of data groups that match the specified criteria.
    #' @param rowNumbers An integer vector specifying row numbers that constrains
    #' the data groups to be found.
    #' @param cells A `PivotCell` object or a list of `PivotCell` objects to specify
    #' one or more cells that must intersect the data groups.
    #' @return A list of data groups matching the specified criteria.
    findRowDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                                 totals="include", calculationNames=NULL,
                                 atLevels=NULL, minChildCount=NULL, maxChildCount=NULL,
                                 emptyGroups="exclude",
                                 outlineGroups="exclude", outlineLinkedGroupExists=NULL,
                                 includeDescendantGroups=FALSE,
                                 rowNumbers=NULL, cells=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", atLevels, missing(atLevels), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", minChildCount, missing(minChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", maxChildCount, missing(maxChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", outlineGroups, missing(outlineGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", outlineLinkedGroupExists, missing(outlineLinkedGroupExists), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findRowDataGroups", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotCell", "list"), allowedListElementClasses="PivotCell")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findRowDataGroups", "Finding row data groups...")
      # find groups
      grps1 <- private$p_rowGroup$findDataGroups(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues, totals=totals,
                                                calculationNames=calculationNames,
                                                atLevels=atLevels, minChildCount=minChildCount, maxChildCount=maxChildCount,
                                                emptyGroups=emptyGroups,
                                                outlineGroups=outlineGroups, outlineLinkedGroupExists=outlineLinkedGroupExists,
                                                includeDescendantGroups=includeDescendantGroups, includeCurrentGroup=FALSE)
      # additional constraints: this logic is not embedded in PivotDataGroup since it needs the row numbers
      if("PivotCell" %in% class(cells)) cells <- list(cells)
      grps2 <- list()
      if((length(rowNumbers)>0)||(length(cells)>0)) {
        if(!private$p_evaluated) stop("PivotTable$findRowDataGroups():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
        if(is.null(private$p_cells)) stop("PivotTable$findRowDataGroups():  No cells exist to examine.", call. = FALSE)
        if(length(cells)>0) {
          fx <- function(x) { return(x$rowNumber) }
          cellRowNumbers <- unique(sapply(cells, fx))
          rowNumbers <- union(rowNumbers, cellRowNumbers)
        }
        if(length(rowNumbers)>0) {
          for(grp in grps1) {
            grpRowNumbers <- self$findGroupRowNumbers(group=grp)
            if(length(intersect(rowNumbers, grpRowNumbers))>0) {
              grps2[[length(grps2)+1]] <- grp
            }
          }
        }
      }
      else {
        grps2 <- grps1
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findRowDataGroups", "Found row data groups.")
      return(invisible(grps2))
    },

    #' @description
    #' Find column data groups that match specified criteria.
    #' @param matchMode Either "simple" (default) or "combinations".\cr
    #' "simple" is used when matching only one variable-value - multiple
    #' variable-value combinations are effectively logical "OR".\cr
    #' "combinations" is used when matching for combinations of variable
    #' values - multiple variable-value combinations are effectively
    #' logical "AND".  A child group is viewed as having the variable-value
    #' filters of itself and it's parent/ancestors, e.g.\cr
    #' `list("TrainCategory"="Express Passenger", "PowerType"="DMU")`,
    #' would return the "DMU" data group underneath "Express Passenger".\cr
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @param variableNames A character vector specifying the name/names of the
    #' variables to find.  This is useful generally only in pivot tables with
    #' irregular layouts, since in regular pivot tables every cell is related
    #' to every variable.
    #' @param variableValues A list specifying the variable names and values to find,
    #' e.g. `variableValues=list("PowerType"=c("DMU", "HST"))`.\cr
    #' Specify "**" as the variable value to match totals for the specified variable.\cr
    #' Specify "!*" as the variable value to match non-totals for the specified variable.\cr
    #' NB: The totals/non-totals criteria above won’t work when visual totals are used.
    #' @param totals A word that specifies how totals are matched (overrides the finer
    #' settings above) - must be one of "include" (default), "exclude" or "only".
    #' @param calculationNames A character vector specifying the name/names of the
    #' calculations to find.
    #' @param atLevels An integer vector constraining the levels in the hierarchy to
    #' search.
    #' @param minChildCount Match only data groups with this minimum number of children.
    #' @param maxChildCount Match only data groups with this maximum number of children.
    #' @param emptyGroups A word that specifies how empty groups are matched -
    #' must be one of "include", "exclude" (default) or "only".
    #' @param includeDescendantGroups Default `FALSE`.  Specify true to also return
    #' all descendants of data groups that match the specified criteria.
    #' @param columnNumbers An integer vector specifying column numbers that constrains
    #' the data groups to be found.
    #' @param cells A `PivotCell` object or a list of `PivotCell` objects to specify
    #' one or more cells that must intersect the data groups.
    #' @return A list of data groups matching the specified criteria.
    findColumnDataGroups = function(matchMode="simple", variableNames=NULL, variableValues=NULL,
                                    totals="include", calculationNames=NULL,
                                    atLevels=NULL, minChildCount=NULL, maxChildCount=NULL,
                                    emptyGroups="exclude", includeDescendantGroups=FALSE,
                                    columnNumbers=NULL, cells=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", atLevels, missing(atLevels), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", minChildCount, missing(minChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", maxChildCount, missing(maxChildCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", emptyGroups, missing(emptyGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", includeDescendantGroups, missing(includeDescendantGroups), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findColumnDataGroups", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotCell", "list"), allowedListElementClasses="PivotCell")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findColumnDataGroups", "Finding column data groups...")
      # find groups
      grps1 <- private$p_columnGroup$findDataGroups(matchMode=matchMode, variableNames=variableNames, variableValues=variableValues, totals=totals,
                                                   calculationNames=calculationNames,
                                                   atLevels=atLevels, minChildCount=minChildCount, maxChildCount=maxChildCount,
                                                   emptyGroups=emptyGroups, outlineGroups="exclude",
                                                   includeDescendantGroups=includeDescendantGroups, includeCurrentGroup=FALSE)
      # additional constraints: this logic is not embedded in PivotDataGroup since it needs the column numbers
      if("PivotCell" %in% class(cells)) cells <- list(cells)
      grps2 <- list()
      if((length(columnNumbers)>0)||(length(cells)>0)) {
        if(!private$p_evaluated) stop("PivotTable$findColumnDataGroups():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
        if(is.null(private$p_cells)) stop("PivotTable$findColumnDataGroups():  No cells exist to examine.", call. = FALSE)
        if(length(cells)>0) {
          fx <- function(x) { return(x$columnNumber) }
          cellColNumbers <- unique(sapply(cells, fx))
          columnNumbers <- union(columnNumbers, cellColNumbers)
        }
        if(length(columnNumbers)>0) {
          for(grp in grps1) {
            grpColNumbers <- self$findGroupColumnNumbers(group=grp)
            if(length(intersect(columnNumbers, grpColNumbers))>0) {
              grps2[[length(grps2)+1]] <- grp
            }
          }
        }
      }
      else {
        grps2 <- grps1
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findColumnDataGroups", "Found column data groups.")
      return(invisible(grps2))
    },

    #' @description
    #' Retrieve row numbers for rows where all cells are empty.
    #' @details
    #' `NULL` cell values are always regarded as empty.
    #' @param NAasEmpty `TRUE` (default) specifies that `NA` is treated as empty.
    #' @param zeroAsEmpty `TRUE` specifies that zero is treated as empty,
    #' default `FALSE`.
    #' @param zeroTolerance The tolerance for zero comparisons, default 0.000001.
    #' @param includeOutlineRows `TRUE` to also examine outline rows, default `FALSE`.
    #' @return An integer vector of row numbers.
    getEmptyRows = function(NAasEmpty=TRUE, zeroAsEmpty=FALSE, zeroTolerance=0.000001, includeOutlineRows=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyRows", NAasEmpty, missing(NAasEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyRows", zeroAsEmpty, missing(zeroAsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyRows", zeroTolerance, missing(zeroTolerance), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer","numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyRows", includeOutlineRows, missing(includeOutlineRows), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getEmptyRows", "Getting empty rows...")
      if(!private$p_evaluated) stop("PivotTable$getEmptyRows():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getEmptyRows():  No cells exist to examine.", call. = FALSE)
      emptyRowNumbers <- vector("integer", 0)
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      for(r in 1:rowCount) {
        if(includeOutlineRows==FALSE) {
          if(private$p_cells$rowGroups[[r]]$isOutline) next
        }
        allCellsNull <- TRUE
        for (c in 1:columnCount) {
          cell <- private$p_cells$getCell(r, c)
          isNULL <- FALSE
          isNULL <- isNULL || is.null(cell$rawValue)
          isNULL <- isNULL || ((NAasEmpty==TRUE)&&(is.na(cell$rawValue)))
          isNULL <- isNULL || ((zeroAsEmpty==TRUE)&&(abs(cell$rawValue)<zeroTolerance))
          if(isNULL==FALSE) {
            allCellsNull <- FALSE
            break
          }
        }
        if(allCellsNull==TRUE) {
          emptyRowNumbers[length(emptyRowNumbers)+1] <- r
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getEmptyRows", "Got empty rows.")
      return(invisible(emptyRowNumbers))
    },

    #' @description
    #' Retrieve column numbers for columns where all cells are empty.
    #' @details
    #' `NULL` cell values are always regarded as empty.
    #' @param NAasEmpty `TRUE` (default) specifies that `NA` is treated as empty.
    #' @param zeroAsEmpty `TRUE` specifies that zero is treated as empty,
    #' default `FALSE`.
    #' @param zeroTolerance The tolerance for zero comparisons, default 0.000001.
    #' @return An integer vector of column numbers.
    getEmptyColumns = function(NAasEmpty=TRUE, zeroAsEmpty=FALSE, zeroTolerance=0.000001) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyColumns", NAasEmpty, missing(NAasEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyColumns", zeroAsEmpty, missing(zeroAsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getEmptyRows", zeroTolerance, missing(zeroTolerance), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer","numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getEmptyColumns", "Getting empty columns...")
      if(!private$p_evaluated) stop("PivotTable$getEmptyColumns():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getEmptyColumns():  No cells exist to examine.", call. = FALSE)
      emptyColumnNumbers <- vector("integer", 0)
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      for(c in 1:columnCount) {
        allCellsNull <- TRUE
        for (r in 1:rowCount) {
          cell <- private$p_cells$getCell(r, c)
          isNULL <- FALSE
          isNULL <- isNULL || is.null(cell$rawValue)
          isNULL <- isNULL || ((NAasEmpty==TRUE)&&(is.na(cell$rawValue)))
          isNULL <- isNULL || ((zeroAsEmpty==TRUE)&&(abs(cell$rawValue)<zeroTolerance))
          if(isNULL==FALSE) {
            allCellsNull <- FALSE
            break
          }
        }
        if(allCellsNull==TRUE) {
          emptyColumnNumbers[length(emptyColumnNumbers)+1] <- c
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getEmptyColumns", "Got empty columns.")
      return(invisible(emptyColumnNumbers))
    },

    #' @description
    #' Get the cell at the specified row and column coordinates in the pivot table.
    #' @details
    #' The row and column numbers refer only to the cells in the body of the pivot
    #' table, i.e. row and column headings are excluded, e.g. row 1 is the first
    #' row of cells underneath the column headings.
    #' @param r Row number of the cell to retrieve.
    #' @param c Column number of the cell to retrieve.
    #' @return A `PivotCell` object representing the cell.
    getCell = function(r=NULL, c=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCell", "Getting cell...")
      if(is.null(private$p_cells)) stop("PivotTable$getCell():  No cells exist to retrieve.", call. = FALSE)
      if(length(r)>1) r <- r[1]
      if(length(c)>1) c <- c[1]
      cell <- private$p_cells$getCell(r=r, c=c)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCell", "Got cell.")
      return(invisible(cell))
    },

    #' @description
    #' Retrieve cells by a combination of row and/or column numbers.
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @details
    #' When `specifyCellsAsList=TRUE` (the default):\cr
    #' Get one or more rows by specifying the row numbers as a vector as
    #' the rowNumbers argument and leaving the columnNumbers argument set
    #' to the default value of `NULL`, or\cr
    #' Get one or more columns by specifying the column numbers as a vector
    #' as the columnNumbers argument and leaving the rowNumbers argument
    #' set to the default value of `NULL`, or\cr
    #' Get one or more individual cells by specifying the cellCoordinates
    #' argument as a list of vectors of length 2, where each element in the
    #' list is the row and column number of one cell,\cr
    #' e.g. `list(c(1, 2), c(3, 4))` specifies two cells, the first located
    #' at row 1, column 2 and the second located at row 3, column 4.\cr
    #' When `specifyCellsAsList=FALSE`:\cr
    #' Get one or more rows by specifying the row numbers as a vector as the
    #' rowNumbers argument and leaving the columnNumbers argument set to the
    #' default value of `NULL`, or\cr
    #' Get one or more columns by specifying the column numbers as a vector
    #' as the columnNumbers argument and leaving the rowNumbers argument set
    #' to the default value of `NULL`, or\cr
    #' Get one or more cells by specifying the row and column numbers as vectors
    #' for the rowNumbers and columnNumbers arguments, or\cr
    #' a mixture of the above, where for entire rows/columns the element in the
    #' other vector is set to `NA`, e.g. to retrieve whole rows, specify the row
    #' numbers as the rowNumbers but set the corresponding elements in the
    #' columnNumbers vector to `NA`.
    #' @param specifyCellsAsList Specify how cells are retrieved.
    #' Default `TRUE`. More information is provided in the details section.
    #' @param rowNumbers A vector of row numbers that specify the rows or
    #' cells to retrieve.
    #' @param columnNumbers A vector of column numbers that specify the columns
    #' or cells to retrieve.
    #' @param cellCoordinates A list of two-element vectors that specify the
    #' coordinates of cells to retrieve.  Ignored when `specifyCellsAsList=FALSE`.
    #' @param excludeEmptyCells Default `FALSE`.  Specify `TRUE` to exclude empty
    #' cells.
    #' @param groups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on either the rows or columns axes.  The cells to be retrieved
    #' must be related to at least one of these groups.
    #' @param rowGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on the rows axis.  The cells to be retrieved must be related to
    #' at least one of these row groups.  If both `rowGroups` and `columnGroups`
    #' are specified, then the cells to be retrieved must be related to at least
    #' one of the specified row groups and one of the specified column groups.
    #' @param columnGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on the columns axis.  The cells to be retrieved must be related to
    #' at least one of these column groups.  If both `rowGroups` and `columnGroups`
    #' are specified, then the cells to be retrieved must be related to at least
    #' one of the specified row groups and one of the specified column groups.
    #' @param matchMode Either "simple" (default) or "combinations":\cr
    #' "simple" specifies that row and column arguments are considered separately
    #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
    #' row 1 and all cells in column 2.\cr
    #' "combinations" specifies that row and column arguments are considered together
    #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
    #' cell single at location (1, 2).\cr
    #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
    #' affected by the match mode.  All other arguments are not.
    #' @return A list of `PivotCell` objects.
    getCells = function(specifyCellsAsList=TRUE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL, excludeEmptyCells=FALSE,
                        groups=NULL, rowGroups=NULL, columnGroups=NULL, matchMode="simple") {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", excludeEmptyCells, missing(excludeEmptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", groups, missing(groups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", rowGroups, missing(rowGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", columnGroups, missing(columnGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getCells", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCells", "Getting cells...")
      if(!private$p_evaluated) stop("PivotTable$getCells():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$getCells(specifyCellsAsList=specifyCellsAsList, rowNumbers=rowNumbers, columnNumber=columnNumbers,
                                        cellCoordinates=cellCoordinates, excludeEmptyCells=excludeEmptyCells,
                                        groups=groups, rowGroups=rowGroups, columnGroups=columnGroups,
                                        matchMode=matchMode)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getCells", "Got cells.")
      return(invisible(cells))
    },

    #' @description
    #' Find cells matching specified criteria.
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @details
    #' The valueRanges parameter can be any of the following
    #' forms:\cr
    #' (1) a specific value, e.g. 12.\cr
    #' (2) a specific value equality condition, e.g. "v==12", where v
    #' represents the cell value.\cr
    #' (3) a value range expression using the following abbreviated form:
    #' "value1<=v<value2", e.g. "10<=v<15".  Only "<" or "<=" can be used
    #' in these value range expressions.\cr
    #' (4) a standard R logical expression, e.g.
    #' "10<=v && v<15".\cr
    #' Basic R functions that test the value can also be
    #' used, e.g. is.na(v).\cr
    #' @param variableNames A character vector specifying the name/names of the
    #' variables to find.  This is useful generally only in pivot tables with
    #' irregular layouts, since in regular pivot tables every cell is related
    #' to every variable.
    #' @param variableValues A list specifying the variable names and values to find,
    #' e.g. `variableValues=list("PowerType"=c("DMU", "HST"))`.\cr
    #' Specify "**" as the variable value to match totals for the specified variable.\cr
    #' Specify "!*" as the variable value to match non-totals for the specified variable.\cr
    #' NB: The totals/non-totals criteria above won’t work when visual totals are used.
    #' @param totals A word that specifies how totals are matched (overrides the finer
    #' settings above) - must be one of "include" (default), "exclude" or "only".
    #' @param calculationNames A character vector specifying the name/names of the
    #' calculations to find.
    #' @param minValue A numerical value specifying a minimum value threshold.
    #' @param maxValue A numerical value specifying a maximum value threshold.
    #' @param exactValues A vector or list specifying a set of allowed values.
    #' @param valueRanges A vector specifying one or more value range expressions which
    #' the cell values must match.  If multiple value range expressions are specified,
    #' then the cell value must match any of one the specified expressions.  See details.
    #' @param includeNull specify TRUE to include `NULL` in the matched cells,
    #' FALSE to exclude `NULL` values.
    #' @param includeNA specify TRUE to include `NA` in the matched cells,
    #' FALSE to exclude `NA` values.
    #' @param emptyCells A word that specifies how empty cells are matched -
    #' must be one of "include" (default), "exclude" or "only".
    #' @param outlineCells A word that specifies how outline cells are matched -
    #' must be one of "include", "exclude" (default) or "only".
    #' @param rowNumbers A vector of row numbers that specify the rows or
    #' cells to constrain the search.
    #' @param columnNumbers A vector of column numbers that specify the columns
    #' or cells to constrain the search.
    #' @param cellCoordinates A list of two-element vectors that specify the
    #' coordinates of cells to constrain the search.
    #' @param groups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on either the rows or columns axes.  The cells to be searched
    #' must be related to at least one of these groups.
    #' @param rowGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on the rows axis.  The cells to be searched must be related to
    #' at least one of these row groups.  If both `rowGroups` and `columnGroups`
    #' are specified, then the cells to be searched must be related to at least
    #' one of the specified row groups and one of the specified column groups.
    #' @param columnGroups A `PivotDataGroup` object or a list of `PivotDataGroup`
    #' objects on the columns axis.  The cells to be searched must be related to
    #' at least one of these column groups.  If both `rowGroups` and `columnGroups`
    #' are specified, then the cells to be searched must be related to at least
    #' one of the specified row groups and one of the specified column groups.
    #' @param cells A `PivotCell` object or a list of `PivotCell`
    #' objects to constrain the scope of the search.
    #' @param rowColumnMatchMode Either "simple" (default) or "combinations":\cr
    #' "simple" specifies that row and column arguments are considered separately
    #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
    #' row 1 and all cells in column 2.\cr
    #' "combinations" specifies that row and column arguments are considered together
    #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
    #' cell single at location (1, 2).\cr
    #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
    #' affected by the match mode.  All other arguments are not.
    #' @return A list of `PivotCell` objects.
    findCells = function(variableNames=NULL, variableValues=NULL, totals="include", calculationNames=NULL,
                         minValue=NULL, maxValue=NULL, exactValues=NULL, valueRanges=NULL, includeNull=TRUE, includeNA=TRUE,
                         emptyCells="include", outlineCells="exclude",
                         # additional arguments to constrain cells matched
                         rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL,
                         groups=NULL, rowGroups=NULL, columnGroups=NULL,
                         rowColumnMatchMode="simple", cells=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", totals, missing(totals), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", calculationNames, missing(calculationNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", valueRanges, missing(valueRanges), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct", "list"), listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", emptyCells, missing(emptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", outlineCells, missing(outlineCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        # additional arguments to constrain cells matched
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", groups, missing(groups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", rowGroups, missing(rowGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", columnGroups, missing(columnGroups), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("PivotCell", "list"), allowedListElementClasses="PivotCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findCells", rowColumnMatchMode, missing(rowColumnMatchMode), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("simple", "combinations"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findCells", "Finding cells...")
      if(!private$p_evaluated) stop("PivotTable$findCells():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$findCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$findCells(variableNames=variableNames, variableValues=variableValues, totals=totals, calculationNames=calculationNames,
                                         minValue=minValue, maxValue=maxValue, exactValues=exactValues, valueRanges=valueRanges,
                                         includeNull=includeNull, includeNA=includeNA,
                                         emptyCells=emptyCells, outlineCells=outlineCells,
                                         # additional arguments to constrain cells matched
                                         rowNumbers=rowNumbers, columnNumbers=columnNumbers, cellCoordinates=cellCoordinates,
                                         groups=groups, rowGroups=rowGroups, columnGroups=columnGroups,
                                         rowColumnMatchMode=rowColumnMatchMode, cells=cells)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$findCells", "Found cells.")
      return(invisible(cells))
    },

    #' @description
    #' Find the column numbers associated with a specific data group or
    #' groups.
    #' @param group A `PivotDataGroup` in the column data groups (i.e. a
    #' column heading) or a list of column data groups.
    #' @param collapse A logical value specifying whether the return value should be
    #' simplified.  See details.
    #' @details
    #' If `group` is a list:  If `collapse` is `FALSE`, then a list of vectors is
    #' returned, if `collapse` is `TRUE`, then a single combined vector is returned.
    #' @return Either a vector of column numbers related to the single specified group
    #' or a list of vectors containing column numbers related to the specified groups.
    findGroupColumnNumbers = function(group=NULL, collapse=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findGroupColumnNumbers", group, missing(group), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findGroupColumnNumbers", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple groups
      if("list" %in% class(group)) {
        fx <- function(x) { return(self$findGroupColumnNumbers(group=x)) }
        if(isTRUE(collapse)) return(invisible(unique(unlist(lapply(group, fx)))))
        else return(invisible(lapply(group, fx)))
      }
      # single group
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$findGroupColumnNumbers", "Finding group column numbers...")
      if(!private$p_evaluated) stop("PivotTable$findGroupColumnNumbers():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$findGroupColumnNumbers():  No cells exist to retrieve.", call. = FALSE)
      matchingColumnNumbers <- private$p_cells$findGroupColumnNumbers(group=group)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$findGroupColumnNumbers", "Found group column numbers.")
      return(invisible(matchingColumnNumbers))
    },

    #' @description
    #' Find the row numbers associated with a specific data group or groups.
    #' @param group A `PivotDataGroup` in the row data groups (i.e. a
    #' row heading) or a list of row data groups.
    #' @param collapse A logical value specifying whether the return value should be
    #' simplified.  See details.
    #' @details
    #' If `group` is a list:  If `collapse` is `FALSE`, then a list of vectors is
    #' returned, if `collapse` is `TRUE`, then a single combined vector is returned.
    #' @return Either a vector of row numbers related to the single specified group
    #' or a list of vectors containing row numbers related to the specified groups.
    findGroupRowNumbers = function(group=NULL, collapse=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findGroupRowNumbers", group, missing(group), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("PivotDataGroup", "list"), allowedListElementClasses="PivotDataGroup")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "findGroupRowNumbers", collapse, missing(collapse), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      # multiple groups
      if("list" %in% class(group)) {
        fx <- function(x) { return(self$findGroupRowNumbers(group=x)) }
        if(isTRUE(collapse)) return(invisible(unique(unlist(lapply(group, fx)))))
        else return(invisible(lapply(group, fx)))
      }
      # single group
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$findGroupRowNumbers", "Finding group row numbers...")
      if(!private$p_evaluated) stop("PivotTable$findGroupRowNumbers():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$findGroupRowNumbers():  No cells exist to retrieve.", call. = FALSE)
      matchingColumnNumbers <- private$p_cells$findGroupRowNumbers(group=group)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$findGroupRowNumbers", "Found group row numbers.")
      return(invisible(matchingColumnNumbers))
    },

    #' @description
    #' Remove a column from the pivot table.
    #' @details
    #' This method removes both the related column group and cells.
    #' @param c The column number.  The first column is column 1, excluding the
    #' column(s) associated with row-headings.
    #' @return No return value.
    removeColumn = function(c=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeColumn", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeColumn", "Removing column...")
      if(!private$p_evaluated) stop("PivotTable$removeColumn():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeColumn():  No cells exist to retrieve.", call. = FALSE)
      private$p_cells$removeColumn(c)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeColumn", "Removed column.")
    },

    #' @description
    #' Remove multiple column from the pivot table.
    #' @details
    #' This method removes both the related column groups and cells.
    #' @param columnNumbers The column numbers.  The first column is column 1, excluding the
    #' column(s) associated with row-headings.
    #' @return No return value.
    removeColumns = function(columnNumbers=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeColumns", columnNumbers, missing(columnNumbers), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeColumns", "Removing columns...")
      if(!private$p_evaluated) stop("PivotTable$removeColumns():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeColumns():  No cells exist to retrieve.", call. = FALSE)
      private$p_cells$removeColumns(columnNumbers)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeColumns", "Removed columns.")
    },

    #' @description
    #' Remove columns where all cells are empty.
    #' @details
    #' `NULL` cell values are always regarded as empty.
    #' @param NAasEmpty `TRUE` (default) specifies that `NA` is treated as empty.
    #' @param zeroAsEmpty `TRUE` specifies that zero is treated as empty,
    #' default `FALSE`.
    #' @param zeroTolerance The tolerance for zero comparisons, default 0.000001.
    #' @return No return value.
    removeEmptyColumns = function(NAasEmpty=TRUE, zeroAsEmpty=FALSE, zeroTolerance=0.000001) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyColumns", NAasEmpty, missing(NAasEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyColumns", zeroAsEmpty, missing(zeroAsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyColumns", zeroTolerance, missing(zeroTolerance), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeEmptyColumns", "Removing empty columns...")
      if(!private$p_evaluated) stop("PivotTable$removeEmptyColumns():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeEmptyColumns():  No cells exist to retrieve.", call. = FALSE)
      columnNumbers <- self$getEmptyColumns(NAasEmpty=NAasEmpty, zeroAsEmpty=zeroAsEmpty, zeroTolerance=zeroTolerance)
      if((!is.null(columnNumbers))&&(length(columnNumbers)>0)) private$p_cells$removeColumns(columnNumbers)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeEmptyColumns", "Removed empty columns.")
    },

    #' @description
    #' Remove a row from the pivot table.
    #' @details
    #' This method removes both the related row group and cells.
    #' @param r The row number.  The first row is row 1, excluding the
    #' row(s) associated with column-headings.
    #' @return No return value.
    removeRow = function(r=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeRow", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeRow", "Removing row...")
      if(!private$p_evaluated) stop("PivotTable$removeRow():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeRow():  No cells exist to retrieve.", call. = FALSE)
      private$p_cells$removeRow(r)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeRow", "Removed row.")
    },

    #' @description
    #' Remove multiple rows from the pivot table.
    #' @details
    #' This method removes both the related row groups and cells.
    #' @param rowNumbers The row numbers.  The first row is row 1, excluding the
    #' rows(s) associated with column-headings.
    #' @return No return value.
    removeRows = function(rowNumbers=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeRows", rowNumbers, missing(rowNumbers), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeRows", "Removing rows...")
      if(!private$p_evaluated) stop("PivotTable$removeRows():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeRows():  No cells exist to retrieve.", call. = FALSE)
      private$p_cells$removeRows(rowNumbers)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeRows", "Removed rows.")
    },

    #' @description
    #' Remove rows where all cells are empty.
    #' @details
    #' `NULL` cell values are always regarded as empty.
    #' @param NAasEmpty `TRUE` (default) specifies that `NA` is treated as empty.
    #' @param zeroAsEmpty `TRUE` specifies that zero is treated as empty,
    #' default `FALSE`.
    #' @param zeroTolerance The tolerance for zero comparisons, default 0.000001.
    #' @param includeOutlineRows `TRUE` to also remove empty outline rows,
    #' default `FALSE`.
    #' @return No return value.
    removeEmptyRows = function(NAasEmpty=TRUE, zeroAsEmpty=FALSE, zeroTolerance=0.000001, includeOutlineRows=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyRows", NAasEmpty, missing(NAasEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyRows", zeroAsEmpty, missing(zeroAsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyRows", zeroTolerance, missing(zeroTolerance), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "removeEmptyRows", includeOutlineRows, missing(includeOutlineRows), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeEmptyRows", "Removing empty rows...")
      if(!private$p_evaluated) stop("PivotTable$removeEmptyRows():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$removeEmptyRows():  No cells exist to retrieve.", call. = FALSE)
      rowNumbers <- self$getEmptyRows(NAasEmpty=NAasEmpty, zeroAsEmpty=zeroAsEmpty, zeroTolerance=zeroTolerance, includeOutlineRows=includeOutlineRows)
      if((!is.null(rowNumbers))&&(length(rowNumbers)>0)) private$p_cells$removeRows(rowNumbers)
      if(private$p_traceEnabled==TRUE) private$p_parentPivot$trace("PivotTable$removeEmptyRows", "Removed empty rows.")
    },

    #' @description
    #' Outputs a plain text representation of the pivot table to the console or
    #' returns a character representation of the pivot table.
    #' @param asCharacter `FALSE`(default) outputs to the console, specify `TRUE`
    #' to instead return a character value (does not output to console).
    #' @param showRowGroupHeaders `TRUE` to include the row group headers in the
    #' output, default `FALSE`.
    #' @return Plain text representation of the pivot table.
    print = function(asCharacter=FALSE, showRowGroupHeaders=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "print", asCharacter, missing(asCharacter), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "print", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$print", "Printing matrix...")
      # if not yet evaluated, need to normalise the row/column groups, otherwise errors occur for pivots of more than one level on row/columns
      if(!private$p_evaluated) {
        self$normaliseColumnGroups()
        self$normaliseRowGroups()
      }
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
          maxWidth <- 0
          # row group header
          if(showRowGroupHeaders==TRUE) {
            if(!is.null(private$p_rowGrpHeaders)) {
              if(l<=length(private$p_rowGrpHeaders)) {
                if(!is.null(private$p_rowGrpHeaders[[l]])) {
                  maxWidth <- max(maxWidth, nchar(private$p_rowGrpHeaders[[l]]))
                }
              }
            }
          }
          # row groups
          levelGroups <- private$p_rowGroup$getLevelGroups(l)
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
          # empty cells at top left or row group headings
          if(rowLevelCount==0) currentLine <- paste0(currentLine, "  ")
          else {
            for(rl in 1:rowLevelCount) {
              if((showRowGroupHeaders==TRUE)&&(!is.null(private$p_rowGrpHeaders))&&
                 (rl<=length(private$p_rowGrpHeaders))&&(!is.null(private$p_rowGrpHeaders[[rl]]))) {
                currentLine <- paste0(currentLine, private$p_rowGrpHeaders[[rl]], repStr(" ", rowLevelWidths[rl]-nchar(private$p_rowGrpHeaders[[rl]])))
              }
              else {
                currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
              }
            }
          }
          # column headings at this level
          levelGroups <- private$p_columnGroup$getLevelGroups(cl)
          for(cg in 1:length(levelGroups)) {
            cgrp <- levelGroups[[cg]]
            if(is.null(cgrp$caption)) currentLine <- paste0(currentLine, repStr(" ", cgrp$fixedWidthSize))
            else if(length(cgrp$caption)==0) currentLine <- paste0(currentLine, repStr(" ", cgrp$fixedWidthSize))
            else if(is.na(cgrp$caption)) currentLine <- paste0(currentLine, repStr(" ", cgrp$fixedWidthSize))
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
              else if(length(rg$caption)==0) currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
              else if(is.na(rg$caption)) currentLine <- paste0(currentLine, repStr(" ", rowLevelWidths[rl]))
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
              else if(length(cell$formattedValue)==0) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
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

    #' @description
    #' Convert the pivot table to a matrix, where the data group headings
    #' are included in the body of the matrix.  This method tends to produce
    #' a character matrix.
    #' @details
    #' The newer `asDataMatrix()` tends to produce more a useful matrix.
    #' See the "Outputs" vignette for a comparison of outputs.
    #' @param includeHeaders `TRUE` (default) to include the headings in the
    #' body of the matrix.  Specifying `FALSE` omits the headings.
    #' @param repeatHeaders `FALSE` (default) only outputs the first occurrence of each
    #' header. Specify `TRUE` to repeat the headings.
    #' @param rawValue `FALSE` (default) outputs the formatted (character) values.
    #' Specify `TRUE` to output the raw cell values.
    #' @param showRowGroupHeaders `TRUE` to include the row group headers in the
    #' matrix, default `FALSE`.
    #' @return A matrix.
    asMatrix = function(includeHeaders=TRUE, repeatHeaders=FALSE, rawValue=FALSE, showRowGroupHeaders=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asMatrix", includeHeaders, missing(includeHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asMatrix", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
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
          if((r==1)&&(showRowGroupHeaders==TRUE)&&(!is.null(private$p_rowGrpHeaders))&&
             (c<=length(private$p_rowGrpHeaders))&&(!is.null(private$p_rowGrpHeaders[[c]]))) {
            m[r, c] <- private$p_rowGrpHeaders[[c]]
          }
          else {
            m[r, c] <- ""
          }
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
        if((!is.null(grps))&&(length(grps)>1)) {
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
          else if(length(v)==0) m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- ""
          else if(is.na(v)) m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- ""
          else m[columnHeaderLevelCount + r, rowHeaderLevelCount + c] <- v
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asMatrix", "Got pivot table as a matrix.")
      return(m)
    },

    #' @description
    #' Convert the pivot table to a matrix, where the data group headings
    #' are included as row/column headings in the matrix.
    #' This method tends to produce a numeric matrix.
    #' @details
    #' Where there are multiple levels in a data group hierarchy, the captions are
    #' concatenated to form the row/column headings in the matrix.
    #' See the "Outputs" vignette for a comparison of outputs.
    #' @param includeHeaders `TRUE` (default) to include the headings in the matrix.
    #' Specifying `FALSE` omits the headings.
    #' @param rawValue `TRUE` (default) outputs the raw cell values.
    #' Specify `FALSE` to output the formatted (character) values.
    #' @param separator Specifies the character value used to concatenate data
    #' group captions where multiple levels exist in the data group hierarchy.
    #' @return A matrix.
    asDataMatrix = function(includeHeaders=TRUE, rawValue=TRUE, separator=" ") {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataMatrix", includeHeaders, missing(includeHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataMatrix", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataMatrix", "Getting pivot table as a data matrix...",
                                                  list(rawValue=rawValue, separator=separator))
      if(!private$p_evaluated) stop("PivotTable$asDataMatrix():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asDataMatrix():  No cells exist to retrieve.", call. = FALSE)
      if(includeHeaders==FALSE) {
        return(private$p_cells$asMatrix(rawValue=rawValue))
      }
      # get the headings dimensions
      rowHeaderLevelCount <- private$p_rowGroup$getLevelCount()
      columnHeaderLevelCount <- private$p_columnGroup$getLevelCount()
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      # column names
      colHeaderLeafGroups <- private$p_columnGroup$getLeafGroups()
      colNames <-  vector("list", length = columnCount)
      for(c in 1:columnCount) {
        colName <- ""
        grps <- colHeaderLeafGroups[[c]]$getAncestorGroups(includeCurrentGroup=TRUE)
        for(cl in 1:columnHeaderLevelCount) {
          if(cl==1) {
            colName <- grps[[columnHeaderLevelCount-cl+1]]$caption
          }
          else {
            colName <- paste0(colName, separator, grps[[columnHeaderLevelCount-cl+1]]$caption)
          }
        }
        colNames[[c]] <- colName
      }
      # row names
      rowHeaderLeafGroups <- private$p_rowGroup$getLeafGroups()
      rowNames <-  vector("list", length = rowCount)
      for(r in 1:rowCount) {
        rowName <- ""
        grps <- rowHeaderLeafGroups[[r]]$getAncestorGroups(includeCurrentGroup=TRUE)
        if((!is.null(grps))&&(length(grps)>1)) {
          for(rl in 1:rowHeaderLevelCount) {
            if(rl==1) {
              rowName <- grps[[rowHeaderLevelCount-rl+1]]$caption
            }
            else {
              rowName <- paste0(rowName, separator, grps[[rowHeaderLevelCount-rl+1]]$caption)
            }
          }
        }
        rowNames[[r]] <- rowName
      }
      # data
      m <- private$p_cells$asMatrix(rawValue=rawValue)
      # apply names
      colnames(m) <- make.unique(unlist(colNames))
      rownames(m) <- make.unique(unlist(rowNames))
      # done
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataMatrix", "Got pivot table as a data matrix.")
      return(m)
    },

    #' @description
    #' Convert the pivot table to a data frame, combining multiple levels of
    #' headings with the specified separator and/or exporting the row groups
    #' as columns in the data frame.
    #' @details
    #' See the "Outputs" vignette for more details and examples
    #' @param separator Specifies the character value used to concatenate data
    #' group captions where multiple levels exist in the data group hierarchy.
    #' @param stringsAsFactors Specify `TRUE` to convert strings to factors,
    #' default is `default.stringsAsFactors()` for R < 4.1.0 and `FALSE`
    #' for R >= 4.1.0.
    #' @param forceNumeric Specify `TRUE` to force the conversion of cell values
    #' to a numeric value, default `FALSE`.
    #' @param rowGroupsAsColumns Specify `TRUE` to include the row groups as
    #' additional columns in the data frame.  Default `FALSE`.
    #' @return A data frame.
    asDataFrame = function(separator=" ", stringsAsFactors=NULL, forceNumeric=FALSE, rowGroupsAsColumns=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataFrame", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataFrame", stringsAsFactors, missing(stringsAsFactors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataFrame", forceNumeric, missing(forceNumeric), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asDataFrame", rowGroupsAsColumns, missing(rowGroupsAsColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataFrame", "Getting pivot table as a data frame...", list(separator=separator, stringsAsFactors=stringsAsFactors))
      if(!private$p_evaluated) stop("PivotTable$asDataFrame():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asDataFrame():  No cells exist to retrieve.", call. = FALSE)
      # stringsAsFactors default depends on the version of R...
      if(is.null(stringsAsFactors)) {
        if(getRversion() < "4.0.0") {
          # old version, retain existing behaviour with no warning
          stringsAsFactors <- default.stringsAsFactors()
        }
        else if (getRversion() < "4.1.0") {
          stringsAsFactors <- default.stringsAsFactors()
          # generate a warning if the default is TRUE as this will change to FALSE in future
          if(stringsAsFactors) {
            warning("PivotTable$asDataFrame(): In a future version of R, default.stringsAsFactors() will be deprecated and removed, at which time the 'stringsAsFactors' argument will default to FALSE.  Explictly set the 'stringsAsFactors' argument to remove this warning.")
          }
        }
        else {
          # default to FALSE for R 4.1.0 onwards
          stringsAsFactors <- FALSE
        }
      }
      # sizing
      rowHeaderLevelCount <- private$p_rowGroup$getLevelCount()
      columnHeaderLevelCount <- private$p_columnGroup$getLevelCount()
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      rowHeaders <- list()
      columnHeaders <- list()
      # set the column headers
      if(rowGroupsAsColumns) {
        if(rowHeaderLevelCount > 0) {
          for(c in 1:rowHeaderLevelCount) {
            headerValue <- ""
            if((!is.null(private$p_rowGrpHeaders))&&(length(private$p_rowGrpHeaders)>=c)) {
              headerValue <- private$p_rowGrpHeaders[[c]]
            }
            else {
              headerValue <- paste0("RowGroup", c)
            }
            columnHeaders[[length(columnHeaders) + 1]] <- headerValue
          }
        }
      }
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
        if((!is.null(grps))&&(length(grps)>1)) {
          for(c in (length(grps)-1):1) {
            grp <- grps[[c]]
            if(nchar(headerValue) == 0) headerValue <- grp$caption
            else headerValue <- paste0(headerValue, separator, grp$caption)
          }
        }
        rowHeaders[[length(rowHeaders) + 1]] <- headerValue
      }
      # get the value vectors to form the data frame
      dfColumns <- list()
      if(rowGroupsAsColumns) {
        if(rowHeaderLevelCount > 0) {
          for(c in 1:rowHeaderLevelCount) {
            rowNames <-  NA
            for(r in 1:rowCount) {
              grps <- rowHeaderLeafGroups[[r]]$getAncestorGroups(includeCurrentGroup=TRUE)
              if((!is.null(grps))&&(length(grps)>1)) {
                rowNames[r] <- grps[[rowHeaderLevelCount-c+1]]$caption
              }
            }
            dfColumns[[length(dfColumns) + 1]] <- rowNames
          }
        }
      }
      for(c in 1:columnCount) {
        columnValues <- NA
        for(r in 1:rowCount) {
          cell <- private$p_cells$getCell(r, c)
          v <- cell$rawValue
          if(forceNumeric) {
            if(!(("integer" %in% class(v))||("numeric" %in% class(v)))) v <- NA
          }
          if(is.null(v)) v <- NA
          else if(length(v) == 0) v <- NA
          columnValues[r] <- v
        }
        dfColumns[[length(dfColumns) + 1]] <- columnValues
      }
      df <- as.data.frame(dfColumns, stringsAsFactors=stringsAsFactors)
      colnames(df) <- make.unique(unlist(columnHeaders))
      rownames(df) <- make.unique(unlist(rowHeaders))
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asDataFrame", "Got pivot table as a data frame.")
      return(df)
    },

    #' @description
    #' Convert the pivot table to tidy data frame, where each cell in the body of
    #' the pivot table becomes one row in the data frame.
    #' @details
    #' See the "Outputs" vignette for more details and examples
    #' @param includeGroupCaptions `TRUE` (default) to include the data group
    #' captions as columns in the data frame.
    #' @param includeGroupValues `TRUE` (default) to include the data group
    #' values as columns in the data frame.
    #' @param separator Specifies the character value used to concatenate
    #' filter values where multiple values exist in a filter.
    #' @param stringsAsFactors Specify `TRUE` to convert strings to factors,
    #' default is `default.stringsAsFactors()` for R < 4.1.0 and `FALSE`
    #' for R >= 4.1.0.
    #' @param excludeEmptyCells Specify `FALSE` to also include rows for
    #' empty cells in the data frame, default `TRUE`.
    #' @return A data frame.
    asTidyDataFrame = function(includeGroupCaptions=TRUE, includeGroupValues=TRUE, separator=" ", stringsAsFactors=NULL, excludeEmptyCells=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", includeGroupCaptions, missing(includeGroupCaptions), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", includeGroupValues, missing(includeGroupValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", separator, missing(separator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", stringsAsFactors, missing(stringsAsFactors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asTidyDataFrame", excludeEmptyCells, missing(excludeEmptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asTidyDataFrame", "Getting pivot table as a tidy data frame...",
                   list(includeGroupCaptions=includeGroupCaptions, includeGroupValues=includeGroupValues, separator=separator, stringsAsFactors=stringsAsFactors))
      if(!private$p_evaluated) stop("PivotTable$asTidyDataFrame():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$asTidyDataFrame():  No cells exist to retrieve.", call. = FALSE)
      # stringsAsFactors default depends on the version of R...
      if(is.null(stringsAsFactors)) {
        if(getRversion() < "4.0.0") {
          # old version, retain existing behaviour with no warning
          stringsAsFactors <- default.stringsAsFactors()
        }
        else if (getRversion() < "4.1.0") {
          stringsAsFactors <- default.stringsAsFactors()
          # generate a warning if the default is TRUE as this will change to FALSE in future
          if(stringsAsFactors) {
            warning("PivotTable$asTidyDataFrame(): In a future version of R, default.stringsAsFactors() will be deprecated and removed, at which time the 'stringsAsFactors' argument will default to FALSE.  Explictly set the 'stringsAsFactors' argument to remove this warning.")
          }
        }
        else {
          # default to FALSE for R 4.1.0 onwards
          stringsAsFactors <- FALSE
        }
      }
      # convert
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
              if(excludeEmptyCells && cell$isEmpty) next
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
                  levelNumber <- rg$levelNumber
                  if(levelNumber==0) break
                  df[[paste0("RowLevel", sprintf("%02d", levelNumber))]][cellNumber] <- rg$caption
                  rg <- rg$parentGroup
                }
                # column heading captions
                cg <- cell$columnLeafGroup
                while(!is.null(cg)) {
                  levelNumber <- cg$levelNumber
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
      return(invisible(as.data.frame(df, stringsAsFactors=stringsAsFactors)))
    },

    #' @description
    #' Generate a list of the merged cell information arising from the data
    #' group hierarchies.
    #' This is an internal method used to support rendering the pivot table.
    #' @param axis Either "row" or "column".
    #' @return A list containing details of the merged cells.
    getMerges = function(axis=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getMerges", axis, missing(axis), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getMerges", "Getting merges...", list(axis=axis))
      if(!private$p_evaluated) stop("PivotTable$getMerges():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getMerges():  No cells exist.", call. = FALSE)
      # get axis objects/info and allowed merges:  doNotMerge, dataGroupsOnly, cellsOnly, dataGroupsAndCellsAs1, dataGroupsAndCellsAs2
      if(axis=="column") {
        groups <- private$p_cells$columnGroups # leaf-level groups
        levelCount <- private$p_columnGroup$getLevelCount(includeCurrentLevel=FALSE)
        cellsAlongAxis <- private$p_cells$rowCount
        defaultMergeEmptySpace <- private$p_mergeEmptyRowSpace
        if(private$p_mergeEmptySpaceDirection=="row") {
          if(defaultMergeEmptySpace=="cellsOnly") defaultMergeEmptySpace <- "doNotMerge"
          if(defaultMergeEmptySpace=="dataGroupsAndCellsAs1") defaultMergeEmptySpace <- "dataGroupsOnly"
          if(defaultMergeEmptySpace=="dataGroupsAndCellsAs2") defaultMergeEmptySpace <- "dataGroupsOnly"
        }
      }
      else {
        groups <- private$p_cells$rowGroups
        levelCount <- private$p_rowGroup$getLevelCount(includeCurrentLevel=FALSE)
        cellsAlongAxis <- private$p_cells$columnCount
        defaultMergeEmptySpace <- private$p_mergeEmptyColumnSpace
        if(private$p_mergeEmptySpaceDirection=="column") {
          if(defaultMergeEmptySpace=="cellsOnly") defaultMergeEmptySpace <- "doNotMerge"
          if(defaultMergeEmptySpace=="dataGroupsAndCellsAs1") defaultMergeEmptySpace <- "dataGroupsOnly"
          if(defaultMergeEmptySpace=="dataGroupsAndCellsAs2") defaultMergeEmptySpace <- "dataGroupsOnly"
        }
      }
      # build the merge info
      mergeInfo <- list()
      for(i in 1:length(groups)) {
        grp <- groups[[i]]
        mergeEmptySpace <- grp$mergeEmptySpace
        if(is.null(mergeEmptySpace)) mergeEmptySpace <- defaultMergeEmptySpace
        # validate the mergeEmptySpace for this row
        if((!(grp$isEmpty))||(grp$isTotal)) {
          if(mergeEmptySpace=="cellsOnly") mergeEmptySpace <- "doNotMerge"
          else if(mergeEmptySpace %in% c("dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2")) mergeEmptySpace <- "dataGroupsOnly"
        }
        # generate the merge info
        if(mergeEmptySpace=="doNotMerge") {
          mergeInfo[[i]] <- list(merge=FALSE, mergeGroups=FALSE, mergeCells=FALSE)
          next
        }
        ancgrps <- grp$getAncestorGroups(includeCurrentGroup=TRUE) # top-most parent is at bottom of returned list
        ancgrps <- rev(ancgrps) # top-most element is at top of list
        mergeFromLevel <- NULL
        mergeToLevel <- NULL
        for(l in 1:length(ancgrps)) {
          lgrp <- ancgrps[[l]]
          if(lgrp$isEmpty||lgrp$isOutline) {
            if(is.null(mergeFromLevel)) mergeFromLevel <- l - 1
            mergeToLevel <- l - 1
          }
          else {
            # reset back to null if descendant becomes non-empty again
            mergeFromLevel <- NULL
            mergeToLevel <- NULL
          }
        }
        isOutline <- FALSE
        if(isTRUE(grp$isOutline)) isOutline <- TRUE  #isTRUE() works with NULL
        if(is.null(mergeFromLevel)) {
          # no merge possible for this group
          mergeInfo[[i]] <- list(merge=FALSE, mergeGroups=FALSE, mergeCells=FALSE)
        }
        else if(mergeEmptySpace=="dataGroupsOnly") {
          if(mergeFromLevel==mergeToLevel) {
            mergeInfo[[i]] <- list(merge=FALSE, mergeGroups=FALSE, mergeCells=FALSE)
          }
          else {
            mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="dataGroupsOnly", isOutline=isOutline,
                                   mergeGroups=TRUE, mergeGroupsFromLevel=mergeFromLevel, mergeGroupSpan=mergeToLevel-mergeFromLevel+1,
                                   mergeCells=FALSE, skipCells=FALSE)
          }
        }
        else if(mergeEmptySpace=="cellsOnly") {
          if(cellsAlongAxis==1) {
            mergeInfo[[i]] <- list(merge=FALSE, mergeGroups=FALSE, mergeCells=FALSE)
          }
          else {
            mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="cellsOnly", isOutline=isOutline,
                                   mergeGroups=FALSE,
                                   mergeCells=TRUE, skipCells=FALSE, mergeCellSpan=cellsAlongAxis)
          }
        }
        else if(mergeEmptySpace=="dataGroupsAndCellsAs1") {
          mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="dataGroupsAndCellsAs1", isOutline=isOutline,
                                 mergeGroups=TRUE, mergeGroupsFromLevel=mergeFromLevel,
                                 mergeGroupSpan= mergeToLevel-mergeFromLevel+1+cellsAlongAxis,
                                 mergeCells=FALSE, skipCells=TRUE)
        }
        else if(mergeEmptySpace=="dataGroupsAndCellsAs2") {
          if((cellsAlongAxis==1)&&(mergeFromLevel==mergeToLevel)) {
            mergeInfo[[i]] <- list(merge=FALSE, mergeGroups=FALSE, mergeCells=FALSE)
          }
          else if(mergeFromLevel==mergeToLevel){
            mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="cellsOnly", isOutline=isOutline,
                                   mergeGroups=FALSE,
                                   mergeCells=TRUE, skipCells=FALSE, mergeCellSpan=cellsAlongAxis)
          }
          else if(cellsAlongAxis==1) {
            mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="dataGroupsOnly", isOutline=isOutline,
                                   mergeGroups=TRUE, mergeGroupsFromLevel=mergeFromLevel, mergeGroupSpan=mergeToLevel-mergeFromLevel+1,
                                   mergeCells=FALSE, skipCells=FALSE)
          }
          else {
            mergeInfo[[i]] <- list(merge=TRUE, mergeEmptySpace="dataGroupsAndCellsAs2", isOutline=isOutline,
                                   mergeGroups=TRUE, mergeGroupsFromLevel=mergeFromLevel, mergeGroupSpan=mergeToLevel-mergeFromLevel+1,
                                   mergeCells=TRUE, skipCells=FALSE, mergeCellSpan=cellsAlongAxis)
          }
        }
        # message(paste0("Merge: ", ifelse(axis=="column", "c", "r"), " ", i, " mg=", mergeInfo[[i]]$merge, " mges=", mergeInfo[[i]]$mergeEmptySpace,
        #                " outln=", mergeInfo[[i]]$isOutline, " mggrps=", mergeInfo[[i]]$mergeGroups, " mgrpsfl=", mergeInfo[[i]]$mergeGroupsFromLevel,
        #                " mgrpssp=", mergeInfo[[i]]$mergeGroupSpan, " mrgclls=", mergeInfo[[i]]$mergeCells, " skpclls=", mergeInfo[[i]]$skipCells, " mrgcllspn=", mergeInfo[[i]]$mergeCellSpan))
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getMerges", "Got merges.")
      return(invisible(mergeInfo))
    },

    #' @description
    #' Convert the pivot table to a `basictabler` table (from the `basictabler`
    #' R package) which allows further custom manipulation of the pivot table.
    #' @details
    #' See the "Outputs" vignette for more details and examples
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @param compatibility A list containing compatibility options to force
    #' legacy behaviours in the resulting `basictabler` table.
    #' @param showRowGroupHeaders `TRUE` to include the row group headers in the
    #' matrix, default `FALSE`.
    #' @return A `basictabler` table.
    asBasicTable = function(exportOptions=NULL, compatibility=NULL, showRowGroupHeaders=FALSE) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asBasicTable", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asBasicTable", compatibility, missing(compatibility), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric", "logical"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "asBasicTable", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asBasicTable", "Converting to basic table...")
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getHtml():  No cells exist to render.", call. = FALSE)
      btbl <- convertPvtTblToBasicTbl(self, exportOptions, compatibility, showRowGroupHeaders=showRowGroupHeaders)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$asBasicTable", "Converted to basic table.")
      private$addTiming("asBasicTable", timeStart)
      return(invisible(btbl))
    },

    #' @description
    #' Get the CSS declarations for the pivot table.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple pivot tables exist.
    #' @return A character value containing the CSS style declaration.
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

    #' @description
    #' Generate a HTML representation of the pivot table, optionally including
    #' additional detail for debugging purposes.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple pivot tables exist.
    #' @param includeHeaderValues Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRCFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeWorkingData Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeEvaluationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationNames Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRawValue Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeTotalInfo Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @param showRowGroupHeaders Default `FALSE`, specify `TRUE` to render the row
    #' group headings.  See the "Data Groups" vignette for details.
    #' @return A list containing HTML tags from the `htmltools` package.
    #' Convert this to a character variable using `as.character()`.
    getHtml = function(styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                       includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                       includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE,
                       exportOptions=NULL, showRowGroupHeaders=FALSE) {
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
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getHtml", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getHtml", "Getting HTML...")
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeWorkingData=includeWorkingData,
                                                   includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                   includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo,
                                                   exportOptions=exportOptions, showRowGroupHeaders=showRowGroupHeaders)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getHtml", "Got HTML.")
      private$addTiming("getHtml", timeStart)
      return(invisible(htmlTable))
    },

    #' @description
    #' Save a HTML representation of the pivot table to file, optionally including
    #' additional detail for debugging purposes.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param filePath The file to save the HTML to.
    #' @param fullPageHTML `TRUE` (default) includes basic HTML around the pivot
    #' table HTML so that the result file is a valid HTML file.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple pivot tables exist.
    #' @param includeHeaderValues Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRCFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeWorkingData Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeEvaluationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationNames Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRawValue Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeTotalInfo Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @param showRowGroupHeaders Default `FALSE`, specify `TRUE` to render the row
    #' group headings.  See the "Data Groups" vignette for details.
    #' @return No return value.
    saveHtml = function(filePath=NULL, fullPageHTML=TRUE, styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                        includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                        includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE,
                        exportOptions=NULL, showRowGroupHeaders=FALSE) {
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
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "saveHtml", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$saveHtml", "Saving HTML...", list(filePath=filePath, fullPageHTML=fullPageHTML))
      if(!private$p_evaluated) stop("PivotTable$getHtml():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      # todo: enable rendering before cells are calculated so the structure of the pivot can be checked as it is being developed
      if(is.null(private$p_cells)) stop("PivotTable$saveHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix, includeHeaderValues=includeHeaderValues,
                                                   includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                   includeWorkingData=includeWorkingData,
                                                   includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                   includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo,
                                                   exportOptions=exportOptions, showRowGroupHeaders=showRowGroupHeaders)
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

    #' @description
    #' Render a HTML representation of the pivot table as an HTML widget,
    #' optionally including additional detail for debugging purposes.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param width The width of the widget.
    #' @param height The height of the widget.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple pivot tables exist.
    #' @param includeHeaderValues Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRCFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeWorkingData Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeEvaluationFilters Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeCalculationNames Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeRawValue Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param includeTotalInfo Default `FALSE`, specify `TRUE` to render
    #' this debug information.
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @param showRowGroupHeaders Default `FALSE`, specify `TRUE` to render the row
    #' group headings.  See the "Data Groups" vignette for details.
    #' @return A HTML widget from the `htmlwidgets` package.
    renderPivot = function(width=NULL, height=NULL, styleNamePrefix=NULL, includeHeaderValues=FALSE, includeRCFilters=FALSE,
                           includeCalculationFilters=FALSE, includeWorkingData=FALSE, includeEvaluationFilters=FALSE,
                           includeCalculationNames=FALSE, includeRawValue=FALSE, includeTotalInfo=FALSE,
                           exportOptions=NULL, showRowGroupHeaders=FALSE) {
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
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "renderPivot", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$renderPivot", "Rendering htmlwidget...", list(width=width, height=height, styleNamePrefix=styleNamePrefix,
                                                                             includeHeaderValues=includeHeaderValues,
                                                                             includeRCFilters=includeRCFilters, includeCalculationFilters=includeCalculationFilters,
                                                                             includeWorkingData=includeWorkingData,
                                                                             includeEvaluationFilters=includeEvaluationFilters, includeCalculationNames=includeCalculationNames,
                                                                             includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo,
                                                                             exportOptions=exportOptions, showRowGroupHeaders=showRowGroupHeaders))
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
                                            includeRawValue=includeRawValue, includeTotalInfo=includeTotalInfo,
                                            exportOptions=exportOptions, showRowGroupHeaders=showRowGroupHeaders)),
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

    #' @description
    #' Generate a Latex representation of the pivot table.
    #' @param caption The caption to appear above the table.
    #' @param label The label to use when referring to the table elsewhere in
    #' the document
    #' @param fromRow The row number to render from.
    #' @param toRow The row number to render to.
    #' @param fromColumn The column number to render from.
    #' @param toColumn The column number to render to.
    #' @param boldHeadings Default `FALSE`, specify `TRUE` to render headings
    #' in bold.
    #' @param italicHeadings Default `FALSE`, specify `TRUE` to render headings
    #' in italic.
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @return A character variable containing the Latex representation of
    #' the pivot table.
    getLatex = function(caption=NULL, label=NULL, fromRow=NULL, toRow=NULL, fromColumn=NULL, toColumn=NULL,
                        boldHeadings=FALSE, italicHeadings=FALSE, exportOptions=NULL) {
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
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "getLatex", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLatex", "Getting Latex...", list(caption=caption, label=label,
                                                                                        fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn,
                                                                                        boldHeadings=boldHeadings, italicHeadings=italicHeadings, exportOptions=exportOptions))
      if(!private$p_evaluated) self$evaluatePivot()
      if(!private$p_evaluated) stop("PivotTable$getLatex():  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$getLatex():  No cells exist to render.", call. = FALSE)
      private$p_latexRenderer$setVisibleRange(fromRow=fromRow, toRow=toRow, fromColumn=fromColumn, toColumn=toColumn)
      ltx <- private$p_latexRenderer$getTableLatex(caption=caption, label=label, boldHeadings=boldHeadings, italicHeadings=italicHeadings, exportOptions=exportOptions)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$getLatex", "Got Latex.")
      private$addTiming("getLatex", timeStart)
      return(ltx)
    },

    #' @description
    #' Write the pivot table into the specified workbook and worksheet at
    #' the specified row-column location.
    #' @param wb A `Workbook` object representing the Excel file being written
    #' to.
    #' @param wsName A character value specifying the name of the worksheet to
    #' write to.
    #' @param topRowNumber An integer value specifying the row number in the
    #' Excel worksheet to write the pivot table.
    #' @param leftMostColumnNumber An integer value specifying the column number
    #' in the Excel worksheet to write the pivot table.
    #' @param outputHeadingsAs Must be one of "rawValue",
    #' "formattedValueAsText" (default) or "formattedValueAsNumber" to specify
    #' how data groups are written into the Excel sheet.
    #' @param outputValuesAs Must be one of "rawValue" (default),
    #' "formattedValueAsText" or "formattedValueAsNumber" to specify
    #' how cell values are written into the Excel sheet.
    #' @param applyStyles Default `TRUE` to write styling information to the cell.
    #' @param mapStylesFromCSS Default `TRUE` to automatically convert CSS style
    #' declarations to their Excel equivalents.
    #' @param exportOptions A list of additional export options - see the
    #' "A1. Appendix" for details.
    #' @param showRowGroupHeaders Default `FALSE`, specify `TRUE` to write row
    #' group headers.
    #' @return No return value.
    writeToExcelWorksheet = function(wb=NULL, wsName=NULL, topRowNumber=NULL, leftMostColumnNumber=NULL, outputHeadingsAs="formattedValueAsText",
                                     outputValuesAs="rawValue", applyStyles=TRUE, mapStylesFromCSS=TRUE, exportOptions=NULL, showRowGroupHeaders=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", topRowNumber, missing(topRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", leftMostColumnNumber, missing(leftMostColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", outputHeadingsAs, missing(outputHeadingsAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", outputValuesAs, missing(outputValuesAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "writeToExcelWorksheet", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("PivotTable$writeToExcelWorksheet():  The openxlsx package is needed to write the pivot table to an Excel file.  Please install it.", call. = FALSE)
      }
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$writeToExcelWorksheet", "Writing to worksheet...")
      private$p_openxlsxRenderer$writeToWorksheet(wb=wb, wsName=wsName, topRowNumber=topRowNumber,
                                                  leftMostColumnNumber=leftMostColumnNumber,
                                                  outputHeadingsAs=outputHeadingsAs, outputValuesAs=outputValuesAs,
                                                  applyStyles=applyStyles, mapStylesFromCSS=mapStylesFromCSS,
                                                  exportOptions=exportOptions, showRowGroupHeaders=showRowGroupHeaders)
      if(private$p_traceEnabled==TRUE) self$trace("PivotTable$writeToExcelWorksheet", "Written to worksheet.")
    },

    #' @description
    #' Capture a call for tracing purposes.
    #' This is an internal method.
    #' @param methodName The name of the method being invoked.
    #' @param desc Short description of method call.
    #' @param detailList A list containing detail such as parameter values.
    #' @return No return value.
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

    #' @description
    #' Output batch information to the console.
    #' @return No return value.
    showBatchInfo = function() {
      message(self$batchInfo)
    },

    #' @description
    #' Return the contents of the pivot table as a list for debugging.
    #' @return A list of various object properties.
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

    #' @description
    #' Return the contents of the pivot table as JSON for debugging.
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
    #' Use the `listviewer` package to view the pivot table as JSON for debugging.
    #' @return No return value.
    viewJSON = function() {
      if (!requireNamespace("listviewer", quietly = TRUE)) {
        stop("PivotTable$asJSON():  The listviewer package is needed to view the internal structure of the PivotTable as JSON.  Please install it.", call. = FALSE)
      }
      listviewer::jsonedit(self$asList(), mode="code")
    },

    #' @description
    #' Clean-up the pivot table.
    #' @return No return value.
    finalize = function() {
      if(!is.null(private$p_traceFile)) close(private$p_traceFile)
    }
  ),
  active = list(

    #' @field argumentCheckMode The level of argument checking to perform.
    #' One of "auto", "none", "minimal", "basic", "balanced" (default)
    #' or "full".
    argumentCheckMode = function(value) { return(private$p_argumentCheckMode) },

    #' @field compatibility A list containing compatibility options to force
    #' legacy behaviours.  See the NEWS file for details.
    compatibility = function(value) { return(private$p_compatibility) },

    #' @field traceEnabled Default `FALSE`.  Specify `TRUE` to generate a trace
    #' for debugging purposes.
    traceEnabled = function(value){
      if(missing(value)) return(invisible(private$p_traceEnabled))
      else {
        if(is.logical(value)) private$p_traceEnabled <- value
        else stop("PivotTable$traceEnabled: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },

    #' @field processingLibrary The package to use when processing data.
    #' Must be one of "auto" (which today is dplyr), "dplyr" or "data.table".
    processingLibrary = function(value) { return(private$p_processingLibrary) },

    #' @field data A `PivotData` object containing the data frames added to the
    #' pivot table.
    data = function(value) { return(private$p_data) },

    #' @field rowGroup The hidden root `PivotDataGroup` at the top of the row
    #' data groups hierarchy.  The children of this group form the first level
    #' of visible row data groups.
    rowGroup = function(value) { return(invisible(private$p_rowGroup ))},

    #' @field columnGroup The hidden root `PivotDataGroup` at the top of the
    #' column data groups hierarchy.  The children of this group form the first
    #' level of visible column data groups.
    columnGroup = function(value) { return(invisible(private$p_columnGroup ))},

    #' @field rowGroupLevelCount The number of visible levels in the row data
    #' group hierarchy.
    rowGroupLevelCount = function(value) { return(invisible(private$p_rowGroup$getLevelCount())) },

    #' @field columnGroupLevelCount The number of visible levels in the column
    #' data group hierarchy.
    columnGroupLevelCount = function(value) { return(invisible(private$p_columnGroup$getLevelCount())) },

    #' @field topColumnGroups A list containing the first level of column data
    #' groups.
    topColumnGroups = function(value) {
      return(private$p_columnGroup$childGroups)
    },

    #' @field leafColumnGroups  A list containing the bottom level of column
    #' data groups.
    leafColumnGroups = function(value) {
      leafGroups = list()
      grps <- private$p_columnGroup$getLeafGroups(leafGroups)
      return(invisible(grps))
    },

    #' @field allColumnGroups  A list containing all of the column data groups.
    allColumnGroups = function(value) {
      grps <- private$p_columnGroup$getDescendantGroups()
      return(invisible(grps))
    },

    #' @field topRowGroups A list containing the first level of row data
    #' groups.
    topRowGroups = function(value) {
      return(private$p_rowGroup$childGroups)
    },

    #' @field leafRowGroups A list containing the bottom level of row data
    #' groups.
    leafRowGroups = function(value) {
      leafGroups = list()
      grps <- private$p_rowGroup$getLeafGroups(leafGroups)
      return(invisible(grps))
    },

    #' @field allRowGroups  A list containing all of the row data groups.
    allRowGroups = function(value) {
      grps <- private$p_rowGroup$getDescendantGroups()
      return(invisible(grps))
    },

    #' @field rowGrpHeaders A list containing the row group headers.
    rowGrpHeaders = function() { return(invisible(private$p_rowGrpHeaders ))},

    #' @field calculationGroups A list containing the calculation groups in
    #' the pivot table.
    calculationGroups = function(value) { return(invisible(private$p_calculationGroups)) },

    #' @field calculationsPosition Either "row" or "column" describing which axis
    #' the calculations are rendered.
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

    #' @field evaluationMode Either "batch" (default) or "sequential" (legacy).
    evaluationMode = function(value) { return(invisible(private$p_evaluationMode)) },

    #' @field batchInfo Diagnostic information describing the batches used in the
    #' last pivot table evaluation.
    batchInfo = function(value) { return(invisible(private$p_lastCellBatchInfo)) },

    #' @field cells A `PivotCells` object that contains all of the cells in the pivot
    #' table.
    cells = function(value) { return(invisible(private$p_cells)) },

    #' @field allCells A list of all of the cells in the pivot table, where each element
    #' in the list is a 'PivotCell' object.
    allCells = function(value) {
      if(!private$p_evaluated) stop("PivotTable$allCells:  Pivot table has not been evaluated.  Call evaluatePivot() to evaluate the pivot table.", call. = FALSE)
      if(is.null(private$p_cells)) stop("PivotTable$allCells:  No cells exist.", call. = FALSE)
      return(invisible(private$p_cells$all))
    },

    #' @field rowCount The number of rows in the pivot table, excluding headings.
    rowCount = function(value) { return(invisible(private$p_cells$rowCount)) },

    #' @field columnCount The number of columns in the pivot table, excluding headings.
    columnCount = function(value) { return(invisible(private$p_cells$columnCount)) },

    #' @field fixedWidthSized The total width of the pivot table in characters if
    #' the pivot table were to be rendered as plain text, e.g. to the console.
    fixedWidthSized = function(value) {
      if(missing(value)) return(invisible(private$p_fixedWidthSized))
      else {
        if(is.logical(value)) private$p_fixedWidthSized <- value
        else stop("PivotTable$fixedWidthSized: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },

    #' @field asCharacter A plain text representation of the pivot table.
    asCharacter = function() { return(self$print(asCharacter=TRUE)) },

    #' @field theme The name of the theme used to style the pivot table.
    #' If setting this property, either a theme name can be used, or
    #' a list can be used (which specifies a simple theme) or a
    #' `PivotStyles` object can be used.
    #' See the "Styling" vignette for details and examples.
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "list", "PivotStyles"), allowedListElementClasses="character")
        }
        if("character" %in% class(value)) private$p_styles <- getTheme(parentPivot=self, themeName=value)
        else if("list" %in% class(value)) private$p_styles <- getSimpleColoredTheme(parentPivot=self, theme=value)
        else if("PivotStyles" %in% class(value)) private$p_styles <- value
        return(invisible())
      }
    },

    #' @field styles A `PivotStyles` object that contains the styles
    #' applied to the pivot table.
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

    #' @field allowExternalStyles Default `FALSE`, which means the `PivotStyles`
    #' object checks that style names specified for styling the different
    #' parts of the pivot table must exist in the styles collection.  If they do
    #' not an error will occur.  Specify `TRUE` to disable this check, e.g. if
    #' the style definitions are not managed by `pivottabler` but instead
    #' in an external system.
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

    #' @field mergeEmptyRowSpace A character value describing how empty
    #' space is merged.  Allowed values:  "doNotMerge", "dataGroupsOnly",
    #' "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2".
    mergeEmptyRowSpace = function(value) {
      if(missing(value)) return(invisible(private$p_mergeEmptyRowSpace))
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mergeEmptyRowSpace", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
        }
        private$p_mergeEmptyRowSpace <- value
        return(invisible())
      }
    },

    #' @field mergeEmptyColumnSpace A character value describing how empty
    #' space is merged.  Allowed values:  "doNotMerge", "dataGroupsOnly",
    #' "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2".
    mergeEmptyColumnSpace = function(value) {
      if(missing(value)) return(invisible(private$p_mergeEmptyColumnSpace))
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mergeEmptyColumnSpace", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2"))
        }
        private$p_mergeEmptyColumnSpace <- value
        return(invisible())
      }
    },

    #' @field mergeEmptySpaceDirection A character value describing how empty
    #' space is merged.  Allowed values:  "row" or "column"
    mergeEmptySpaceDirection = function(value) {
      if(missing(value)) return(invisible(private$p_mergeEmptySpaceDirection))
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "PivotTable", "mergeEmptySpaceDirection", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character", allowedValues=c("row", "column"))
        }
        private$p_mergeEmptySpaceDirection <- value
        return(invisible())
      }
    },

    #' @field allTimings Get a data frame containing timing details
    #' of pivot table operations.
    allTimings = function(value) {
      descriptions <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$desc), NA, x$desc)) })
      user <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["user.self"]), NA, x$time["user.self"])) })
      system <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["sys.self"]), NA, x$time["sys.self"])) })
      elapsed <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["elapsed"]), NA, x$time["elapsed"])) })
      return(data.frame(action=descriptions, user=user, system=system, elapsed=elapsed))
    },

    #' @field significantTimings Get a data frame containing timing details
    #' of significant pivot table operations (i.e. where elapsed>0.1).
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
    p_defaults = NULL,
    p_lastInstanceId = NULL,
    p_data = NULL,
    p_styles = NULL,
    p_rowGroup = NULL,
    p_columnGroup = NULL,
    p_rowGrpHeaders = NULL,
    p_calculationsPosition = NULL,
    p_calculationGroups = NULL,
    p_calculationsSet = FALSE,
    p_evaluationMode = "batch",
    p_evaluated = FALSE,
    p_cells = NULL,
    p_lastCellBatchInfo = NULL,
    p_fixedWidthSized = FALSE,
    p_mergeEmptyRowSpace = "dataGroupsAndCellsAs2",
    p_mergeEmptyColumnSpace = "dataGroupsAndCellsAs2",
    p_mergeEmptySpaceDirection = "row",
    p_htmlRenderer = NULL,
    p_latexRenderer = NULL,
    p_openxlsxRenderer = NULL,
    p_compatibility = NULL,
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
