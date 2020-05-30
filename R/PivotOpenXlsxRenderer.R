
#' R6 class that renders a pivot table into an Excel worksheet.
#'
#' @description
#' The `PivotOpenXlsxRenderer` class creates a representation of a
#' pivot table in an Excel file using the `openxlsx` package.
#' See the "Excel Export" vignette for details and examples.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotOpenXlsxRenderer <- R6::R6Class("PivotOpenXlsxRenderer",
  public = list(

    #' @description
    #' Create a new `PivotOpenXlsxRenderer` object.
    #' @param parentPivot The pivot table that this `PivotOpenXlsxRenderer`
    #' instance belongs to.
    #' @return A new `PivotOpenXlsxRenderer` object.
    initialize = function(parentPivot) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$new", "Creating new OpenXlsx Renderer...")
      private$p_styles <- PivotOpenXlsxStyles$new(parentPivot=private$p_parentPivot)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$new", "Created new OpenXlsx Renderer.")
    },

    #' An internal method used when rendering a pivot table to HTML.
    #' Clear the IsRendered flags that exist on the `PivotDataGroup` class.
    #' @return No return value.
    clearIsRenderedFlags = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$clearIsRenderedFlags", "Clearing isRendered flags...")
      clearFlags <- function(dg) {
        grp <- dg
        while(!is.null(grp)) {
          grp$isRendered <- FALSE
          grp <- grp$parentGroup
        }
      }
      rowGroups <- private$p_parentPivot$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(rowGroups, clearFlags)
      columnGroups <- private$p_parentPivot$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE)
      lapply(columnGroups, clearFlags)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$clearIsRenderedFlags", "Cleared isRendered flags...")
      return(invisible())
    },

    #' @description
    #' Writes a value to a cell and applies styling as needed.
    #' @param wb A `Workbook` object representing the Excel file being written
    #' to.
    #' @param wsName A character value specifying the name of the worksheet to
    #' write to.
    #' @param rowNumber An integer value specifying the row number of the cell
    #' to write to.
    #' @param columnNumber An integer value specifying the column number of the
    #' cell to write to.
    #' @param value The value to write into the cell.
    #' @param applyStyles Default `TRUE` to write styling information to the cell.
    #' @param baseStyleName A character value specifying a named style defined
    #' in the pivot table.
    #' @param style A `PivotStyle` object containing CSS style declarations to
    #' override the base style.
    #' @param mapFromCss Default `TRUE` to automatically convert CSS style
    #' declarations to their Excel equivalents.
    #' @param mergeRows An integer vector specifying the row extent of a merged
    #' cell.
    #' @param mergeColumns An integer vector specifying the column extent of a
    #' merged cell.
    #' @return No return value.
    writeToCell = function(wb=NULL, wsName=NULL, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL) { #, debugMsg=NULL) {
       if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "character", "Date", "POSIXct"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotStyle")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mergeRows, missing(mergeRows), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mergeColumns, missing(mergeColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      # if(!is.null(debugMsg)) {
      #   message(paste0("writeToCell: ", debugMsg, ": ", as.character(value), ", rn=", rowNumber, " cn=", columnNumber,
      #                  ", mr=", suppressWarnings(min(mergeRows)), " to ", suppressWarnings(max(mergeRows)),
      #                  ", mc=", suppressWarnings(min(mergeColumns)), " to ", suppressWarnings(max(mergeColumns))))
      # }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$writeToWorksheet", "Writing to cell...")
      # write the value
      if (!is.null(value) && (length(value) > 0)) {
        openxlsx::writeData(wb, sheet=wsName, x=value, colNames=FALSE, rowNames=FALSE, startCol=columnNumber, startRow=rowNumber)
      }
      # merge cells
      isMergedCells <- isNumericValue(mergeRows)&&isNumericValue(mergeColumns)&&((length(mergeRows)>1)||(length(mergeColumns)>1))
      if(isMergedCells) openxlsx::mergeCells(wb, sheet=wsName, cols=mergeColumns, rows=mergeRows)
      # styling
      if(applyStyles) {
        openxlsxStyle <- NULL
        # just a base style (these were all already added to the OpenXlsxStyles collection, so can just do a find based on the name only)
        if(isTextValue(baseStyleName)&&is.null(style)) {
          openxlsxStyle <- private$p_styles$findNamedStyle(baseStyleName)
          if(is.null(openxlsxStyle)) stop(paste0("PivotOpenXlsxRenderer$writeToWorksheet(): Unable to find named style '", baseStyleName, "'."), call. = FALSE)
          if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
        }
        # base style and overlaid style, or just an overlaid style
        else if(!is.null(style)) {
          if(isTextValue(baseStyleName)) {
            # need to get the base style and overlay the additional style attributes
            baseStyle <- private$p_parentPivot$styles$getStyle(baseStyleName)
            fullStyle <- baseStyle$getCopy(newStyleName="")
            fullStyle$setPropertyValues(style$declarations)
          }
          else fullStyle <- style
          openxlsxStyle <- private$p_styles$findOrAddStyle(action="findOrAdd", baseStyleName=baseStyleName, isBaseStyle=FALSE, style=fullStyle, mapFromCss=mapFromCss)
          if(is.null(openxlsxStyle)) stop("PivotOpenXlsxRenderer$writeToWorksheet(): Failed to find or add style.", call. = FALSE)
          if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
        }
        # min heights/widths
        if(!is.null(openxlsxStyle)) {
          cw <- openxlsxStyle$minColumnWidth
          rh <- openxlsxStyle$minRowHeight
          if((!is.null(cw)) && (cw > private$p_minimumColumnWidths[columnNumber])) private$p_minimumColumnWidths[columnNumber] <- cw
          if((!is.null(rh)) && (rh > private$p_minimumRowHeights[rowNumber])) private$p_minimumRowHeights[rowNumber] <- rh
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$writeToWorksheet", "Written to cell.")
    },

    #' @description
    #' Write the pivot table into the specified workbook and worksheet at
    #' the specified row-column location.
    #' @param wb A `Workbook` object representing the Excel file being written
    #' to.
    #' @param wsName A character value specifying the name of the worksheet to
    #' write to.
    #' @param topRowNumber An integer value specifying the row number in the
    #' Excel worksheet  to write the pivot table.
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
    writeToWorksheet = function(wb=NULL, wsName=NULL, topRowNumber=NULL, leftMostColumnNumber=NULL, outputHeadingsAs="formattedValueAsText",
                                outputValuesAs="rawValue", applyStyles=TRUE, mapStylesFromCSS=TRUE, exportOptions=NULL, showRowGroupHeaders=FALSE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", topRowNumber, missing(topRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", leftMostColumnNumber, missing(leftMostColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", outputHeadingsAs, missing(outputHeadingsAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", outputValuesAs, missing(outputValuesAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", showRowGroupHeaders, missing(showRowGroupHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$writeToWorksheet", "Writing to worksheet...")

      # clear the is rendered flags
      self$clearIsRenderedFlags()

      # clear the pivot styles
      private$p_styles$clearStyles()

      # create an OpenXlsxStyle for each named style in the pivot table
      if(applyStyles) private$p_styles$addNamedStyles(mapFromCss=mapStylesFromCSS)

      # output the styles
      # message(private$p_styles$asString(seperator="\r\n\r\n"))

      # do the export, going cell by cell
      # for each header and cell, if the style is basic, find the basic style matching on name only
      # if the style is not basic (i.e. has additional style settings applied directly to that header/cell)
      # then need to do a full match on all of the details of the style (slower)

      # get the style names
      tableStyle = private$p_parentPivot$styles$tableStyle
      rootStyle = private$p_parentPivot$styles$rootStyle
      rowHeaderStyle = private$p_parentPivot$styles$rowHeaderStyle
      colHeaderStyle = private$p_parentPivot$styles$colHeaderStyle
      cellStyle = private$p_parentPivot$styles$cellStyle
      outlineRowHeaderStyle = private$p_parentPivot$styles$outlineRowHeaderStyle
      if(is.null(outlineRowHeaderStyle)) outlineRowHeaderStyle <- rowHeaderStyle
      outlineColHeaderStyle = private$p_parentPivot$styles$outlineColHeaderStyle
      if(is.null(outlineColHeaderStyle)) outlineColHeaderStyle <- colHeaderStyle
      outlineCellStyle = private$p_parentPivot$styles$outlineCellStyle
      if(is.null(outlineCellStyle)) outlineCellStyle <- cellStyle
      totalStyle = private$p_parentPivot$styles$totalStyle

      # get the data groups:  these are the leaf level groups
      rowGroups <- private$p_parentPivot$cells$rowGroups
      columnGroups <- private$p_parentPivot$cells$columnGroups
      # message(paste0("rg=", length(rowGroups), " cg=", length(columnGroups)))

      # get the dimensions of the various parts of the table...
      # ...headings:
      rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
      columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
      # message(paste0("rglc=", rowGroupLevelCount, " cglc=", columnGroupLevelCount))

      # ...cells:
      rowCount <- private$p_parentPivot$cells$rowCount
      columnCount <- private$p_parentPivot$cells$columnCount
      # message(paste0("rc=", rowCount, " cc=", columnCount))

      # ... merges
      rowMerges <- private$p_parentPivot$getMerges(axis="row")

      # initialise the minimum widths/heights
      private$p_minimumRowHeights = numeric(1048576)
      private$p_minimumColumnWidths = numeric(16384)

      # special case of no rows and no columns, return a blank empty table
      if((rowGroupLevelCount==0)&&(columnGroupLevelCount==0)) {
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value="(no data)",
                         applyStyles=applyStyles, baseStyleName=cellStyle, style=NULL, mapFromCss=mapStylesFromCSS)#, debugMsg="special case")
        return(invisible(NULL))
      }

      # there must always be at least one row and one column
      insertDummyRowHeading <- (rowGroupLevelCount==0) & (columnGroupLevelCount > 0)
      insertDummyColumnHeading <- (columnGroupLevelCount==0) & (rowGroupLevelCount > 0)
      columnOffsetDueToDummyRow <- ifelse(insertDummyRowHeading, 1, 0)
      rowOffsetDueToDummyColumn <- ifelse(insertDummyColumnHeading, 1, 0)

      # render the column headings, with a large blank cell at the start over the row headings
      if(insertDummyColumnHeading) {
        # rendering the column headings (special case of no column groups existing)
        if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
          rowGrpHeaders <- private$p_parentPivot$rowGrpHeaders
          for(c in 1:rowGroupLevelCount) {
            rowGrpHeader <- NULL
            if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
            if(is.null(rowGrpHeader)) rowGrpHeader <- character(0)
            self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber+c-1, value=rowGrpHeader,
                             applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                             mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1+rowOffsetDueToDummyColumn),
                             mergeColumns=(leftMostColumnNumber+c-1):(leftMostColumnNumber+c-1))
          }
        }
        else
        {
          self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value=character(0),
                           applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                           mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1+rowOffsetDueToDummyColumn),
                           mergeColumns=leftMostColumnNumber:(leftMostColumnNumber+rowGroupLevelCount-1+columnOffsetDueToDummyRow))#, debugMsg="top-left blank with dummy column")
        }
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber+rowGroupLevelCount+columnOffsetDueToDummyRow,
                         value=character(0), applyStyles=applyStyles, baseStyleName=colHeaderStyle, style=NULL, mapFromCss=mapStylesFromCSS)#, debugMsg="dummy column heading")
      }
      else {
        # rendering the column headings (normal scenario)
        for(r in 1:columnGroupLevelCount) {
          if(r==1) {
            # generate the large top-left blank cell or cells
            if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
              rowGrpHeaders <- private$p_parentPivot$rowGrpHeaders
              for(c in 1:rowGroupLevelCount) {
                rowGrpHeader <- NULL
                if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
                if(is.null(rowGrpHeader)) rowGrpHeader <- character(0)
                self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber+c-1, value=rowGrpHeader,
                                 applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                                 mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1+rowOffsetDueToDummyColumn),
                                 mergeColumns=(leftMostColumnNumber+c-1):(leftMostColumnNumber+c-1))
              }
            }
            else {
              self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value=character(0),
                               applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                               mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1+rowOffsetDueToDummyColumn),
                               mergeColumns=leftMostColumnNumber:(leftMostColumnNumber+rowGroupLevelCount-1+columnOffsetDueToDummyRow))#, debugMsg="top-left blank normal")
            }
          }
          # get the groups at this level
          grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r)
          xlColumnNumber <- leftMostColumnNumber+rowGroupLevelCount+columnOffsetDueToDummyRow
          for(c in 1:length(grps)) {
            grp <- grps[[c]]
            chs <- colHeaderStyle
            if(!is.null(grp$baseStyleName)) chs <- grp$baseStyleName
            xlRowNumber <- topRowNumber+r-1+rowOffsetDueToDummyColumn
            value <- private$getExportValue(grp$sortValue, grp$caption, outputHeadingsAs,
                                            useCaptionIfRawValueNull=TRUE)
            value <- exportValueAs(grp$sortValue, value, exportOptions, blankValue=character(0))
            self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                             applyStyles=applyStyles, baseStyleName=chs, style=grp$style, mapFromCss=mapStylesFromCSS,
                             mergeRows=xlRowNumber,
                             mergeColumns=xlColumnNumber:(xlColumnNumber+length(grp$leafGroups)-1))#, debugMsg="col group")
            xlColumnNumber <- xlColumnNumber + length(grp$leafGroups)
          }
        }
      }

      # render the rows
      for(r in 1:rowCount) {
        # row number
        xlRowNumber <- topRowNumber + columnGroupLevelCount + r - 1 + rowOffsetDueToDummyColumn
        xlColumnNumber <- leftMostColumnNumber
        # merge info
        rowMerge <- rowMerges[[r]]
        # render the row headings
        rg <- NULL
        if(insertDummyRowHeading) {
          self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=character(0),
                           applyStyles=applyStyles, baseStyleName=rowHeaderStyle, style=NULL, mapFromCss=mapStylesFromCSS)#, debugMsg="dummy row heading")
        }
        else {
          # get the leaf row group, then render any parent data groups that haven't yet been rendered
          rg <- rowGroups[[r]]
          ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
          for(c in (length(ancrgs)-1):1) { # start iterating at last item minus 1, which is first visible level in the pivot (level number=1)
            ancg <- ancrgs[[c]]
            xlColumnNumber <- leftMostColumnNumber + length(ancrgs) - c - 1 + columnOffsetDueToDummyRow
            if(ancg$isRendered) next
            rowGroupLevelNumber <- rowGroupLevelCount - c + 1
            # merge info
            rowMrgColumnSpan <- NULL
            lastDataGroupInRow <- FALSE
            if(rowMerge$merge && rowMerge$mergeGroups && (rowGroupLevelNumber==rowMerge$mergeGroupsFromLevel)) {
              rowMrgColumnSpan <- rowMerge$mergeGroupSpan
              lastDataGroupInRow <- TRUE
            }
            # style info
            if(ancg$isOutline && ancg$styleAsOutline) rhs <- outlineRowHeaderStyle
            else rhs <- rowHeaderStyle
            if(!is.null(ancg$baseStyleName)) rhs <- ancg$baseStyleName
            value <- private$getExportValue(ancg$sortValue, ancg$caption, outputHeadingsAs,
                                            useCaptionIfRawValueNull=TRUE)
            value <- exportValueAs(ancg$sortValue, value, exportOptions, blankValue=character(0))
            if(is.null(rowMrgColumnSpan)||(rowMrgColumnSpan==1)) mergeColumns <- xlColumnNumber
            else mergeColumns <- xlColumnNumber:(xlColumnNumber+rowMrgColumnSpan-1)
            self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                             applyStyles=applyStyles, baseStyleName=rhs, style=ancg$style, mapFromCss=mapStylesFromCSS,
                             mergeRows=xlRowNumber:(xlRowNumber+length(ancg$leafGroups)-1),
                             mergeColumns=mergeColumns)#, debugMsg="row heading")
            ancg$isRendered <- TRUE
            if(lastDataGroupInRow) break
          }
        }
        # render the cell values
        if(!(rowMerge$merge && isTRUE(rowMerge$skipCells))) {
          styleAsOutline <- FALSE
          rgCellBaseStyleName <- NULL
          rgCellStyle <- NULL
          if(!is.null(rg)) {
            styleAsOutline <- rg$isOutline && rg$styleAsOutline
            rgCellBaseStyleName <- rg$netCellBaseStyleName
            rgCellStyle <- rg$netCellStyle
          }
          if(rowMerge$mergeCells) {
            # special case of all the cells being merged
            xlRowNumber <- topRowNumber + columnGroupLevelCount + r - 1 + rowOffsetDueToDummyColumn
            xlColumnNumber <- leftMostColumnNumber + rowGroupLevelCount + columnOffsetDueToDummyRow
            cs <- cellStyle
            if(!is.null(rgCellBaseStyleName)) cs <- rgCellBaseStyleName
            else if(styleAsOutline && !is.null(outlineCellStyle)) cs <- outlineCellStyle
            sd <- NULL
            if(!is.null(rgCellStyle)) sd <- rgCellStyle
            self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber,
                             mergeRows=xlRowNumber, mergeColumns=xlColumnNumber:(xlColumnNumber+columnCount-1), value="",
                             applyStyles=applyStyles, baseStyleName=cs, style=sd, mapFromCss=mapStylesFromCSS)
          }
          else {
            # normal scenario
            for(c in 1:columnCount) {
              xlRowNumber <- topRowNumber + columnGroupLevelCount + r - 1 + rowOffsetDueToDummyColumn
              xlColumnNumber <- leftMostColumnNumber + rowGroupLevelCount + c - 1 + columnOffsetDueToDummyRow
              # get the cell
              cell <- private$p_parentPivot$cells$getCell(r, c)
              # get the column group
              columnGroup <- private$p_parentPivot$getLeafColumnGroup(c=c)
              # base style name precedence:  from the cell, from the column group, from the row group,
              #                              then default outline (if outline), default total (if total), default cell
              cgBaseStyleName <- columnGroup$netCellBaseStyleName
              if(!is.null(cell$baseStyleName)) cs <- cell$baseStyleName
              else if(!is.null(cgBaseStyleName)) cs <- cgBaseStyleName
              else if(!is.null(rgCellBaseStyleName)) cs <- rgCellBaseStyleName
              else if(styleAsOutline && (!is.null(outlineCellStyle))) cs <- outlineCellStyle
              else if(cell$isTotal) cs <- totalStyle
              else cs <- cellStyle
              # style overrides precedence:  from the cell, from the column group, from the row group
              #                              given these styles are not single values, but lists of declarations, the styles
              #                              aggregate via a set operation, i.e. intersect/union from row -> col -> cell
              sd <- NULL
              if(!is.null(rgCellStyle)) {
                if(is.null(sd)) { sd <- rgCellStyle$getCopy("") }
                else { sd$setPropertyValues(rgCellStyle$declarations) }
              }
              colGroupStyle <- columnGroup$netCellStyle
              if(!is.null(colGroupStyle)) {
                if(is.null(sd)) { sd <- colGroupStyle$getCopy("") }
                else { sd$setPropertyValues(colGroupStyle$declarations) }
              }
              if(!is.null(cell$style)) {
                if(is.null(sd)) { sd <- cell$style$getCopy("") }
                else { sd$setPropertyValues(cell$style$declarations) }
              }
              # render the cell
              value <- private$getExportValue(cell$rawValue, cell$formattedValue, outputValuesAs,
                                              useCaptionIfRawValueNull=FALSE)
              value <- exportValueAs(cell$rawValue, value, exportOptions, blankValue=character(0))
              self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                               applyStyles=applyStyles, baseStyleName=cs, style=sd, mapFromCss=mapStylesFromCSS)#, debugMsg="value")
            }
          }
        }
      }

      # set the minimum heights / widths
      for(r in 1:length(private$p_minimumRowHeights)) {
        if(private$p_minimumRowHeights[r] > 0)
          openxlsx::setRowHeights(wb, sheet=wsName, rows=r, heights=private$p_minimumRowHeights[r])
      }
      for(c in 1:length(private$p_minimumColumnWidths)) {
        if(private$p_minimumColumnWidths[c] > 0)
          openxlsx::setColWidths(wb, sheet=wsName, cols=c, widths=private$p_minimumColumnWidths[c])
      }

      # TRELLO NOTES

      # PivotOpenXlsxStyles collection builds up a collection of styles defined
      # in the Excel document.  These can be reused as follows: Before rendering
      # begins, a PivotOpenXlsxStyle object is created for each named style
      # defined in the pivot table. As each cell is rendered to the workbook, if
      # a pivot cell with a named style but no other style overrides is
      # encountered, then these styles are used.  If a pivot cell is encountered
      # with overrides, then the styles collection is searched to find the
      # matching style.  If not found, then a new style is added which can be
      # reused later. The PivotOpenXlsxStyle object looks for styles named
      # "xl-".  If found, these are used.  These closely map to the arguments of
      # the openxlsx createStyle() function.  If style settings named "xl-" are
      # not found, then an attempt is made to use the CSS equivalents.  See the
      # ExcelExport.xlsx file for details. Currently, the following still needs
      # doing in terms of style settings:
      #
      # * font-name, font-size, etc have been mapped, but the general CSS font
      # setting (e.g. font: 15px arial, sans-serif;) has not been mapped.
      # * border settings have been partially mapped.  border, border-top,
      # border-bottom, etc have been mapped.  The very specific (e.g.
      # border-bottom-width, border-bottom-style, etc) have not been mapped.

      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$writeToWorksheet", "Written to worksheet.")
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_styles = NULL,
    p_minimumRowHeights = NULL,
    p_minimumColumnWidths = NULL,

    # private functions:
    getExportValue = function(rawValue, formattedValue, outputAs, useCaptionIfRawValueNull) {
      if(is.null(rawValue) && useCaptionIfRawValueNull) value <- formattedValue
      else if(outputAs=="rawValue") value <- rawValue
      else if(outputAs=="formattedValueAsText") value <- formattedValue
      else if(outputAs=="formattedValueAsNumber") {
        value <- suppressWarnings(as.numeric(formattedValue))
        if(!isNumericValue(value)) value <- formattedValue
      }
      else value <- rawValue
      return(value)
    }
  )
)
