#' A class that renders a pivot table into an Excel worksheet.
#'
#' The PivotOpenXlsxRenderer class creates a representation of a pivot table in an Excel file using the openxlsx package.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmltools
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   render to Excel via the openxlsx package
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot table renderer, specifying the
#'   field value documented above.}
#'
#'   \item{\code{clearIsRenderedFlags()}}{Clear the IsRendered flags that exist
#'   on the PivotDataGroup class.}
#'   \item{\code{writeToCell(wb=NULL, wsName=NULL, rowNumber=NULL,
#'   columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL,
#'   style=NULL, mapFromCss=TRUE)}}{Writes a value to a cell and applies styling
#'   as needed.}
#'   \item{\code{writeToWorksheet(wb=NULL, wsName=NULL, topRowNumber=NULL,
#'   leftMostColumnNumber=NULL, outputValuesAs="value", applyStyles=TRUE,
#'   mapStylesFromCSS=TRUE, exportOptions=NULL)}}{Output the pivot table into
#'   the specified workbook and worksheet at the specified row-column location.}
#' }

PivotOpenXlsxRenderer <- R6::R6Class("PivotOpenXlsxRenderer",
  public = list(
    initialize = function(parentPivot) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$new", "Creating new OpenXlsx Renderer...")
      private$p_styles <- PivotOpenXlsxStyles$new(parentPivot=private$p_parentPivot)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$new", "Created new OpenXlsx Renderer.")
    },
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
    writeToCell = function(wb=NULL, wsName=NULL, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL) {
       if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "character"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotStyle")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mergeRows, missing(mergeRows), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToCell", mergeColumns, missing(mergeColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxRenderer$writeToWorksheet", "Writing to cell...")
      # write the value
      if (!is.null(value) && (length(value) > 0)) {
        openxlsx::writeData(wb, sheet=wsName, x=value, colNames=FALSE, rowNames=FALSE, startCol=columnNumber, startRow=rowNumber)
      }
      # merge cells
      isMergedCells <- isNumericValue(mergeRows)&&isNumericValue(mergeColumns)
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
    writeToWorksheet = function(wb=NULL, wsName=NULL, topRowNumber=NULL, leftMostColumnNumber=NULL, outputValuesAs="rawValue", applyStyles=TRUE, mapStylesFromCSS=TRUE, exportOptions=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", topRowNumber, missing(topRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", leftMostColumnNumber, missing(leftMostColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", outputValuesAs, missing(outputValuesAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxRenderer", "writeToWorksheet", exportOptions, missing(exportOptions), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
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
      totalStyle = private$p_parentPivot$styles$totalStyle

      # get the data groups:  these are the leaf level groups
      rowGroups <- private$p_parentPivot$cells$rowGroups
      columnGroups <- private$p_parentPivot$cells$columnGroups

      # get the dimensions of the various parts of the table...
      # ...headings:
      rowGroupLevelCount <- private$p_parentPivot$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
      columnGroupLevelCount <- private$p_parentPivot$columnGroup$getLevelCount(includeCurrentLevel=FALSE)

      # ...cells:
      rowCount <- private$p_parentPivot$cells$rowCount
      columnCount <- private$p_parentPivot$cells$columnCount

      # initialise the minimum widths/heights
      private$p_minimumRowHeights = numeric(1048576)
      private$p_minimumColumnWidths = numeric(16384)

      # special case of no rows and no columns, return a blank empty table
      if((rowGroupLevelCount==0)&&(columnGroupLevelCount==0)) {
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value="(no data)",
                         applyStyles=applyStyles, baseStyleName=cellStyle, style=NULL, mapFromCss=mapStylesFromCSS)
        return(invisible(NULL))
      }

      # there must always be at least one row and one column
      insertDummyRowHeading <- (rowGroupLevelCount==0) & (columnGroupLevelCount > 0)
      insertDummyColumnHeading <- (columnGroupLevelCount==0) & (rowGroupLevelCount > 0)

      # render the column headings, with a large blank cell at the start over the row headings
      if(insertDummyColumnHeading) {
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value=character(0),
                         applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                         mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1),
                         mergeColumns=leftMostColumnNumber:(leftMostColumnNumber+rowGroupLevelCount-1))
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber+rowGroupLevelCount, value=character(0),
                         applyStyles=applyStyles, baseStyleName=colHeaderStyle, style=NULL, mapFromCss=mapStylesFromCSS)

      }
      else {
        for(r in 1:columnGroupLevelCount) {
          if(r==1) { # generate the large top-left blank cell
            self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value=character(0),
                             applyStyles=applyStyles, baseStyleName=rootStyle, style=NULL, mapFromCss=mapStylesFromCSS,
                             mergeRows=topRowNumber:(topRowNumber+columnGroupLevelCount-1),
                             mergeColumns=leftMostColumnNumber:(leftMostColumnNumber+rowGroupLevelCount-1))
          }
          # get the groups at this level
          grps <- private$p_parentPivot$columnGroup$getLevelGroups(level=r)
          xlColumnNumber <- leftMostColumnNumber+rowGroupLevelCount
          for(c in 1:length(grps)) {
            grp <- grps[[c]]
            chs <- colHeaderStyle
            if(!is.null(grp$baseStyleName)) chs <- grp$baseStyleName
            xlRowNumber <- topRowNumber+r-1
            value <- exportValueAs(grp$sortValue, grp$caption, exportOptions, blankValue=character(0))
            self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                             applyStyles=applyStyles, baseStyleName=chs, style=grp$style, mapFromCss=mapStylesFromCSS,
                             mergeRows=xlRowNumber,
                             mergeColumns=xlColumnNumber:(xlColumnNumber+length(grp$leafGroups)-1))
            xlColumnNumber <- xlColumnNumber + length(grp$leafGroups)
          }
        }
      }

      # render the rows
      for(r in 1:rowCount) {
        # row number
        xlRowNumber <- topRowNumber + columnGroupLevelCount + r - 1
        xlColumnNumber <- leftMostColumnNumber
        # render the row headings
        if(insertDummyRowHeading) {
          self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=character(0),
                           applyStyles=applyStyles, baseStyleName=rowHeaderStyle, style=NULL, mapFromCss=mapStylesFromCSS)
        }
        else {
          # get the leaf row group, then render any parent data groups that haven't yet been rendered
          rg <- rowGroups[[r]]
          ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
          for(c in (length(ancrgs)-1):1) { # 2 (not 1) since the top ancestor is parentPivot private$rowGroup, which is just a container
            ancg <- ancrgs[[c]]
            xlColumnNumber <- leftMostColumnNumber + length(ancrgs) - c - 1
            if(!ancg$isRendered) {
              rhs <- rowHeaderStyle
              if(!is.null(ancg$baseStyleName)) rhs <- ancg$baseStyleName
              value <- exportValueAs(ncg$sortValue, ncg$caption, exportOptions, blankValue=character(0))
              self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                               applyStyles=applyStyles, baseStyleName=rhs, style=ancg$style, mapFromCss=mapStylesFromCSS,
                               mergeRows=xlRowNumber:(xlRowNumber+length(ancg$leafGroups)-1),
                               mergeColumns=xlColumnNumber)
              ancg$isRendered <- TRUE
            }
          }
        }
        # render the cell values
        for(c in 1:columnCount) {
          xlRowNumber <- topRowNumber + columnGroupLevelCount + r - 1
          xlColumnNumber <- leftMostColumnNumber + rowGroupLevelCount + c - 1
          cell <- private$p_parentPivot$cells$getCell(r, c)
          if(cell$isTotal) cs <- totalStyle
          else cs <- cellStyle
          if(!is.null(cell$baseStyleName)) cs <- cell$baseStyleName
          if(outputValuesAs=="rawValue") value <- cell$rawValue
          else if(outputValuesAs=="formattedValueAsText") value <- cell$formattedValue
          else if(outputValuesAs=="formattedValueAsNumber") {
            value <- suppressWarnings(as.numeric(cell$formattedValue))
            if(!isNumericValue(value)) value <- cell$formattedValue
          }
          else value <- cell$rawValue
          value <- exportValueAs(cell$rawValue, value, exportOptions, blankValue=character(0))
          self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                           applyStyles=applyStyles, baseStyleName=cs, style=cell$style, mapFromCss=mapStylesFromCSS)
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
    p_minimumColumnWidths = NULL
  )
)
