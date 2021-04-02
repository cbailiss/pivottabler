
#' Convert a pivot table style to a basictabler style.
#'
#' \code{convertPvtStyleToBasicStyle} is a utility function that converts a pivot
#' table style to a basictabler style from the basictabler package.
#'
#' @param btbl The basic table that will own the new style.
#' @param pvtStyle The pivot style to convert.
#' @return a basictabler style.

convertPvtStyleToBasicStyle <- function(btbl=NULL, pvtStyle=NULL) {
  if(is.null(pvtStyle)) { return(invisible(NULL)) }
  bStyle <- basictabler::TableStyle$new(btbl, styleName=pvtStyle$name, declarations=pvtStyle$declarations)
  return(invisible(bStyle))
}

#' Get pivot table style declarations from a pivot table style.
#'
#' \code{getPvtStyleDeclarations} is a utility function that reads the styles
#' from a pivot table style.
#'
#' @param pvtStyle The pivot table style to read.
#' @return a list of style declarations.

getPvtStyleDeclarations <- function(pvtStyle=NULL) {
  if(is.null(pvtStyle)) { return(invisible(NULL)) }
  return(invisible(pvtStyle$declarations))
}

#' Convert a pivot table to a basic table.
#'
#' \code{convertPvtTblToBasicTbl} is a utility function that converts a pivot
#' table to a basic table from the basictabler package.
#'
#' @param pvt The pivot table to convert.
#' @param exportOptions Options specifying how values are exported.
#' @param compatibility Compatibility options specified when creating the
#'   basictabler table.
#' @param showRowGroupHeaders Show captions at the top of the columns that
#' comprise the row groups (i.e. in the top left root of then pivot table).
#' @return a basictabler table.

convertPvtTblToBasicTbl <- function(pvt=NULL, exportOptions=NULL, compatibility=NULL, showRowGroupHeaders=FALSE) {
  # pre-reqs
  if (!requireNamespace("basictabler", quietly = TRUE)) {
    stop("convertPvtTblToBasicTbl():  The basictabler package is needed convert a pivot tabler to a basic table.  Please install the basictabler package.", call. = FALSE)
  }
  basictblrversion <- utils::packageDescription("basictabler")$Version
  if(numeric_version(basictblrversion) < numeric_version("0.2.0")) {
    stop("convertPvtTblToBasicTbl():  Version 0.2.0 or above of the basictabler package is needed to convert a pivot table to a basic table.  Please install an updated version of the basictabler package.", call. = FALSE)
  }
  isBasicTblrZeroPt3 <- numeric_version(basictblrversion) >= numeric_version("0.3.0")
  # create the new basic table
  if(isBasicTblrZeroPt3) btbl <- basictabler::BasicTable$new(compatibility=compatibility)
  else btbl <- basictabler::BasicTable$new()
  # copy the styles over from the pivot table
  themeName <- pvt$theme
  if(is.null(themeName)) themeName <- "pivot_theme"
  btStyles <- basictabler::TableStyles$new(parentTable=btbl, themeName=themeName)
  if(!is.null(pvt$styles)) {
    # styles
    if(!is.null(pvt$styles$styles)) {
      for(i in 1:length(pvt$styles$styles)) {
        srcStyle <- pvt$styles$styles[[i]]
        btStyles$addStyle(styleName=srcStyle$name, declarations=srcStyle$declarations)
      }
    }
    # style names
    btStyles$allowExternalStyles <- pvt$styles$allowExternalStyles
    btStyles$tableStyle <- pvt$styles$tableStyle
    btStyles$rootStyle <- pvt$styles$rootStyle
    btStyles$rowHeaderStyle <- pvt$styles$rowHeaderStyle
    btStyles$colHeaderStyle <- pvt$styles$colHeaderStyle
    btStyles$cellStyle <- pvt$styles$cellStyle
    btStyles$totalStyle <- pvt$styles$totalStyle
    # the following don't exist as properties of the btStyles object
    outlineRowHeaderStyle <- pvt$styles$outlineRowHeaderStyle
    outlineColHeaderStyle <- pvt$styles$outlineColHeaderStyle
    outlineCellStyle <- pvt$styles$outlineCellStyle
  }
  btbl$theme <- btStyles
  # get the data groups:  these are the leaf level groups
  rowGroups <- pvt$cells$rowGroups
  columnGroups <- pvt$cells$columnGroups
  # clear the isRendered flags
  clearFlags <- function(dg) {
    dg$isRendered <- FALSE
  }
  lapply(pvt$rowGroup$getDescendantGroups(includeCurrentGroup=TRUE), clearFlags)
  lapply(pvt$columnGroup$getDescendantGroups(includeCurrentGroup=TRUE), clearFlags)
  # get the dimensions of the various parts of the table...
  # ...headings:
  rowGroupLevelCount <- pvt$rowGroup$getLevelCount(includeCurrentLevel=FALSE)
  columnGroupLevelCount <- pvt$columnGroup$getLevelCount(includeCurrentLevel=FALSE)
  # ...cells:
  rowCount <- pvt$cells$rowCount
  columnCount <- pvt$cells$columnCount
  # ... merges
  rowMerges <- pvt$getMerges(axis="row")
  # extend the table
  btbl$cells$extendCells(columnGroupLevelCount + rowCount, rowGroupLevelCount + columnCount)
  # special case of no rows and no columns, return a blank empty table
  if((rowGroupLevelCount==0)&&(columnGroupLevelCount==0)) {
    btbl$cells$setCell(1, 1, cellType="cell", rawValue="(no data)", styleDeclarations=list("text-align"="center", "padding"="6px"))
    return(btbl)
  }
  # there must always be at least one row and one column
  insertDummyRowHeading <- (rowGroupLevelCount==0) & (columnGroupLevelCount > 0)
  insertDummyColumnHeading <- (columnGroupLevelCount==0) & (rowGroupLevelCount > 0)
  # cursors to keep track of current position within the table
  br <- 0 # this increases and does not reset
  bc <- 0 # this is reset to 1 on each new row
  # render the column headings, with a large blank cell at the start over the row headings
  if(insertDummyColumnHeading) {
    # rendering the column headings (special case of no column groups existing)
    if(isBasicTblrZeroPt3) {
      if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
        rowGrpHeaders <- pvt$rowGrpHeaders
        for(c in 1:rowGroupLevelCount) {
          rowGrpHeader <- NULL
          if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
          if(is.null(rowGrpHeader)) {
            btbl$cells$setBlankCell(r=1, c=c, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=1, asNBSP=TRUE)
          }
          else {
            btbl$cells$setCell(r=1, c=c, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=1,
                               rawValue=rowGrpHeader, formattedValue=rowGrpHeader, baseStyleName=btStyles$rootStyle)
          }
        }
      }
      else {
        btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount, asNBSP=TRUE)
      }
      btbl$cells$setBlankCell(r=1, c=2, cellType="columnHeader", visible=TRUE, asNBSP=TRUE)
    }
    else {
      btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount)
      btbl$cells$setBlankCell(r=1, c=2, cellType="columnHeader", visible=TRUE)
    }
    br <- 1
  }
  else {
    # rendering the column headings (normal scenario)
    for(r in 1:columnGroupLevelCount) {
      br <- br + 1
      if(r==1) {
        # generate the large top-left blank cell or cells
        if(isBasicTblrZeroPt3) {
          if((showRowGroupHeaders==TRUE)&&(rowGroupLevelCount>0)) {
            rowGrpHeaders <- pvt$rowGrpHeaders
            for(c in 1:rowGroupLevelCount) {
              rowGrpHeader <- NULL
              if((0<c)&&(c<=length(rowGrpHeaders))) rowGrpHeader <- rowGrpHeaders[[c]]
              if(is.null(rowGrpHeader)) {
                btbl$cells$setBlankCell(r=1, c=c, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=1, asNBSP=TRUE)
              }
              else {
                btbl$cells$setCell(r=1, c=c, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=1,
                                   rawValue=rowGrpHeader, formattedValue=rowGrpHeader, baseStyleName=btStyles$rootStyle)
              }
            }
          }
          else {
            btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount, asNBSP=TRUE)
          }
        }
        else { btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount) }
      }
      bc <- rowGroupLevelCount
      # get the groups at this level
      grps <- pvt$columnGroup$getLevelGroups(level=r)
      for(c in 1:length(grps)) {
        grp <- grps[[c]]
        bc <- bc + 1
        btbl$cells$setCell(r=br, c=bc, cellType="columnHeader", visible=TRUE, colSpan=length(grp$leafGroups),
                           rawValue=ifelse(is.null(grp$sortValue)&&!is.null(grp$caption), grp$caption, grp$sortValue),
                           formattedValue=exportValueAs(grp$sortValue, grp$caption, exportOptions, blankValue=""),
                           baseStyleName=grp$baseStyleName, styleDeclarations=getPvtStyleDeclarations(grp$style))
        bc <- bc + length(grp$leafGroups) - 1
      }
    }
  }
  # render the rows
  for(r in 1:rowCount) {
    bc <- 0
    br <- br + 1
    rowMerge <- rowMerges[[r]]
    rg <- NULL
    # render the row headings
    if(insertDummyRowHeading) {
      bc <- bc + 1
      if(isBasicTblrZeroPt3) { btbl$cells$setBlankCell(ri=br, c=bc, cellType="rowHeader", visible=TRUE, asNBSP=TRUE) }
      else { btbl$cells$setBlankCell(ri=br, c=bc, cellType="rowHeader", visible=TRUE) }
    }
    else {
      # get the leaf row group, then render any parent data groups that haven't yet been rendered
      rg <- rowGroups[[r]]
      ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
      for(c in (length(ancrgs)-1):1) { # start iterating at last item minus 1, which is first visible level in the pivot (level number=1)
        ancg <- ancrgs[[c]]
        bc <- bc + 1
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
        rhs <- NULL
        if(ancg$isOutline) rhs <- outlineRowHeaderStyle
        if(!is.null(ancg$baseStyleName)) rhs <- ancg$baseStyleName
        # render headers
        btbl$cells$setCell(r=br, c=bc, cellType="rowHeader", visible=TRUE, rowSpan=length(ancg$leafGroups), colSpan=rowMrgColumnSpan,
                           rawValue=ifelse(is.null(ancg$sortValue)&&!is.null(ancg$caption), ancg$caption, ancg$sortValue),
                           formattedValue=exportValueAs(ancg$sortValue, ancg$caption, exportOptions, blankValue=""),
                           baseStyleName=rhs, styleDeclarations=getPvtStyleDeclarations(ancg$style))
        ancg$isRendered <- TRUE
        if(lastDataGroupInRow) {
          bc <- rowGroupLevelCount # needed in the event cells are written below
          break
        }
      }
    }
    # render the cell values
    if(!(rowMerge$merge && isTRUE(rowMerge$skipCells))) {
      isOutlineCells <- FALSE
      rgCellBaseStyleName <- NULL
      rgCellStyle <- NULL
      if(!is.null(rg)) {
        isOutlineCells <- rg$isOutline
        rgCellBaseStyleName <- rg$cellBaseStyleName
        rgCellStyle <- rg$cellStyle
      }
      if(rowMerge$mergeCells) {
        # special case of all the cells being merged
        cs <- NULL
        if(!is.null(rgCellBaseStyleName)) cs <-  rgCellBaseStyleName
        else if(isOutlineCells && !is.null(outlineCellStyle)) cs <- outlineCellStyle
        sd <- NULL
        if(!is.null(rgCellStyle)) sd <- rgCellStyle
        bc <- bc + 1
        btbl$cells$setCell(r=br, c=bc, cellType="cell", visible=TRUE,
                           rawValue="", formattedValue="", colSpan=columnCount,
                           baseStyleName=cs, styleDeclarations=getPvtStyleDeclarations(sd))
      }
      else {
        # normal scenario
        for(c in 1:columnCount) {
          cell <- pvt$cells$getCell(r, c)
          cellType <- "cell"
          if(cell$isTotal) cellType <- "total"
          cs <- NULL
          if(!is.null(cell$baseStyleName)) cs <-  cell$baseStyleName
          else if(!is.null(rgCellBaseStyleName)) cs <-  rgCellBaseStyleName
          else if(isOutlineCells && (!is.null(outlineCellStyle))) cs <- outlineCellStyle
          sd <- NULL
          if(!is.null(cell$style)) sd <- cell$style
          else if(!is.null(rgCellStyle)) sd <- rgCellStyle
          bc <- bc + 1
          btbl$cells$setCell(r=br, c=bc, cellType=cellType, visible=TRUE,
                             rawValue=cell$rawValue, formattedValue=exportValueAs(cell$rawValue, cell$formattedValue, exportOptions, blankValue=""),
                             baseStyleName=cs, styleDeclarations=getPvtStyleDeclarations(sd))
        }
      }
    }
  }
  return(invisible(btbl))
}
