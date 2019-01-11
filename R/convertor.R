
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
#' @return a basictabler table.

convertPvtTblToBasicTbl <- function(pvt=NULL, exportOptions=NULL) {
  # pre-reqs
  if (!requireNamespace("basictabler", quietly = TRUE)) {
    stop("convertPvtTblToBasicTbl():  The basictabler package is needed convert a pivot tabler to a basic table.  Please install the basictabler package.", call. = FALSE)
  }
  basictblrversion <- utils::packageDescription("basictabler")$Version
  if(numeric_version(basictblrversion) < numeric_version("0.1.1.9000")) {
    stop("convertPvtTblToBasicTbl():  Version 0.2.0 or above of the basictabler package is needed to convert a pivot table to a basic table.  Please install an updated version of the basictabler package.", call. = FALSE)
  }
  # create the new basic table
  btbl <- basictabler::BasicTable$new()
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
    btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount)
    btbl$cells$setBlankCell(r=1, c=2, cellType="columnHeader", visible=TRUE)
    br <- 1
  }
  else {
    for(r in 1:columnGroupLevelCount) {
      br <- br + 1
      if(r==1) { # generate the large top-left blank cell
        btbl$cells$setBlankCell(r=1, c=1, cellType="root", visible=TRUE, rowSpan=columnGroupLevelCount, colSpan=rowGroupLevelCount)
      }
      bc <- rowGroupLevelCount
      # get the groups at this level
      grps <- pvt$columnGroup$getLevelGroups(level=r)
      for(c in 1:length(grps)) {
        grp <- grps[[c]]
        bc <- bc + 1
        btbl$cells$setCell(r=br, c=bc, cellType="columnHeader", visible=TRUE, colSpan=length(grp$leafGroups),
                           rawValue=grp$sortValue, formattedValue=exportValueAs(grp$sortValue, grp$caption, exportOptions, blankValue=""),
                           baseStyleName=grp$baseStyleName, styleDeclarations=getPvtStyleDeclarations(grp$style))
        bc <- bc + length(grp$leafGroups) - 1
      }
    }
  }
  # render the rows
  for(r in 1:rowCount) {
    bc <- 0
    br <- br + 1
    # render the row headings
    if(insertDummyRowHeading) {
      bc <- bc + 1
      btbl$cells$setBlankCell(ri=br, c=bc, cellType="rowHeader", visible=TRUE)
    }
    else {
      # get the leaf row group, then render any parent data groups that haven't yet been rendered
      rg <- rowGroups[[r]]
      ancrgs <- rg$getAncestorGroups(includeCurrentGroup=TRUE)
      for(c in (length(ancrgs)-1):1) { # 2 (not 1) since the top ancestor is parentPivot private$rowGroup, which is just a container
        ancg <- ancrgs[[c]]
        bc <- bc + 1
        if(ancg$isRendered==FALSE) {
          btbl$cells$setCell(r=br, c=bc, cellType="rowHeader", visible=TRUE, rowSpan=length(ancg$leafGroups),
                             rawValue=ancg$sortValue, formattedValue=exportValueAs(ancg$sortValue, ancg$caption, exportOptions, blankValue=""),
                             baseStyleName=ancg$baseStyleName, styleDeclarations=getPvtStyleDeclarations(ancg$style))
          ancg$isRendered <- TRUE
        }
      }
    }
    # render the cell values
    for(c in 1:columnCount) {
      cell <- pvt$cells$getCell(r, c)
      cellType <- "cell"
      if(cell$isTotal) cellType <- "total"
      cllstyl <- NULL
      bc <- bc + 1
      btbl$cells$setCell(r=br, c=bc, cellType=cellType, visible=TRUE,
                         rawValue=cell$rawValue, formattedValue=exportValueAs(cell$rawValue, cell$formattedValue, exportOptions, blankValue=""),
                         baseStyleName=cell$baseStyleName, styleDeclarations=getPvtStyleDeclarations(cell$style))
    }
  }
  return(invisible(btbl))
}
