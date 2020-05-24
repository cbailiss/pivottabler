
#' R6 class that defines a collection of Excel styles as used by the openxlsx
#' package.
#'
#' @description
#' The `PivotOpenXlsxStyles` class stores a collection of `PivotOpenXlsxStyle`
#' style objects.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotOpenXlsxStyles <- R6::R6Class("PivotOpenXlsxStyles",
  public = list(

    #' @description
    #' Create a new `PivotOpenXlsxStyles` object.
    #' @param parentPivot The pivot table that this `PivotOpenXlsxStyles`
    #' instance belongs to.
    #' @return A new `PivotOpenXlsxStyles` object.
    initialize = function(parentPivot) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$new", "Creating new Pivot OpenXlsx Styles...")
      private$p_styles <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$new", "Created new Pivot OpenXlsx Styles.")
    },

    #' @description
    #' Clear the internal list of styles.
    #' @return No return value.
    clearStyles = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) p
      private$p_parentPivot$trace("PivotOpenXlsxStyles$clearStyles", "Clearing styles...")
      private$p_styles <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$clearStyles", "Cleared styles.")
    },

    #' @description
    #' Find an existing openxlsx style matching the name of a base style.
    #' @param baseStyleName The name of the base style to find.
    #' @return A `PivotOpenXlsxStyle` object with the specified name.
    findNamedStyle = function(baseStyleName) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "initialize", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$findNamedStyle", "Finding named style...")

      # This function is a slimmer version of the full function below
      matchedStyle <- NULL
      if(length(private$p_styles)>0) {
        for(i in 1:length(private$p_styles))
          if(private$p_styles[[i]]$isBasicStyleNameMatch(baseStyleName)) {
            matchedStyle <- private$p_styles[[i]]
            break
          }
      }

      # finished
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$findNamedStyle", "Found named style.")
      return(invisible(matchedStyle))
    },

    #' @description
    #' Find an existing openxlsx style, add a new openxlsx style matching a base
    #' style and/or existing `PivotStyle` object.
    #' @details
    #' This function is used in two different ways:
    #' (1) When adding base styles (i.e. named styles in the pivot table) to
    #' this `PivotOpenXlsxStyles` collection:
    #' In this case, `baseStyleName` is the name of the style and `isBaseStyle=TRUE`
    #' (so matching is by name only) and `style` is the `PivotStyle` object for the
    #' base style.
    #' (2) When finding styles that have been applied to individual cells using the
    #' `PivotStyle` object that is attached to each cell:
    #' In this case, `baseStyleName` may or may not be present, `isBaseStyle=FALSE`
    #' and `style` is the `PivotStyle` object from the cell.
    #' @param action Must be one of "find" (to search for an existing style), "add"
    #' (to add a new style) or "findOrAdd" (default, to first search for an existing
    #' style, and if no match is found then add a new style)
    #' @param baseStyleName The name of the base style to find.
    #' @param isBaseStyle `TRUE` if the style being sought is a base style.
    #' @param style An existing `PivotStyle` object.
    #' @param mapFromCss Default `TRUE`, to create a new `PivotOpenXlsxStyle` by
    #' mapping from CSS style declarations.
    #' @return A `PivotOpenXlsxStyle` object that has been found or added.
    findOrAddStyle = function(action="findOrAdd", baseStyleName=NULL, isBaseStyle=NULL, style=NULL, mapFromCss=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "findOrAddStyle", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("find", "add", "findOrAdd"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "findOrAddStyle", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "findOrAddStyle", isBaseStyle, missing(isBaseStyle), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "findOrAddStyle", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotStyle")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "findOrAddStyle", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$findOrAddStyle", "Finding and/or adding style...")

      # font name
      fontName <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-font-name"))
      if(mapFromCss&&(!isTextValue(xlStyleValue))) {
        fontFamilyList <- style$getPropertyValue("font-family")
        if(isTextValue(fontFamilyList)) {
          fontFamilyList <- cleanCssValue(parseCssString(fontFamilyList))
          if(length(fontFamilyList)>0) xlStyleValue <- trimws(fontFamilyList[1])
        }
      }
      if(isTextValue(xlStyleValue)) fontName <- xlStyleValue

      # font size
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-font-size"))
      if(isTextValue(xlStyleValue)) xlStyleValue <- suppressWarnings(max(min(as.numeric(xlStyleValue), 72), 4))
      if(is.null(xlStyleValue)||is.na(xlStyleValue)) xlStyleValue <- NULL
      if(mapFromCss&&(!isTextValue(xlStyleValue))) {
        fontSize <- style$getPropertyValue("font-size")
        xlStyleValue <- parseCssSizeToPt(fontSize)
      }
      if(!isNumericValue(xlStyleValue)) fontSize <- 11
      else fontSize <- xlStyleValue

      # bold
      bold <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-bold"))
      if(isTextValue(xlStyleValue)) {
        if(tolower(xlStyleValue)=="bold") bold <- TRUE
        else bold <- FALSE
      }
      else if(mapFromCss) {
        fontWeight <- cleanCssValue(style$getPropertyValue("font-weight"))
        if(isTextValue(fontWeight)) {
          if(tolower(fontWeight) %in% c("bold", "bolder")) bold <- TRUE
          else {
            fontWeight <- suppressWarnings(as.numeric(fontWeight))
            if((isNumericValue(fontWeight))&&(fontWeight>=600)) bold <- TRUE
          }
        }
      }

      # italic
      italic <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-italic"))
      if(isTextValue(xlStyleValue)) {
        if(tolower(xlStyleValue)=="italic") italic <- TRUE
        else italic <- FALSE
      }
      else if(mapFromCss) {
        fontStyle <- cleanCssValue(style$getPropertyValue("font-style"))
        if(isTextValue(fontStyle)&&(tolower(fontStyle) %in% c("italic", "oblique"))) italic <- TRUE
      }

      # underline
      underline <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-underline"))
      if(isTextValue(xlStyleValue)) {
        if(tolower(xlStyleValue)=="underline") underline <- TRUE
        else underline <- FALSE
      }
      else if(mapFromCss) {
        textDecoration <- cleanCssValue(style$getPropertyValue("text-decoration"))
        if(isTextValue(textDecoration)&&(tolower(textDecoration)=="underline")) underline <- TRUE
      }

      # strikethrough
      strikethrough <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-strikethrough"))
      if(isTextValue(xlStyleValue)) {
        if(tolower(xlStyleValue)=="strikethrough") strikethrough <- TRUE
        else strikethrough <- FALSE
      }
      else if(mapFromCss) {
        textDecoration <- cleanCssValue(style$getPropertyValue("text-decoration"))
        if(isTextValue(textDecoration)&&(tolower(textDecoration)=="line-through")) strikethrough <- TRUE
      }

      # superscript
      superscript <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-superscript"))
      if(isTextValue(xlStyleValue)&&(tolower(xlStyleValue)=="superscript")) superscript <- TRUE

      # subscript
      subscript <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-subscript"))
      if(isTextValue(xlStyleValue)&&(tolower(xlStyleValue)=="subscript")) subscript <- TRUE

      # fill color
      fillColor <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-fill-color"))
      check <- grep("#[0-9A-F]{6}", xlStyleValue)
      if((length(check)==0)||(check==FALSE)) xlStyleValue <- NULL
      if(mapFromCss && is.null(xlStyleValue)) {
        backgroundColor <- style$getPropertyValue("background-color")
        xlStyleValue <- parseColor(backgroundColor)
      }
      if(isTextValue(xlStyleValue)) fillColor <- xlStyleValue

      # text color
      textColor <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-text-color"))
      check <- grep("#[0-9A-F]{6}", xlStyleValue)
      if((length(check)==0)||(check==FALSE)) xlStyleValue <- NULL
      if(mapFromCss && is.null(xlStyleValue)) {
        color <- style$getPropertyValue("color")
        xlStyleValue <- parseColor(color)
      }
      if(isTextValue(xlStyleValue)) textColor <- xlStyleValue

      # horizontal alignment
      hAlign <- NULL
      xlStyleValue <- tolower(cleanCssValue(style$getPropertyValue("xl-h-align")))
      if(isTextValue(xlStyleValue)) {
        if(xlStyleValue=="left") hAlign <- "left"
        else if(xlStyleValue=="center") hAlign <- "center"
        else if(xlStyleValue=="right") hAlign <- "right"
      }
      else if(mapFromCss) {
        textAlign <- tolower(cleanCssValue(style$getPropertyValue("text-align")))
        if(isTextValue(textAlign)) {
          if(textAlign=="left") hAlign <- "left"
          else if(textAlign=="center") hAlign <- "center"
          else if(textAlign=="right") hAlign <- "right"
        }
      }

      # vertical alignment
      vAlign <- NULL
      xlStyleValue <- tolower(cleanCssValue(style$getPropertyValue("xl-v-align")))
      if(isTextValue(xlStyleValue)) {
        if(xlStyleValue=="top") vAlign <- "top"
        else if(xlStyleValue=="middle") vAlign <- "middle"
        else if(xlStyleValue=="bottom") vAlign <- "bottom"
      }
      else if(mapFromCss) {
        verticalAlign <- tolower(cleanCssValue(style$getPropertyValue("vertical-align")))
        if(isTextValue(verticalAlign)) {
          if(verticalAlign=="top") vAlign <- "top"
          else if(verticalAlign=="top") vAlign <- "text-top"
          else if(verticalAlign=="middle") vAlign <- "middle"
          else if(verticalAlign=="bottom") vAlign <- "text-bottom"
          else if(verticalAlign=="bottom") vAlign <- "bottom"
        }
      }

      # wrap text
      wrapText <- FALSE
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-wrap-text"))
      if(isTextValue(xlStyleValue)&&(tolower(xlStyleValue)=="wrap")) wrapText <- TRUE

      # text rotation
      textRotation <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-text-rotation"))
      if(isTextValue(xlStyleValue)) xlStyleValue <- suppressWarnings(max(min(as.numeric(xlStyleValue), 359), 0))
      if(isNumericValue(xlStyleValue)) textRotation <- xlStyleValue

      # indent
      indent <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-indent"))
      if(isTextValue(xlStyleValue)) xlStyleValue <- suppressWarnings(max(min(as.numeric(xlStyleValue), 500), 0))
      if(isNumericValue(xlStyleValue)&&(xlStyleValue>0)) indent <- xlStyleValue

      # border all
      borderAll <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-border"))
      if(isTextValue(xlStyleValue)) borderAll <- parseXlBorder(xlStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border")
        if(isTextValue(cssBorder)) cssBorder <- getXlBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderAll <- cssBorder
      }

      # border left
      borderLeft <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-border-left"))
      if(isTextValue(xlStyleValue)) borderLeft <- parseXlBorder(xlStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-left")
        if(isTextValue(cssBorder)) cssBorder <- getXlBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderLeft <- cssBorder
      }

      # border right
      borderRight <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-border-right"))
      if(isTextValue(xlStyleValue)) borderRight <- parseXlBorder(xlStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-right")
        if(isTextValue(cssBorder)) cssBorder <- getXlBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderRight <- cssBorder
      }

      # border top
      borderTop <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-border-top"))
      if(isTextValue(xlStyleValue)) borderTop <- parseXlBorder(xlStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-top")
        if(isTextValue(cssBorder)) cssBorder <- getXlBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderTop <- cssBorder
      }

      # border bottom
      borderBottom <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-border-bottom"))
      if(isTextValue(xlStyleValue)) borderBottom <- parseXlBorder(xlStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-bottom")
        if(isTextValue(cssBorder)) cssBorder <- getXlBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderBottom <- cssBorder
      }

      # value format
      valueFormat <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-value-format"))
      if(isTextValue(xlStyleValue)) valueFormat <- xlStyleValue

      # minimum column width
      minColumnWidth <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-min-column-width"))
      if(isTextValue(xlStyleValue)) xlStyleValue <- suppressWarnings(max(min(as.numeric(xlStyleValue), 255), 0))
      if(isNumericValue(xlStyleValue)) minColumnWidth <- xlStyleValue

      # minimum row height
      minRowHeight <- NULL
      xlStyleValue <- cleanCssValue(style$getPropertyValue("xl-min-row-height"))
      if(isTextValue(xlStyleValue)) xlStyleValue <- suppressWarnings(max(min(as.numeric(xlStyleValue), 400), 0))
      if(isNumericValue(xlStyleValue)) minRowHeight <- xlStyleValue

      # find a matching style?
      matchedStyle <- NULL
      if(action %in% c("find", "findOrAdd")) {
        if(isBaseStyle) {
          if(length(private$p_styles)>0) {
            for(i in 1:length(private$p_styles))
              if(private$p_styles[[i]]$isBasicStyleNameMatch(baseStyleName)) {
                matchedStyle <- private$p_styles[[i]]
                break
              }
          }
        }
        else {
          if(length(private$p_styles)>0) {
            for(i in 1:length(private$p_styles))
              if(private$p_styles[[i]]$isFullStyleDetailMatch(baseStyleName=baseStyleName, isBaseStyle=isBaseStyle,
                         fontName=fontName, fontSize=fontSize, bold=bold,
                         italic=italic, underline=underline, strikethrough=strikethrough,
                         superscript=superscript, subscript=subscript, fillColor=fillColor,
                         textColor=textColor, hAlign=hAlign, vAlign=vAlign, wrapText=wrapText,
                         textRotation=textRotation, indent=indent,
                         borderAll=borderAll, borderLeft=borderLeft, borderRight=borderRight,
                         borderTop=borderTop, borderBottom=borderBottom,
                         valueFormat=valueFormat,
                         minColumnWidth=minColumnWidth, minRowHeight=minRowHeight)) {
                matchedStyle <- private$p_styles[[i]]
                break
              }
          }
        }
      }

      # create the new style?
      if(is.null(matchedStyle) && (action %in% c("add", "findOrAdd"))) {
        matchedStyle <- PivotOpenXlsxStyle$new(private$p_parentPivot,
                                        baseStyleName=baseStyleName, isBaseStyle=isBaseStyle,
                                        fontName=fontName, fontSize=fontSize,
                                        bold=bold, italic=italic, underline=underline,
                                        strikethrough=strikethrough, superscript=superscript,
                                        subscript=subscript, fillColor=fillColor, textColor=textColor,
                                        hAlign=hAlign, vAlign=vAlign, wrapText=wrapText,
                                        textRotation=textRotation, indent=indent,
                                        borderAll=borderAll,
                                        borderLeft=borderLeft, borderRight=borderRight,
                                        borderTop=borderTop, borderBottom=borderBottom,
                                        valueFormat=valueFormat,
                                        minColumnWidth=minColumnWidth, minRowHeight=minRowHeight)
        private$p_styles[[length(private$p_styles)+1]] <- matchedStyle
      }

      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$findOrAddStyle", "Found and/or added style.")
      return(matchedStyle)
    },

    #' @description
    #' Populate the OpenXlsx styles based on the styles defined in the pivot table.
    #' @param mapFromCss Default `TRUE`, to create a new `PivotOpenXlsxStyle` by
    #' mapping from CSS style declarations.
    #' @return No return value.
    addNamedStyles = function(mapFromCss=TRUE) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "addNamedStyles", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }

      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$addNamedStyles", "Adding named styles...")
      for(i in 1:length(private$p_parentPivot$styles$styles)) {
        style <- private$p_parentPivot$styles$styles[[i]]
        if(!is.null(self$findNamedStyle(baseStyleName=style$name))) next
        openxlsxStyle <- self$findOrAddStyle(action="add", baseStyleName=style$name, isBaseStyle=TRUE, style=style, mapFromCss=mapFromCss)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyles$addNamedStyles", "Added named styles.")
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      if(length(private$p_styles) > 0) {
        for (i in 1:length(private$p_styles)) {
          lst[[i]] = private$p_styles[[i]]$asList()
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
    #' multiple styles.
    #' @return A character summary of various object properties.
    asString = function(seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyles", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       }
      cstr <- ""
       if(length(private$p_styles)>0) {
         for(i in 1:length(private$p_styles)) {
           cg <- private$p_styles[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           cstr <- paste0(cstr, sep, cg$asString())
         }
       }
       return(cstr)
    }
  ),
  active = list(

    #' @field count The number of `PivotOpenXlsxStyle` objects in this
    #' `PivotOpenXlsxStyles` collection.
    count = function(value) { return(invisible(length(private$p_styles))) },

    #' @field styles A list containing the `PivotOpenXlsxStyle` objects in this
    #' `PivotOpenXlsxStyles` collection.
    styles = function(value) { return(invisible(private$p_styles)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_styles = NULL
  )
)
