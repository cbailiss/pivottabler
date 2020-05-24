
#' R6 class that specifies Excel styling as used by the openxlsx package.
#'
#' @description
#' The `PivotOpenXlsxStyle` class specifies the styling for cells in an
#' Excel worksheet.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' # PivotOpenXlsxStyle objects are created by the PivotOpenXlsxRenderer class.
#' # See that class for details.

PivotOpenXlsxStyle <- R6::R6Class("PivotOpenXlsxStyle",
  public = list(

    #' @description
    #' Create a new `PivotOpenXlsxStyle` object.
    #' @param parentPivot The pivot table that this `PivotOpenXlsxStyle`
    #' instance belongs to.
    #' @param baseStyleName The name of the base style in the pivot table.
    #' @param isBaseStyle `TRUE` when this style is the equivalent of a named style in
    #' the pivot table, `FALSE` if this style has additional settings over and above
    #' the base style of the same name.
    #' @param fontName The name of the font (single font name, not a CSS style
    #' list).
    #' @param fontSize The size of the font (units: point, 4-72).
    #' @param bold `TRUE` if text is bold.
    #' @param italic `TRUE` if text is italic.
    #' @param underline `TRUE` if text is underlined.
    #' @param strikethrough `TRUE` if text has a line through it.
    #' @param superscript `TRUE` if text is small and raised.
    #' @param subscript `TRUE` if text is small and lowered.
    #' @param fillColor The background colour for the cell (as a hex value, e.g.
    #' #00FF00).
    #' @param textColor The color of the text (as a hex value).
    #' @param hAlign The horizontal alignment of the text:  left, center or right.
    #' @param vAlign The vertical alignment of the text:  top, middle or bottom.
    #' @param wrapText `TRUE` if the text is allowed to wrap onto multiple lines.
    #' @param textRotation The rotation angle of the text (0 to 359) or 255 for
    #' vertical.
    #' @param indent Horizontal indentation of cell contents (0 to 250.).
    #' @param borderAll A list (with elements style and color) specifying the border
    #' settings for all four sides of each cell at once.
    #' @param borderLeft A list (with elements style and color) specifying the
    #' border settings for the left border of each cell.
    #' @param borderRight A list (with elements style and color) specifying the
    #' border settings for the right border of each cell.
    #' @param borderTop A list (with elements style and color) specifying the border
    #' settings for the top border of each cell.
    #' @param borderBottom A list (with elements style and color) specifying the
    #' border settings for the bottom border of each cell.
    #' @param valueFormat The Excel formatting applied to the field value.  One of
    #' the following values: GENERAL, NUMBER, CURRENCY, ACCOUNTING, DATE,
    #' LONGDATE, TIME, PERCENTAGE, FRACTION, SCIENTIFIC, TEXT, COMMA. Or for
    #' dates/datetimes, a combination of d, m, y. Or for numeric values, use
    #' a numeric format code such as 0.00, #,###.00, etc
    #' @param minColumnWidth The minimum width of this column (0 to 255).
    #' @param minRowHeight The minimum height of this row (0 to 400).
    #' @return A new `PivotOpenXlsxStyle` object.
    initialize = function(parentPivot, baseStyleName=NULL, isBaseStyle=NULL,
                         fontName=NULL, fontSize=NULL, bold=NULL,
                         italic=NULL, underline=NULL, strikethrough=NULL,
                         superscript=NULL, subscript=NULL, fillColor=NULL,
                         textColor=NULL, hAlign=NULL, vAlign=NULL, wrapText=NULL,
                         textRotation=NULL, indent=NULL,
                         borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                         borderTop=NULL, borderBottom=NULL, valueFormat=NULL,
                         minColumnWidth=NULL, minRowHeight=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", baseStyleName, missing(baseStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", isBaseStyle, missing(isBaseStyle), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", fontName, missing(fontName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", fontSize, missing(fontSize), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=4, maxValue=72)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", bold, missing(bold), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", italic, missing(italic), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", underline, missing(underline), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", strikethrough, missing(strikethrough), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", superscript, missing(superscript), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", subscript, missing(subscript), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", fillColor, missing(fillColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", textColor, missing(textColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", hAlign, missing(hAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("left", "center", "right"))
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", vAlign, missing(vAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("top", "middle", "bottom"))
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", wrapText, missing(wrapText), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", textRotation, missing(textRotation), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", indent, missing(indent), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=500)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", borderAll, missing(borderAll), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", borderLeft, missing(borderLeft), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", borderRight, missing(borderRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", borderTop, missing(borderTop), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", borderBottom, missing(borderBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", valueFormat, missing(valueFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", minColumnWidth, missing(minColumnWidth), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=255)
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotOpenXlsxStyle", "initialize", minRowHeight, missing(minRowHeight), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=400)
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyle$new", "Creating new Pivot Style...", list())

     if(!is.null(fillColor)) {
       check <- grep("#[0-9A-F]{6}", fillColor)
       if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  fillColor must be in the format #NNNNNN.", call. = FALSE)
     }
     if(!is.null(textColor)) {
       check <- grep("#[0-9A-F]{6}", textColor)
       if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  textColor must be in the format #NNNNNN.", call. = FALSE)
     }

     allowedStyles <- c("none", "thin", "medium", "dashed", "dotted", "thick", "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot")
     if(!is.null(borderAll)) {
       borderStyle <- borderAll[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("PivotOpenXlsxStyle$initialize():  borderAll$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderAll[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  borderAll$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderLeft)) {
       borderStyle <- borderLeft[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("PivotOpenXlsxStyle$initialize():  borderLeft$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderLeft[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  borderLeft$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderRight)) {
       borderStyle <- borderRight[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("PivotOpenXlsxStyle$initialize():  borderRight$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderRight[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  borderRight$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderTop)) {
       borderStyle <- borderTop[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("PivotOpenXlsxStyle$initialize():  borderTop$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderTop[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  borderTop$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderBottom)) {
       borderStyle <- borderBottom[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("PivotOpenXlsxStyle$initialize():  borderBottom$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderBottom[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("PivotOpenXlsxStyle$initialize():  borderAll$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }

     private$p_baseStyleName <- baseStyleName
     private$p_isBaseStyle <- isBaseStyle
     private$p_fontName <- fontName
     private$p_fontSize <- fontSize
     private$p_bold <- bold
     private$p_italic <- italic
     private$p_underline <- underline
     private$p_strikethrough <- strikethrough
     private$p_superscript <- superscript
     private$p_subscript <- subscript
     private$p_fillColor <- fillColor
     private$p_textColor <- textColor
     private$p_hAlign <- hAlign
     private$p_vAlign <- vAlign
     private$p_wrapText <- wrapText
     private$p_textRotation <- textRotation
     private$p_indent <- indent
     private$p_borderAll <- borderAll
     private$p_borderLeft <- borderLeft
     private$p_borderRight <- borderRight
     private$p_borderTop <- borderTop
     private$p_borderBottom <- borderBottom
     private$p_valueFormat <- valueFormat
     private$p_minColumnWidth <- minColumnWidth
     private$p_minRowHeight <- minRowHeight

     self$createOpenXslxStyle()

     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotOpenXlsxStyle$new", "Created new Pivot Style")
   },

   #' @description
   #' Test whether a style is a base style with the specified name.
   #' @param baseStyleName The name of the style to match.
   #' @return No return value.
   isBasicStyleNameMatch = function(baseStyleName=NULL) {
     return(private$p_isBaseStyle && (baseStyleName==private$p_baseStyleName))
   },

   #' @description
   #' Test whether a style has matching style attributes.
   #' @details
   #' Base styles are compared on name only, otherwise the style attributes
   #' are compared.
   #' @param baseStyleName The name of the base style in the pivot table.
   #' @param isBaseStyle `TRUE` when this style is the equivalent of a named style in
   #' the pivot table, `FALSE` if this style has additional settings over and above
   #' the base style of the same name.
   #' @param fontName The name of the font (single font name, not a CSS style
   #' list).
   #' @param fontSize The size of the font (units: point, 4-72).
   #' @param bold `TRUE` if text is bold.
   #' @param italic `TRUE` if text is italic.
   #' @param underline `TRUE` if text is underlined.
   #' @param strikethrough `TRUE` if text has a line through it.
   #' @param superscript `TRUE` if text is small and raised.
   #' @param subscript `TRUE` if text is small and lowered.
   #' @param fillColor The background colour for the cell (as a hex value, e.g.
   #' #00FF00).
   #' @param textColor The color of the text (as a hex value).
   #' @param hAlign The horizontal alignment of the text:  left, center or right.
   #' @param vAlign The vertical alignment of the text:  top, middle or bottom.
   #' @param wrapText `TRUE` if the text is allowed to wrap onto multiple lines.
   #' @param textRotation The rotation angle of the text (0 to 359) or 255 for
   #' vertical.
   #' @param indent Horizontal indentation of cell contents (0 to 250.).
   #' @param borderAll A list (with elements style and color) specifying the border
   #' settings for all four sides of each cell at once.
   #' @param borderLeft A list (with elements style and color) specifying the
   #' border settings for the left border of each cell.
   #' @param borderRight A list (with elements style and color) specifying the
   #' border settings for the right border of each cell.
   #' @param borderTop A list (with elements style and color) specifying the border
   #' settings for the top border of each cell.
   #' @param borderBottom A list (with elements style and color) specifying the
   #' border settings for the bottom border of each cell.
   #' @param valueFormat The Excel formatting applied to the field value.  One of
   #' the following values: GENERAL, NUMBER, CURRENCY, ACCOUNTING, DATE,
   #' LONGDATE, TIME, PERCENTAGE, FRACTION, SCIENTIFIC, TEXT, COMMA. Or for
   #' dates/datetimes, a combination of d, m, y. Or for numeric values, use
   #' a numeric format code such as 0.00, #,###.00, etc
   #' @param minColumnWidth The minimum width of this column (0 to 255).
   #' @param minRowHeight The minimum height of this row (0 to 400).
   #' @return No return value.
   isFullStyleDetailMatch = function(baseStyleName=NULL, isBaseStyle=NULL,
                           fontName=NULL, fontSize=NULL, bold=NULL,
                           italic=NULL, underline=NULL, strikethrough=NULL,
                           superscript=NULL, subscript=NULL, fillColor=NULL,
                           textColor=NULL, hAlign=NULL, vAlign=NULL, wrapText=NULL,
                           textRotation=NULL, indent=NULL,
                           borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                           borderTop=NULL, borderBottom=NULL,
                           valueFormat=NULL,
                           minColumnWidth=NULL, minRowHeight=NULL) {
      if(isBaseStyle && private$p_isBaseStyle) {
        # if this is a base style and the style we are trying to find a match for is also a base style
        # (i.e. with no other additional style settings applied over the top) then just compare the names
        return(private$isMatch(baseStyleName, private$p_baseStyleName))
      }
      else {
        # message(paste0("Match to ", private$p_baseStyleName, " isBaseStyle=", private$p_isBaseStyle))
        # message(paste0("fontName: ", private$isMatch(fontName, private$p_fontName)))
        # message(paste0("fontSize: ", private$isMatch(fontSize, private$p_fontSize)))
        # message(paste0("bold: ", private$isMatch(bold, private$p_bold)))
        # message(paste0("italic: ", private$isMatch(italic, private$p_italic)))
        # message(paste0("underline: ", private$isMatch(underline, private$p_underline)))
        # message(paste0("strikethrough: ", private$isMatch(strikethrough, private$p_strikethrough)))
        # message(paste0("superscript: ", private$isMatch(superscript, private$p_superscript)))
        # message(paste0("subscript: ", private$isMatch(subscript, private$p_subscript)))
        # message(paste0("fillColor: ", private$isMatch(fillColor, private$p_fillColor)))
        # message(paste0("textColor: ", private$isMatch(textColor, private$p_textColor)))
        # message(paste0("hAlign: ", private$isMatch(hAlign, private$p_hAlign)))
        # message(paste0("vAlign: ", private$isMatch(vAlign, private$p_vAlign)))
        # message(paste0("wrapText: ", private$isMatch(wrapText, private$p_wrapText)))
        # message(paste0("textRotation: ", private$isMatch(textRotation, private$p_textRotation)))
        # message(paste0("indent: ", private$isMatch(indent, private$p_indent)))
        # message(paste0("borderAll: ", private$isBorderMatch(borderAll, private$p_borderAll)))
        # message(paste0("borderLeft: ", private$isBorderMatch(borderLeft, private$p_borderLeft)))
        # message(paste0("borderRight: ", private$isBorderMatch(borderRight, private$p_borderRight)))
        # message(paste0("borderTop: ", private$isBorderMatch(borderTop, private$p_borderTop)))
        # message(paste0("borderBottom: ", private$isBorderMatch(borderBottom, private$p_borderBottom)))
        # message(paste0("valueFormat: ", private$isMatch(valueFormat, private$p_valueFormat)))
        # message(paste0("minColumnWidth: ", private$isMatch(minColumnWidth, private$p_minColumnWidth)))
        # message(paste0("minRowHeight: ", private$isMatch(minRowHeight, private$p_minRowHeight)))
        # message("")
        return(private$isMatch(fontName, private$p_fontName) && private$isMatch(fontSize, private$p_fontSize) &&
                private$isMatch(bold, private$p_bold) && private$isMatch(italic, private$p_italic) &&
                private$isMatch(underline, private$p_underline) && private$isMatch(strikethrough, private$p_strikethrough) &&
                private$isMatch(superscript, private$p_superscript) && private$isMatch(subscript, private$p_subscript) &&
                private$isMatch(fillColor, private$p_fillColor) && private$isMatch(textColor, private$p_textColor) &&
                private$isMatch(hAlign, private$p_hAlign) && private$isMatch(vAlign, private$p_vAlign) &&
                private$isMatch(wrapText, private$p_wrapText) && private$isMatch(textRotation, private$p_textRotation) &&
                private$isMatch(indent, private$p_indent) &&
                private$isBorderMatch(borderAll, private$p_borderAll) &&
                private$isBorderMatch(borderLeft, private$p_borderLeft) && private$isBorderMatch(borderRight, private$p_borderRight) &&
                private$isBorderMatch(borderTop, private$p_borderTop) && private$isBorderMatch(borderBottom, private$p_borderBottom) &&
                private$isMatch(valueFormat, private$p_valueFormat) &&
                private$isMatch(minColumnWidth, private$p_minColumnWidth) && private$isMatch(minRowHeight, private$p_minRowHeight))
      }
    },

   #' @description
   #' Create an `openxlsx` style from this style definition.
   #' @return No return value.  Retrieve the style using the `openxlsxStyle` property.
    createOpenXslxStyle = function() {
      # consolidate the borders
      borderSides <- list()
      borderColors <- list()
      borderStyles <- list()
      if((!is.null(private$p_borderAll)) &&
         (isTextValue(private$p_borderAll[["style"]]))){
        borderSides <- list("left", "right", "top", "bottom")
        if(isTextValue(private$p_borderAll[["color"]])) {
          clr <- private$p_borderAll[["color"]]
        }
        else clr <- "#000000"
        borderColors <- list(clr, clr, clr, clr)
        stl <- private$p_borderAll[["style"]]
        borderStyles <- list(stl, stl, stl, stl)
      }
      else {
        if((!is.null(private$p_borderLeft)) &&
           (isTextValue(private$p_borderLeft[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "left"
          if(isTextValue(private$p_borderLeft[["color"]])) {
            clr <- private$p_borderLeft[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderLeft[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderRight)) &&
           (isTextValue(private$p_borderRight[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "right"
          if(isTextValue(private$p_borderRight[["color"]])) {
            clr <- private$p_borderRight[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderRight[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderTop)) &&
           (isTextValue(private$p_borderTop[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "top"
          if(isTextValue(private$p_borderTop[["color"]])) {
            clr <- private$p_borderTop[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderTop[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderBottom)) &&
           (isTextValue(private$p_borderBottom[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "bottom"
          if(isTextValue(private$p_borderBottom[["color"]])) {
            clr <- private$p_borderBottom[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderBottom[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
      }
      borderSides <- unlist(borderSides)
      borderColors <- unlist(borderColors)
      borderStyles <- unlist(borderStyles)

      # consolidate the text decoration
      textDecoration <- list()
      if(private$p_bold) textDecoration[[length(textDecoration)+1]] <- "bold"
      if(private$p_italic) textDecoration[[length(textDecoration)+1]] <- "italic"
      if(private$p_underline) textDecoration[[length(textDecoration)+1]] <- "underline"
      if(private$p_strikethrough) textDecoration[[length(textDecoration)+1]] <- "strikeout"
      textDecoration <- unlist(textDecoration)

      # other values
      valueFormat <- private$p_valueFormat
      if(!isTextValue(valueFormat)) valueFormat <- "GENERAL"
      vAlign <- private$p_vAlign
      if(isTextValue(vAlign)&&(vAlign=="middle")) vAlign <- "center"

      # message(private$p_vAlign)
      # message(class(private$p_vAlign))
      # message(paste0("borderSides= ", paste(borderSides, collapse=",")))
      # message(paste0("borderColors= ", paste(borderColors, collapse=",")))
      # message(paste0("borderStyles= ", paste(borderStyles, collapse=",")))

      # create the style
      private$p_openxlsxStyle <- openxlsx::createStyle(
        fontName=private$p_fontName, fontSize=private$p_fontSize,
        fontColour=private$p_textColor, numFmt=valueFormat,
        border=borderSides, borderColour=borderColors, borderStyle=borderStyles,
        fgFill=private$p_fillColor, halign=private$p_hAlign, valign=vAlign,
        textDecoration=textDecoration, wrapText=private$p_wrapText,
        textRotation=private$p_textRotation, indent=private$p_indent)
    },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       baseStyleName = private$p_baseStyleName,
       isBaseStyle = private$p_isBaseStyle, # TRUE if this style is equivalent to the base style of the same name
       fontName = private$p_fontName,
       fontSize = private$p_fontSize,
       bold = private$p_bold,
       italic = private$p_italic,
       underline = private$p_underline,
       strikethrough = private$p_strikethrough,
       superscript = private$p_superscript,
       subscript = private$p_subscript,
       fillColor = private$p_fillColor,
       textColor = private$p_textColor,
       hAlign = private$p_hAlign,
       vAlign = private$p_vAlign,
       wrapText = private$p_wrapText,
       textRotation = private$p_textRotation,
       indent = private$p_indent,
       borderAll = private$p_borderAll,
       borderLeft = private$p_borderLeft,
       borderRight = private$p_borderRight,
       borderTop = private$p_borderTop,
       borderBottom = private$p_borderBottom,
       valueFormat = private$p_valueFormat,
       minColumnWidth = private$p_minColumnWidth,
       minRowHeight = private$p_minRowHeight
     )
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
   #' @return A character summary of various object properties.
   asString = function() {
     lst <- self$asList()
     if(is.null(lst)||length(lst)==0) return("")
     nms <- names(lst)
     getNvp <- function(i) {
       v <- lst[[i]]
       if((!is.null(v))&&(length(v)>1)) {
         v <- paste0("(", paste(v, collapse=", "), ")")
       }
       paste0(nms[i], "=", v)
     }
     nvp <- sapply(1:length(lst), getNvp)
     return(invisible(paste0("{ ", paste(nvp, collapse=", "), " }")))
   }
  ),
  active = list(

    #' @field baseStyleName The name of the base style in the pivot table.
    baseStyleName = function(value) { return(invisible(private$p_baseStyleName)) },

    #' @field isBaseStyle `TRUE` when this style is the equivalent of a named style in
    #' the pivot table, `FALSE` if this style has additional settings over and above
    #' the base style of the same name.
    isBaseStyle = function(value) { return(invisible(private$p_isBaseStyle)) },

    #' @field fontName The name of the font (single font name, not a CSS style
    #' list).
    fontName = function(value) { return(invisible(private$p_fontName)) },

    #' @field fontSize The size of the font (units: point, 4-72).
    fontSize = function(value) { return(invisible(private$p_fontSize)) },

    #' @field bold `TRUE` if text is bold.
    bold = function(value) { return(invisible(private$p_bold)) },

    #' @field italic `TRUE` if text is italic.
    italic = function(value) { return(invisible(private$p_italic)) },

    #' @field underline `TRUE` if text is underlined.
    underline = function(value) { return(invisible(private$p_underline)) },

    #' @field strikethrough `TRUE` if text has a line through it.
    strikethrough = function(value) { return(invisible(private$p_strikethrough)) },

    #' @field superscript `TRUE` if text is small and raised.
    superscript = function(value) { return(invisible(private$p_superscript)) },

    #' @field subscript `TRUE` if text is small and lowered.
    subscript = function(value) { return(invisible(private$p_subscript)) },

    #' @field fillColor The background colour for the cell (as a hex value, e.g.
    #' #00FF00).
    fillColor = function(value) { return(invisible(private$p_fillColor)) },

    #' @field textColor The color of the text (as a hex value).
    textColor = function(value) { return(invisible(private$p_textColor)) },

    #' @field hAlign The horizontal alignment of the text:  left, center or right.
    hAlign = function(value) { return(invisible(private$p_hAlign)) },

    #' @field vAlign The vertical alignment of the text:  top, middle or bottom.
    vAlign = function(value) { return(invisible(private$p_vAlign)) },

    #' @field wrapText `TRUE` if the text is allowed to wrap onto multiple lines.
    wrapText = function(value) { return(invisible(private$p_wrapText)) },

    #' @field textRotation The rotation angle of the text (0 to 359) or 255 for
    #' vertical.
    textRotation = function(value) { return(invisible(private$p_textRotation)) },

    #' @field indent Horizontal indentation of cell contents (0 to 250.).
    indent = function(value) { return(invisible(private$p_indent)) },

    #' @field borderAll A list (with elements style and color) specifying the border
    #' settings for all four sides of each cell at once.
    borderAll = function(value) { return(invisible(private$p_borderAll)) },

    #' @field borderLeft A list (with elements style and color) specifying the
    #' border settings for the left border of each cell.
    borderLeft = function(value) { return(invisible(private$p_borderLeft)) },

    #' @field borderRight A list (with elements style and color) specifying the
    #' border settings for the right border of each cell.
    borderRight = function(value) { return(invisible(private$p_borderRight)) },

    #' @field borderTop A list (with elements style and color) specifying the border
    #' settings for the top border of each cell.
    borderTop = function(value) { return(invisible(private$p_borderTop)) },

    #' @field borderBottom A list (with elements style and color) specifying the
    #' border settings for the bottom border of each cell.
    borderBottom = function(value) { return(invisible(private$p_borderBottom)) },

    #' @field valueFormat The Excel formatting applied to the field value.  One of
    #' the following values: GENERAL, NUMBER, CURRENCY, ACCOUNTING, DATE,
    #' LONGDATE, TIME, PERCENTAGE, FRACTION, SCIENTIFIC, TEXT, COMMA. Or for
    #' dates/datetimes, a combination of d, m, y. Or for numeric values, use
    #' a numeric format code such as 0.00, #,###.00, etc
    valueFormat = function(value) { return(invisible(private$p_valueFormat)) },

    #' @field minColumnWidth The minimum width of this column (0 to 255).
    minColumnWidth = function(value) { return(invisible(private$p_minColumnWidth)) },

    #' @field minRowHeight The minimum height of this row (0 to 400).
    minRowHeight = function(value) { return(invisible(private$p_minRowHeight)) },

    #' @field openxlsxStyle The style object returned from `openxlsx::createStyle()`.
    openxlsxStyle = function(value) { return(invisible(private$p_openxlsxStyle)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_baseStyleName = NULL,
    p_isBaseStyle = NULL, # TRUE if this style is equivalent to the base style of the same name
    p_fontName = NULL,
    p_fontSize = NULL,
    p_bold = NULL,
    p_italic = NULL,
    p_underline = NULL,
    p_strikethrough = NULL,
    p_superscript = NULL,
    p_subscript = NULL,
    p_fillColor = NULL,
    p_textColor = NULL,
    p_hAlign = NULL,
    p_vAlign = NULL,
    p_wrapText = NULL,
    p_textRotation = NULL,
    p_indent = NULL,
    p_borderAll = NULL,
    p_borderLeft = NULL,
    p_borderRight = NULL,
    p_borderTop = NULL,
    p_borderBottom = NULL,
    p_valueFormat = NULL,
    p_minColumnWidth = NULL,
    p_minRowHeight = NULL,
    isMatch = function(value1, value2) {
      if(is.null(value1) && is.null(value2)) return(TRUE)
      if(is.null(value1)) return(FALSE)
      if(is.null(value2)) return(FALSE)
      if(is.na(value1) && is.na(value2)) return(TRUE)
      if(is.na(value1)) return(FALSE)
      if(is.na(value2)) return(FALSE)
      if(length(value1) != length(value2)) return(FALSE)
      return(value1==value2)
    },
    isBorderMatch = function(border1, border2) {
      if(is.null(border1) && is.null(border2)) return(TRUE)
      return(private$isMatch(border1[["style"]], border2[["style"]]) &&
             private$isMatch(border1[["color"]], border2[["color"]]))
    },
    p_openxlsxStyle = NULL
  )
)




