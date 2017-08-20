#' A class that specifies Excel styling as used by the openxlsx package.
#'
#' The PivotOpenXlsxStyle class specifies the styling for cells in an
#' Excel worksheet.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   define styles.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # PivotOpenXlsxStyle objects are created by the PivotOpenXlsxRenderer class.
#' # See that class for details.
#' @field parentPivot Owning pivot table.
#' @field baseStyleName The name of the base style in the pivot table.
#' @field isBaseStyle TRUE when this style is the equivalent of a named style in
#'   the pivot table, FALSE if this style has additional settings over and above
#'   the base style of the same name.
#' @field fontName The name of the font (single font name, not a CSS style
#'   list).
#' @field fontSize The size of the font (units: point).
#' @field bold TRUE if text is bold.
#' @field italic TRUE if text is italic.
#' @field underline TRUE if text is underlined.
#' @field strikethrough TRUE if text has a line through it.
#' @field superscript TRUE if text is small and raised.
#' @field subscript TRUE if text is small and lowered.
#' @field fillColor The background colour for the cell (as a hex value, e.g.
#'   #00FF00).
#' @field textColor The color of the text (as a hex value).
#' @field hAlign The horizontal alignment of the text:  left, center or right.
#' @field vAlign The vertical alignment of the text:  top, middle or bottom.
#' @field wrapText TRUE if the text is allowed to wrap onto multiple lines.
#' @field textRotation The rotation angle of the text or 255 for vertical.
#' @field border A list (with elements style and color) specifying the border
#'   settings for all four sides of each cell at once.
#' @field borderLeft A list (with elements style and color) specifying the
#'   border settings for the left border of each cell.
#' @field borderRight A list (with elements style and color) specifying the
#'   border settings for the right border of each cell.
#' @field borderTop A list (with elements style and color) specifying the border
#'   settings for the top border of each cell.
#' @field borderBottom A list (with elements style and color) specifying the
#'   border settings for the bottom border of each cell.
#' @field valueFormat The Excel formatting applied to the field value.  One of
#'   the following values: GENERAL, NUMBER, CURRENCY, ACCOUNTING, DATE,
#'   LONGDATE, TIME, PERCENTAGE, FRACTION, SCIENTIFIC, TEXT, COMMA. Or for
#'   dates/datetimes, a combination of d, m, y. Or for numeric values, use 0.00
#'   etc.
#' @field minColumnWidth The minimum width of this column.
#' @field minRowHeight The minimum height of this row.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new Excel style, specifying the field
#'   values documented above.}
#'
#'   \item{\code{isBasicStyleNameMatch(baseStyleName=NULL)}}{Find a matching
#'   base style by name.}
#'   \item{\code{isFullStyleDetailMatch = function(baseStyleName=NULL,
#'   isBaseStyle=NULL, fontName=NULL, fontSize=NULL, bold=NULL, italic=NULL,
#'   underline=NULL, strikethrough=NULL, superscript=NULL, subscript=NULL,
#'   fillColor=NULL, textColor=NULL, hAlign=NULL, vAlign=NULL, wrapText=NULL,
#'   textRotation=NULL, indent=NULL, borderAll=NULL, borderLeft=NULL,
#'   borderRight=NULL, borderTop=NULL, borderBottom=NULL, valueFormat=NULL,
#'   minColumnWidth=NULL, minRowHeight=NULL}}{Find a matching style matching on
#'   all the attributes of the style.}
#'   \item{\code{createOpenXslxStyle()}}{Create the openxlsx style.}
#'   \item{\code{asList()}}{Get a list representation of this style.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this style.}
#'   \item{\code{asString()}}{Get a text representation of this style.}
#' }

PivotOpenXlsxStyle <- R6::R6Class("PivotOpenXlsxStyle",
  public = list(
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
   isBasicStyleNameMatch = function(baseStyleName=NULL) {
     return(private$p_isBaseStyle && (baseStyleName==private$p_baseStyleName))
   },
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
        return(isMatch(baseStyleName, private$p_baseStyleName))
      }
      else {
        return(isMatch(baseStyleName, private$p_baseStyleName) &&
                isMatch(fontName, private$p_fontName) && isMatch(fontSize, private$p_fontSize) &&
                isMatch(bold, private$p_bold) && isMatch(italic, private$p_italic) &&
                isMatch(underline, private$p_underline) && isMatch(strikethrough, private$p_strikethrough) &&
                isMatch(superscript, private$p_superscript) && isMatch(subscript, private$p_subscript) &&
                isMatch(fillColor, private$p_fillColor) && isMatch(textColor, private$p_textColor) &&
                isMatch(hAlign, private$p_hAlign) && isMatch(vAlign, private$p_vAlign) &&
                isMatch(wrapText, private$p_wrapText) && isMatch(textRotation, private$p_textRotation) &&
                isMatch(indent, private$p_indent) &&
                isBorderMatch(borderAll, private$p_borderAll) &&
                isBorderMatch(borderLeft, private$p_borderLeft) && isBorderMatch(borderRight, private$p_borderRight) &&
                isBorderMatch(borderTop, private$p_borderTop) && isBorderMatch(borderBottom, private$p_borderBottom) &&
                isMatch(valueFormat, private$p_valueFormat) && isMatch(aaa, private$p_) &&
                isMatch(minColumnWidth, private$p_minColumnWidth) && isMatch(aaa, private$p_) &&
                isMatch(aaa, private$p_) && isMatch(minRowHeight, private$p_minRowHeight))
      }
    },
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

      # create the style
      private$p_openxlsxStyle <- createStyle(
        fontName=private$p_fontName, fontSize=private$p_fontSize,
        fontColour=private$p_textColor, numFmt=valueFormat,
        border=borderSides, borderColour=borderColors, borderStyle=borderStyles,
        fgFill=private$p_fillColor, halign=private$p_hAlign, valign=vAlign,
        textDecoration=textDecoration, wrapText=private$p_wrapText,
        textRotation=private$p_textRotation, indent=private$p_indent)
    },
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
   asJSON = function() { return(jsonlite::toJSON(asList())) },
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
    baseStyleName = function(value) { return(invisible(private$p_baseStyleName)) },
    isBaseStyle = function(value) { return(invisible(private$p_isBaseStyle)) },
    fontName = function(value) { return(invisible(private$p_fontName)) },
    fontSize = function(value) { return(invisible(private$p_fontSize)) },
    bold = function(value) { return(invisible(private$p_bold)) },
    italic = function(value) { return(invisible(private$p_italic)) },
    underline = function(value) { return(invisible(private$p_underline)) },
    strikethrough = function(value) { return(invisible(private$p_strikethrough)) },
    superscript = function(value) { return(invisible(private$p_superscript)) },
    subscript = function(value) { return(invisible(private$p_subscript)) },
    fillColor = function(value) { return(invisible(private$p_fillColor)) },
    textColor = function(value) { return(invisible(private$p_textColor)) },
    hAlign = function(value) { return(invisible(private$p_hAlign)) },
    vAlign = function(value) { return(invisible(private$p_vAlign)) },
    wrapText = function(value) { return(invisible(private$p_wrapText)) },
    textRotation = function(value) { return(invisible(private$p_textRotation)) },
    indent = function(value) { return(invisible(private$p_indent)) },
    borderAll = function(value) { return(invisible(private$p_borderAll)) },
    borderLeft = function(value) { return(invisible(private$p_borderLeft)) },
    borderRight = function(value) { return(invisible(private$p_borderRight)) },
    borderTop = function(value) { return(invisible(private$p_borderTop)) },
    borderBottom = function(value) { return(invisible(private$p_borderBottom)) },
    valueFormat = function(value) { return(invisible(private$p_valueFormat)) },
    minColumnWidth = function(value) { return(invisible(private$p_minColumnWidth)) },
    minRowHeight = function(value) { return(invisible(private$p_minRowHeight)) },
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
      if(is.na(value1) && is.na(value2)) return(TRUE)
      if(length(value1) != length(value2)) return(FALSE)
      return(value1==value2)
    },
    isBorderMatch = function(border1, border2) {
      if(is.null(border1) && is.null(border2)) return(TRUE)
      return(isMatch(border1[["style"]], border2[["style"]]) &&
             isMatch(border1[["color"]], border2[["color"]]))
    },
    p_openxlsxStyle = NULL
  )
)




