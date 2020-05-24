
#' R6 class that specifies styling.
#'
#' @description
#' The `PivotStyle` class specifies the styling for headers and cells in a pivot
#' table.  Styles are specified in the form of Cascading Style Sheet (CSS)
#' name-value pairs.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotStyle <- R6::R6Class("PivotStyle",
  public = list(

   #' @description
   #' Create a new `PivotStyle` object.
   #' @param parentPivot The pivot table that this `PivotStyle`
   #' instance belongs to.
   #' @param styleName The name of the style (for a named style).
   #' @param declarations CSS style declarations in the form of a list, e.g.
   #' `list("font-weight"="bold", "color"="#0000FF")`
   #' @return A new `PivotStyle` object.
   initialize = function(parentPivot, styleName=NULL, declarations= NULL) { # declarations = list(font="...", color="...")
     if(parentPivot$argumentCheckMode > 0) {
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyle", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyle", "initialize", styleName, missing(styleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyle", "initialize", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     private$p_parentPivot <- parentPivot
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$new", "Creating new Pivot Style...", list())
     if((!is.null(declarations))&&(length(declarations)!=length(names(declarations))))
       stop("PivotStyle$initialize(): One or more style declarations are missing a name.", call. = FALSE)
     private$p_declarations <- list()
     private$p_name <- styleName
     if(!is.null(declarations)) {
       if(length(declarations)>0) {
         nms <- names(declarations)
         for(i in 1:length(declarations)) {
            nme <- nms[i]
            val <- declarations[[i]]
            if(is.null(nme)) next
            if(is.null(val)) next
            private$p_declarations[[tolower(trimws(nme))]] <- as.character(val)
         }
       }
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$new", "Created new Pivot Style")
   },

   #' @description
   #' Set a single style property.
   #' @param property The name of the style property to set, e.g. "font-weight".
   #' @param value The value of the style property to set, e.g. "bold".
   #' @return No return value.
   setPropertyValue = function(property=NULL, value=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "setPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "setPropertyValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "integer", "numeric"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$setPropertyValue", "Setting property value...", list(property=property, value=value))
     private$p_declarations[[tolower(trimws(property))]] <- value
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$setPropertyValue", "Set property value.")
     return(invisible())
   },

   #' @description
   #' Set multiple style properties.
   #' @param declarations CSS style declarations in the form of a list, e.g.
   #' `list("font-weight"="bold", "color"="#0000FF")`
   #' @return No return value.
   setPropertyValues = function(declarations=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "setPropertyValues", declarations, missing(declarations), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$setPropertyValues", "Setting property values...")
     nms <- names(declarations)
     if(length(nms)==0) return(invisible())
     for(i in 1:length(nms)) {
       property <- nms[i]
       if(is.null(property)) stop("PivotStyle$setPropertyValues():  NULL style property encountered.")
       value <- declarations[[i]]
       private$p_declarations[[tolower(trimws(property))]] <- value
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$setPropertyValues", "Set property values.")
     return(invisible())
   },

   #' @description
   #' Get the value of a single style property.
   #' @param property The name of the style property to set, e.g. "font-weight".
   #' @return The value of the style property.
   getPropertyValue = function(property=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "getPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$getPropertyValue", "Getting property value...", list(property=property))
     val <- private$p_declarations[[tolower(trimws(property))]]
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$getPropertyValue", "Got property value.")
     return(invisible(val))
   },

   #' @description
   #' Get the style definition in the form of a CSS rule.
   #' @param selector A CSS selector, used to select the element(s) to be styled.
   #' @return The style declarations in the form of a CSS rule,
   #' i.e. selector { property-name1: property-value1,
   #' property-name2: property-value2, ... }
   #' e.g. div { font-weight: bold, color: #0000FF }
   asCSSRule = function(selector=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$asCSSRule", "Getting CSS rule...")
     cssRule <- NULL
     if(is.null(private$p_declarations)||(length(private$p_declarations)==0)) stop("PivotStyle$asCSSRule(): Encountered an empty style declaration.", call. = FALSE)
     if(!is.null(selector)) cssRule <- paste0(selector, " {")
     nms <- names(private$p_declarations)
     for(i in 1:length(private$p_declarations)) {
       if(is.null(nms[i])||is.na(nms[i])||(nchar(nms[i])==0)) stop("PivotStyle$asCSSRule(): Encountered a style declaration without a name.", call. = FALSE)
       if(startsWith(tolower(nms[i]), "xl-")) next # exclude Excel declarations from CSS/HTML output
       cssRule <- paste0(cssRule, nms[i], ": ", private$p_declarations[[i]], "; ")
     }
     if(!is.null(selector)) cssRule <- paste0(cssRule, "}")
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$asCSSRule", "Got CSS rule.")
     return(invisible(cssRule))
   },

   #' @description
   #' Get the style definition in the form of a named CSS style.
   #' @param styleNamePrefix A prefix to prepend to the style name.
   #' @return The style declarations in the form of named CSS style,
   #' i.e. .prefix-stylename { property-name1: property-value1,
   #' property-name2: property-value2, ... }
   #' e.g. .pvt1Cell { font-weight: bold, color: #0000FF }
   asNamedCSSStyle = function(styleNamePrefix=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "asNamedCSSStyle", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$asCSSRule", "Getting named CSS rule...")
     if(is.null(styleNamePrefix)) { selector <- paste0(".", private$p_name) }
     else { selector <- paste0(".", styleNamePrefix, private$p_name) }
     cssRule <- self$asCSSRule(selector=selector)
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$asNamedCSSStyle", "Got named CSS rule.")
     return(invisible(cssRule))
   },

   #' @description
   #' Create a copy of this `PivotStyle` object.
   #' @param newStyleName The name of the new style.
   #' @return A `PivotStyle` object.
   getCopy = function(newStyleName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "getCopy", newStyleName, missing(newStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     copy <- PivotStyle$new(parentPivot=private$p_parentPivot, styleName=newStyleName, declarations=private$p_declarations)
     return(invisible(copy))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       name = private$p_name,
       declarations = private$p_declarations
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
   }
  ),
  active = list(

    #' @field name The name of the style (for a named style).
    name = function(value) { return(invisible(private$p_name)) },

    #' @field declarations A list containing the style declarations.
    declarations = function(value) { return(invisible(private$p_declarations)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_declarations = NULL
  )
)

# Examples:

# Fonts:
# https://www.w3schools.com/css/css_font.asp
# font-family: "Times New Roman", Times, serif;
# font-style: normal / italic / oblique;
# font-size: 100% / 40px / 0.875em; * 16px=1em, 14px/16=0.875em */
# font-weight: normal / bold;
# font-variant: normal / small-caps;

# Text:
# https://www.w3schools.com/css/css_text.asp
# https://www.w3schools.com/cssref/css_colors_legal.asp
# https://www.w3schools.com/colors/colors_names.asp
# https://www.w3schools.com/cssref/pr_pos_vertical-align.asp
# color: red / #ff0000 / rgb(255,0,0) / rgba(255, 0, 0, 0.3) / hsl(120, 100%, 50%) / hsla(120, 100%, 50%, 0.3)
# text-align: left / center / right / justify
# vertical-align: baseline / length / sub / super / top / text-top / middle / bottom / text-bottom / initial / inherit
# text-decoration: none / underline / line-through / overline
# text-transform: uppercase / lowercase / capitalize
# text-indent: 50px
# letter-spacing: 3px
# line-height: 0.8;
# word-spacing: -5px / 10px;
# text-shadow: 3px 2px red;

# Vertical Text: (probably needs some experimenting to get working)
# https://css-tricks.com/rotated-table-column-headers/
# https://davidwalsh.name/css-vertical-text
# https://css-tricks.com/snippets/css/text-rotation/
# transform: translate(25px, 51px);
# transform: rotate(315deg); rotate(90deg);
# transform-origin: left top 0;
# float: left;  # emulates auto-width

# Background:
# https://www.w3schools.com/css/css_background.asp
# background-color: lightblue;

# Cell spacing, cell padding:
# http://stackoverflow.com/questions/339923/set-cellpadding-and-cellspacing-in-css
# https://www.w3schools.com/css/css_table.asp
# Cell padding:  td {padding: 6px;}
# Cell spacing:  table {border-spacing: 2px;}
# Cell borders:  table th td {border: 1px solid black;}
# Min width:  td {min-width: 100px;}
# Width:  td {width: 100px / 10% / ...}
# Max width:  td {max-width: 100px;}
# Height:  Same three properties: min-height, height, max-height
# Horizontal dividers:  border-bottom: 1px solid #ddd;
# Hoverable table: tr:hover {background-color: #f5f5f5}
# Striped table: tr:nth-child(even) {background-color: #f2f2f2}
