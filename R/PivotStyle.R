#' A class that specifies styling.
#'
#' The PivotStyle class specifies the styling for headers and cells in a pivot
#' table.  Styles are specified in the form of Cascading Style Sheet (CSS)
#' name-value pairs.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   define styles.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # PivotStyle objects are normally created indirectly via one of the helper
#' # methods.
#' # For an example, see the PivotStyles class.
#' @field parentPivot Owning pivot table.
#' @field styleName Style unique name.
#' @field declarations CSS style declarations.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new style declaration, specifying the field
#'   values documented above.}
#'
#'   \item{\code{setPropertyValue(property, value)}}{Set a single style
#'   property.}
#'   \item{\code{setPropertyValues(declarations)}}{Set multiple style
#'   properties, where declarations is a list similar to the code example
#'   below.}
#'   \item{\code{getPropertyValue(property)}}{Get the style declarations for a
#'   single property.}
#'   \item{\code{asCSSRule(selector)}}{Get this style definition in the form of
#'   a CSS rule.}
#'   \item{\code{asNamedCSSStyle(styleNamePrefix)}}{Get this style definition in
#'   the form of a named CSS style.}
#'   \item{\code{getCopy(newStyleName)}}{Get a copy of this style.}
#'   \item{\code{asList()}}{Get a list representation of this style.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this style.}
#' }

PivotStyle <- R6::R6Class("PivotStyle",
  public = list(
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
   getPropertyValue = function(property=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "getPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$getPropertyValue", "Getting property value...", list(property=property))
     val <- private$p_declarations[[tolower(trimws(property))]]
     if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyle$getPropertyValue", "Got property value.")
     return(invisible(val))
   },
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
   getCopy = function(newStyleName=NULL) {
     if(private$p_parentPivot$argumentCheckMode > 0) {
       checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyle", "getCopy", newStyleName, missing(newStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     copy <- PivotStyle$new(parentPivot=private$p_parentPivot, styleName=newStyleName, declarations=private$p_declarations)
     return(invisible(copy))
   },
   asList = function() {
     lst <- list(
       name = private$p_name,
       declarations = private$p_declarations
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
    name = function(value) { return(invisible(private$p_name)) },
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
