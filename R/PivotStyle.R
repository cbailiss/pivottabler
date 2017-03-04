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


PivotStyle <- R6::R6Class("PivotStyle",
  public = list(
   initialize = function(parentPivot, styleName=NULL, declarations= NULL) { # declarations = list(font="...", color="...")
     checkArgument("PivotStyle", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
     checkArgument("PivotStyle", "initialize", styleName, missing(styleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotStyle", "initialize", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
     private$p_parentPivot <- parentPivot
     private$p_parentPivot$message("PivotStyle$new", "Creating new Pivot Style...", list())
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
            private$p_declarations[[nme]] <- val
         }
       }
     }
     private$p_parentPivot$message("PivotStyle$new", "Created new Pivot Style")
   },
   setPropertyValue = function(property=NULL, value=NULL) {
     checkArgument("PivotStyle", "setPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotStyle", "setPropertyValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotStyle$setPropertyValue", "Setting property value...", list(property=property, value=value))
     private$p_declarations[[property]] <- value
     private$p_parentPivot$message("PivotStyle$setPropertyValue", "Set property value.")
     return(invisible())
   },
   getPropertyValue = function(property=NULL) {
     checkArgument("PivotStyle", "getPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     private$p_parentPivot$message("PivotStyle$getPropertyValue", "Getting property value...", list(property=property))
     val <- private$p_declarations[[property]]
     private$p_parentPivot$message("PivotStyle$getPropertyValue", "Got property value.")
     return(invisible(val))
   },
   asCSSRule = function(selector=NULL) {
     checkArgument("PivotStyle", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotStyle$asCSSRule", "Getting CSS rule...")
     cssRule <- NULL
     if(!is.null(selector)) cssRule <- paste0(selector, " {")
     nms <- names(private$p_declarations)
     for(i in 1:length(private$p_declarations)) {
       if(length(nms[i])==0) stop("PivotStyle$asCSSRule(): Encountered a style declaration without a name.", call. = FALSE)
       cssRule <- paste0(cssRule, nms[i], ": ", private$p_declarations[[i]], "; ")
     }
     if(!is.null(selector)) cssRule <- paste0(cssRule, "}")
     private$p_parentPivot$message("PivotStyle$asCSSRule", "Got CSS rule.")
     return(invisible(cssRule))
   },
   asNamedCSSStyle = function(stylePrefix=NULL) {
     checkArgument("PivotStyle", "asNamedCSSStyle", stylePrefix, missing(stylePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     private$p_parentPivot$message("PivotStyle$asCSSRule", "Getting named CSS rule...")
     if(is.null(stylePrefix)) { selector <- paste0(".", private$p_name) }
     else { selector <- paste0(".", stylePrefix, private$p_name) }
     cssRule <- self$asCSSRule(selector=selector)
     private$p_parentPivot$message("PivotStyle$asNamedCSSStyle", "Got named CSS rule.")
     return(invisible(cssRule))
   },
   getCopy = function() {
     copy <- list()
     return(invisible(copy))
   },
   asList = function() {
     lst <- list(
       declarations = private$p_declarations
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
    name = function(value) { return(invisible(private$p_name)) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_declarations = NULL
  )
)

