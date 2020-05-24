
#' R6 class that defines a collection of styles.
#'
#' @description
#' The `PivotStyles` class is a collection of `PivotStyle` objects.
#' It defines all of the base styles needed to style/theme a
#' pivot table.  It also defines the names of the styles that are used for
#' styling the different parts of the pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' pivotStyles <- PivotStyles$new(pt, themeName="compact")
#' pivotStyles$addStyle(styleName="MyNewStyle", list(
#'     font="0.75em arial",
#'     padding="2px",
#'     border="1px solid lightgray",
#'     "vertical-align"="middle",
#'     "text-align"="center",
#'     "font-weight"="bold",
#'     "background-color"="#F2F2F2"
#'   ))

PivotStyles <- R6::R6Class("PivotStyles",
  public = list(

    #' @description
    #' Create a new `PivotStyles` object.
    #' @param parentPivot The pivot table that this `PivotStyles`
    #' instance belongs to.
    #' @param themeName A theme name to represent this collection of styles.
    #' @param allowExternalStyles Default `FALSE`, which means this `PivotStyles`
    #' object checks that style names specified for styling the different
    #' parts of the pivot table must exist in the styles collection.  If they do
    #' not an error will occur.  Specify `TRUE` to disable this check, e.g. if
    #' the style definitions are not managed by `pivottabler` but instead
    #' in an external system.
    #' @return A new `PivotStyles` object.
    initialize = function(parentPivot, themeName=NULL, allowExternalStyles=FALSE) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyles", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyles", "initialize", themeName, missing(themeName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotStyles", "initialize", allowExternalStyles, missing(allowExternalStyles), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$new", "Creating new Pivot Styles...")
      private$p_theme <- themeName
      private$p_allowExternalStyles <- allowExternalStyles
      private$p_styles <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$new", "Created new Pivot Styles.")
    },

    #' @description
    #' Check whether a style with the specified style name exists in the
    #' collection.
    #' @param styleName The name of the style.
    #' @return `TRUE` if a style with the specified name exists, `FALSE` otherwise.
    isExistingStyle = function(styleName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "isExistingStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$isExistingStyle", "Checking style exists...", list(styleName=styleName))
      styleExists <- styleName %in% names(private$p_styles)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$isExistingStyle", "Checked style exists.")
      return(invisible(styleExists))
    },

    #' @description
    #' Retrieve a style with the specified style name.
    #' @param styleName The name of the style.
    #' @return A `PivotStyle` object.
    getStyle = function(styleName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "getStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$getStyle", "Getting style...", list(styleName=styleName))
      style <- private$p_styles[[styleName]]
      if(is.null(style)) {
        stop(paste0("PivotStyles$getStyle(): No style exists with the name '", styleName, "'"), call. = FALSE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$getStyle", "Got style.")
      return(invisible(style))
    },

    #' @description
    #' Add a new style to the collection of styles.
    #' @param styleName The name of the new style.
    #' @param declarations CSS style declarations in the form of a list, e.g.
    #' `list("font-weight"="bold", "color"="#0000FF")`
    #' @return The newly created `PivotStyle` object.
    addStyle = function(styleName=NULL, declarations= NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$addStyle", "Adding style...", list(styleName=styleName))
      if(styleName %in% names(private$p_styles)) {
        stop(paste0("PivotStyles$addStyle():  A style already exists",
                    " with the name '", styleName, "'.  styleName must unique."), call. = FALSE)
      }
      style <- PivotStyle$new(private$p_parentPivot, styleName, declarations)
      private$p_styles[[styleName]] <- style
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$addStyle", "Added style.")
      return(invisible(style))
    },

    #' @description
    #' Create a copy of a style with the specified name and store
    #' it in the collection.
    #' @param styleName The name of the style to copy.
    #' @param newStyleName The name for the new style.
    #' @return The newly created `PivotStyle` object.
    copyStyle = function(styleName=NULL, newStyleName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "copyStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "copyStyle", newStyleName, missing(newStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$copyStyle", "Copying style...", list(styleName=styleName, newStyleName=newStyleName))
      style <- self$getStyle(styleName=styleName)
      newStyle <- self$addStyle(styleName=newStyleName, declarations=style$declarations)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$copyStyle", "Copied style.")
      return(invisible(newStyle))
   },

   #' @description
   #' Get a style definition in the form of a CSS rule.
   #' @param styleName The name of the style.
   #' @param selector A CSS selector, used to select the element(s) to be styled.
   #' @return The style declarations in the form of a CSS rule,
   #' i.e. selector { property-name1: property-value1,
   #' property-name2: property-value2, ... }
   #' e.g. div { font-weight: bold, color: #0000FF }
    asCSSRule = function(styleName=NULL, selector=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "asCSSRule", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$asCSSRule", "Getting style as CSS rule...", list(styleName=styleName))
      style <- self$getStyle(styleName)
      cssRule <- style$asCSSRule(selector=selector)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$asCSSRule", "Got style as CSS rule.")
      return(invisible(cssRule))
    },

   #' @description
   #' Get a style definition in the form of a named CSS style.
   #' @param styleName The name of the style.
   #' @param styleNamePrefix A prefix to prepend to the style name.
   #' @return The style declarations in the form of named CSS style,
   #' i.e. .prefix-stylename { property-name1: property-value1,
   #' property-name2: property-value2, ... }
   #' e.g. .pvt1Cell { font-weight: bold, color: #0000FF }
    asNamedCSSStyle = function(styleName=NULL, styleNamePrefix=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "asNamedCSSStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "asNamedCSSStyle", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$asNamedCSSStyle", "Getting style as named CSS rule...", list(styleName=styleName, styleNamePrefix=styleNamePrefix))
      style <- self$getStyle(styleName)
      cssRule <- style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotStyles$asNamedCSSStyle", "Got style as named CSS rule.")
      return(invisible(cssRule))
    },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      if(length(private$p_styles) > 0) {
        groupNames <- names(private$p_styles)
        for (i in 1:length(private$p_styles)) {
          groupName <- groupNames[i]
          lst[[groupName]] = private$p_styles[[groupName]]$asList()
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
   #' @param seperator A character value to use when concatenating
   #' multiple styles.
   #' @return A character summary of various object properties.
    asString = function(seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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

    #' @field count The number of `PivotStyle` objects in this
    #' `PivotStyles` collection.
    count = function(value) { return(invisible(length(private$p_styles))) },

    #' @field theme A theme name to represent this collection of styles.
    theme = function(value) { return(invisible(private$p_theme)) },

    #' @field styles A list containing the `PivotStyle` objects in this
    #' `PivotStyles` collection.
    styles = function(value) { return(invisible(private$p_styles)) },

    #' @field allowExternalStyles Default `FALSE`, which means this `PivotStyles`
    #' object checks that style names specified for styling the different
    #' parts of the pivot table must exist in the styles collection.  If they do
    #' not an error will occur.  Specify `TRUE` to disable this check, e.g. if
    #' the style definitions are not managed by `pivottabler` but instead
    #' in an external system.
    allowExternalStyles = function(value) {
      if(missing(value)) return(invisible(private$p_allowExternalStyles))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        }
        private$p_allowExternalStyles <- value
        return(invisible())
      }
    },

    #' @field tableStyle The name of the style for the HTML table element.
    tableStyle = function(value) {
      if(missing(value)) return(invisible(private$p_tableStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "tableStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$tableStyle: '", value, "' style not found in styles list."))
        }
        private$p_tableStyle <- value
        return(invisible())
      }
    },

    #' @field rootStyle The name of the style for the HTML cell at the top left of
    #'   the pivot table.
    rootStyle = function(value) {
      if(missing(value)) return(invisible(private$p_rootStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "rootStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$rootStyle: '", value, "' style not found in styles list."))
        }
        private$p_rootStyle <- value
        return(invisible())
      }
    },

    #' @field rowHeaderStyle The name of the style for the row headers in the pivot
    #'   table.
    rowHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_rowHeaderStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "rowHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$rowHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_rowHeaderStyle <- value
        return(invisible())
      }
    },

    #' @field colHeaderStyle The name of the style for the column headers in the
    #'   pivot table.
    colHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_colHeaderStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "colHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$colHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_colHeaderStyle <- value
        return(invisible())
      }
    },

    #' @field outlineRowHeaderStyle The name of the style for the outline row
    #'   headers in the pivot table.
    outlineRowHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_outlineRowHeaderStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "outlineRowHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$outlineRowHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_outlineRowHeaderStyle <- value
        return(invisible())
      }
    },

    #' @field outlineColHeaderStyle The name of the style for the outline column
    #'   headers in the pivot table.
    outlineColHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_outlineColHeaderStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "outlineColHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$outlineColHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_outlineColHeaderStyle <- value
        return(invisible())
      }
    },

    #' @field cellStyle The name of the cell style for the non-total cells in the
    #'   body of the pivot table.
    cellStyle = function(value) {
      if(missing(value)) return(invisible(private$p_cellStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "cellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$cellStyle: '", value, "' style not found in styles list."))
        }
        private$p_cellStyle <- value
        return(invisible())
      }
    },

    #' @field outlineCellStyle The name of the cell style for the non-total
    #'   outline cells in the body of the pivot table.
    outlineCellStyle = function(value) {
      if(missing(value)) return(invisible(private$p_outlineCellStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "outlineCellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$outlineCellStyle: '", value, "' style not found in styles list."))
        }
        private$p_outlineCellStyle <- value
        return(invisible())
      }
    },

    #' @field totalStyle The name of the cell style for the total cells in the pivot
    #'   table.
    totalStyle = function(value) {
      if(missing(value)) return(invisible(private$p_totalStyle))
      else {
        if(private$p_parentPivot$argumentCheckMode > 0) {
          checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotStyles", "totalStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$totalStyle: '", value, "' style not found in styles list."))
        }
        private$p_totalStyle <- value
        return(invisible())
      }
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_theme = NULL,
    p_allowExternalStyles = FALSE,
    p_styles = NULL,
    p_tableStyle = NULL,
    p_rootStyle = NULL,
    p_rowHeaderStyle = NULL,
    p_colHeaderStyle = NULL,
    p_outlineRowHeaderStyle = NULL,
    p_outlineColHeaderStyle = NULL,
    p_cellStyle = NULL,
    p_outlineCellStyle = NULL,
    p_totalStyle = NULL
  )
)
