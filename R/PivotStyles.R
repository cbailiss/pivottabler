PivotStyles <- R6::R6Class("PivotStyles",
  public = list(
    initialize = function(parentPivot, themeName=NULL, allowExternalStyles=FALSE) {
      checkArgument("PivotStyles", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      checkArgument("PivotStyles", "initialize", themeName, missing(themeName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotStyles", "initialize", allowExternalStyles, missing(allowExternalStyles), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotStyles$new", "Creating new Pivot Styles...")
      private$p_theme <- themeName
      private$p_allowExternalStyles <- allowExternalStyles
      private$p_styles <- list()
      private$p_parentPivot$message("PivotStyles$new", "Created new Pivot Styles.")
    },
    isExistingStyle = function(styleName=NULL) {
      checkArgument("PivotStyles", "isExistingStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotStyles$isExistingStyle", "Checking style exists...", list(styleName=styleName))
      styleExists <- styleName %in% names(private$p_styles)
      private$p_parentPivot$message("PivotStyles$isExistingStyle", "Checked style exists.")
      return(styleExists)
    },
    getStyle = function(styleName=NULL) {
      checkArgument("PivotStyles", "getStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotStyles$getStyle", "Getting style...", list(styleName=styleName))
      style <- private$p_styles[[styleName]]
      if(is.null(style)) {
        stop(paste0("PivotStyles$getStyle(): No style exists with the name '", styleName, "'"), call. = FALSE)
      }
      private$p_parentPivot$message("PivotStyles$getStyle", "Got style.")
      return(style)
    },
    addStyle = function(styleName=NULL, declarations= NULL) {
      checkArgument("PivotStyles", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotStyles", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      private$p_parentPivot$message("PivotStyles$addStyle", "Adding style...", list(styleName=styleName))
      if(styleName %in% names(private$p_styles)) {
        stop(paste0("PivotStyles$addStyle():  A style already exists",
                    " with the name '", styleName, "'.  styleName must unique."), call. = FALSE)
      }
      style <- PivotStyle$new(private$p_parentPivot, styleName, declarations)
      private$p_styles[[styleName]] <- style
      private$p_parentPivot$message("PivotStyles$addStyle", "Added style.")
      return(style)
    },
    asCSSRule = function(styleName=NULL, selector=NULL) {
      checkArgument("PivotStyles", "asCSSRule", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotStyles", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_parentPivot$message("PivotStyles$asCSSRule", "Getting style as CSS rule...", list(styleName=styleName))
      style <- self$getStyle(styleName)
      cssRule <- style$asCSSRule(selector=selector)
      private$p_parentPivot$message("PivotStyles$asCSSRule", "Got style as CSS rule.")
      return(cssRule)
    },
    asNamedCSSStyle = function(styleName=NULL, stylePrefix=NULL) {
      checkArgument("PivotStyles", "asNamedCSSStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotStyles", "asNamedCSSStyle", stylePrefix, missing(stylePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_parentPivot$message("PivotStyles$asNamedCSSStyle", "Getting style as named CSS rule...", list(styleName=styleName))
      style <- self$getStyle(styleName)
      cssRule <- style$asNamedCSSStyle(stylePrefix=stylePrefix)
      private$p_parentPivot$message("PivotStyles$asNamedCSSStyle", "Got style as named CSS rule.")
      return(cssRule)
    },
    asList = function() {
      lst <- list()
      if(length(private$p_styles) > 0) {
        groupNames <- names(private$p_styles)
        for (i in 1:length(private$p_styles)) {
          groupName <- groupNames[i]
          lst[[groupName]] = private$p_styles[[groupName]]$asList()
        }
      }
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(seperator=", ") {
       checkArgument("PivotStyles", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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
    count = function(value) { return(length(private$p_styles)) },
    theme = function(value) { return(private$p_theme) },
    styles = function(value) { return(private$p_styles) },
    allowExternalStyles = function(value) {
      if(missing(value)) return(private$p_allowExternalStyles)
      else {
        checkArgument("PivotStyles", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        private$p_allowExternalStyles <- value
      }
    },
    tableStyle = function(value) {
      if(missing(value)) return(private$p_tableStyle)
      else {
        checkArgument("PivotStyles", "tableStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$tableStyle: '", value, "' style not found in styles list."))
        }
        private$p_tableStyle <- value
      }
    },
    rootStyle = function(value) {
      if(missing(value)) return(private$p_rootStyle)
      else {
        checkArgument("PivotStyles", "rootStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$rootStyle: '", value, "' style not found in styles list."))
        }
        private$p_rootStyle <- value
      }
    },
    rowHeaderStyle = function(value) {
      if(missing(value)) return(private$p_rowHeaderStyle)
      else {
        checkArgument("PivotStyles", "rowHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$rowHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_rowHeaderStyle <- value
      }
    },
    colHeaderStyle = function(value) {
      if(missing(value)) return(private$p_colHeaderStyle)
      else {
        checkArgument("PivotStyles", "colHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$colHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_colHeaderStyle <- value
      }
    },
    cellStyle = function(value) {
      if(missing(value)) return(private$p_cellStyle)
      else {
        checkArgument("PivotStyles", "cellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$cellStyle: '", value, "' style not found in styles list."))
        }
        private$p_cellStyle <- value
      }
    },
    totalStyle = function(value) {
      if(missing(value)) return(private$p_totalStyle)
      else {
        checkArgument("PivotStyles", "totalStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("PivotStyles$totalStyle: '", value, "' style not found in styles list."))
        }
        private$p_totalStyle <- value
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
    p_cellStyle = NULL,
    p_totalStyle = NULL
  )
)
