PivotFilters <- R6::R6Class("PivotFilters",
  public = list(
    initialize = function(parentPivot, variableName=NULL, values=NULL) {
      checkArgument("PivotFilters", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      checkArgument("PivotFilters", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotFilters", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotFilters$new", "Creating new Pivot Filters...", list(variableName=variableName, values=values))

      private$p_filters <- list()
      if(!missing(variableName)&!is.null(variableName)) {
        self$setFilterValues(variableName, values, action="replace")
      }
      private$p_parentPivot$message("PivotFilters$new", "Created new Pivot Filters.")
    },
    setFilters = function(filters, action="replace") {
      checkArgument("PivotFilters", "setFilters", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
      checkArgument("PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilters", "Setting filters...", list(action=action, filters=filters$asString()))
      if(length(filters$filters)>0) {
        for(i in 1:length(filters$filters))
        {
          self$setFilter(filters$filters[[i]], action)
        }
      }
      private$p_parentPivot$message("PivotFilters$setFilters", "Set filters.")
    },
    setFilter = function(filter, action="replace") {
      checkArgument("PivotFilters", "setFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      checkArgument("PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilters", "Setting filter...", list(action=action, filter=filter$asString()))
      self$setFilterValues(filter$variableName, filter$values, action)
      private$p_parentPivot$message("PivotFilters$setFilter", "Set filter.")
    },
    setFilterValues = function(variableName=NULL, values=NULL, action="replace") {
      checkArgument("PivotFilters", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotFilters", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      checkArgument("PivotFilters", "setFilterValues", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("union", "intersect", "replace"))
      private$p_parentPivot$message("PivotFilters$setFilterValues", "Setting filter values...",
                                    list(action=action, variableName=variableName, values=values))
      filter <- PivotFilter$new(private$p_parentPivot, variableName, values)
      variablesNames <- names(private$p_filters)
      if(action=="union") {
        if(!(variableName %in% variablesNames)) { private$p_filters[[length(private$p_filters)+1]] <- filter }
        else { private$p_filters[[variableName]]$union(filter) }
      }
      else if(action=="intersect") {
        if(!(variableName %in% variablesNames))
          stop("PivotFilters$setFilterValues():  Cannot intersect as variable name does not exist in current filters.", call. = FALSE)
        private$p_filters[[variableName]]$intersect(filter)
      }
      else if(action=="replace") {
        private$p_filters[[variableName]] <- filter
      }
      else {
        stop(paste0("PivotFilters$setFilterValues():  action must be one of union, intersect, replace.  Unknown action: ", action), call. = FALSE)
      }
      private$p_parentPivot$message("PivotFilters$setFilterValues", "Set filter values.")
    },
    addFilter = function(filter) {
      checkArgument("PivotFilters", "addFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      private$p_parentPivot$message("PivotFilters$addFilter", "Adding filter...", list(filter=filter$asString()))

      index <- length(private$p_filters) + 1
      private$p_filters[[index]] <- filter
      private$p_parentPivot$message("PivotFilters$addFilter", "Added filter.")
    },
    getCopy = function() {
      copy <- PivotFilters$new(private$p_parentPivot)
      if(length(private$p_filters) > 0) {
        for(i in 1:length(private$p_filters)) {
          copy$addFilter(private$p_filters[[i]]$getCopy())
        }
      }
      return(copy)
    },
    asList = function() {
      lst <- list()
      if(length(private$p_filters) > 0) {
        for (i in 1:length(private$p_filters)) {
          lst[[i]] = private$p_filters[[i]]$asList()
        }
      }
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(includeVariableName=TRUE, seperator=", ") {
       checkArgument("PivotFilters", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument("PivotFilters", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       fstr <- ""
       if(length(private$p_filters)>0) {
         for(i in 1:length(private$p_filters)) {
           f <- private$p_filters[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           fstr <- paste0(fstr, sep, f$asString(includeVariableName=includeVariableName))
         }
       }
       return(fstr)
    }
  ),
  active = list(
    count = function(Value) { return(length(private$p_filters)) },
    filters = function(value) { return(private$p_filters) }
  ),
  private = list(
    p_parentPivot = NULL,
    p_filters = NULL
  )
)
