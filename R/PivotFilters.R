
#' R6 class that defines a set of filter conditions.
#'
#' @description
#' The `PivotFilters` class allows multiple filter conditions relating to
#' different data frame columns to be combined, i.e. a `PivotFilters` object
#' typically contains multiple \code{\link{PivotFilter}} objects.
#'
#' @details
#' As well as acting as a container for multiple filter conditions, the
#' `PivotFilters` class also contains logic for combining filter.
#' The `action` parameter in many of the methods controls how two filters
#' are combined.\cr
#' Most common cases:\cr
#' (1) When working out the rowColFilters for each pivot table cell, the
#'     filters from the row and column leaf groups are combined using
#'     `action="intersect"`.\cr
#' (2) When combining the rowColFilters with calculation filters the
#'     action could be any of (in order of most typical)
#'     "intersect", "replace" or "union".\cr
#'     "intersect" would apply additional restrictions, e.g. see the
#'     example in the Calculations vignette that has a measure for
#'     weekend trains only.\cr
#'     "replace" would apply when doing things like percentage of row
#'     total calculations - again, see example in the calculations vignette\cr
#'     "union" is probably much less likely (hard to envisage many
#'     situations when that would be needed).\cr
#' (3) In custom calculation functions, the action could be any of
#'     "intersect", "replace" or "union".\cr
#' NOTE: `pivottabler` does not allow complex conditions to be built up,
#'     such as ((A=X) or (B=Y)) and (C=2) since there is complex precedence
#'     involved and conditions like this are not typical of pivot tables.
#'     If they were really needed, a workaround would be to use a custom
#'     calculation function and include this logic in that function.\cr
#' See Appendix 2 vignette for many more complex calculation details.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @export
#' @format \code{\link{R6Class}} object.
#' @examples
#' pt <- PivotTable$new()
#' # ...
#' # PivotFilters constructor allows a filter to be defined
#' filters <- PivotFilters$new(pt, variableName="Year", values=2017)
#' # Create a new filter
#' filter <- PivotFilter$new(pt, variableName="Country", values="England")
#' # Combine the filters
#' filters$setFilter(filter)
#' # filters now contains criteria for both Year and Country
#' # Now add another filter, this time via an alternative method
#' filters$setFilterValues(variableName="Product", values="Cadbury Dairy Milk
#' Chocolate 100g")
#' # filters now contains criteria for Year, Country and Product

PivotFilters <- R6::R6Class("PivotFilters",
  public = list(

    #' @description
    #' Create a new `PivotFilters` object, optionally adding a filter.
    #' @param parentPivot The pivot table that this `PivotFilters`
    #' instance belongs to.
    #' @param variableName The name of the column in the data frame that this filter
    #' applies to.  Specify `NULL` to skip adding a filter.
    #' @param type Must be either "ALL", "VALUES" or "NONE".  "VALUES" is the most
    #' common type and means the data is filtered to a subset of values.  "ALL" means
    #' there is no filtering, i.e. all values match.  "NONE" means there can be no
    #' matching values/data.
    #' @param values A single data value or a vector of multiple data values that
    #' the filter will match on.
    #' @return A new `PivotFilters` object.
    initialize = function(parentPivot=NULL, variableName=NULL, type="ALL", values=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilters", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilters", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilters", "initialize", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotFilters", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$new", "Creating new Pivot Filters...", list(variableName=variableName, values=values))
      private$p_filters <- list()
      if(!missing(variableName)&&!is.null(variableName)) {
        self$setFilterValues(variableName=variableName, type=type, values=values, action="replace")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$new", "Created new Pivot Filters.")
    },

    #' @description
    #' Remove all filters from this `PivotFilters` object.
    #' @return No return value.
    clearFilters = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$clearFilters", "Clearing filters...")
      private$p_filters <- list()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$clearFilters", "Cleared filters.")
      return(invisible())
    },

    #' @description
    #' Remove the filters for all variables except those specified.
    #' @param variableNames A character vector specifying the variable names
    #' to retain the filter criteria for. Filter criteria for all other variables
    #' will be cleared.
    #' @return No return value.
    keepOnlyFiltersFor = function(variableNames=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "keepOnlyFiltersFor", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$keepOnlyFiltersFor", "Keeping filters only for...", list(variableNames=variableNames))
      newlst <- list()
      if((!is.null(variableNames))&&(length(private$p_filters)>0)) {
        nms <- names(private$p_filters)
        for(i in 1:length(private$p_filters)) {
          if(nms[i] %in% variableNames) newlst[[length(newlst)+1]] <- private$p_filters[[i]]
        }
      }
      private$p_filters <- newlst
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$keepOnlyFiltersFor", "Kept filters only for.")
      return(invisible())
    },

    #' @description
    #' Remove the filters for the specified variables.
    #' @param variableNames A character vector specifying the variable names
    #' for which the filter criteria will be cleared.  Filter criteria for all
    #' other variables will be retained.
    #' @return No return value.
    removeFiltersFor = function(variableNames=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "removeFiltersFor", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$removeFiltersFor", "Removing filters for...", list(variableNames=variableNames))
      newlst <- list()
      if((!is.null(variableNames))&&(length(private$p_filters)>0)) {
        nms <- names(private$p_filters)
        for(i in 1:length(private$p_filters)) {
          if(!(nms[i] %in% variableNames)) newlst[[length(newlst)+1]] <- private$p_filters[[i]]
        }
      }
      private$p_filters <- newlst
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$removeFiltersFor", "Removed filters for.")
      return(invisible())
    },

    #' @description
    #' Find a filter with the specified variable name.
    #' @param variableName The variable name to find a filter for.
    #' @return A `PivotFilter` object that filters on the specified variable.
    getFilter = function(variableName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "getFilter", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilter", "Getting filter...", list(variableName=variableName))
      filter <- private$p_filters[[variableName]]
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilter", "Got filter.")
      return(invisible(filter))
    },


    # isFilterMatch is used when searching for row/column headings (see the "Finding and Formatting" vignette),
    # i.e. it compares the criteria of one filter to specified criteria, e.g. to find the PivotDataGroup thats is Country=UK.

    #' @description
    #' Tests whether this `PivotFilters` object matches specified criteria.
    #' @param matchMode Either "simple" (default) or "combinations".\cr
    #' "simple" is used when matching only one variable-value, multiple
    #' variable-value combinations are effectively logical "OR", i.e.
    #' any one single `PivotFilter` match means the `PivotFilters` object
    #' is a match.\cr
    #' "combinations" is used when matching for combinations of variable
    #' values, multiple variable-value combinations are effectively
    #' logical "AND", i.e. there must be a matching `PivotFilter` for
    #' every variable name / variable values criteria specified.\cr
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @param variableNames The variable name(s) to find a filter for.  This
    #' can be a vector containing more than one variable name.
    #' @param variableValues A list specifying the variable names and values
    #' to find, e.g. `variableValues=list("PowerType"=c("DMU", "HST"))`.
    #' @return `TRUE` if this filters object matches the specified criteria,
    #' `FALSE` otherwise.
    isFilterMatch = function(matchMode="simple", variableNames=NULL, variableValues=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "isFilterMatch", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "isFilterMatch", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "isFilterMatch", variableValues, missing(variableValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$isFilterMatch", "Checking if is filter match...")
      # Summary
      # variableNames can be a vector (i.e. more than one item specified)
      # variableValues can be a vector (i.e. more than one item specified)
      # For matchMode=simple, a single match is good enough, i.e. any given data group
      # must match only one variableNames element and only one variableValues element.
      # For matchMode=combinations, any given data group must match all of the
      # specified variablesNames and variableValues
      if(!is.null(variableNames)) {
        matched <- FALSE
        if(length(variableNames) > 0) {
          for(i in 1:length(variableNames)) {
            filter <- self$getFilter(variableNames[i])
            if((matchMode=="simple")&&(!is.null(filter))) {
              matched <- TRUE
              break # i.e. one match from variableNames is enough
            }
            if((matchMode=="combinations")&&(is.null(filter))) return(invisible(FALSE))
          }
          if((matchMode=="simple")&&(!matched)) return(invisible(FALSE))
        }
      }
      if(is.null(variableValues)) {
        if(matchMode=="simple") return(invisible(TRUE))
      }
      else {
        if(length(variableValues) > 0) {
          varNames <- names(variableValues)
          for(i in 1:length(varNames)) {
            filter <- self$getFilter(varNames[i])
            varValues <- variableValues[[i]]
            # special cases
            if("**" %in% varValues) {
              # asterix means the filter should exist and should be of type ALL
              if(is.null(filter)) {
                if(matchMode=="simple") next
                return(invisible(FALSE))
              }
              if(filter$type=="ALL") {
                if(matchMode=="simple") return(invisible(TRUE))
                else next
              }
              else return(invisible(FALSE))
            }
            if("!*" %in% varValues) {
              # asterix means the filter should exist and the filter type should not be ALL
              if(is.null(filter)) {
                if(matchMode=="simple") next
                return(invisible(FALSE))
              }
              if(filter$type=="ALL") {
                if(matchMode=="simple") next
                else return(invisible(FALSE))
              }
              else {
                if(matchMode=="simple") return(invisible(TRUE))
                next
              }
            }
            # normal values criteria
            if(is.null(filter)) {
              if(matchMode=="simple") next
              else return(invisible(FALSE))
            }
            if(is.null(varValues)) next
            if(length(varValues)==0) next
            intrsct <- intersect(filter$values, varValues)
            if(is.null(intrsct)) {
              if(matchMode=="simple") next
              else return(invisible(FALSE))
            }
            if(length(intrsct)==0) {
              if(matchMode=="simple") next
              else return(invisible(FALSE))
            }
            if((length(intrsct)>0)&&(matchMode=="simple")) {
              return(invisible(TRUE)) # i.e. a single match is good enough
            }
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$isFilterMatch", "Checked if is filter match.")
      if(matchMode=="simple") return(invisible(FALSE))
      else return(invisible(TRUE))
    },

    #' @description
    #' Update the value of this `PivotFilters` object with the filters from the
    #' specified `PivotFilters` object, either intersecting, replacing or unioning
    #' the filter criteria.
    #' @param filters A `PivotFilters` object.
    #' @param action Specifies how the criteria defined in `filters` should be
    #' combined with the existing filter criteria.
    #' Must be one of "intersect", "replace" or "union".
    #' @return No return value.
    setFilters = function(filters=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilters", "Setting filters...", list(action=action, filters=filters$asString()))
      if(length(filters$filters)>0) {
        for(i in 1:length(filters$filters)) {
          self$setFilter(filters$filters[[i]], action=action)
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilters", "Set filters.")
      return(invisible())
    },

    #' @description
    #' Update the value of this `PivotFilters` object with the filters from the
    #' specified `PivotFilter` object, either intersecting, replacing or unioning
    #' the filter criteria.
    #' @param filter A `PivotFilter` object.
    #' @param action Specifies how the criteria defined in `filter` should be
    #' combined with the existing filter criteria.
    #' Must be one of "intersect", "replace" or "union".
    #' @return No return value.
    setFilter = function(filter=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilters", "Setting filter...", list(action=action, filter=filter$asString()))
      self$setFilterValues(variableName=filter$variableName, type=filter$type, values=filter$values, action=action)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilter", "Set filter.")
      return(invisible())
    },

    #' @description
    #' Update the value of this `PivotFilters` object with additional filter criteria,
    #' either intersecting, replacing or unioning the filter criteria.
    #' @param variableName The name of the column in the data frame that this criteria
    #' applies to.
    #' @param type Must be either "ALL", "VALUES" or "NONE".
    #' @param values A single data value or a vector of multiple data values that
    #' comprise the additional filter criteria.
    #' @param action Specifies how the criteria defined in `filter` should be
    #' combined with the existing filter criteria.
    #' Must be one of "intersect", "replace" or "union".
    #' @return No return value.
    setFilterValues = function(variableName=NULL, type="ALL", values=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("intersect", "replace", "union"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilterValues", "Setting filter values...",
                                    list(action=action, variableName=variableName, type=type, values=values))
      filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, type=type, values=values)
      variablesNames <- names(private$p_filters)
      if(action=="intersect") {
        if(variableName %in% variablesNames) { private$p_filters[[variableName]]$intersect(filter) }
        else { private$p_filters[[variableName]] <- filter }
      }
      else if(action=="replace") {
        private$p_filters[[variableName]] <- filter
      }
      else if(action=="union") {
        if(variableName %in% variablesNames) { private$p_filters[[variableName]]$union(filter) }
        # no "else", since for this variable, this filter is ALL, so ALL or something = ALL.
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilterValues", "Set filter values.")
      return(invisible())
    },

    #' @description
    #' Add a new `PivotFilter` object to the filter list in this `PivotFilters`
    #' object.
    #' @param filter A `PivotFilter` object.
    #' @return No return value.
    addFilter = function(filter=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "addFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$addFilter", "Adding filter...", list(filter=filter$asString()))
      private$p_filters[[filter$variableName]] <- filter
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$addFilter", "Added filter.")
      return(invisible())
    },

    #' @description
    #' Filters the specified data frame using the filters defined in this
    #' `PivotFilters` object and returns the results as another data frame.
    #' @param dataFrame A data frame to filter.
    #' @return A data frame filtered according to the criteria in this `PivotFilters` object.
    getFilteredDataFrame = function(dataFrame=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, TRUE, "PivotFilters", "getFilteredDataFrame", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilteredDataFrame", "Getting filtered data frame...")
      # use data
      data <- dataFrame
      # short circuiting where possible
      if(self$isNONE) {
        data <- data[0, ] # returns the structure but no data
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilteredDataFrame", "Got filtered data frame (NO DATA).")
        return(invisible(data))
      }
      if(self$isALL) {
        if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilteredDataFrame", "Got filtered data frame (ALL DATA).")
        return(invisible(data))
      }
      # check the filtering mode
      filterMode <- private$p_parentPivot$processingLibrary
      # if(filterMode=="base") {
      #   # build a subsetting query
      #   # todo: checking the escaping of the variable names and values below
      #   # filterCmd is e.g. "data <- data[(", filters[1]$variableName, " %in% filters[1]$values) & (", filters[2]$variableName, " %in% filters[2]$values), ]"
      #   if(length(private$p_filters) > 0)
      #   {
      #     filterCmd <- NULL
      #     filterCount <- 0
      #     for(j in 1:length(private$p_filters)) {
      #       filter <- private$p_filters[[j]]
      #       if(is.null(filter$variableName))
      #         stop("PivotFilters$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
      #       if(is.null(filter$values)) next
      #       if(length(filter$values)==0) next
      #       if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
      #       if(length(filter$values)>0) {
      #         # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
      #         filterCmd <- paste0(filterCmd, "(data$", filter$variableName, " %in% private$p_filters$values)")
      #         filterCount <- filterCount + 1
      #       }
      #     }
      #     if(filterCount > 0) {
      #       filterCmd <- paste0("data <- data[", filterCmd, ", ]")
      #       eval(parse(text=filterCmd))
      #     }
      #   }
      # }
      # else
      if(filterMode=="dplyr") {
        # build a dplyr query
        # todo: checking the escaping of the variable names and values below
        # filterCmd is e.g. "data <- filter(data, (", filters[1]$variableName, " %in% filters[1]$values) & (", filters[2]$variableName, " %in% filters[2]$values)"
        if(length(private$p_filters) > 0)
        {
          filterCmd <- NULL
          filterCount <- 0
          for(j in 1:length(private$p_filters)) {
            filter <- private$p_filters[[j]]
            if(is.null(filter$variableName))
              stop("PivotFilters$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
            if(is.null(filter$values)) next
            if(length(filter$values)==0) next
            if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
            if(length(filter$values)>0) {
              # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
              filterCmd <- paste0(filterCmd, "(", filter$safeVariableName, " %in% private$p_filters[[", j, "]]$values)")
              filterCount <- filterCount + 1
            }
          }
          if(filterCount > 0) {
            filterCmd <- paste0("data <- dplyr::filter(data,", filterCmd, ")")
            eval(parse(text=filterCmd))
          }
        }
      }
      else if(filterMode=="data.table") {
        # build a data.table query
        # todo: checking the escaping of the variable names and values below
        # filterCmd is e.g. "data <- data[", filters[1]$variableName, " %in% filters[1]$values & ", filters[2]$variableName, " %in% filters[2]$values)]"
        if(length(private$p_filters) > 0)
        {
          filterCmd <- NULL
          filterCount <- 0
          for(j in 1:length(private$p_filters)) {
            filter <- private$p_filters[[j]]
            if(is.null(filter$variableName))
              stop("PivotFilters$getFilteredDataFrame(): filter$variableName must not be null", call. = FALSE)
            if(is.null(filter$values)) next
            if(length(filter$values)==0) next
            if(!is.null(filterCmd)) filterCmd <- paste0(filterCmd, " & ")
            if(length(filter$values)>0) {
              # %in% handles NA correctly for our use-case, i.e. NA %in% NA returns TRUE, not NA
              filterCmd <- paste0(filterCmd, "(", filter$safeVariableName, " %in% private$p_filters[[", j, "]]$values)")
              filterCount <- filterCount + 1
            }
          }
          if(filterCount > 0) {
            # check is a data table
            if(private$p_parentPivot$argumentCheckMode == 4) {
              if(!data.table::is.data.table(data))
                stop(paste0("PivotFilters$getFilteredDataFrame(): A data.table was expected but the following was encountered: ",
                            paste(class(data), sep="", collapse=", ")), call. = FALSE)
            }
            # apply the filter
            filterCmd <- paste0("data <- data[", filterCmd, "]")
            eval(parse(text=filterCmd))
          }
        }
      }
      else stop(paste0("PivotFilters$getFilteredDataFrame(): Unknown filterMode encountered: ", filterMode), call. = FALSE)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilteredDataFrame", "Got filtered data frame (SOME DATA).")
      return(invisible(data))
    },

    #' @description
    #' Create a copy of this `PivotFilters` object.
    #' @return A copy of this `PivotFilters` object.
    getCopy = function() {
      copy <- PivotFilters$new(private$p_parentPivot)
      if(length(private$p_filters) > 0) {
        for(i in 1:length(private$p_filters)) {
          copy$addFilter(private$p_filters[[i]]$getCopy())
        }
      }
      return(invisible(copy))
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      if(length(private$p_filters) > 0) {
        for (i in 1:length(private$p_filters)) {
          lst[[i]] = private$p_filters[[i]]$asList()
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
    #' @param includeVariableName `TRUE` (default) to include the variable name in
    #'  the string.
    #' @param seperator A character value used when concatenating
    #' multiple filters.
    #' @return A character summary of various object properties.
    asString = function(includeVariableName=TRUE, seperator=", ") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", includeVariableName, missing(includeVariableName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       }
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

    #' @field count The number of `PivotFilter` objects in this `PivotFilters` object.
    count = function(value) { return(invisible(length(private$p_filters))) },

    #' @field filters A list of `PivotFilter` objects in this `PivotFilters` object.
    filters = function(value) { return(invisible(private$p_filters)) },

    #' @field isALL If TRUE, this `PivotFilters` object matches all data.
    isALL = function(value) {
      if(self$count==0) return(TRUE)
      else {
        bALL <- TRUE
        for(i in 1:length(private$p_filters)) {
          f <- private$p_filters[[i]]
          if(f$type!="ALL") {
            bALL <- FALSE
            break
          }
        }
        return(bALL)
      }
    },

    #' @field isNONE If TRUE, this `PivotFilters` object matches no data.
    isNONE = function(value) {
      if(self$count==0) return(FALSE)
      else {
        for(i in 1:length(private$p_filters)) {
          f <- private$p_filters[[i]]
          if(f$type=="NONE") return(TRUE)
        }
        return(FALSE)
      }
    },

    #' @field filteredVariables The names of the variables that are filtered by this
    #' `PivotFilters` object.
    filteredVariables = function(value) { # returns a character vector of variable names that are filtered
      variableNames = NULL
      if(!is.null(private$p_filters)) {
        if(length(private$p_filters)>0) {
          for(i in 1:length(private$p_filters)) {
            filter <- private$p_filters[[i]]
            if(filter$type=="VALUES") {
              variableNames[length(variableNames)+1] <- filter$variableName
            }
          }
        }
      }
      return(variableNames)
    },

    #' @field filteredValues A list of the criteria values for each of the variables
    #' filtered by this `PivotFilters` object, where the list element names are the
    #' variable names.
    filteredValues = function(value) { # returns a list of variable names that are filtered plus the filter values
      values = list()
      if(!is.null(private$p_filters)) {
        if(length(private$p_filters)>0) {
          for(i in 1:length(private$p_filters)) {
            filter <- private$p_filters[[i]]
            if(filter$type=="VALUES") {
              values[[filter$variableName]] <- filter$values
            }
          }
        }
      }
      return(values)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_filters = NULL
  )
)
