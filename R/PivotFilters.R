#' A class that defines a set of filter conditions
#'
#' The PivotFilters class allows multiple filter conditions relating to
#' different data frame columns to be combined, i.e. a PivotFilters object can
#' contain multiple \code{\link{PivotFilter}} objects.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom data.table data.table is.data.table
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a set of filter conditions.
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
#' @field parentPivot Owning pivot table.
#' @field count The number of PivotFilter objects in this PivotFilters object.
#' @field filters The PivotFilter objects in this PivotFilters object.
#' @field isALL If TRUE, this PivotFilters object matches all data.
#' @field isNONE If TRUE, this PivotFilters object matches no data.
#' @field filteredVariables A list of the variables that are filtered by this
#'   PivotFilters object.
#' @field filteredValues A list of the criteria values for each of the variables
#'   filtered by this PivotFilters object.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new pivot calculation, specifying the field
#'   values documented above.}
#'
#'   \item{\code{getFilter(variableName=NULL)}}{Find a filter with the specified
#'   variable name.}
#'   \item{\code{isFilterMatch(variableNames=NULL, variableValues=NULL)}}{Tests
#'   whether these filters match the specified criteria.}
#'   \item{\code{setFilters(filters=NULL, action="replace")}}{Update the value of this
#'   PivotFilters object with the filters from the specified PivotFilters
#'   object, either unioning, intersecting or replacing the filter criteria.}
#'   \item{\code{setFilter(filter=NULL, action="replace")}}{Update the value of this
#'   PivotFilters object with the specified PivotFilter object, either unioning,
#'   intersecting or replacing the filter criteria.}
#'   \item{\code{setFilterValues(variableName=NULL, type=NULL, values=NULL,
#'   action="replace")}}{Update the value of this PivotFilters object with the
#'   specified criteria, either unioning, intersecting or replacing the filter
#'   criteria.}
#'   \item{\code{addFilter()}}{Directly add a PivotFilter object to this
#'   PivotFilters object.}
#'   \item{\code{getFilteredDataFrame(dataFrame=NULL)}}{Filters the specified
#'   data frame and returns the results as another data frame.}
#'   \item{\code{getCopy()}}{Get a copy of this set of filters.}
#'   \item{\code{asList()}}{Get a list representation of this PivotFilters
#'   object.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this PivotFilters
#'   object.}
#'   \item{\code{asString(includeVariableName=TRUE, seperator=", ")}}{Get a text
#'   representation of this PivotFilters object.}
#' }

PivotFilters <- R6::R6Class("PivotFilters",
  public = list(
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
    getFilter = function(variableName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "initialize", variableName, missing(variableName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilter", "Getting filter...", list(variableName=variableName))
      filter <- private$p_filters[[variableName]]
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$getFilter", "Got filter.")
      return(invisible(filter))
    },
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
    setFilters = function(filters=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", filters, missing(filters), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilters")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("and", "replace"))
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
    setFilter = function(filter=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilters", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("and", "replace"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilters", "Setting filter...", list(action=action, filter=filter$asString()))
      self$setFilterValues(variableName=filter$variableName, type=filter$type, values=filter$values, action=action)
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilter", "Set filter.")
      return(invisible())
    },
    setFilterValues = function(variableName=NULL, type="ALL", values=NULL, action="replace") {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", variableName, missing(variableName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", values, missing(values), allowMissing=TRUE, allowNull=TRUE, mustBeAtomic=TRUE)
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("ALL", "VALUES", "NONE"))
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "setFilterValues", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("and", "replace"))
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilterValues", "Setting filter values...",
                                    list(action=action, variableName=variableName, type=type, values=values))
      filter <- PivotFilter$new(private$p_parentPivot, variableName=variableName, type=type, values=values)
      variablesNames <- names(private$p_filters)
      if(action=="and") {
        if(variableName %in% variablesNames) { private$p_filters[[variableName]]$and(filter) }
        else { private$p_filters[[variableName]] <- filter }
      }
      else if(action=="replace") {
        private$p_filters[[variableName]] <- filter
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$setFilterValues", "Set filter values.")
      return(invisible())
    },
    addFilter = function(filter=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotFilters", "addFilter", filter, missing(filter), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotFilter")
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$addFilter", "Adding filter...", list(filter=filter$asString()))
      private$p_filters[[filter$variableName]] <- filter
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotFilters$addFilter", "Added filter.")
      return(invisible())
    },
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
              filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% private$p_filters[[", j, "]]$values)")
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
              filterCmd <- paste0(filterCmd, "(", filter$variableName, " %in% private$p_filters[[", j, "]]$values)")
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
    getCopy = function() {
      copy <- PivotFilters$new(private$p_parentPivot)
      if(length(private$p_filters) > 0) {
        for(i in 1:length(private$p_filters)) {
          copy$addFilter(private$p_filters[[i]]$getCopy())
        }
      }
      return(invisible(copy))
    },
    asList = function() {
      lst <- list()
      if(length(private$p_filters) > 0) {
        for (i in 1:length(private$p_filters)) {
          lst[[i]] = private$p_filters[[i]]$asList()
        }
      }
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
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
    count = function(value) { return(invisible(length(private$p_filters))) },
    filters = function(value) { return(invisible(private$p_filters)) },
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
