#' A class that represents a batch calculation.
#'
#' The PivotBatch class represents one combination of variables and calculations
#' that are needed when calculating the values of cells in a pivot table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   to (do xyz).
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.
#' @field parentPivot Owning pivot table.
#' @field batchId A unique integer identifier for this batch.
#' @field compatibleCount The number of pivot cell calculations that this batch supports.
#' @field asString A text description of this batch.
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new Pivot Batch.}
#'
#'   \item{\code{isCompatible(dataName=NULL, variableNames=NULL)}}{Checks
#'   whether the specified data name and combination of variable names are
#'   compatible with this batch.}
#'   \item{\code{addCompatible(values=NULL, calculationName=NULL,
#'   calculationGroupName=NULL){Adds another cell calculation to this batch.}
#' }

PivotBatch <- R6::R6Class("PivotBatch",
  public = list(
    # the values and calculation don't affect compatibility.
    # values isn't used for now, just keeps track of all of the distinct values to be calculated for each variable
    initialize = function(parentPivot=NULL, batchId=0, dataName=NULL,
                          variableNames=NULL, values=NULL, # values(varName1=values1, varName2=values2, ...)
                          calculationName=NULL, calculationGroupName=NULL) {
      checkArgument("PivotBatch", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      checkArgument("PivotBatch", "initialize", batchId, missing(batchId), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("numeric", "integer"))
      checkArgument("PivotBatch", "initialize", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatch", "initialize", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      checkArgument("PivotBatch", "initialize", values, missing(values), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      checkArgument("PivotBatch", "initialize", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatch", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotBatch$new", "Creating new Pivot Batch...")
      private$p_batchId <- batchId
      private$p_dataName <- dataName
      private$p_variableNames <- variableNames
      private$p_values <- values
      private$p_calculations <- list(c(calculationGroupName=calculationGroupName, calculationName=calculationName))
      private$p_compatibleCount <- 1
      private$p_parentPivot$message("PivotBatch$new", "Created new Pivot Batch.")
    },
    isCompatible = function(dataName=NULL, variableNames=NULL) {
      checkArgument("PivotBatch", "isCompatible", dataName, missing(dataName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatch", "isCompatible", variableNames, missing(variableNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      private$p_parentPivot$message("PivotBatch$isCompatible", "Checking batch compatibility...")
      bIsCompatible <- FALSE
      if(private$p_dataName==dataName) {
        if(is.null(private$p_variableNames)&&is.null(variableNames)) {
          bIsCompatible <- TRUE
        }
        else if((!is.null(private$p_variableNames))&&(!is.null(variableNames))) {
          if(length(private$p_variableNames)==length(variableNames)) {
            intrsct <- intersect(private$p_variableNames, variableNames)
            if(length(private$p_variableNames)==length(intrsct)) {
              bIsCompatible <- TRUE
            }
          }
        }
      }
      private$p_parentPivot$message("PivotBatch$isCompatible", "Checked batch compatibility.")
      return(bIsCompatible)
    },
    addCompatible = function(values=NULL, calculationName=NULL, calculationGroupName=NULL) {
      checkArgument("PivotBatch", "addCompatible", values, missing(values), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
      checkArgument("PivotBatch", "addCompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatch", "addCompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotBatch$addCompatible", "Adding compatibile calculation...")
      if(!is.null(values)) {
        if(length(values)>0) {
          nms <- names(values)
          for(i in 1:length(values)) {
            private$p_values[[nms[i]]] <- union(private$p_values[[nms[i]]], values[[i]])
          }
        }
      }
      bCalcAlreadyExists <- FALSE
      if(!is.null(private$p_calculations)) {
        if(length(private$p_calculations)>0) {
          for(i in 1:length(private$p_calculations)) {
            calc <- private$p_calculations[[i]]
            if(calc["calculationGroupName"]==calculationGroupName) {
              if(calc["calculationName"]==calculationName) {
                bCalcAlreadyExists <- TRUE
                break
              }
            }
          }
        }
      }
      if(!bCalcAlreadyExists) {
        calc <- c(calculationGroupName=calculationGroupName, calculationName=calculationName)
        private$p_calculations[[length(private$p_calculations)+1]] <- calc
      }
      private$p_compatibleCount <- private$p_compatibleCount+1
      private$p_parentPivot$message("PivotBatch$addCompatible", "Added compatibile calculation.")
    }
  ),
  active = list(
    batchId = function(value) { return(invisible(private$p_batchId)) },
    compatibleCount = function(value) { return(invisible(private$p_compatibleCount)) },
    asString = function(value) {
      vstr <- ""
      if(length(private$p_variableNames)==0) {
        vstr <- "0 VARS: []"
      }
      else {
        if(length(private$p_variableNames)==1) {
          vstr <- paste0("1 VAR: [", private$p_variableNames, "]")
        }
        else {
          vstr <- paste0(length(private$p_variableNames), " VARS: [",
                         paste(private$p_variableNames, sep="", collapse=", "), "]")
        }
      }
      cstr <- ""
      calcCount <- 0
      if(length(private$p_calculations)>0) {
        for(i in 1:length(private$p_calculations)) {
          calc <- private$p_calculations[[i]]
          calcCount <- calcCount + 1
          if(nchar(cstr)>0) cstr <- paste0(cstr, ", ", calc["calculationGroupName"], ":", calc["calculationName"])
          else cstr <- paste0(calc["calculationGroupName"], ":", calc["calculationName"])
        }
      }
      bstr <- paste0("BATCH ", private$p_batchId, ": DATA: ", private$p_dataName, ", ",
                     vstr, ", ",
                     ifelse(calcCount==1, "1 CALC: [", paste0(calcCount, " CALCS: [")), cstr, "] ",
                     " => ", private$p_compatibleCount, " CELL CALC", ifelse(private$p_compatibleCount==1, "", "S"))
      return(bstr)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_batchId = 0,
    p_dataName = NULL,            # the name of the data frame
    p_variableNames = NULL,       # a character vector specifying the grain of the calculation
    p_values = NULL,              # a list, where the element names are variable names, and the elements are lists of values
    p_calculations = NULL,        # a list, where each list element is a two element character vector (vector element names are calculationGroupName and calculationName)
    p_compatibleCount = 0
  )
)
