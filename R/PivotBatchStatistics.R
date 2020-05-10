
#' R6 class that provides summary statistics for batch calculations.
#'
#' @description
#' The `PivotBatchStatistics` class contains a set of summary statistics that
#' track how many calculations are batch compatible/incompatible.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the pivot table.
#' # It is not intended to be created outside of the pivot table.

PivotBatchStatistics <- R6::R6Class("PivotBatchStatistics",
  public = list(

    #' @description
    #' Create a new `PivotBatchStatistics` object.
    #' @param parentPivot The pivot table that this `PivotBatchStatistics`
    #' instance belongs to.
    #' @return A new `PivotBatchStatistics` object.
    initialize = function(parentPivot=NULL) {
      if(parentPivot$argumentCheckMode > 0) {
        checkArgument(parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      }
      private$p_parentPivot <- parentPivot
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchStatistics$new", "Creating new batch statistics...")
      self$reset()
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchStatistics$new", "Created new batch statistics.")
    },

    #' @description
    #' Clear the batch statistics.
    #' @return No return value.
    reset = function() {
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchStatistics$reset", "Resetting batch statistics...")
      private$p_statistics <- list()
      calcGrps <- private$p_parentPivot$calculationGroups
      if(!is.null(calcGrps)) {
        if(length(calcGrps$groups)>0) {
          for(i in 1:length(calcGrps$groups)) {
            calcGrp <- calcGrps$groups[[i]]
            calcs <- calcGrp$calculations
            lstCalcs <- list()
            if(!is.null(calcs)) {
              if(length(calcs)>0) {
                for(j in 1:length(calcs)) {
                  calc <- calcs[[j]]
                  statsCalc <- c(total=0, noData=0, incompatible=0, compatible=0)
                  lstCalcs[[calc$calculationName]] <- statsCalc
                }
              }
            }
            private$p_statistics[[calcGrp$calculationGroupName]] <- lstCalcs
          }
        }
      }
      if(private$p_parentPivot$traceEnabled==TRUE) private$p_parentPivot$trace("PivotBatchStatistics$reset", "Reset batch statistics.")
    },

    #' @description
    #' Increment the noData count for a batch.
    #' @param calculationName The name of the calculation to increment the count for.
    #' @param calculationGroupName The name of the calculation group for the calculation.
    #' @return No return value.
    incrementNoData = function(calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementNoData", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementNoData", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      private$p_statistics[[calculationGroupName]][[calculationName]]["noData"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["noData"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    },

    #' @description
    #' Increment the compatible count for a batch.
    #' @param calculationName The name of the calculation to increment the count for.
    #' @param calculationGroupName The name of the calculation group for the calculation.
    #' @return No return value.
    incrementCompatible = function(calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementCompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementCompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      private$p_statistics[[calculationGroupName]][[calculationName]]["compatible"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["compatible"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    },

    #' @description
    #' Increment the incompatible count for a batch.
    #' @param calculationName The name of the calculation to increment the count for.
    #' @param calculationGroupName The name of the calculation group for the calculation.
    #' @return No return value.
    incrementIncompatible = function(calculationName=NULL, calculationGroupName=NULL) {
      if(private$p_parentPivot$argumentCheckMode > 0) {
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementIncompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentPivot$argumentCheckMode, FALSE, "PivotBatchStatistics", "incrementIncompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      private$p_statistics[[calculationGroupName]][[calculationName]]["incompatible"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["incompatible"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    }
  ),
  active = list(

    #' @field asString A text description of the batch statistics.
    asString = function(value) {
      str <- ""
      if(is.null(private$p_statistics)) return(NULL)
      if(length(private$p_statistics)>0) {
        grpNms <- names(private$p_statistics)
        for(i in 1:length(private$p_statistics)) {
          calcGrp <- private$p_statistics[[i]]
          if(is.null(calcGrp)) next
          if(length(calcGrp)==0) next
          calcNms <- names(private$p_statistics[[i]])
          for(j in 1:length(calcGrp)) {
            calc <- private$p_statistics[[i]][[j]]
            if(is.null(calc)) next
            noData <- calc["noData"]
            compatible <- calc["compatible"]
            incompatible <- calc["incompatible"]
            total <- calc["total"]
            compatPerc <- round(compatible / (compatible+incompatible) * 100, 1)
            cstr <- paste0(grpNms[i], ":", calcNms[j], ": compatible%=", compatPerc, "%, ",
                           "compatible=", compatible, ", incompatible=", incompatible, ", ",
                           "noData=", noData, ", total=", total)
            if(nchar(str)>0) str <- paste0("\n", cstr)
            else str <- cstr
          }
        }
      }
      return(str)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_statistics = NULL
  )
)
