#' A class that provides summary statistics for batch calculations.
#'
#' The PivotBatchStatistics class contains a set of summary statistics that
#' track how many calculations are batch compatible/incompatible.
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
#' @field asString A text description of the batch statistics.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new Pivot Batch Statistics.}
#'
#'   \item{\code{reset()}}{Clear the batch statistics.}
#'   \item{\code{incrementNoData()}}{Increment the noData count for a batch.}
#'   \item{\code{incrementCompatible()}}{Increment the compatible count for a batch.}
#'   \item{\code{incrementIncompatible()}}{Increment the incompatible count for a batch.}
#' }

PivotBatchStatistics <- R6::R6Class("PivotBatchStatistics",
  public = list(
    initialize = function(parentPivot=NULL) {
      checkArgument("PivotBatchStatistics", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotBatchStatistics$new", "Creating new batch statistics...")
      self$reset()
      private$p_parentPivot$message("PivotBatchStatistics$new", "Created new batch statistics.")
    },
    reset = function() {
      private$p_parentPivot$message("PivotBatchStatistics$new", "Resetting batch statistics...")
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
      private$p_parentPivot$message("PivotBatchStatistics$new", "Reset batch statistics.")
    },
    incrementNoData = function(calculationName=NULL, calculationGroupName=NULL) {
      checkArgument("PivotBatchStatistics", "incrementNoData", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchStatistics", "incrementNoData", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_statistics[[calculationGroupName]][[calculationName]]["noData"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["noData"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    },
    incrementCompatible = function(calculationName=NULL, calculationGroupName=NULL) {
      checkArgument("PivotBatchStatistics", "incrementCompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchStatistics", "incrementCompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_statistics[[calculationGroupName]][[calculationName]]["compatible"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["compatible"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    },
    incrementIncompatible = function(calculationName=NULL, calculationGroupName=NULL) {
      checkArgument("PivotBatchStatistics", "incrementIncompatible", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      checkArgument("PivotBatchStatistics", "incrementIncompatible", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_statistics[[calculationGroupName]][[calculationName]]["incompatible"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["incompatible"]+1
      private$p_statistics[[calculationGroupName]][[calculationName]]["total"] <- private$p_statistics[[calculationGroupName]][[calculationName]]["total"]+1
    }
  ),
  active = list(
    asString = function(value) {
      str <- ""
      if(is.null(private$p_statistics)) return(NULL)
      if(length(private$p_statistics)>0) {
        grpNms <- names(private$p_statistics)
        for(i in 1:length(private$p_statistics)) {
          calcGrp <- private$p_statistics[[i]]
          if(is.null(calcGrp)) next
          calcNms <- names(private$p_statistics[[i]])
          for(j in 1:length(private$p_statistics)) {
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
