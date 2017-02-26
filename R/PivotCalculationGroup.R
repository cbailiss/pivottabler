PivotCalculationGroup <- R6::R6Class("PivotCalculationGroup",
  public = list(
    initialize = function(parentPivot, calculationGroupName=NULL) {
      checkArgument("PivotCalculationGroup", "initialize", parentPivot, missing(parentPivot), allowMissing=FALSE, allowNull=FALSE, allowedClasses="PivotTable")
      checkArgument("PivotCalculationGroup", "initialize", calculationGroupName, missing(calculationGroupName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot <- parentPivot
      private$p_parentPivot$message("PivotCalculationGroup$new", "Creating new Pivot Calculation Group...")
      private$p_name <- calculationGroupName
      private$p_calculations <- list()
      private$p_parentPivot$message("PivotCalculationGroup$new", "Created new Pivot Calculation Group.")
    },
    isExistingCalculation = function(calculationName) {
      checkArgument("PivotCalculationGroup", "isExistingCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotCalculationGroup$isExistingCalculation", "Checking calculation exists...",
                                    list(calculationName=calculationName))
      calcExists <- calculationName %in% names(private$p_calculations)
      private$p_parentPivot$message("PivotCalculationGroup$isExistingCalculation", "Checked calculation exists.")
      return(calcExists)
    },
    getCalculation = function(calculationName) {
      checkArgument("PivotCalculationGroup", "getCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      private$p_parentPivot$message("PivotCalculationGroup$getCalculation", "Getting calculation...",
                                    list(calculationName=calculationName))
      calculation <- private$p_calculations[[calculationGroupName]]
      if(is.null(calculation)) {
        stop(paste0("PivotCalculationGroups$getCalculation(): No calculation exists with the name '", calculationName, "'"), call. = FALSE)
      }
      private$p_parentPivot$message("PivotCalculationGroup$getCalculation", "Got calculation.")
      return(calculation)
    },
    defineCalculation = function(calculationName=NULL, caption=NULL, visible=TRUE, displayOrder=NULL,
                         filters=NULL, format=NULL, dataName=NULL, type="summary", executionOrder=NULL,
                         summariseExpression=NULL, calculationExpression=NULL, calculationFunction=NULL) {
     checkArgument("PivotCalculationGroup", "defineCalculation", calculationName, missing(calculationName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", caption, missing(caption), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", visible, missing(visible), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
     checkArgument("PivotCalculationGroup", "defineCalculation", displayOrder, missing(displayOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCalculationGroup", "defineCalculation", filters, missing(filters), allowMissing=TRUE, allowNull=TRUE, allowedClasses="PivotFilters")
     checkArgument("PivotCalculationGroup", "defineCalculation", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", dataName, missing(dataName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", type, missing(type), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("summary", "calculation", "function"))
     checkArgument("PivotCalculationGroup", "defineCalculation", executionOrder, missing(executionOrder), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
     checkArgument("PivotCalculationGroup", "defineCalculation", summariseExpression, missing(summariseExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", calculationExpression, missing(calculationExpression), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     checkArgument("PivotCalculationGroup", "defineCalculation", calculationFunction, missing(calculationFunction), allowMissing=TRUE, allowNull=TRUE, allowedClasses="function")
      private$p_parentPivot$message("PivotCalculationGroup$defineCalculation", "Defining calculation...")
      if(calculationName %in% names(private$p_calculations)) {
        stop(paste0("PivotCalculationGroup$defineCalculation():  A Calculation already exists",
                    " in the Calculation Group with the name '", calculationName, "'.  calculationName must unique."), call. = FALSE)
      }
      if(is.null(displayOrder)) displayOrder <- length(private$p_calculations) + 1
      if(is.null(executionOrder)) executionOrder <- length(private$p_calculations) + 1
      calculation <- PivotCalculation$new(private$p_parentPivot, calculationName=calculationName, caption=caption,
                                          visible=visible, displayOrder=displayOrder, filters=filters, format=format,
                                          dataName=dataName, type=type, executionOrder=executionOrder,
                                          summariseExpression=summariseExpression,
                                          calculationExpression=calculationExpression, calculationFunction=calculationFunction)
      private$p_calculations[[calculationName]] <- calculation
      private$p_parentPivot$message("PivotCalculationGroup$defineCalculation", "Defined calculation.")
      return(calculation)
    },
    asList = function() {
      lst <- list()
      if(length(private$p_calculations) > 0) {
        calcNames <- names(private$p_calculations)
        for (i in 1:length(private$p_calculations)) {
          calcName <- calcNames[i]
          lst[[calcName]] = private$p_calculations[[calcName]]$asList()
        }
      }
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(seperator=", ") {
       checkArgument("PivotCalculationGroup", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       cstr <- ""
       if(length(private$p_calculations)>0) {
         for(i in 1:length(private$p_calculations)) {
           calc <- private$p_calculations[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           cstr <- paste0(cstr, sep, calc$asString())
         }
       }
       return(cstr)
    }
  ),
  active = list(
    calculationGroupName = function(value) { return(private$p_name) },
    count = function(value) { return(length(private$p_calculations)) },
    calculations = function(value) { return(private$p_calculations) },
    visibleCount = function(value) {
      cnt <- 0
      for(i in 1:length(private$p_calculations)) {
        if(private$p_calculations[[i]]$visible==TRUE) cnt<- cnt + 1
      }
      return(cnt)
    },
    visibleCalculations = function(value) {
      visibleCalcs <- list()
      for(i in 1:length(private$p_calculations)) {
        if(private$p_calculations[[i]]$visible==TRUE) {
          index <- length(visibleCalcs) + 1
          visibleCalcs[[index]] <- private$p_calculations[[i]]
        }
      }
      return(visibleCalcs)
    }
  ),
  private = list(
    p_parentPivot = NULL,
    p_name = NULL,
    p_calculations = NULL
  )
)
