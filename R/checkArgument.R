#' Perform basic checks on a function argument.
#'
#' \code{checkArgument} is a utility function that provides basic assurances
#' about function argument values and generates standardised error messages when
#' invalid values are encountered.
#'
#' @param argumentCheckMode A number between 0 and 4 specifying the checks to
#'   perform.
#' @param checkDataTypes A logical value specifiying whether the data types
#'   should be checked when argumentCheckMode=3
#' @param className The name of the calling class, for inclusion in error
#'   messages.
#' @param methodName The name of the calling method, for inclusion in error
#'   messages.
#' @param argumentValue The value to check.
#' @param isMissing Whether the argument is missing in the calling function.
#' @param allowMissing Whether missing values are permitted.
#' @param allowNull Whether null values are permitted.
#' @param allowedClasses The names of the allowed classes for argumentValue.
#' @param mustBeAtomic Whether the argument value must be atomic.
#' @param allowedListElementClasses For argument values that are lists(), the
#'   names of the allowed classes for the elements in the list.
#' @param listElementsMustBeAtomic For argument values that are lists(), whether
#'   the list elements must be atomic.
#' @param allowedValues For argument values that must be one value from a set
#'   list, the list of allowed values.
#' @param minValue For numerical values, the lowest allowed value.
#' @param maxValue For numerical values, the highest allowed value.
#' @param maxLength For character values, the maximum allowed length (in
#'   characters) of the value.
#' @return No return value.  If invalid values are encountered, the
#'   \code{stop()} function is used to interrupt execution.

checkArgument <- function(argumentCheckMode, checkDataTypes, className, methodName, argumentValue, isMissing, # no point putting "=NULL" for these args, as if
                          # they aren't present then there isn't enough information to generate a meaningful error message anyway
                          allowMissing=FALSE, allowNull=FALSE, allowedClasses=NULL, mustBeAtomic=FALSE, allowedListElementClasses=NULL, listElementsMustBeAtomic=FALSE,
                          allowedValues=NULL, minValue=NULL, maxValue=NULL, maxLength=NULL) {
  if(argumentCheckMode==0) return(invisible())
  argumentName <- substitute(argumentValue)
  if(is.null(className)||(length(className)==0)) classPrefix <- ""
  else classPrefix <- paste0(className, "$")
  if(isMissing&(!allowMissing)) stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be specified"), call. = FALSE)
  if(is.null(argumentValue)&&(!allowNull)) stop(paste0(classPrefix, methodName, "():  ", argumentName, " must not be null"), call. = FALSE)
  if(argumentCheckMode==1) return(invisible())
  if((argumentCheckMode==4)||((argumentCheckMode==3)&&(checkDataTypes==TRUE))) {
    if((!is.null(argumentValue))&&(!is.null(allowedClasses))&&(length(allowedClasses)>0)) {
      if(length(intersect(allowedClasses, class(argumentValue))) == 0) {
        if(length(allowedClasses) > 1) {
          stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be one of the following types: (",
                      paste(allowedClasses, sep="", collapse = ", "), ").  Type encountered: ",
                      paste(class(argumentValue), collapse=", "), "."), call. = FALSE)
        }
        else {
          stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be of type ", allowedClasses, ".  ",
                      "Type encountered: ", paste(class(argumentValue), sep="", collapse=", "), "."), call. = FALSE)
        }
      }
      if("list" %in% allowedClasses) {
        if(!is.null(allowedListElementClasses)) {
          invalidTypes <- list()
          nonAtomicTypes <- list()
          if(length(argumentValue)>0) {
            for(i in 1:length(argumentValue))
            {
              if(is.null(argumentValue[[i]])) next
              if(length(allowedListElementClasses)>0) {
                elementTypes <- class(argumentValue[[i]])
                if(length(intersect(allowedListElementClasses, elementTypes)) == 0) { invalidTypes[[length(invalidTypes)+1]] <- elementTypes }
              }
              if(listElementsMustBeAtomic==TRUE) {
                if(!is.atomic(argumentValue[[i]])) nonAtomicTypes[[length(nonAtomicTypes)+1]] <- class(argumentValue[[i]])
              }
            }
            if(length(invalidTypes)>0)
              stop(paste0(classPrefix, methodName, "():  [", paste(unlist(invalidTypes), collapse=", "), "] is/are invalid data types for the ", argumentName,
                          " argument. Elements of the ", argumentName, " list must be one of the following types: [",
                          paste(allowedListElementClasses, collapse=", "), "]"), call. = FALSE)
            if(length(nonAtomicTypes>0))
              stop(paste0(classPrefix, methodName, "():  [", paste(unlist(invalidTypes), collapse=", "), "] is/are invalid data types for the ", argumentName,
                          " argument. Elements of the ", argumentName, " list must be atomic."), call. = FALSE)
          }
        }
      }
    }
    if((mustBeAtomic==TRUE)&&!(is.atomic(argumentValue))) {
      stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be one of the atomic data types"), call. = FALSE)
    }
  }
  if(argumentCheckMode>=2) {
    if(!is.null(allowedValues)) {
      invalidValues <- setdiff(argumentValue, allowedValues)
      if(length(invalidValues)>0)
        stop(paste0(classPrefix, methodName, "():  [", paste(invalidValues, collapse=", "), "] is/are invalid values for the ", argumentName,
                    " argument. ", argumentName, " must be one of the following values: [", paste(allowedValues, collapse=", "), "]"), call. = FALSE)
    }
    if((!is.null(minValue))&&(!is.null(argumentValue))) {
      if(any(argumentValue < minValue)) {
        stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be greater than or equal to ", minValue), call. = FALSE)
      }
    }
    if((!is.null(maxValue))&&(!is.null(argumentValue))) {
      if(any(argumentValue > maxValue)) {
        stop(paste0(classPrefix, methodName, "():  ", argumentName, " must be less than or equal to ", maxValue), call. = FALSE)
      }
    }
    if(!is.null(allowedClasses)) {
      if("character" %in% allowedClasses) {
        if(!is.null(maxLength)) {
          if(any(nchar(argumentValue)>maxLength)) {
            stop(paste0(classPrefix, methodName, "():  ", argumentName, " must have length less than or equal to ", maxLength, " characters"), call. = FALSE)
          }
        }
      }
    }
  }
  return(invisible())
}
