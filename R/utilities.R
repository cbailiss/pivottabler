#' Check whether a text value is present.
#'
#' \code{isTextValue} is a utility function returns TRUE only when a text value
#' is present.  NULL, NA, character(0) and "" all return FALSE.
#'
#' @param value The value to check.
#' @return TRUE if a non-blank text value is present.

isTextValue <- function(value) {
  if(is.null(value)) return(FALSE)
  l <- length(value)
  if(l==0) return(FALSE)
  else if(l==1) {
    if(is.na(value)) return(FALSE)
    if(nchar(value)==0) return(FALSE)
    else return(TRUE)
  }
  else {
    if(sum(nchar(value), na.rm=TRUE)==0) return(FALSE)
    else return(TRUE)
  }
}

#' Check whether a numeric value is present.
#'
#' \code{isNumericValue} is a utility function returns TRUE only when a numeric value
#' is present.  NULL, NA, numeric(0) and integer(0) all return FALSE.
#'
#' @param value The value to check.
#' @return TRUE if a numeric value is present.

isNumericValue <- function(value) {
  if(is.null(value)) return(FALSE)
  l <- length(value)
  if(l==0) return(FALSE)
  else {
    if(length(value[is.na(value)])>0) return(FALSE)
    if(is.numeric(value)) return(TRUE)
    else return(FALSE)
  }
}

#' Check whether a text value is present in another text value.
#'
#' \code{containsText} is a utility function returns TRUE if one text value is
#' present in another.  Case sensitive.  If textToSearch is a vector, returns
#' TRUE if any element contains textToFind.
#'
#' @param textToSearch The value to be searched.
#' @param textToFind The value to find.
#' @return TRUE if the textToFind value is found.

containsText <- function(textToSearch, textToFind) {
  if(!isTextValue(textToSearch)) return(FALSE)
  if(!isTextValue(textToFind)) return(FALSE)
  r <- regexpr(textToFind, textToSearch, fixed=TRUE)
  return((length(r[r!=-1]))>0)
}

#' Handle an identifier that may be illegal (e.g. containing spaces).
#'
#' \code{processIdentifier} is a utility function that wraps an illegal
#' identifier in backticks.
#'
#' @param identifier The identifier that may be illegal.
#' @return The identifier wrapped in backticks (if illegal) or unchanged.

processIdentifier <- function(identifier) {
  if(is.null(identifier)) return(NULL)
  id <- trimws(identifier)
  if (startsWith(id, "`") && endsWith(id, "`")) return(identifier)
  if (make.names(identifier) == identifier) return(identifier)
  else return(paste0("`", identifier, "`"))
}

#' Handle identifiers that may be illegal (e.g. containing spaces).
#'
#' \code{processIdentifiers} is a utility function that wraps illegal
#' identifiers in backticks.
#'
#' @param identifiers The identifiers that may be illegal.
#' @return The identifiers wrapped in backticks (if illegal) or unchanged.

processIdentifiers <- function(identifiers) {
  if(is.null(identifiers)) return(NULL)
  return(sapply(identifiers, processIdentifier, USE.NAMES=FALSE))
}
