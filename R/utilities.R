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
    if(anyNA(value)) return(FALSE)
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
    if(anyNA(value)) return(FALSE)
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

#' Should the current value be skipped during export?
#'
#' \code{skipExportingValue} is a utility function that returns true if
#' the current value should be skipped when exporting.
#'
#' @param rawValue The value to check.
#' @param exportOptions A list of options controlling export behaviour.
#' @return TRUE or FALSE indicating whether the current value should be
#' skipped.

skipExportingValue <- function(rawValue, exportOptions) {
  if(is.null(rawValue)) return(invisible(TRUE))
  if(is.null(exportOptions)) return(invisible(FALSE))
  if((NA %in% rawValue) && ("skipNA" %in% names(exportOptions)) && (exportOptions$skipNA==TRUE)) return(invisible(TRUE))
  if((NaN %in% rawValue) && ("skipNAN" %in% names(exportOptions)) && (exportOptions$skipNAN==TRUE)) return(invisible(TRUE))
  if((-Inf %in% rawValue) && ("skipNegInf" %in% names(exportOptions)) && (exportOptions$skipNegInf==TRUE)) return(invisible(TRUE))
  if((Inf %in% rawValue) && ("skipPosInf" %in% names(exportOptions)) && (exportOptions$skipPosInf==TRUE)) return(invisible(TRUE))
  return(invisible(FALSE))
}

#' Replace the current value with a placeholder during export.
#'
#' \code{exportValueAs} is a utility function that returns either the
#' original value or a replacement placeholder value for export.
#'
#' @param rawValue The raw value to check.
#' @param formattedValue The formatted value to be exported.
#' @param exportOptions A list of options controlling export behaviour.
#' @param blankValue The 'placeholder' value to be exported when skipping the value.
#' @return Either the original value or a placeholder value.

exportValueAs <- function(rawValue, formattedValue, exportOptions, blankValue=character(0)) {
  if(is.null(rawValue)) {
    if(is.null(formattedValue)) return(invisible(blankValue))
    else return(invisible(formattedValue))
  }
  if(is.null(exportOptions)) return(invisible(formattedValue))
  if(NA %in% rawValue) {
    if(("skipNA" %in% names(exportOptions)) && (exportOptions$skipNA==TRUE)) return(invisible(blankValue))
    if("exportNAAs" %in% names(exportOptions)) return(invisible(exportOptions$exportNAAs))
  }
  if(NaN %in% rawValue) {
    if(("skipNaN" %in% names(exportOptions)) && (exportOptions$skipNaN==TRUE)) return(invisible(blankValue))
    if("exportNaNAs" %in% names(exportOptions)) return(invisible(exportOptions$exportNaNAs))
  }
  if(-Inf %in% rawValue) {
    if(("skipNegInf" %in% names(exportOptions)) && (exportOptions$skipNegInf==TRUE)) return(invisible(blankValue))
    if(("exportNegInfAs" %in% names(exportOptions))) return(invisible(exportOptions$exportNegInfAs))
  }

  if(Inf %in% rawValue) {
    if(("skipPosInf" %in% names(exportOptions)) && (exportOptions$skipPosInf==TRUE)) return(invisible(blankValue))
    if(("exportPosInfAs" %in% names(exportOptions))) return(invisible(exportOptions$exportPosInfAs))
  }
  return(invisible(formattedValue))
}

