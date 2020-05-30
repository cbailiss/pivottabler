#' Convert a value of 1 to a NULL value.
#'
#' \code{oneToNull} is a utility function that returns NULL when a value of
#' 0 or 1 is passed to it, otherwise it returns the original value.
#'
#' @param value The value to check.
#' @param convertOneToNULL TRUE to convert 1 to NULL.
#' @return NULL if value is 0 or 1, otherwise value.

oneToNULL <- function(value, convertOneToNULL) {
  if(!convertOneToNULL) return(value)
  else if(is.null(value)) return(NULL)
  else if(value==0) return(NULL)
  else if(value==1) return(NULL)
  else return(value)
}

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

#' Clean the arguments specified for an outline group
#'
#' \code{cleanOutlineArg} checks values and provides defaults.
#'
#' @param pvt The pivot table.
#' @param outline Either a logical value (TRUE to use the default
#' outline settings) or a list specifying outline settings.
#' @param defaultCaption The default caption of the outline group.
#' @param defaultIsEmpty Specify whether the outline group is
#' empty or contains a value (typically a sub-total)
#' @return A listed containing checked/cleaned outline group
#' settings.

cleanOutlineArg = function(pvt, outline=NULL, defaultCaption="{value}", defaultIsEmpty=TRUE) { # checks values and provides defaults for settings (so following code does not need is.null checks)
  if(pvt$argumentCheckMode > 0) {
    checkArgument(pvt$argumentCheckMode, FALSE, "(global)", "cleanOutlineArg", outline, missing(outline), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("logical", "list"))
    checkArgument(pvt$argumentCheckMode, FALSE, "(global)", "cleanOutlineArg", defaultCaption, missing(defaultCaption), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
    checkArgument(pvt$argumentCheckMode, FALSE, "(global)", "cleanOutlineArg", defaultIsEmpty, missing(defaultIsEmpty), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  }
  if(pvt$traceEnabled==TRUE) pvt$trace("PivotDataGroup$cleanOutlineArg", "Cleaning outline argument...")
  if("logical" %in% class(outline)) {
    if(isTRUE(outline)) outline <- list()
    else outline <- NULL
  }
  clean <- list()
  # global switch
  if(is.null(outline)) {
    clean$outline <- FALSE
    return(clean)
  }
  clean$outline <- TRUE
  # setting: caption
  if(is.null(outline$caption)) clean$caption <- defaultCaption
  else {
    if(!("character" %in% class(outline$caption))) stop("cleanOutlineArg(): The caption for the outline data group must be a character value.", call. = FALSE)
    clean$caption <- outline$caption
  }
  # setting: isEmpty
  if(is.null(outline$isEmpty)) clean$isEmpty <- defaultIsEmpty
  else {
    if(!("logical" %in% class(outline$isEmpty))) stop("cleanOutlineArg(): The isEmpty value for the outline data group must be a logical (TRUE/FALSE) value.", call. = FALSE)
    clean$isEmpty <- outline$isEmpty
  }
  # setting: mergeSpace
  allowedMergeSpaceValues <- c("doNotMerge", "dataGroupsOnly", "cellsOnly", "dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2")
  if(is.null(outline$mergeSpace)) clean$mergeSpace <- ifelse(defaultIsEmpty, "dataGroupsAndCellsAs2", "dataGroupsOnly")
  else {
    if(!("character" %in% class(outline$mergeSpace))) stop("cleanOutlineArg(): The mergeSpace value for the outline data group must be a character value.", call. = FALSE)
    if(!(outline$mergeSpace %in% allowedMergeSpaceValues)) stop("cleanOutlineArg(): The mergeSpace value for the outline data group must be one of the following values: doNotMerge, dataGroupsOnly, cellsOnly, dataGroupsAndCellsAs1, dataGroupsAndCellsAs2.", call. = FALSE)
    clean$mergeSpace <- outline$mergeSpace
  }
  # setting: styling
  allowedStylingValues <- c("plain", "outline")
  if(is.null(outline$styling)) clean$styling <- "outline"
  else {
    if(!("character" %in% class(outline$styling))) stop("cleanOutlineArg(): The styling value for the outline data group must be a character value.", call. = FALSE)
    if(!(outline$styling %in% allowedStylingValues)) stop("cleanOutlineArg(): The styling value for the outline data group must be one of the following values: plain, outline.", call. = FALSE)
    clean$styling <- outline$styling
  }
  # setting: groupStyleName (allow null)
  if(!is.null(outline$groupStyleName)) {
    if(!("character" %in% class(outline$groupStyleName))) stop("cleanOutlineArg(): The groupStyleName for the outline data group must be a character value.", call. = FALSE)
    clean$groupStyleName <- outline$groupStyleName
  }
  # setting: groupStyleDeclarations (allow null)
  if(!is.null(outline$groupStyleDeclarations)) {
    if(!("list" %in% class(outline$groupStyleDeclarations))) stop("cleanOutlineArg(): The groupStyleDeclarations for the outline data group must a list.", call. = FALSE)
    clean$groupStyleDeclarations <- outline$groupStyleDeclarations
  }
  # setting: cellStyleName (allow null)
  if(!is.null(outline$cellStyleName)) {
    if(!("character" %in% class(outline$cellStyleName))) stop("cleanOutlineArg(): The cellStyleName for the outline data group must be a character value.", call. = FALSE)
    clean$cellStyleName <- outline$cellStyleName
  }
  # setting: cellStyleDeclarations (allow null)
  if(!is.null(outline$cellStyleDeclarations)) {
    if(!("list" %in% class(outline$cellStyleDeclarations))) stop("cleanOutlineArg(): The cellStyleDeclarations for the outline data group must a list.", call. = FALSE)
    clean$cellStyleDeclarations <- outline$cellStyleDeclarations
  }
  # check mergeSpace and isEmpty are compatible
  if(!(clean$isEmpty)) {
    if(clean$mergeSpace=="cellsOnly") clean$mergeSpace <- "doNotMerge"
    else if(clean$mergeSpace %in% c("dataGroupsAndCellsAs1", "dataGroupsAndCellsAs2")) clean$mergeSpace <- "dataGroupsOnly"
  }
  # setting: nocgApplyOutlineStyling (allow null)
  if(!is.null(outline$nocgApplyOutlineStyling)) {
    if(!("logical" %in% class(outline$nocgApplyOutlineStyling))) stop("cleanOutlineArg(): The nocgApplyOutlineStyling for the outline data group must be a logical value.", call. = FALSE)
    clean$nocgApplyOutlineStyling <- outline$nocgApplyOutlineStyling
  }
  # setting: nocgGroupStyleName (allow null)
  if(!is.null(outline$nocgGroupStyleName)) {
    if(!("character" %in% class(outline$nocgGroupStyleName))) stop("cleanOutlineArg(): The nocgGroupStyleName for the outline data group must be a character value.", call. = FALSE)
    clean$nocgGroupStyleName <- outline$nocgGroupStyleName
  }
  # setting: nocgGroupStyleDeclarations (allow null)
  if(!is.null(outline$nocgGroupStyleDeclarations)) {
    if(!("list" %in% class(outline$nocgGroupStyleDeclarations))) stop("cleanOutlineArg(): The nocgGroupStyleDeclarations for the outline data group must a list.", call. = FALSE)
    clean$nocgGroupStyleDeclarations <- outline$nocgGroupStyleDeclarations
  }
  # setting: nocgCellStyleName (allow null)
  if(!is.null(outline$nocgCellStyleName)) {
    if(!("character" %in% class(outline$nocgCellStyleName))) stop("cleanOutlineArg(): The nocgCellStyleName for the outline data group must be a character value.", call. = FALSE)
    clean$nocgCellStyleName <- outline$nocgCellStyleName
  }
  # setting: nocgCellStyleDeclarations (allow null)
  if(!is.null(outline$nocgCellStyleDeclarations)) {
    if(!("list" %in% class(outline$nocgCellStyleDeclarations))) stop("cleanOutlineArg(): The nocgCellStyleDeclarations for the outline data group must a list.", call. = FALSE)
    clean$nocgCellStyleDeclarations <- outline$nocgCellStyleDeclarations
  }
  # finished
  if(pvt$traceEnabled==TRUE) pvt$trace("cleanOutlineArg", "Cleaned outline argument.")
  return(clean)
}
