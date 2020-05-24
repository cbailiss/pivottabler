#
# VALUE RANGE EXPRESSION UTILITY FUNCTIONS
#

#' Determine if a value range expression is a single value.
#'
#' \code{vreIsSingleValue} is a utility function that returns `TRUE` if the
#' specified value range expression is either numeric, a number expressed as
#' text or an expression of the form "v=" or "v==".
#'
#' @param vre The value range expression to examine.
#' @return `TRUE` if vre is a single value, `FALSE` otherwise.


vreIsSingleValue <-function(vre) {
  # already numeric
  if(is.numeric(vre)) return(TRUE)
  # numeric as text
  rgex <- "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$"
  if(isTRUE(grep(rgex, vre, value=FALSE)==1)) return(TRUE)
  # otherwise, check the notation
  return(startsWith(vre, "v="))
}

#' Read the value from a single-valued value range expression.
#'
#' \code{vreGetSingleValue} is a utility function reads the single value
#' from a value range expression (it assumes the specified
#' is either numeric, a number expressed as
#' text or an expression of the form "v=" or "v==").
#'
#' @param vre The value range expression to examine.
#' @return The value read from the expression.


vreGetSingleValue <-function(vre) {
  # already numeric
  if(is.numeric(vre)) return(vre)
  # numeric as text
  rgex <- "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$"
  if(isTRUE(grep(rgex, vre, value=FALSE)==1)) return(as.numeric(vre))
  # otherwise, check the notation
  if(!startsWith(vre, "v=")) stop("vreGetSingleValue():  The specified value range expression is not a single-valued value range.")
  value <- vre
  value <- gsub("v==", "", value)
  value <- gsub("v=", "", value)
  eval(parse(text=paste0("value<-", value)))
  return(value)
}

#' Determine if a value range expression is a simple range expression.
#'
#' \code{vreIsSingleValue} is a utility function that returns `TRUE` if the
#' specified value range expression is a simple range expression of the
#' form "value1<=v<value2", where the logical comparisons can be < or <= only
#' and the values must be numbers.
#'
#' @param vre The value range expression to examine.
#' @return `TRUE` if vre is a simple range expression, `FALSE` otherwise.

vreIsSimpleNumericRange <- function(vre) {
  rgex <- "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}<{1}={0,1}v{1}<{1}={0,1}[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$"
  return(isTRUE(grep(rgex, vre, value=FALSE)==1))
}

#' Convert a simple range expression to a standard R logical expression.
#'
#' \code{vreConvertSimpleNumericRange} is a utility function that converts
#' a simple range expression of the form "value1<=v<value2" to a standard
#' R logical expression of the form "value1<=v && v<value2".
#'
#' @param vre The value range expression to examine.
#' @return A standard R logical expression.

vreConvertSimpleNumericRange <- function(vre) {
  if(!vreIsSimpleNumericRange(vre)) stop("vreConvertSimpleNumericRange():  The specified value range expression is not a simple numeric value range in the form value1<=v<value2.")
  return(gsub("v", "v && v", vre))
}

#' Test if two numeric values are equal within tolerance.
#'
#' \code{vreIsEqual} tests whether two values are equal
#' within sqrt(.Machine$double.eps).
#'
#' @param value1 The first value to compare.
#' @param value2 The second value to compare.
#' @return `TRUE` if the two numbers are equal, `FALSE` otherwise.

vreIsEqual <- function(value1, value2) {
  if(is.null(value1)||(length(value1)==0)) return(FALSE)
  if(is.null(value2)||(length(value2)==0)) return(FALSE)
  tolerance <- sqrt(.Machine$double.eps) # follow same alogorith used by base::all.equal()
  return(abs(value1-value2) <= tolerance)
}

#' Test whether a value matches a value range expression.
#'
#' \code{vreIsMatch} tests a value (e.g. from a cell) matches
#' the criteria specified in a value range expression.
#'
#' @param vre The value range expression.
#' @param v The value.
#' @param testOnly `TRUE` if this comparison is just a test.
#' @return `TRUE` if v matches the criteria specified in the value range expression,
#' `FALSE` otherwise.

vreIsMatch <- function(vre, v, testOnly=FALSE) {
  # check a value range expression was specified
  if(is.null(vre)||(length(vre)==0)) stop("vreIsMatch():  The value range expression must be specified.")
  # if vre is a fixed value, test for equality within machine tolerance
  if(vreIsSingleValue(vre)) {
    return(vreIsEqual(vreGetSingleValue(vre), v))
  }
  # if vre is a simple range, convert it to a normal logical expression
  if(vreIsSimpleNumericRange(vre)) {
    vre <- vreConvertSimpleNumericRange(vre)
  }
  # test value
  test <- paste0("isTRUE(", vre, ")")
  testOrEval <- "evaluating"
  if(isTRUE(testOnly)) testOrEval <- "testing"
  result = tryCatch({
    eval(parse(text=test), envir=list(v=v))
  }, warning = function(w) {
    warning(paste0("vreIsMatch(): Warning when ", testOrEval, " value range expression '", vre, "', details: ", w))
    return(FALSE)
  }, error = function(e) {
    warning(paste0("vreIsMatch(): Error when ", testOrEval, ", value range expression '", vre, "', details: ", e))
    return(FALSE)
  }, finally = {
    # no cleanup needed
  })
  return(result)
}

#' Rescale a number from one range into another range.
#'
#' \code{vreScaleNumber} takes a value from one range and
#' scales it proportionally into another range.
#'
#' @param n1 The lower value of the target range.
#' @param n2 The upper value of the target range.
#' @param vMin The lower value of the source range.
#' @param vMax The upper value of the source range.
#' @param value The source value to rescale into the target range.
#' @param decimalPlaces The number of decimal places to round the result to.
#' @return The value rescaled into the target range.

vreScaleNumber <- function(n1, n2, vMin, vMax, value, decimalPlaces=3) {
  if(length(n1)==0) return(NULL)
  if(length(n2)==0) return(NULL)
  if(length(vMin)==0) return(NULL)
  if(length(vMax)==0) return(NULL)
  if(length(value)==0) return(NULL)
  if(is.na(n1)) return(NULL)
  if(is.na(n2)) return(NULL)
  if(is.na(vMin)) return(NULL)
  if(is.na(vMax)) return(NULL)
  if(is.na(value)) return(NULL)
  if(n1==n2) return(n1)
  v <- value
  if(v < vMin) v <- vMin
  if(v > vMax) v <- vMax
  if(n1<n2) {
    return(round(n1+((v-vMin)/(vMax-vMin)*(n2-n1)), decimalPlaces))
  }
  else {
    return(round(n1-((v-vMin)/(vMax-vMin)*(n1-n2)), decimalPlaces))
  }
}

#' Convert a colour in hex format (#RRGGBB) into a list.
#'
#' \code{vreHexToClr} converts a colour in hex format
#' (#RRGGBB) into a list of three element (r, g and b).
#'
#' @param hexclr The colour to convert.
#' @return The converted colour.

vreHexToClr <- function(hexclr) {
  clr <- list()
  clr$r <- strtoi(paste0("0x", substr(hexclr, 2, 3)))
  clr$g <- strtoi(paste0("0x", substr(hexclr, 4, 5)))
  clr$b <- strtoi(paste0("0x", substr(hexclr, 6, 7)))
  return(clr)
}

#' Scale a number from a range into a colour gradient.
#'
#' \code{vreScale2Colours} takes a value from a range and
#' scales it proportionally into a colour from a colour gradient.
#'
#' @param clr1 The colour representing the lower value of the target range.
#' @param clr2 The colour representing the upper value of the target range.
#' @param vMin The lower value of the source range.
#' @param vMax The upper value of the source range.
#' @param value The source value to rescale into the target range.
#' @return The value scaled into the target colour gradient.

vreScale2Colours <- function(clr1, clr2, vMin, vMax, value) {
  r <- round(vreScaleNumber(clr1$r, clr2$r, vMin, vMax, value))
  g <- round(vreScaleNumber(clr1$g, clr2$g, vMin, vMax, value))
  b <- round(vreScaleNumber(clr1$b, clr2$b, vMin, vMax, value))
  return(paste0("#",
                format(as.hexmode(r), width=2),
                format(as.hexmode(g), width=2),
                format(as.hexmode(b), width=2)))
}

