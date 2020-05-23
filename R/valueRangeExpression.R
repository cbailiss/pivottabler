#
# VALUE RANGE EXPRESSION UTILITY FUNCTIONS
#

vreIsSingleValue <-function(vre) {
  # already numeric
  if(is.numeric(vre)) return(TRUE)
  # numeric as text
  rgex <- "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$"
  if(isTRUE(grep(rgex, vre, value=FALSE)==1)) return(TRUE)
  # otherwise, check the notation
  return(startsWith(vre, "v="))
}

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

vreIsSimpleNumericRange <- function(vre) {
  rgex <- "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}<{1}={0,1}v{1}<{1}={0,1}[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$"
  return(isTRUE(grep(rgex, vre, value=FALSE)==1))
}

vreConvertSimpleNumericRange <- function(vre) {
  if(!vreIsSimpleNumericRange(vre)) stop("vreConvertSimpleNumericRange():  The specified value range expression is not a simple numeric value range in the form value1<=v<value2.")
  return(gsub("v", "v && v", vre))
}

vreIsEqual <- function(value1, value2) {
  if(is.null(value1)||(length(value1)==0)) return(FALSE)
  if(is.null(value2)||(length(value2)==0)) return(FALSE)
  tolerance <- sqrt(.Machine$double.eps) # follow same alogorith used by base::all.equal()
  return(abs(value1-value2) <= tolerance)
}

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

vreScaleNumber <- function(n1, n2, vMin, vMax, value) {
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
    return(n1+((v-vMin)/(vMax-vMin)*(n2-n1)))
  }
  else {
    return(n1-((v-vMin)/(vMax-vMin)*(n1-n2)))
  }
}

vreHexToClr <- function(hexclr) {
  clr <- list()
  clr$r <- strtoi(paste0("0x", substr(hexclr, 2, 3)))
  clr$g <- strtoi(paste0("0x", substr(hexclr, 4, 5)))
  clr$b <- strtoi(paste0("0x", substr(hexclr, 6, 7)))
  return(clr)
}

vreScale2Colours <- function(clr1, clr2, vMin, vMax, value) {
  r <- round(vreScaleNumber(clr1$r, clr2$r, vMin, vMax, value))
  g <- round(vreScaleNumber(clr1$g, clr2$g, vMin, vMax, value))
  b <- round(vreScaleNumber(clr1$b, clr2$b, vMin, vMax, value))
  return(paste0("#",
                format(as.hexmode(r), width=2),
                format(as.hexmode(g), width=2),
                format(as.hexmode(b), width=2)))
}

# x <- list(list(a=2, b=3), list(a=1, b=4), list(a=0, b=12), list(a=8, b=5))
# fx <- function(t) { t$a }
# x <- x[order(sapply(x, fx))]
