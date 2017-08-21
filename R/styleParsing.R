#' Cleans up a CSS attribute value.
#'
#' \code{cleanCssValue} is a utility function that performs some basic cleanup
#' on CSS attribute values.  Leading and trailing whitespace is removed.  The
#' CSS values "initial" and "inherit" are blocked.  The function is vectorised
#' so can be used with arrays.
#'
#' @param cssValue The value to cleanup.
#' @return The cleaned value.

cleanCssValue <- function(cssValue) {
  if(is.null(cssValue)) return(NULL)
  value <- trimws(cssValue)
  value <- ifelse(endsWith(value, ";"), substr(value, 1, nchar(value)-1), value)
  value <- value[tolower(value) != "initial"]
  value <- value[tolower(value) != "inherit"]
  return(value)
}


#' Find the first value in an array that is larger than the specified value.
#'
#' \code{getNextPosition} is a utility function that helps when parsing strings
#' that contain delimiters.
#'
#' @param positions An ordered numeric vector.
#' @param afterPosition The value to start searching after.
#' @return The first value in the array larger than afterPosition.

getNextPosition <- function(positions, afterPosition) {
  laterPositions <- positions[positions > afterPosition]
  if(length(laterPositions)==0) return(NULL)
  else return(laterPositions[1])
}


#' Split a CSS attribute value into a vector/array.
#'
#' \code{parseCssString} is a utility function that splits a string into a
#' vector/array.  The function pays attention to text qualifiers (single and
#' double quotes) so won't split if the delimiter occurs inside a value.
#'
#' @param text The text to split.
#' @param separator The field separator, default comma.
#' @param removeEmptyString TRUE to not return empty string / whitespace values.
#' @return An R vector containing the values from text split up.

parseCssString <- function(text, separator=",", removeEmptyString=TRUE) {
  cText <- trimws(text)
  if(endsWith(cText, ";")) cText <- substr(cText, 1, nchar(cText)-1)
  i <- 1
  iEnd <- nchar(cText)
  quote1 <- gregexpr("'", cText, fixed=TRUE)[[1]]
  quote2 <- gregexpr("\"", cText, fixed=TRUE)[[1]]
  sep <- gregexpr(separator, cText, fixed=TRUE)[[1]]
  ws <- c(" ", "\t", "\r", "\n")
  results <- list()
  while (i <= iEnd) {
    chr <- substr(cText, i, i)
    # message(chr)
    # skip past whitespace
    if(chr %in% ws) {
      i <- i + 1
      next
    }
    else if(chr==separator) { # comma - zero length string
      results[[length(results)+1]] <- ""
      i <- i + 1
    }
    else if(chr=="\"") { # double quotes
      # next double quotes
      j <- getNextPosition(quote2, i)
      # message(paste0("jd=", j))
      if(is.null(j)||(j<=0)) {
        # malformed, just take the rest of the string and break out of the loop
        results[[length(results)+1]] <- trimws(substr(cText, i+1, iEnd))
        break
      }
      else {
        results[[length(results)+1]] <- trimws(substr(cText, i+1, j-1))
        k <- getNextPosition(sep, j)
        if(is.null(k)||(k<=0)) break
        i <- k + 1
      }
    }
    else if(chr=="'") { # single quote
      # next single quote
      j <- getNextPosition(quote1, i)
      # message(paste0("js=", j))
      if(is.null(j)||(j<=0)) {
        # malformed, just take the rest of the string and break out of the loop
        results[[length(results)+1]] <- trimws(substr(cText, i+1, iEnd))
        break
      }
      else {
        results[[length(results)+1]] <- trimws(substr(cText, i+1, j-1))
        k <- getNextPosition(sep, j)
        if(is.null(k)||(k<=0)) break
        i <- k + 1
      }
    }
    else {
      # find next comma
      j <- getNextPosition(sep, i)
      # message(paste0("jc=", j))
      if(is.null(j)||(j<=0)) {
        # malformed, just take the rest of the string and break out of the loop
        results[[length(results)+1]] <- trimws(substr(cText, i, iEnd))
        break
      }
      else {
        results[[length(results)+1]] <- trimws(substr(cText, i, j-1))
        i <- j + 1
      }
    }
  }
  results <- unlist(results)
  if(removeEmptyString) results <- results[nchar(results)>0]
  return(results)
}


#' Convert a CSS size value into points.
#'
#' \code{parseCssSizeToPt} will take a CSS style and convert it to points.
#' Supported input size units are in, cm, mm, pt, pc, px, em, %.  The following
#' are converted exactly:  in, cm, mm, pt, pc: using 1in = 2.54cm = 25.4mm =
#' 72pt = 6pc.  The following are converted approximately:  px, em, %: using
#' approx 1em=16px=12pt and 100%=1em, so approx 25.4mm = 96px
#'
#' @param size A size specified in common CSS units.
#' @return The size converted to points.

parseCssSizeToPt <- function(size) {
  # able to convert in, cm, mm, pt, pc, px, em, %
  # convert exactly:  in, cm, mm, pt, pc: using 1in = 2.54cm = 25.4mm = 72pt = 6pc
  # convert approximately:  px, em, %: using approx 1em=16px=12pt and 100%=1em, so approx 25.4mm = 96px
  # references:
  #   https://www.w3.org/Style/Examples/007/units.en.html
  cSize <- tolower(cleanCssValue(size))
  if(!isTextValue(cSize)) return(NULL)
  # convert named sizes, percentages, em and px
  percent <- NULL
  if(cSize=="xx-small") percent <- 50
  else if(cSize=="x-small") percent <- 60
  else if(cSize=="small") percent <- 80
  else if(cSize=="medium") percent <- 100
  else if(cSize=="large") percent <- 110
  else if(cSize=="x-large") percent <- 150
  else if(cSize=="xx-large") percent <- 200
  else if(endsWith(cSize, "%")&&(nchar(cSize) > 1)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-1))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) percent <- testValue
  }
  else if(endsWith(cSize, "em")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) percent <- 100 * testValue
  }
  else if(endsWith(cSize, "px")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) percent <- testValue * 100 / 16
  }
  if(!is.null(percent)) {
    pt <- round(percent * 12 / 50) / 2 # round to nearest 0.5 pt
    pt <- min(max(pt, 4), 72)
    return(pt)
  }
  # convert in, cm, mm
  mm <- NULL
  if(endsWith(cSize, "in")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- 25.4 * testValue
  }
  if(endsWith(cSize, "cm")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- 10 * testValue
  }
  if(endsWith(cSize, "mm")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- testValue
  }
  if(!is.null(mm)) {
    pt <- round(mm * 144 / 25.4) / 2 # round to nearest 0.5 pt
    pt <- min(max(pt, 4), 72)
    return(pt)
  }
  # convert pt
  if(endsWith(cSize, "pt")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) {
      pt <- testValue
      pt <- min(max(pt, 4), 72)
      return(pt)
    }
  }
  # convert pc
  if(endsWith(cSize, "pc")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) {
      pt <- 12 * testValue
      pt <- min(max(pt, 4), 72)
      return(pt)
    }
  }
  # unknown units
  return(NULL)
}


#' Convert a CSS size value into pixels
#'
#' \code{parseCssSizeToPx} will take a CSS style and convert it to pixels
#' Supported input size units are in, cm, mm, pt, pc, px, em, %.  The following
#' are converted exactly:  in, cm, mm, pt, pc: using 1in = 2.54cm = 25.4mm =
#' 72pt = 6pc.  The following are converted approximately:  px, em, %: using
#' approx 1em=16px=12pt and 100%=1em, so approx 25.4mm = 96px
#'
#' @param size A size specified in common CSS units.
#' @return The size converted to pixels.

parseCssSizeToPx <- function(size) {
  # able to convert in, cm, mm, pt, pc, px, em, %
  # convert exactly:  in, cm, mm, pt, pc: using 1in = 2.54cm = 25.4mm = 72pt = 6pc
  # convert approximately:  px, em, %: using approx 1em=16px=12pt and 100%=1em, so approx 25.4mm = 96px
  # references:
  #   https://www.w3.org/Style/Examples/007/units.en.html
  cSize <- tolower(cleanCssValue(size))
  if(!isTextValue(cSize)) return(NULL)
  # convert named sizes, percentages, em
  percent <- NULL
  if(cSize=="xx-small") percent <- 50
  else if(cSize=="x-small") percent <- 60
  else if(cSize=="small") percent <- 80
  else if(cSize=="medium") percent <- 100
  else if(cSize=="large") percent <- 110
  else if(cSize=="x-large") percent <- 150
  else if(cSize=="xx-large") percent <- 200
  else if(endsWith(cSize, "%")&&(nchar(cSize) > 1)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-1))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) percent <- testValue
  }
  else if(endsWith(cSize, "em")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) percent <- 100 * testValue
  }
  if(!is.null(percent)) {
    px <- round(percent * 16 / 10) / 10 # round to nearest 0.1px
    px <- min(max(px, 0), 768)
    return(px)
  }
  # convert in, cm, mm
  mm <- NULL
  if(endsWith(cSize, "in")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- 25.4 * testValue
  }
  if(endsWith(cSize, "cm")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- 10 * testValue
  }
  if(endsWith(cSize, "mm")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) mm <- testValue
  }
  if(!is.null(mm)) {
    px <- round(mm * 960 / 25.4) / 10 # round to nearest 0.1 px
    px <- min(max(px, 0), 768)
    return(px)
  }
  # convert pt
  if(endsWith(cSize, "pt")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) {
      px <- 4 * testValue / 3
      px <- min(max(px, 0), 768)
      return(px)
    }
  }
  # convert pc
  if(endsWith(cSize, "pc")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) {
      px <- 4 * 12 * testValue / 3
      px <- min(max(px, 0), 768)
      return(px)
    }
  }
  # convert px
  if(endsWith(cSize, "px")&&(nchar(cSize) > 2)) {
    testValue <- trimws(substr(cSize, 1, nchar(cSize)-2))
    testValue <- suppressWarnings(as.numeric(testValue))
    if(!is.na(testValue)) {
      px <- testValue
      px <- min(max(px, 0), 768)
      return(px)
    }
  }
  # unknown units
  return(NULL)
}


#' Convert a CSS colour into a hex based colour code.
#'
#' \code{parseColor} converts a colour value specified in CSS to a hex based
#' colour code.  Example supported input values/formats/named colours are:
#' #0080FF, rgb(0, 128, 255), rgba(0, 128, 255, 0.5) and red, green, etc.
#'
#' @param color The colour to convert.
#' @return The colour as a hex code, e.g. #FF00A0.

parseColor <- function(color) { # returns a colour in the form #[0-9A-F]
  cColor <- cleanCssValue(color)
  if(!isTextValue(cColor)) return(NULL)

  # colour already in hex format?
  check <- grep("#[0-9A-F]{6}", toupper(cColor))
  if((length(check)>0)&&(check==TRUE)) return(toupper(cColor))

  # to lower after above check
  cColor <- tolower(cColor)

  # RGB color?
  if(startsWith(cColor, "rgb(") && endsWith(cColor, ")") && (nchar(cColor) > 5)) {
    s <- substr(cColor, 5, nchar(cColor)-1)
    clrs <- trimws(unlist(strsplit(s, ",")))
    if(length(clrs)==3) {
      clrRed <- suppressWarnings(min(max(as.numeric(clrs[1]), 0), 255))
      clrGreen <- suppressWarnings(min(max(as.numeric(clrs[2]), 0), 255))
      clrBlue <- suppressWarnings(min(max(as.numeric(clrs[3]), 0), 255))
      if((!is.na(clrRed))&&(!is.na(clrGreen))&&(!is.na(clrBlue))) {
        return(paste0("#",
                      as.character.hexmode(clrRed, width=2, upper.case=TRUE),
                      as.character.hexmode(clrGreen, width=2, upper.case=TRUE),
                      as.character.hexmode(clrBlue, width=2, upper.case=TRUE)))
      }
    }
  }

  # RGBA color?
  if(startsWith(cColor, "rgba(") && endsWith(cColor, ")") && (nchar(cColor) > 6)) {
    s <- substr(cColor, 6, nchar(cColor)-1)
    clrs <- trimws(unlist(strsplit(s, ",")))
    if(length(clrs)>=3) {
      clrRed <- suppressWarnings(min(max(as.numeric(clrs[1]), 0), 255))
      clrGreen <- suppressWarnings(min(max(as.numeric(clrs[2]), 0), 255))
      clrBlue <- suppressWarnings(min(max(as.numeric(clrs[3]), 0), 255))
      if((!is.na(clrRed))&&(!is.na(clrGreen))&&(!is.na(clrBlue))) {
        return(paste0("#",
                      as.character.hexmode(clrRed, width=2, upper.case=TRUE),
                      as.character.hexmode(clrGreen, width=2, upper.case=TRUE),
                      as.character.hexmode(clrBlue, width=2, upper.case=TRUE)))
      }
    }
  }

  # named colours
  if(cColor=="aliceblue") return("#F0F8FF")
  else if(cColor=="antiquewhite") return("#FAEBD7")
  else if(cColor=="aqua") return("#00FFFF")
  else if(cColor=="aquamarine") return("#7FFFD4")
  else if(cColor=="azure") return("#F0FFFF")
  else if(cColor=="beige") return("#F5F5DC")
  else if(cColor=="bisque") return("#FFE4C4")
  else if(cColor=="black") return("#000000")
  else if(cColor=="blanchedalmond") return("#FFEBCD")
  else if(cColor=="blue") return("#0000FF")
  else if(cColor=="blueviolet") return("#8A2BE2")
  else if(cColor=="brown") return("#A52A2A")
  else if(cColor=="burlywood") return("#DEB887")
  else if(cColor=="cadetblue") return("#5F9EA0")
  else if(cColor=="chartreuse") return("#7FFF00")
  else if(cColor=="chocolate") return("#D2691E")
  else if(cColor=="coral") return("#FF7F50")
  else if(cColor=="cornflowerblue") return("#6495ED")
  else if(cColor=="cornsilk") return("#FFF8DC")
  else if(cColor=="crimson") return("#DC143C")
  else if(cColor=="cyan") return("#00FFFF")
  else if(cColor=="darkblue") return("#00008B")
  else if(cColor=="darkcyan") return("#008B8B")
  else if(cColor=="darkgoldenrod") return("#B8860B")
  else if(cColor=="darkgray") return("#A9A9A9")
  else if(cColor=="darkgrey") return("#A9A9A9")
  else if(cColor=="darkgreen") return("#006400")
  else if(cColor=="darkkhaki") return("#BDB76B")
  else if(cColor=="darkmagenta") return("#8B008B")
  else if(cColor=="darkolivegreen") return("#556B2F")
  else if(cColor=="darkorange") return("#FF8C00")
  else if(cColor=="darkorchid") return("#9932CC")
  else if(cColor=="darkred") return("#8B0000")
  else if(cColor=="darksalmon") return("#E9967A")
  else if(cColor=="darkseagreen") return("#8FBC8F")
  else if(cColor=="darkslateblue") return("#483D8B")
  else if(cColor=="darkslategray") return("#2F4F4F")
  else if(cColor=="darkslategrey") return("#2F4F4F")
  else if(cColor=="darkturquoise") return("#00CED1")
  else if(cColor=="darkviolet") return("#9400D3")
  else if(cColor=="deeppink") return("#FF1493")
  else if(cColor=="deepskyblue") return("#00BFFF")
  else if(cColor=="dimgray") return("#696969")
  else if(cColor=="dimgrey") return("#696969")
  else if(cColor=="dodgerblue") return("#1E90FF")
  else if(cColor=="firebrick") return("#B22222")
  else if(cColor=="floralwhite") return("#FFFAF0")
  else if(cColor=="forestgreen") return("#228B22")
  else if(cColor=="fuchsia") return("#FF00FF")
  else if(cColor=="gainsboro") return("#DCDCDC")
  else if(cColor=="ghostwhite") return("#F8F8FF")
  else if(cColor=="gold") return("#FFD700")
  else if(cColor=="goldenrod") return("#DAA520")
  else if(cColor=="gray") return("#808080")
  else if(cColor=="grey") return("#808080")
  else if(cColor=="green") return("#008000")
  else if(cColor=="greenyellow") return("#ADFF2F")
  else if(cColor=="honeydew") return("#F0FFF0")
  else if(cColor=="hotpink") return("#FF69B4")
  else if(cColor=="indianred") return("#CD5C5C")
  else if(cColor=="indigo") return("#4B0082")
  else if(cColor=="ivory") return("#FFFFF0")
  else if(cColor=="khaki") return("#F0E68C")
  else if(cColor=="lavender") return("#E6E6FA")
  else if(cColor=="lavenderblush") return("#FFF0F5")
  else if(cColor=="lawngreen") return("#7CFC00")
  else if(cColor=="lemonchiffon") return("#FFFACD")
  else if(cColor=="lightblue") return("#ADD8E6")
  else if(cColor=="lightcoral") return("#F08080")
  else if(cColor=="lightcyan") return("#E0FFFF")
  else if(cColor=="lightgoldenrodyellow") return("#FAFAD2")
  else if(cColor=="lightgray") return("#D3D3D3")
  else if(cColor=="lightgrey") return("#D3D3D3")
  else if(cColor=="lightgreen") return("#90EE90")
  else if(cColor=="lightpink") return("#FFB6C1")
  else if(cColor=="lightsalmon") return("#FFA07A")
  else if(cColor=="lightseagreen") return("#20B2AA")
  else if(cColor=="lightskyblue") return("#87CEFA")
  else if(cColor=="lightslategray") return("#778899")
  else if(cColor=="lightslategrey") return("#778899")
  else if(cColor=="lightsteelblue") return("#B0C4DE")
  else if(cColor=="lightyellow") return("#FFFFE0")
  else if(cColor=="lime") return("#00FF00")
  else if(cColor=="limegreen") return("#32CD32")
  else if(cColor=="linen") return("#FAF0E6")
  else if(cColor=="magenta") return("#FF00FF")
  else if(cColor=="maroon") return("#800000")
  else if(cColor=="mediumaquamarine") return("#66CDAA")
  else if(cColor=="mediumblue") return("#0000CD")
  else if(cColor=="mediumorchid") return("#BA55D3")
  else if(cColor=="mediumpurple") return("#9370DB")
  else if(cColor=="mediumseagreen") return("#3CB371")
  else if(cColor=="mediumslateblue") return("#7B68EE")
  else if(cColor=="mediumspringgreen") return("#00FA9A")
  else if(cColor=="mediumturquoise") return("#48D1CC")
  else if(cColor=="mediumvioletred") return("#C71585")
  else if(cColor=="midnightblue") return("#191970")
  else if(cColor=="mintcream") return("#F5FFFA")
  else if(cColor=="mistyrose") return("#FFE4E1")
  else if(cColor=="moccasin") return("#FFE4B5")
  else if(cColor=="navajowhite") return("#FFDEAD")
  else if(cColor=="navy") return("#000080")
  else if(cColor=="oldlace") return("#FDF5E6")
  else if(cColor=="olive") return("#808000")
  else if(cColor=="olivedrab") return("#6B8E23")
  else if(cColor=="orange") return("#FFA500")
  else if(cColor=="orangered") return("#FF4500")
  else if(cColor=="orchid") return("#DA70D6")
  else if(cColor=="palegoldenrod") return("#EEE8AA")
  else if(cColor=="palegreen") return("#98FB98")
  else if(cColor=="paleturquoise") return("#AFEEEE")
  else if(cColor=="palevioletred") return("#DB7093")
  else if(cColor=="papayawhip") return("#FFEFD5")
  else if(cColor=="peachpuff") return("#FFDAB9")
  else if(cColor=="peru") return("#CD853F")
  else if(cColor=="pink") return("#FFC0CB")
  else if(cColor=="plum") return("#DDA0DD")
  else if(cColor=="powderblue") return("#B0E0E6")
  else if(cColor=="purple") return("#800080")
  else if(cColor=="rebeccapurple") return("#663399")
  else if(cColor=="red") return("#FF0000")
  else if(cColor=="rosybrown") return("#BC8F8F")
  else if(cColor=="royalblue") return("#4169E1")
  else if(cColor=="saddlebrown") return("#8B4513")
  else if(cColor=="salmon") return("#FA8072")
  else if(cColor=="sandybrown") return("#F4A460")
  else if(cColor=="seagreen") return("#2E8B57")
  else if(cColor=="seashell") return("#FFF5EE")
  else if(cColor=="sienna") return("#A0522D")
  else if(cColor=="silver") return("#C0C0C0")
  else if(cColor=="skyblue") return("#87CEEB")
  else if(cColor=="slateblue") return("#6A5ACD")
  else if(cColor=="slategray") return("#708090")
  else if(cColor=="slategrey") return("#708090")
  else if(cColor=="snow") return("#FFFAFA")
  else if(cColor=="springgreen") return("#00FF7F")
  else if(cColor=="steelblue") return("#4682B4")
  else if(cColor=="tan") return("#D2B48C")
  else if(cColor=="teal") return("#008080")
  else if(cColor=="thistle") return("#D8BFD8")
  else if(cColor=="tomato") return("#FF6347")
  else if(cColor=="turquoise") return("#40E0D0")
  else if(cColor=="violet") return("#EE82EE")
  else if(cColor=="wheat") return("#F5DEB3")
  else if(cColor=="white") return("#FFFFFF")
  else if(cColor=="whitesmoke") return("#F5F5F5")
  else if(cColor=="yellow") return("#FFFF00")
  else if(cColor=="yellowgreen") return("#9ACD32")
  else return(NULL)
}


#' Parse a CSS border value.
#'
#' \code{parseCssBorder} parses the CSS combined border declarations (i.e.
#' border, border-left, border-right, border-top, border-bottom) and returns a
#' list containing the width, style and color as separate elements.
#'
#' @param text The border declaration to parse.
#' @return A list containing three elements: width, style and color.

parseCssBorder <- function(text) {
  # parses a combined border declaration,
  # e.g. 1:  border: thin solid #FF00BB
  # e.g. 2:  border-left: thin solid red
  # e.g. 3:  border-left: 2px solid rgb(0, 255, 0)
  # does not currently support specifying different values for different sides,
  # e.g. (not supported): border: thin thick solid thin red blue

  cText <- trimws(text)
  if(!isTextValue(cText)) return(NULL)

  if(endsWith(cText, ";")) cText <- substr(cText, 1, nchar(cText)-1)
  i <- 1
  iEnd <- nchar(cText)
  space <- gregexpr("[ \t\r\n]", cText)[[1]]
  closeBracket <- gregexpr(")", cText, fixed=TRUE)[[1]]
  ws <- c(" ", "\t", "\r", "\n")
  parts <- list()
  while (i <= iEnd) {
    chr <- substr(cText, i, i)
    # message(chr)
    # skip past whitespace
    if(chr %in% ws) {
      i <- i + 1
      next
    }
    else {
      # find next space
      j <- getNextPosition(space, i)
      # message(paste0("sp=", j))
      if(is.null(j)||(j<=0)) {
        # malformed, just take the rest of the string and break out of the loop
        parts[[length(parts)+1]] <- trimws(substr(cText, i, iEnd))
        break
      }
      else {
        word <- trimws(substr(cText, i, j-1))
        if(startsWith(tolower(word), "rgb(")) {
          k <- getNextPosition(closeBracket, i)
          word <- trimws(substr(cText, i, k))
          parts[[length(parts)+1]] <- word
          i <- k + 1
        }
        else if(startsWith(tolower(word), "rgba(")) {
          k <- getNextPosition(closeBracket, i)
          word <- trimws(substr(cText, i, k))
          parts[[length(parts)+1]] <- word
          i <- k + 1
        }
        else {
          parts[[length(parts)+1]] <- word
          i <- j + 1
        }
      }
    }
  }
  parts <- unlist(parts)
  parts <- parts[nchar(parts)>0]

  # get width, style and color
  borderWidth <- NULL
  borderStyle <- NULL
  borderColor <- NULL
  allowedWidths <- c("thin", "medium", "thick")
  allowedStyles <- c("none", "hidden", "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", "outset")
  for(i in 1:length(parts)) {
    # examine each part
    part <- tolower(cleanCssValue(parts[i]))
    if(!isTextValue(part)) next
    # width?
    if(is.null(borderWidth)) {
      if(part %in% allowedWidths) borderWidth <- part
      else {
        numericalWidth <- parseCssSizeToPx(part)
        if(!is.null(numericalWidth)) borderWidth <- numericalWidth
      }
    }
    # style
    if(is.null(borderStyle)) {
      if(part %in% allowedStyles) borderStyle <- part
    }
    # color
    if(is.null(borderColor)) {
      color <- parseColor(part)
      if(!is.null(color)) borderColor <- color
    }
  }
  if(!isTextValue(borderStyle)) return(NULL)
  result <- list(width=borderWidth, style=borderStyle, color=borderColor)
  return(result)
}


#' Convert CSS border values to those used by the openxlsx package.
#'
#' \code{getXlBorderStyleFromCssBorder} takes border parameters expressed as a
#' list (containing elements: width and style) and returns a border style that
#' is compatible with the openxlsx package.
#'
#' @param border A list containing elements width and style.
#' @return An openxlsx border style.

getXlBorderStyleFromCssBorder <- function(border) {
  # border is a return value from the parseCssBorder() function
  if(!isTextValue(border)) return(NULL)
  cssBorderStyle <- border[["style"]]
  cssBorderWidth <- border[["width"]]
  if(!isTextValue(cssBorderStyle)) return(NULL)
  if(!isTextValue(cssBorderWidth)) cssBorderWidth <- "thin"
  if(cssBorderStyle %in% c("none", "hidden", "initial", "inherit")) return(NULL)
  if(cssBorderStyle %in% c("groove", "ridge", "inset", "outset")) cssBorderStyle <- "solid"
  if(cssBorderStyle=="solid") {
    if(cssBorderWidth=="thin") return("thin")
    else if(cssBorderWidth=="medium") return("medium")
    else if(cssBorderWidth=="thick") return("thick")
    else {
      numericalWidth <- suppressWarnings(as.numeric(cssBorderWidth))
      if(is.na(numericalWidth)) return("thin")
      else if(numericalWidth < 1) return("hair")
      else if(numericalWidth < 1.5) return("thin")
      else if(numericalWidth < 2.5) return("medium")
      else return("thick")
    }
  }
  else if(cssBorderStyle=="dotted") {
    if(cssBorderWidth=="thin") return("dotted")
    else if(cssBorderWidth=="medium") return("mediumDashDot")
    else if(cssBorderWidth=="thick") return("mediumDashDot")
    else {
      numericalWidth <- suppressWarnings(as.numeric(cssBorderWidth))
      if(is.na(numericalWidth)) return("dotted")
      else if(numericalWidth < 1) return("dotted")
      else if(numericalWidth < 1.5) return("mediumDashDot")
      else if(numericalWidth < 2.5) return("mediumDashDot")
      else return("mediumDashDot")
    }
  }
  else if(cssBorderStyle=="dashed") {
    if(cssBorderWidth=="thin") return("dashed")
    else if(cssBorderWidth=="medium") return("mediumDashed")
    else if(cssBorderWidth=="thick") return("mediumDashed")
    else {
      numericalWidth <- suppressWarnings(as.numeric(cssBorderWidth))
      if(is.na(numericalWidth)) return("dashed")
      else if(numericalWidth < 1) return("dashed")
      else if(numericalWidth < 1.5) return("mediumDashed")
      else if(numericalWidth < 2.5) return("mediumDashed")
      else return("mediumDashed")
    }
  }
  else if(cssBorderStyle=="double") {
    if(cssBorderWidth=="thin") return("thin")
    else if(cssBorderWidth=="medium") return("double")
    else if(cssBorderWidth=="thick") return("double")
    else {
      numericalWidth <- suppressWarnings(as.numeric(cssBorderWidth))
      if(is.na(numericalWidth)) return("thin")
      else if(numericalWidth < 1) return("thin")
      else if(numericalWidth < 1.5) return("double")
      else if(numericalWidth < 2.5) return("double")
      else return("double")
    }
  }
  return(NULL)
}


#' Convert CSS border values to those used by the openxlsx package.
#'
#' \code{getXlBorderFromCssBorder} parses the CSS combined border declarations
#' (i.e. border, border-left, border-right, border-top, border-bottom) and
#' returns a list containing an openxlsx border style and color as separate
#' elements.
#'
#' @param text The border declaration to parse.
#' @return A list containing two elements: style and color.

getXlBorderFromCssBorder <- function(text) {
  cssBorder <- parseCssBorder(text)
  if(is.null(cssBorder)) return(NULL)
  xlBorderStyle <- getXlBorderStyleFromCssBorder(cssBorder)
  if(is.null(cssBorder)) return(NULL)
  return(list(style=xlBorderStyle, color=cssBorder[["color"]]))
}


#' Parse an xl-border value.
#'
#' \code{parseXlBorder} parses the combined xl border declarations (i.e.
#' xl-border, xl-border-left, xl-border-right, xl-border-top, xl-border-bottom)
#' and returns a list containing style and color as separate elements.
#'
#' @param text The border declaration to parse.
#' @return A list containing two elements: style and color.

parseXlBorder <- function(text) {
  # parses a combined border declaration,
  # e.g. 1:  xl-border: thin #FF00BB
  # e.g. 2:  xl-border-left: thin red
  # e.g. 3:  xl-border-left: dashed rgb(0, 255, 0)
  # does not currently support specifying different values for different sides,
  # e.g. (not supported): xl-border: thin thick red blue

  cText <- trimws(text)
  if(!isTextValue(cText)) return(NULL)

  if(endsWith(cText, ";")) cText <- substr(cText, 1, nchar(cText)-1)
  i <- 1
  iEnd <- nchar(cText)
  space <- gregexpr("[ \t\r\n]", cText)[[1]]
  closeBracket <- gregexpr(")", cText, fixed=TRUE)[[1]]
  ws <- c(" ", "\t", "\r", "\n")
  parts <- list()
  while (i <= iEnd) {
    chr <- substr(cText, i, i)
    # message(chr)
    # skip past whitespace
    if(chr %in% ws) {
      i <- i + 1
      next
    }
    else {
      # find next space
      j <- getNextPosition(space, i)
      # message(paste0("sp=", j))
      if(is.null(j)||(j<=0)) {
        # malformed, just take the rest of the string and break out of the loop
        parts[[length(parts)+1]] <- trimws(substr(cText, i, iEnd))
        break
      }
      else {
        word <- trimws(substr(cText, i, j-1))
         if(startsWith(tolower(word), "rgb(")) {
          k <- getNextPosition(closeBracket, i)
          word <- trimws(substr(cText, i, k))
          parts[[length(parts)+1]] <- word
          i <- k + 1
        }
        else if(startsWith(tolower(word), "rgba(")) {
          k <- getNextPosition(closeBracket, i)
          word <- trimws(substr(cText, i, k))
          parts[[length(parts)+1]] <- word
          i <- k + 1
        }
        else {
          parts[[length(parts)+1]] <- word
          i <- j + 1
        }
      }
    }
  }
  parts <- unlist(parts)
  parts <- parts[nchar(parts)>0]

  # get width, style and color
  borderStyle <- NULL
  borderColor <- NULL
  allowedStyles <- c("none", "thin", "medium", "dashed", "dotted", "thick", "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot")
  for(i in 1:length(parts)) {
    # examine each part
    part <- tolower(cleanCssValue(parts[i]))
    # style
    if(is.null(borderStyle)) {
      if(part %in% allowedStyles) borderStyle <- part
    }
    # color
    if(is.null(borderColor)) {
      color <- parseColor(part)
      if(!is.null(color)) borderColor <- color
    }
  }
  result <- list(style=borderStyle, color=borderColor)
  return(result)
}
