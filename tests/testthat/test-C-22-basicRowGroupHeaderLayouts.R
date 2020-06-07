library(testthat)

# most common expectations:
# equality:        expect_equal() and expect_identical()
# regexp:          expect_match()
# catch-all:       expect_true() and expect_false()
# console output:  expect_output()
# messages:        expect_message()
# warning:         expect_warning()
# errors:          expect_error()

escapeString <- function(s) {
  t <- gsub("(\\\\)", "\\\\\\\\", s)
  t <- gsub("(\n)", "\\\\n", t)
  t <- gsub("(\r)", "\\\\r", t)
  t <- gsub("(\")", "\\\\\"", t)
  return(t)
}

prepStr <- function(s) {
  t <- escapeString(s)
  u <- eval(parse(text=paste0("\"", t, "\"")))
  if(s!=u) stop("Unable to escape string!")
  t <- paste0("\thtml <- \"", t, "\"")
  utils::writeClipboard(t)
  return(invisible())
}

evaluationMode <- "sequential"
processingLibrary <- "dplyr"
description <- "test: sequential dplyr"
countFunction <- "n()"
isDevelopmentVersion <- (length(strsplit(packageDescription("pivottabler")$Version, "\\.")[[1]]) > 3)

testScenarios <- function(description="test", releaseEvaluationMode="batch", releaseProcessingLibrary="dplyr", runAllForReleaseVersion=FALSE) {
  isDevelopmentVersion <- (length(strsplit(packageDescription("pivottabler")$Version, "\\.")[[1]]) > 3)
  if(isDevelopmentVersion||runAllForReleaseVersion) {
    evaluationModes <- c("sequential", "batch")
    processingLibraries <- c("dplyr", "data.table")
  }
  else {
    evaluationModes <- releaseEvaluationMode
    processingLibraries <- releaseProcessingLibrary
  }
  testCount <- length(evaluationModes)*length(processingLibraries)
  c1 <- character(testCount)
  c2 <- character(testCount)
  c3 <- character(testCount)
  c4 <- character(testCount)
  testCount <- 0
  for(evaluationMode in evaluationModes)
    for(processingLibrary in processingLibraries) {
      testCount <- testCount + 1
      c1[testCount] <- evaluationMode
      c2[testCount] <- processingLibrary
      c3[testCount] <- paste0(description, ": ", evaluationMode, " ", processingLibrary)
      c4[testCount] <- ifelse(processingLibrary=="data.table", ".N", "n()")
    }
  df <- data.frame(evaluationMode=c1, processingLibrary=c2, description=c3, countFunction=c4, stringsAsFactors=FALSE)
  return(df)
}



# Test data

library(dplyr)
library(lubridate)
library(pivottabler)
trains <- mutate(bhmtrains,
                 GbttDate=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
                 GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))
trains <- filter(trains, GbttMonth>=make_date(year=2017, month=1, day=1))




context("ROW GROUP HEADER BASIC LAYOUT TESTS")

scenarios <- testScenarios("Row group header basic layout test: One group only")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addRowDataGroups("TOC", header="Train Company", addTotal=FALSE)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"CentreColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Arriva Trains Wales</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">CrossCountry</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">London Midland</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}



scenarios <- testScenarios("Row group header basic layout test: Two row groups only")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(trains)
    pt$addRowDataGroups("TOC", header="Train Company", addTotal=FALSE)
    pt$addRowDataGroups("TrainCategory", header="Train Category", addTotal=FALSE)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"LeftColumnHeader\">Train Category</th>\n    <th class=\"CentreColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">London Midland</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}



scenarios <- testScenarios("Row group header basic layout test: One row group and calculation")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addRowDataGroups("TOC", header="Train Company")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"CentreColumnHeader\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Arriva Trains Wales</th>\n    <td class=\"RightCell\">2618</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">CrossCountry</th>\n    <td class=\"RightCell\">15378</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">London Midland</th>\n    <td class=\"RightCell\">32677</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <td class=\"RightCell\">5717</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Total</th>\n    <td class=\"Total\">56390</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 112780)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}



scenarios <- testScenarios("Row group header basic layout test: Two row groups and calculation")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(trains)
    pt$addRowDataGroups("TOC", header="Train Company")
    pt$addRowDataGroups("TrainCategory", header="Train Category", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"LeftColumnHeader\">Train Category</th>\n    <th class=\"CentreColumnHeader\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\">2062</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\">556</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\">15336</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\">42</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"2\">London Midland</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\">9736</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Ordinary Passenger</th>\n    <td class=\"RightCell\">22941</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <th class=\"LeftCell\">Express Passenger</th>\n    <td class=\"RightCell\">5717</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Total</th>\n    <th class=\"LeftCell\"></th>\n    <td class=\"Total\">56390</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 112780)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}



scenarios <- testScenarios("Row group header basic layout test: Row and column only")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
    pt$addRowDataGroups("TOC", header="Train Company", addTotal=FALSE)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"CentreColumnHeader\">January 2017</th>\n    <th class=\"CentreColumnHeader\">February 2017</th>\n    <th class=\"CentreColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Arriva Trains Wales</th>\n    <td class=\"RightCell\"></td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">CrossCountry</th>\n    <td class=\"RightCell\"></td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">London Midland</th>\n    <td class=\"RightCell\"></td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <td class=\"RightCell\"></td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}



scenarios <- testScenarios("Row group header basic layout test: Row, column and calculation")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
    pt$addRowDataGroups("TOC", header="Train Company")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Train Company</th>\n    <th class=\"CentreColumnHeader\">January 2017</th>\n    <th class=\"CentreColumnHeader\">February 2017</th>\n    <th class=\"CentreColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Arriva Trains Wales</th>\n    <td class=\"RightCell\">1402</td>\n    <td class=\"RightCell\">1216</td>\n    <td class=\"Total\">2618</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">CrossCountry</th>\n    <td class=\"RightCell\">8033</td>\n    <td class=\"RightCell\">7345</td>\n    <td class=\"Total\">15378</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">London Midland</th>\n    <td class=\"RightCell\">17029</td>\n    <td class=\"RightCell\">15648</td>\n    <td class=\"Total\">32677</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <td class=\"RightCell\">3004</td>\n    <td class=\"RightCell\">2713</td>\n    <td class=\"Total\">5717</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Total</th>\n    <td class=\"Total\">29468</td>\n    <td class=\"Total\">26922</td>\n    <td class=\"Total\">56390</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 225560)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}


