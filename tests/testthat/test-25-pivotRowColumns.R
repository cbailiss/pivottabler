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


context("PIVOT ROW/COLUMN TESTS")


scenarios <- testScenarios("pvt row/col tests:  get empty rows")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC", fromData=FALSE,
                        explicitListOfValues=list("Arriva Trains Wales", "CrossCountry",
                                                  "South West Trains", "Southern", "Virgin Trains"))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # sum(pt$getEmptyRows())
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">South West Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Southern</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 238282)
    expect_equal(sum(pt$getEmptyRows()), 7)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  get empty columns")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE,
                           explicitListOfValues=list("Express Passenger",
                                                     "Empty Coaching Stock",
                                                     "Ordinary Passenger"))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # sum(pt$getEmptyColumns())
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Empty Coaching Stock</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_equal(sum(pt$getEmptyColumns()), 2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove row (single level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC", fromData=FALSE,
                        explicitListOfValues=list("Arriva Trains Wales", "CrossCountry",
                                                  "South West Trains", "Southern", "Virgin Trains"))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeRow(3)
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Southern</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 238282)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove rows (multi-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$addRowDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeRows(c(2, 3, 6, 11, 15))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove empty rows (single-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC", fromData=FALSE,
                        explicitListOfValues=list("Arriva Trains Wales", "CrossCountry",
                                                  "South West Trains", "Southern", "Virgin Trains"))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeEmptyRows()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 238282)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove empty rows (multi-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$addRowDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeEmptyRows()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove column (single level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC", fromData=FALSE,
                           explicitListOfValues=list("Arriva Trains Wales", "CrossCountry",
                                                  "South West Trains", "Southern", "Virgin Trains"))
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeColumn(3)
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\">CrossCountry</th>\n    <th class=\"ColumnHeader\">Southern</th>\n    <th class=\"ColumnHeader\">Virgin Trains</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Total\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">22928</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 238282)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove columns (multi-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$addColumnDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeColumns(c(2, 3, 6, 11, 15))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Virgin Trains</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">22196</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">22928</td>\n    <td class=\"Total\">11229</td>\n    <td class=\"Total\">37050</td>\n    <td class=\"Total\">48279</td>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove empty columns (single-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC", fromData=FALSE,
                           explicitListOfValues=list("Arriva Trains Wales", "CrossCountry",
                                                     "South West Trains", "Southern", "Virgin Trains"))
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeEmptyColumns()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\">CrossCountry</th>\n    <th class=\"ColumnHeader\">Virgin Trains</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Total\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">22928</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 238282)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  remove empty columns (multi-level)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$addColumnDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$removeEmptyColumns()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Virgin Trains</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">3909</td>\n    <td class=\"Total\">22196</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">22928</td>\n    <td class=\"Total\">11229</td>\n    <td class=\"Total\">37050</td>\n    <td class=\"Total\">48279</td>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



