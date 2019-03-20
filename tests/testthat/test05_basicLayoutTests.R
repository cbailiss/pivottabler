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


context("BASIC LAYOUT TESTS")


scenarios <- testScenarios("basic layout tests:  empty pivot")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "    "
    html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  empty pivot plus data")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "    "
    html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  just a total")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "  TotalTrains  \n        83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 83710)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  two measures")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "  TotalTrains  MaxSchedSpeed  \n        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 83835)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows only")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE))
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "Arriva Trains Wales    \nCrossCountry           \nLondon Midland         \nVirgin Trains          \nTotal                  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows plus total")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     TotalTrains  \nArriva Trains Wales         3909  \nCrossCountry               22928  \nLondon Midland             48279  \nVirgin Trains               8594  \nTotal                      83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows plus two measures")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     TotalTrains  MaxSchedSpeed  \nArriva Trains Wales         3909             90  \nCrossCountry               22928            125  \nLondon Midland             48279            110  \nVirgin Trains               8594            125  \nTotal                      83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns only")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "  Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \n                                                                           "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus total")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "  Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \n                 3909         22928           48279           8594  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus two totals")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "  Arriva Trains Wales         CrossCountry                London Midland              Virgin Trains               Total                       \n  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  \n         3909             90        22928            125        48279            110         8594            125        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows and columns only")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales                                                \nCrossCountry                                                       \nLondon Midland                                                     \nVirgin Trains                                                      \nTotal                                                              "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and calculation")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales               3079                 830   3909  \nCrossCountry                     22865                  63  22928  \nLondon Midland                   14487               33792  48279  \nVirgin Trains                     8594                       8594  \nTotal                            49025               34685  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and two calculations")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     Express Passenger           Ordinary Passenger          Total                       \n                     TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  \nArriva Trains Wales         3079             90          830             90         3909             90  \nCrossCountry               22865            125           63            100        22928            125  \nLondon Midland             14487            110        33792            100        48279            110  \nVirgin Trains               8594            125                                     8594            125  \nTotal                      49025            125        34685            100        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus total on row")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "             Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \nTotalTrains                 3909         22928           48279           8594  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus two totals on rows")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "               Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \nTotalTrains                   3909         22928           48279           8594  83710  \nMaxSchedSpeed                   90           125             110            125    125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and calculation on rows")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales               3079                 830   3909  \nCrossCountry                     22865                  63  22928  \nLondon Midland                   14487               33792  48279  \nVirgin Trains                     8594                       8594  \nTotal                            49025               34685  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and two calculations on rows")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asCharacter=TRUE))
    str <- "                                    Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales  TotalTrains                 3079                 830   3909  \n                     MaxSchedSpeed                 90                  90     90  \nCrossCountry         TotalTrains                22865                  63  22928  \n                     MaxSchedSpeed                125                 100    125  \nLondon Midland       TotalTrains                14487               33792  48279  \n                     MaxSchedSpeed                110                 100    110  \nVirgin Trains        TotalTrains                 8594                       8594  \n                     MaxSchedSpeed                125                        125  \nTotal                TotalTrains                49025               34685  83710  \n                     MaxSchedSpeed                125                 100    125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
