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



context("OUTLINE LAYOUT TESTS")

scenarios <- testScenarios("Basic outline layout (outline before)")
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
    pt$addRowDataGroups("TOC", outlineBefore=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getCss()))
    # prepStr(as.character(pt$getHtml()))
    css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.OutlineColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.OutlineRowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.OutlineCell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; background-color: #F8F8F8; font-weight: bold; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n"
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getCss()), css)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (outline before and after)")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC", outlineBefore=TRUE, outlineAfter=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (doNotMerge)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="doNotMerge"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Arriva Trains Wales</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">CrossCountry</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">London Midland</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Virgin Trains</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (dataGroupsOnly)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="dataGroupsOnly"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (cellsOnly)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="cellsOnly"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Arriva Trains Wales</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">CrossCountry</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">London Midland</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Virgin Trains</th>\n    <th class=\"OutlineRowHeader\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (dataGroupsAndCellsAs1)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="dataGroupsAndCellsAs1"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Arriva Trains Wales</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">CrossCountry</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">London Midland</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Virgin Trains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Basic outline layout (dataGroupsAndCellsAs2)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="dataGroupsAndCellsAs2"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - with totals")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(isEmpty=FALSE, mergeSpace="dataGroupsOnly"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - simple 3 row group levels)")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(mergeSpace="dataGroupsAndCellsAs2"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType", outlineBefore=list(mergeSpace="dataGroupsAndCellsAs2"), outlineTotal=TRUE)
    pt$addRowDataGroups("Status")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"6\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"10\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">HST</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"11\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"11\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - formatted 3 row group levels")
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
    pt$addRowDataGroups("TOC",
                        outlineBefore=list(isEmpty=FALSE,
                                           mergeSpace="dataGroupsOnly",
                                           groupStyleDeclarations=list(color="blue"),
                                           cellStyleDeclarations=list(color="blue")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue"),
                                          cellStyleDeclarations=list(color="blue")))
    pt$addRowDataGroups("PowerType", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE, mergeSpace="dataGroupsOnly"))
    pt$addRowDataGroups("Status", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">3079</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">830</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">22865</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">63</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">22133</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">HST</th>\n    <td class=\"OutlineCell\">732</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">14487</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">33792</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">5638</td>\n    <td class=\"OutlineCell\">5591</td>\n    <td class=\"OutlineCell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\">8849</td>\n    <td class=\"OutlineCell\">28201</td>\n    <td class=\"OutlineCell\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">8594</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \"></td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">2137</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\">6457</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - simple 3 row group levels part tabular")
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
    pt$addRowDataGroups("TOC", outlineBefore=list(isEmpty=FALSE, mergeSpace="dataGroupsOnly"), outlineTotal=TRUE)
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$addRowDataGroups("Status")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">HST</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - formatted 3 row group levels part tabular")
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
    pt$addRowDataGroups("TOC",
                        outlineBefore=list(isEmpty=FALSE,
                                           mergeSpace="dataGroupsOnly",
                                           groupStyleDeclarations=list(color="blue")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue")))
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$addRowDataGroups("Status")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">HST</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Outline layout - outlined calculations on rows")
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
    pt$addColumnDataGroups("PowerType")
    pt$defineCalculation(calculationName="NumberOfTrains", caption="Number of Trains",
                         summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaximumSpeedMPH", caption="Maximum Speed (MPH)",
                         summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups(outlineBefore=list(isEmpty=FALSE, mergeSpace="dataGroupsOnly",
                                                  groupStyleDeclarations=list(color="blue"),
                                                  cellStyleDeclarations=list(color="blue")),
                               outlineAfter=TRUE)
    pt$addRowDataGroups("TOC", addTotal=FALSE)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Number of Trains</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">32987</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">15306</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">732</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">6484</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">28201</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"8\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Maximum Speed (MPH)</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">90</td>\n    <td class=\"Total\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Total\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">100</td>\n    <td class=\"Total\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Total\">100</td>\n    <td class=\"Total\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">125</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">&nbsp;</th>\n    <td class=\"OutlineCell\" colspan=\"8\">&nbsp;</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 505565)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Sort outlined groups - by group value")
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
    pt$addRowDataGroups("TOC",
                        outlineBefore=list(groupStyleDeclarations=list(color="blue")),
                        outlineAfter=list(isEmpty=FALSE,
                                          mergeSpace="dataGroupsOnly",
                                          caption="Total ({value})",
                                          groupStyleDeclarations=list("font-style"="italic")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue"),
                                          cellStyleDeclarations=list("color"="blue")))
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, orderBy="value", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Sort outlined groups - by calculation sorting on two levels")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC",
                        outlineBefore=list(groupStyleDeclarations=list(color="blue")),
                        outlineAfter=list(isEmpty=FALSE,
                                          mergeSpace="dataGroupsOnly",
                                          caption="Total ({value})",
                                          groupStyleDeclarations=list("font-style"="italic")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue"),
                                          cellStyleDeclarations=list("color"="blue")))
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, orderBy="calculation", sortOrder="desc")
    pt$sortRowDataGroups(levelNumber=2, orderBy="calculation", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("Sort outlined groups - sorting only a subset of the row groups")
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
    pt$addRowDataGroups("TOC",
                        outlineBefore=list(groupStyleDeclarations=list(color="blue")),
                        outlineAfter=list(isEmpty=FALSE,
                                          mergeSpace="dataGroupsOnly",
                                          caption="Total ({value})",
                                          groupStyleDeclarations=list("font-style"="italic")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue"),
                                          cellStyleDeclarations=list("color"="blue")))
    pt$addRowDataGroups("PowerType", addTotal=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, fromIndex=4, orderBy="value", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  custom balance sheet 1")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    df <- data.frame(
      Level1 = c("Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income","Net entrepreneurial income"),
      Level2 = c("Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Net operating surplus","Interests and rents","Interests and rents","Interests and rents"),
      Level3 = c("Factor income","Factor income","Factor income","Factor income","Factor income","Factor income","Factor income","Factor income","Compensation of employees","Paid rent","Paid interest","Received interest"),
      Level4 = c("Net value added","Net value added","Net value added","Net value added","Net value added","Net value added","Taxes and subsidies","Taxes and subsidies",NA,NA,NA,NA),
      Level5 = c("Gross value added","Gross value added","Gross value added","Gross value added","Gross value added","Depreciation","Other taxes on production","Other subsidies (non-product specific)",NA,NA,NA,NA),
      Level6 = c("Production of the agricultural industry","Production of the agricultural industry","Production of the agricultural industry","Production of the agricultural industry","Intermediate services",NA,NA,NA,NA,NA,NA,NA),
      Level7 = c("Crop production","Livestock production","Production of agricultural services","Other production",NA,NA,NA,NA,NA,NA,NA,NA),
      MaxGroupLevel = c(7,7,7,7,6,5,5,5,3,3,3,3),
      Value = c(4210.9,4857.7,676.6,405.8,-6299,-2086.7,-145.4,2920.6,-1245,-236.5,-244.7,10.1)
    )

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(df)
    pt$addRowDataGroups("Level1", onlyAddGroupIf="MaxGroupLevel>=1", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE))
    pt$addRowDataGroups("Level2", onlyAddGroupIf="MaxGroupLevel>=2", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE),
                        dataSortOrder="custom", customSortOrder=c("Net operating surplus", "Interests and rents"))
    pt$addRowDataGroups("Level3", addTotal=FALSE, onlyAddGroupIf="MaxGroupLevel>=3", outlineBefore=list(isEmpty=FALSE),
                        dataSortOrder="custom", customSortOrder=c("Factor income", "Compensation of employees",
                                                                  "Paid rent", "Paid interest", "Received interest"))
    pt$addRowDataGroups("Level4", onlyAddGroupIf="MaxGroupLevel>=4", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE))
    pt$addRowDataGroups("Level5", onlyAddGroupIf="MaxGroupLevel>=5", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE),
                        dataSortOrder="custom", customSortOrder=c("Gross value added", "Depreciation", "Other taxes on production",
                                                                  "Other subsidies (non-product specific)"))
    pt$addRowDataGroups("Level6", onlyAddGroupIf="MaxGroupLevel>=6", addTotal=FALSE, outlineBefore=list(isEmpty=FALSE),
                        dataSortOrder="custom", customSortOrder=c("Production of the agricultural industry", "Intermediate Services"))
    pt$addRowDataGroups("Level7", onlyAddGroupIf="MaxGroupLevel>=7", addTotal=FALSE,
                        dataSortOrder="custom", customSortOrder=c("Crop production", "Livestock production",
                                                                  "Production of agricultural services", "Other production"))
    pt$defineCalculation(calculationName="Value", summariseExpression="sum(Value)")
    pt$evaluatePivot()
    pt$removeEmptyRows()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE))
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"7\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Value</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"7\">Net entrepreneurial income</th>\n    <td class=\"OutlineCell\">2824.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"19\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Net operating surplus</th>\n    <td class=\"OutlineCell\">3295.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"14\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Factor income</th>\n    <td class=\"OutlineCell\">4540.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"12\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Net value added</th>\n    <td class=\"OutlineCell\">1765.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Gross value added</th>\n    <td class=\"OutlineCell\">3852</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"6\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Production of the agricultural industry</th>\n    <td class=\"OutlineCell\">10151</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">Crop production</th>\n    <td class=\"Cell\">4210.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Livestock production</th>\n    <td class=\"Cell\">4857.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Production of agricultural services</th>\n    <td class=\"Cell\">676.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Other production</th>\n    <td class=\"Cell\">405.8</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Intermediate services</th>\n    <td class=\"OutlineCell\">-6299</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Depreciation</th>\n    <td class=\"OutlineCell\">-2086.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Taxes and subsidies</th>\n    <td class=\"OutlineCell\">2775.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Other taxes on production</th>\n    <td class=\"OutlineCell\">-145.4</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Other subsidies (non-product specific)</th>\n    <td class=\"OutlineCell\">2920.6</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Compensation of employees</th>\n    <td class=\"OutlineCell\">-1245</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Interests and rents</th>\n    <td class=\"OutlineCell\">-471.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Paid rent</th>\n    <td class=\"OutlineCell\">-236.5</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Paid interest</th>\n    <td class=\"OutlineCell\">-244.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Received interest</th>\n    <td class=\"OutlineCell\">10.1</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE)), 31557)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  custom balance sheet 2")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    df <- data.frame(
      Level1 = rep("Net entrepreneurial income", times=12),
      Level2 = c(rep("Net operating surplus", 9), rep("Interests and rents", 3)),
      Level3 = c(rep("Factor income", 8),"Compensation of employees","Paid rent",
                 "Paid interest","Received interest"),
      Level4 = c(rep("Net value added", 6), rep("Taxes and subsidies", 2), rep(NA, 4)),
      Level5 = c(rep("Gross value added", 5),"Depreciation","Other taxes on production",
                 "Other subsidies (non-product specific)", rep(NA, 4)),
      Level6 = c(rep("Production of the agricultural industry", 4),
                 "Intermediate services", rep(NA, 7)),
      Level7 = c("Crop production","Livestock production",
                 "Production of agricultural services","Other production", rep(NA, 8)),
      MaxGroupLevel = c(7,7,7,7,6,5,5,5,3,3,3,3),
      Budget2019 = c(4150.39,4739.2,625.6,325.8,-6427,-2049.3,
                     -145.4,2847.3,-1149,-221.2,-307.6,12.8),
      Actual2019 = c(3978.8,4341.1,603.7,343,-6063.9,-2079.6,
                     -136.8,2578.6,-1092.9,-203.3,-327.6,14.1),
      Budget2020 = c(4210.9,4857.7,676.6,405.8,-6299,-2086.7,
                     -145.4,2920.6,-1245,-236.5,-244.7,10.1),
      Actual2020 = c(4373.7,5307.6,693.9,408.2,-7065.3,-1985,
                     -154.2,3063,-1229.3,-268.2,-250.3,11.1)
    )

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$setDefault(addTotal=FALSE, outlineBefore=list(isEmpty=FALSE))
    pt$addData(df)
    pt$addRowDataGroups("Level1", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>1")
    pt$addRowDataGroups("Level2", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>2",
                        dataSortOrder="custom",
                        customSortOrder=c("Net operating surplus", "Interests and rents"))
    pt$addRowDataGroups("Level3", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>3",
                        dataSortOrder="custom",
                        customSortOrder=c("Factor income", "Compensation of employees",
                                          "Paid rent", "Paid interest", "Received interest"))
    pt$addRowDataGroups("Level4", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>4")
    pt$addRowDataGroups("Level5", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>5",
                        dataSortOrder="custom",
                        customSortOrder=c("Gross value added", "Depreciation",
                                          "Other taxes on production",
                                          "Other subsidies (non-product specific)"))
    pt$addRowDataGroups("Level6", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>6",
                        dataSortOrder="custom",
                        customSortOrder=c("Production of the agricultural industry",
                                          "Intermediate Services"))
    pt$addRowDataGroups("Level7", dataSortOrder="custom",
                        customSortOrder=c("Crop production", "Livestock production",
                                          "Production of agricultural services", "Other production"))
    pt$defineCalculation(calculationName="Budget",
                         summariseExpression="sum(Budget2020)")
    pt$defineCalculation(calculationName="Actual",
                         summariseExpression="sum(Actual2020)")
    pt$defineCalculation(calculationName="Variance",
                         summariseExpression="sum(Actual2020)-sum(Budget2020)",
                         format="%.1f")
    pt$evaluatePivot()

    # get the row groups relating to outline groups
    # (above leaf level) and leaf level groups
    grps <- pt$findRowDataGroups(outlineGroups="only",
                                 outlineLinkedGroupExists=FALSE)
    grps <- c(grps, pt$findRowDataGroups(atLevel=7))
    # set the styling of these groups so the text isn't bold
    pt$setStyling(groups=grps, declarations =list("font-weight"="normal"))

    # find the cells corresponding to these groups
    rowNumbers <- sapply(grps, pt$findGroupRowNumbers)
    cells <- pt$getCells(rowNumbers=rowNumbers)
    # set the styling of these cells to be normal cells
    # instead of the darker outline styling
    pt$setStyling(cells=cells, baseStyleName="Cell")

    # apply the red style for negative variance
    cells <- pt$findCells(calculationNames="Variance",
                          minValue=-1000, maxValue=0,
                          includeNull=FALSE, includeNA=FALSE)
    pt$setStyling(cells=cells, declarations=list("color"="#9C0006"))
    # apply the green style for positive variance
    cells <- pt$findCells(calculationNames="Variance",
                          minValue=0, maxValue=10000,
                          includeNull=FALSE, includeNA=FALSE)
    pt$setStyling(cells=cells, declarations=list("color"="#006100"))

    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE))
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"7\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Budget</th>\n    <th class=\"ColumnHeader\">Actual</th>\n    <th class=\"ColumnHeader\">Variance</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"7\">Net entrepreneurial income</th>\n    <td class=\"OutlineCell\">2824.4</td>\n    <td class=\"OutlineCell\">2905.2</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">80.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"19\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Net operating surplus</th>\n    <td class=\"OutlineCell\">3295.5</td>\n    <td class=\"OutlineCell\">3412.6</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">117.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"14\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Factor income</th>\n    <td class=\"OutlineCell\">4540.5</td>\n    <td class=\"OutlineCell\">4641.9</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">101.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"12\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Net value added</th>\n    <td class=\"OutlineCell\">1765.3</td>\n    <td class=\"OutlineCell\">1733.1</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-32.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Gross value added</th>\n    <td class=\"OutlineCell\">3852</td>\n    <td class=\"OutlineCell\">3718.1</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-133.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"6\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Production of the agricultural industry</th>\n    <td class=\"OutlineCell\">10151</td>\n    <td class=\"OutlineCell\">10783.4</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">632.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Crop production</th>\n    <td class=\"Cell\">4210.9</td>\n    <td class=\"Cell\">4373.7</td>\n    <td class=\"Cell\" style=\"color: #006100; \">162.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Livestock production</th>\n    <td class=\"Cell\">4857.7</td>\n    <td class=\"Cell\">5307.6</td>\n    <td class=\"Cell\" style=\"color: #006100; \">449.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Production of agricultural services</th>\n    <td class=\"Cell\">676.6</td>\n    <td class=\"Cell\">693.9</td>\n    <td class=\"Cell\" style=\"color: #006100; \">17.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Other production</th>\n    <td class=\"Cell\">405.8</td>\n    <td class=\"Cell\">408.2</td>\n    <td class=\"Cell\" style=\"color: #006100; \">2.4</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"2\">Intermediate services</th>\n    <td class=\"Cell\">-6299</td>\n    <td class=\"Cell\">-7065.3</td>\n    <td class=\"Cell\" style=\"color: #9C0006; \">-766.3</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Depreciation</th>\n    <td class=\"Cell\">-2086.7</td>\n    <td class=\"Cell\">-1985</td>\n    <td class=\"Cell\" style=\"color: #006100; \">101.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Taxes and subsidies</th>\n    <td class=\"OutlineCell\">2775.2</td>\n    <td class=\"OutlineCell\">2908.8</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">133.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Other taxes on production</th>\n    <td class=\"Cell\">-145.4</td>\n    <td class=\"Cell\">-154.2</td>\n    <td class=\"Cell\" style=\"color: #9C0006; \">-8.8</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Other subsidies (non-product specific)</th>\n    <td class=\"Cell\">2920.6</td>\n    <td class=\"Cell\">3063</td>\n    <td class=\"Cell\" style=\"color: #006100; \">142.4</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Compensation of employees</th>\n    <td class=\"Cell\">-1245</td>\n    <td class=\"Cell\">-1229.3</td>\n    <td class=\"Cell\" style=\"color: #006100; \">15.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Interests and rents</th>\n    <td class=\"OutlineCell\">-471.1</td>\n    <td class=\"OutlineCell\">-507.4</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-36.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Paid rent</th>\n    <td class=\"Cell\">-236.5</td>\n    <td class=\"Cell\">-268.2</td>\n    <td class=\"Cell\" style=\"color: #9C0006; \">-31.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Paid interest</th>\n    <td class=\"Cell\">-244.7</td>\n    <td class=\"Cell\">-250.3</td>\n    <td class=\"Cell\" style=\"color: #9C0006; \">-5.6</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Received interest</th>\n    <td class=\"Cell\">10.1</td>\n    <td class=\"Cell\">11.1</td>\n    <td class=\"Cell\" style=\"color: #006100; \">1.0</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE)), 65002)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("pvt row/col tests:  custom balance sheet 3")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    df <- data.frame(
      Level1 = rep("Net entrepreneurial income", times=12),
      Level2 = c(rep("Net operating surplus", 9), rep("Interests and rents", 3)),
      Level3 = c(rep("Factor income", 8),"Compensation of employees","Paid rent",
                 "Paid interest","Received interest"),
      Level4 = c(rep("Net value added", 6), rep("Taxes and subsidies", 2), rep(NA, 4)),
      Level5 = c(rep("Gross value added", 5),"Depreciation","Other taxes on production",
                 "Other subsidies (non-product specific)", rep(NA, 4)),
      Level6 = c(rep("Production of the agricultural industry", 4),
                 "Intermediate services", rep(NA, 7)),
      Level7 = c("Crop production","Livestock production",
                 "Production of agricultural services","Other production", rep(NA, 8)),
      MaxGroupLevel = c(7,7,7,7,6,5,5,5,3,3,3,3),
      Budget2019 = c(4150.39,4739.2,625.6,325.8,-6427,-2049.3,
                     -145.4,2847.3,-1149,-221.2,-307.6,12.8),
      Actual2019 = c(3978.8,4341.1,603.7,343,-6063.9,-2079.6,
                     -136.8,2578.6,-1092.9,-203.3,-327.6,14.1),
      Budget2020 = c(4210.9,4857.7,676.6,405.8,-6299,-2086.7,
                     -145.4,2920.6,-1245,-236.5,-244.7,10.1),
      Actual2020 = c(4373.7,5307.6,693.9,408.2,-7065.3,-1985,
                     -154.2,3063,-1229.3,-268.2,-250.3,11.1)
    )

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt <- PivotTable$new()
    ob <- list(isEmpty=FALSE, nocgApplyOutlineStyling=FALSE,
               nocgGroupStyleDeclarations=list("font-weight"="normal"))
    pt$setDefault(addTotal=FALSE, outlineBefore=ob)
    pt$addData(df)
    pt$addRowDataGroups("Level1", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>1")
    pt$addRowDataGroups("Level2", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>2",
                        dataSortOrder="custom",
                        customSortOrder=c("Net operating surplus", "Interests and rents"))
    pt$addRowDataGroups("Level3", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>3",
                        dataSortOrder="custom",
                        customSortOrder=c("Factor income", "Compensation of employees",
                                          "Paid rent", "Paid interest", "Received interest"))
    pt$addRowDataGroups("Level4", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>4")
    pt$addRowDataGroups("Level5", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>5",
                        dataSortOrder="custom",
                        customSortOrder=c("Gross value added", "Depreciation",
                                          "Other taxes on production",
                                          "Other subsidies (non-product specific)"))
    pt$addRowDataGroups("Level6", outlineBefore=TRUE,
                        onlyAddOutlineChildGroupIf="MaxGroupLevel>6",
                        dataSortOrder="custom",
                        customSortOrder=c("Production of the agricultural industry",
                                          "Intermediate Services"))
    pt$addRowDataGroups("Level7", dataSortOrder="custom",
                        customSortOrder=c("Crop production", "Livestock production",
                                          "Production of agricultural services", "Other production"),
                        styleDeclarations=list("font-weight"="normal"))
    pt$defineCalculation(calculationName="Budget",
                         summariseExpression="sum(Budget2020)")
    pt$defineCalculation(calculationName="Actual",
                         summariseExpression="sum(Actual2020)")
    pt$defineCalculation(calculationName="Variance",
                         summariseExpression="sum(Actual2020)-sum(Budget2020)",
                         format="%.1f")
    pt$evaluatePivot()

    # apply the red style for negative variance
    cells <- pt$findCells(calculationNames="Variance",
                          minValue=-1000, maxValue=0,
                          includeNull=FALSE, includeNA=FALSE)
    pt$setStyling(cells=cells, declarations=list("color"="#9C0006"))
    # apply the green style for positive variance
    cells <- pt$findCells(calculationNames="Variance",
                          minValue=0, maxValue=10000,
                          includeNull=FALSE, includeNA=FALSE)
    pt$setStyling(cells=cells, declarations=list("color"="#006100"))

    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE))
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"7\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Budget</th>\n    <th class=\"ColumnHeader\">Actual</th>\n    <th class=\"ColumnHeader\">Variance</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"7\">Net entrepreneurial income</th>\n    <td class=\"OutlineCell\">2824.4</td>\n    <td class=\"OutlineCell\">2905.2</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">80.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"19\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Net operating surplus</th>\n    <td class=\"OutlineCell\">3295.5</td>\n    <td class=\"OutlineCell\">3412.6</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">117.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"14\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Factor income</th>\n    <td class=\"OutlineCell\">4540.5</td>\n    <td class=\"OutlineCell\">4641.9</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">101.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"12\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Net value added</th>\n    <td class=\"OutlineCell\">1765.3</td>\n    <td class=\"OutlineCell\">1733.1</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-32.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Gross value added</th>\n    <td class=\"OutlineCell\">3852</td>\n    <td class=\"OutlineCell\">3718.1</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-133.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"6\">&nbsp;</th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Production of the agricultural industry</th>\n    <td class=\"OutlineCell\">10151</td>\n    <td class=\"OutlineCell\">10783.4</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">632.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">&nbsp;</th>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Crop production</th>\n    <td class=\"Cell\">4210.9</td>\n    <td class=\"Cell\">4373.7</td>\n    <td class=\"Cell\" style=\"color: #006100; \">162.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Livestock production</th>\n    <td class=\"Cell\">4857.7</td>\n    <td class=\"Cell\">5307.6</td>\n    <td class=\"Cell\" style=\"color: #006100; \">449.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Production of agricultural services</th>\n    <td class=\"Cell\">676.6</td>\n    <td class=\"Cell\">693.9</td>\n    <td class=\"Cell\" style=\"color: #006100; \">17.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \">Other production</th>\n    <td class=\"Cell\">405.8</td>\n    <td class=\"Cell\">408.2</td>\n    <td class=\"Cell\" style=\"color: #006100; \">2.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"2\">Intermediate services</th>\n    <td class=\"Total\">-6299</td>\n    <td class=\"Total\">-7065.3</td>\n    <td class=\"Total\" style=\"color: #9C0006; \">-766.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Depreciation</th>\n    <td class=\"Total\">-2086.7</td>\n    <td class=\"Total\">-1985</td>\n    <td class=\"Total\" style=\"color: #006100; \">101.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"4\">Taxes and subsidies</th>\n    <td class=\"OutlineCell\">2775.2</td>\n    <td class=\"OutlineCell\">2908.8</td>\n    <td class=\"OutlineCell\" style=\"color: #006100; \">133.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Other taxes on production</th>\n    <td class=\"Total\">-145.4</td>\n    <td class=\"Total\">-154.2</td>\n    <td class=\"Total\" style=\"color: #9C0006; \">-8.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"3\">Other subsidies (non-product specific)</th>\n    <td class=\"Total\">2920.6</td>\n    <td class=\"Total\">3063</td>\n    <td class=\"Total\" style=\"color: #006100; \">142.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Compensation of employees</th>\n    <td class=\"Total\">-1245</td>\n    <td class=\"Total\">-1229.3</td>\n    <td class=\"Total\" style=\"color: #006100; \">15.7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"6\">Interests and rents</th>\n    <td class=\"OutlineCell\">-471.1</td>\n    <td class=\"OutlineCell\">-507.4</td>\n    <td class=\"OutlineCell\" style=\"color: #9C0006; \">-36.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">&nbsp;</th>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Paid rent</th>\n    <td class=\"Total\">-236.5</td>\n    <td class=\"Total\">-268.2</td>\n    <td class=\"Total\" style=\"color: #9C0006; \">-31.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Paid interest</th>\n    <td class=\"Total\">-244.7</td>\n    <td class=\"Total\">-250.3</td>\n    <td class=\"Total\" style=\"color: #9C0006; \">-5.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; \" colspan=\"5\">Received interest</th>\n    <td class=\"Total\">10.1</td>\n    <td class=\"Total\">11.1</td>\n    <td class=\"Total\" style=\"color: #006100; \">1.0</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE)), 65002)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


