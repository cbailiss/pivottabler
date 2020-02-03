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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Arriva Trains Wales</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">CrossCountry</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">London Midland</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Virgin Trains</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Arriva Trains Wales</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">CrossCountry</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">London Midland</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\">Virgin Trains</th>\n    <th class=\"OutlineRowHeader\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Arriva Trains Wales</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">CrossCountry</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">London Midland</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"5\">Virgin Trains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"6\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"10\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">HST</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"11\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"11\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">3079</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">830</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">22865</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">63</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">22133</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">HST</th>\n    <td class=\"OutlineCell\">732</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">14487</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">33792</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">5638</td>\n    <td class=\"OutlineCell\">5591</td>\n    <td class=\"OutlineCell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\">8849</td>\n    <td class=\"OutlineCell\">28201</td>\n    <td class=\"OutlineCell\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">8594</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \"></td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">DMU</th>\n    <td class=\"OutlineCell\">2137</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">EMU</th>\n    <td class=\"OutlineCell\">6457</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">HST</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Total\">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">CrossCountry</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"7\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Total\">21621</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">546</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">548</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22133</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">HST</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">709</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">London Midland</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Total\">11054</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Total\">168</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">5638</td>\n    <td class=\"Total\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">35930</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">1082</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\">38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8849</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Virgin Trains</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"8\"></th>\n    <th class=\"RowHeader\" rowspan=\"4\">DMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2028</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">107</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">107</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">2137</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">EMU</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6331</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">119</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"3\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

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
    pt$defineCalculation(calculationName="NumberOfTrains", caption="Number of Trains",
                         summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaximumSpeedMPH", caption="Maximum Speed (MPH)",
                         summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addColumnDataGroups("PowerType")
    pt$addRowCalculationGroups(outlineBefore=list(isEmpty=FALSE, mergeSpace="dataGroupsOnly",
                                                  groupStyleDeclarations=list(color="blue"),
                                                  cellStyleDeclarations=list(color="blue")),
                               outlineAfter=TRUE)
    pt$addRowDataGroups("TOC", addTotal=FALSE)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Number of Trains</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">32987</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">15306</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">732</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">6484</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">28201</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"8\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Maximum Speed (MPH)</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">100</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">90</td>\n    <td class=\"Total\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Total\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">100</td>\n    <td class=\"Total\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Total\">100</td>\n    <td class=\"Total\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">125</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"8\">&nbsp;</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Arriva Trains Wales)</th>\n    <td class=\"OutlineCell\">3079</td>\n    <td class=\"OutlineCell\">830</td>\n    <td class=\"OutlineCell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (Virgin Trains)</th>\n    <td class=\"OutlineCell\">8594</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (London Midland)</th>\n    <td class=\"OutlineCell\">14487</td>\n    <td class=\"OutlineCell\">33792</td>\n    <td class=\"OutlineCell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"font-style: italic; \" colspan=\"2\">Total (CrossCountry)</th>\n    <td class=\"OutlineCell\">22865</td>\n    <td class=\"OutlineCell\">63</td>\n    <td class=\"OutlineCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">49025</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">34685</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


