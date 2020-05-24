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





context("FIND CELLS TESTS")


scenarios <- testScenarios("find cells tests:  variableValues 1")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
    cells <- pt$findCells(variableValues=list("PowerType"=c("DMU", "HST")))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 15)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 80406)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  variableValues 2")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
    cells <- pt$findCells(variableValues=list("PowerType"=c("DMU", "HST"), "TOC"="London Midland"))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 3)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 11229)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  totals")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
    cells <- pt$findCells(variableValues=list("PowerType"="**"))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">830</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">33792</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \"></td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">34685</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 15)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  grand total")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
    cells <- pt$findCells(variableValues=list("TrainCategory"="**", "PowerType"="**", "TOC"="**"))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\" style=\"background-color: #FF00FF; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 1)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 83710)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  conditional formatting")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    redStyle <- PivotStyle$new(pt, "redStyle", list("background-color"="#FFC7CE", "color"="#9C0006"))
    cells <- pt$findCells(minValue=30000, maxValue=50000, includeNull=FALSE, includeNA=FALSE)
    lst <- lapply(cells, function(cell) {cell$style <- redStyle})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">33792</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 5)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 198768)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  constrained with row/column numbers or groups")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$addRowDataGroups("Status")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()

    rgrps <- pt$leafRowGroups
    cgrps <- pt$leafColumnGroups

    cells <- pt$findCells(groups=cgrps[1:2], emptyCells="exclude")
    pt$setStyling(cells=cells, declarations=list("background-color"="yellow"))

    cells <- pt$findCells(groups=list(rgrps[[6]], rgrps[[8]]), minValue=10)
    pt$setStyling(cells=cells, declarations=list("background-color"="lightgreen"))

    cells <- pt$findCells(rowNumbers=2:4, columnGroups=cgrps[7:8],
                          emptyCells="exclude",
                          rowColumnMatchMode="combinations")
    pt$setStyling(cells=cells, declarations=list("background-color"="pink"))

    rgrps <- rgrps[11:17]
    cgrps <- list(cgrps[[4]], cgrps[[5]], cgrps[[7]])
    cells <- pt$findCells(rowGroups=rgrps, columnGroups=cgrps,
                          rowColumnMatchMode="combinations")
    pt$setStyling(cells=cells, declarations=list("background-color"="blanchedalmond"))

    cells <- pt$findCells(rowGroups=rgrps, columnGroups=cgrps,
                          rowColumnMatchMode="combinations",
                          minValue=10000, emptyCells="exclude")
    pt$setStyling(cells=cells, declarations=list("background-color"="orange"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3018</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Cell\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">C</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">59</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \">15</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \">830</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">21561</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\">22270</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Cell\">22330</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">C</th>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">546</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">23</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">569</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">571</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">26</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">22133</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">732</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">63</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">63</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">5534</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8599</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14133</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Cell\">32851</td>\n    <td class=\"Cell\">46984</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">C</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">101</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">235</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">336</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Cell\">914</td>\n    <td class=\"Cell\">1250</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">18</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">4</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">27</td>\n    <td class=\"Cell\">45</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">5638</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: orange; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2028</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">8359</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\">8359</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">C</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">107</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">226</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\">226</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">9</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\">9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2137</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">32987</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">49025</td>\n    <td class=\"Cell\" style=\"background-color: blanchedalmond; \">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 753390)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find cells tests:  value range expressions")
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
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    cells <- pt$findCells(valueRanges=c("30000<=v<34000", "40000<=v<50000"), includeNull=FALSE, includeNA=FALSE)
    pt$setStyling(cells=cells, declarations=list("background-color"="#FFC7CE", "color"="#9C0006"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\" style=\"background-color: #FFC7CE; color: #9C0006; \">33792</td>\n    <td class=\"Total\" style=\"background-color: #FFC7CE; color: #9C0006; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: #FFC7CE; color: #9C0006; \">32987</td>\n    <td class=\"Total\">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\" style=\"background-color: #FFC7CE; color: #9C0006; \">49025</td>\n    <td class=\"Total\">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 4)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 164083)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
