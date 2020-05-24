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


context("GET CELLS TESTS")


scenarios <- testScenarios("all cells test")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)

    allCells <- pt$allCells

    fx <- function(x) { is.null(x$rawValue) }
    cellsWithValues <- allCells[!sapply(allCells, fx)]
    fx <- function(x) { x$rawValue }
    values <- sapply(cellsWithValues, fx)

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_equal(length(allCells), 15)
    expect_equal(sum(values), 334840)
  })
}


scenarios <- testScenarios("get cell test")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    cell <- pt$getCell(2, 2)

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_equal(cell$rawValue, 63)
  })
}


scenarios <- testScenarios("get cells tests:  whole rows (specifyCellsAsList=FALSE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=FALSE, rowNumbers=c(1, 3))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">830</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">830</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">5638</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8849</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">5591</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">28201</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">33792</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 16)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 156564)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  whole rows (specifyCellsAsList=TRUE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=TRUE, rowNumbers=c(1, 3))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">830</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">830</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">5638</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8849</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">5591</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">28201</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">33792</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 16)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 156564)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  whole columns (specifyCellsAsList=FALSE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=FALSE, columnNumbers=2)
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 5)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 30612)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  whole columns (specifyCellsAsList=TRUE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=TRUE, columnNumbers=2)
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 5)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 30612)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  rows, columns and cells (specifyCellsAsList=FALSE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=FALSE, rowNumbers=c(2, NA, 5), columnNumbers=c(NA, 4, 7))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22133</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">732</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 13)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 178654)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  rows, columns and cells (specifyCellsAsList=TRUE)")
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
    cells <- pt$getCells(specifyCellsAsList=TRUE, rowNumbers=2, columnNumbers=4, cellCoordinates=list(c(5, 7)))
    lst <- lapply(cells, function(cell) {cell$style <- highlight})
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(cells)
    # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22133</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">732</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\" style=\"background-color: #00FF00; \">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(cells), 13)
    expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 178654)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  match modes")
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
    cells <- pt$getCells(rowNumbers=2:3, columnNumbers=4:6, matchMode="combinations")
    pt$setStyling(cells=cells, declarations=list("background-color"="#FFCC66"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\" style=\"background-color: #FFCC66; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: #FFCC66; \">63</td>\n    <td class=\"Cell\" style=\"background-color: #FFCC66; \"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: #FFCC66; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: #FFCC66; \">5591</td>\n    <td class=\"Cell\" style=\"background-color: #FFCC66; \">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">32987</td>\n    <td class=\"Total\">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("get cells tests:  groups")
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
    pt$addRowDataGroups("Status")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    rgrps <- pt$leafRowGroups
    cgrps <- pt$leafColumnGroups

    cells <- pt$getCells(groups=list(cgrps[[1]], cgrps[[2]]))
    pt$setStyling(cells=cells, declarations=list("background-color"="yellow"))

    cells <- pt$getCells(groups=list(rgrps[[2]], rgrps[[4]]))
    pt$setStyling(cells=cells, declarations=list("background-color"="lightblue"))

    cells <- pt$getCells(columnGroups=list(cgrps[[4]], cgrps[[5]]))
    pt$setStyling(cells=cells, declarations=list("background-color"="lightgreen"))

    cells <- pt$getCells(rowGroups=list(rgrps[[6]], rgrps[[8]]))
    pt$setStyling(cells=cells, declarations=list("background-color"="pink"))

    cells <- pt$getCells(rowGroups=list(rgrps[[11]], rgrps[[13]], rgrps[[15]]),
                         columnGroups=list(cgrps[[7]], cgrps[[8]]),
                         matchMode="combinations")
    pt$setStyling(cells=cells, declarations=list("background-color"="orange"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3018</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">3018</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">815</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">815</td>\n    <td class=\"Total\">3833</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\" style=\"background-color: lightblue; \">59</td>\n    <td class=\"Cell\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Cell\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">59</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">15</td>\n    <td class=\"Cell\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Total\" style=\"background-color: lightblue; \">15</td>\n    <td class=\"Total\" style=\"background-color: lightblue; \">74</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">2</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: lightblue; \">3079</td>\n    <td class=\"Total\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Total\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">3079</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">830</td>\n    <td class=\"Total\" style=\"background-color: lightblue; \"></td>\n    <td class=\"Total\" style=\"background-color: lightblue; \">830</td>\n    <td class=\"Total\" style=\"background-color: lightblue; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">CrossCountry</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">21561</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\">709</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">22270</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">60</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">60</td>\n    <td class=\"Total\">22330</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\" style=\"background-color: pink; \">546</td>\n    <td class=\"Cell\" style=\"background-color: pink; \"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \">23</td>\n    <td class=\"Total\" style=\"background-color: pink; \">569</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">2</td>\n    <td class=\"Cell\" style=\"background-color: pink; \"></td>\n    <td class=\"Total\" style=\"background-color: pink; \">2</td>\n    <td class=\"Total\" style=\"background-color: pink; \">571</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">26</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">26</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1</td>\n    <td class=\"Total\">27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: pink; \">22133</td>\n    <td class=\"Total\" style=\"background-color: pink; \"></td>\n    <td class=\"Total\" style=\"background-color: pink; \">732</td>\n    <td class=\"Total\" style=\"background-color: pink; \">22865</td>\n    <td class=\"Total\" style=\"background-color: pink; \">63</td>\n    <td class=\"Total\" style=\"background-color: pink; \"></td>\n    <td class=\"Total\" style=\"background-color: pink; \">63</td>\n    <td class=\"Total\" style=\"background-color: pink; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">London Midland</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">5534</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8599</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">14133</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">5520</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Total\">32851</td>\n    <td class=\"Total\">46984</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">101</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">235</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">336</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">67</td>\n    <td class=\"Cell\">847</td>\n    <td class=\"Total\">914</td>\n    <td class=\"Total\">1250</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">18</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">4</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Total\" style=\"background-color: orange; \">27</td>\n    <td class=\"Total\" style=\"background-color: orange; \">45</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: yellow; \">5638</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">8849</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">14487</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">5591</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Virgin Trains</th>\n    <th class=\"RowHeader\">A</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2028</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">6331</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">8359</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: orange; \"></td>\n    <td class=\"Total\" style=\"background-color: orange; \">8359</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">C</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">107</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">119</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">226</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">226</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">R</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">9</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: orange; \"></td>\n    <td class=\"Total\" style=\"background-color: orange; \">9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: yellow; \">2137</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">6457</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">8594</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\" style=\"background-color: yellow; \">32987</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">49025</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 753390)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
