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


context("MAP STYLING TESTS")


scenarios <- testScenarios("map styling:  smoke tests")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()

    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="range",
                  mappings=list(0, "green", 10000, "yellow", 35000, "red"))
    pt$mapStyling(cells=pt$allCells, styleProperty="color", valueType="color", mapType="range",
                  mappings=list(from=c(0, 10000, 35000), to=c("white", "black", "white")))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">830</td>\n    <td class=\"Total\" style=\"background-color: green; color: white; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">63</td>\n    <td class=\"Total\" style=\"background-color: yellow; color: black; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">33792</td>\n    <td class=\"Total\" style=\"background-color: red; color: white; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: green; color: white; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: red; color: white; \">49025</td>\n    <td class=\"Total\" style=\"background-color: yellow; color: black; \">34685</td>\n    <td class=\"Total\" style=\"background-color: red; color: white; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=text, mapType=value")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="text", mapType="value",
                  mappings=list(3079, "bold", 14487, "bold", 33792, "bold", 63, 100, 83710, 900))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">14487</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=text, mapType=logic")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="text", mapType="logic",
                  mappings=list("v==3079", "bold", "v<1000", 100, "5000<=v<25000", "bold", "v>50000", 900))
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="text", mapType="logic",
                  mappings=list("is.null(v)", "red"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3079</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\" style=\"font-weight: bold; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: red; \"></td>\n    <td class=\"Total\" style=\"font-weight: bold; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=text, mapType=range")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="text", mapType="range",
                  mappings=list(0, 100, 5000, "normal", 30000, "bold", 80000, 900))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">3079</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">830</td>\n    <td class=\"Total\" style=\"font-weight: 100; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: normal; \">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\" style=\"font-weight: normal; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: normal; \">14487</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">33792</td>\n    <td class=\"Total\" style=\"font-weight: bold; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: normal; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"font-weight: normal; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"font-weight: bold; \">49025</td>\n    <td class=\"Total\" style=\"font-weight: bold; \">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=number, mapType=value")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="number", mapType="value",
                  mappings=list(3079, 700, 14487, 700, 33792, 700, 63, 100, 83710, 900))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">14487</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=number, mapType=logic")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="number", mapType="logic",
                  mappings=list("v==3079", 700, "v<1000", 100, "5000<=v<25000", 700, "v>50000", 900))
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="text", mapType="logic",
                  mappings=list("is.null(v)", "red"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3079</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\" style=\"font-weight: 700; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: red; \"></td>\n    <td class=\"Total\" style=\"font-weight: 700; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=number, mapType=range")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="font-weight", valueType="number", mapType="range",
                  mappings=list(0, 100, 5000, 700, 30000, 700, 80000, 900))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">3079</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">830</td>\n    <td class=\"Total\" style=\"font-weight: 100; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">22865</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">63</td>\n    <td class=\"Total\" style=\"font-weight: 700; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">14487</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">33792</td>\n    <td class=\"Total\" style=\"font-weight: 700; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"font-weight: 700; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"font-weight: 700; \">49025</td>\n    <td class=\"Total\" style=\"font-weight: 700; \">34685</td>\n    <td class=\"Total\" style=\"font-weight: 900; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=number, mapType=continuous")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()

    pt$setStyling(cells=pt$allCells, declarations=list("color"="white", "background-color"="green"))
    pt$mapStyling(cells=pt$allCells, styleProperty="opacity", valueType="number", mapType="continuous",
                  mappings=list(0, 0.35, 85000, 1))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.374; \">3079</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.356; \">830</td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.38; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.525; \">22865</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">63</td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.525; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.461; \">14487</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.608; \">33792</td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.719; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.416; \">8594</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; \"></td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.416; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.725; \">49025</td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.615; \">34685</td>\n    <td class=\"Total\" style=\"color: white; background-color: green; opacity: 0.99; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=color, mapType=value")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="value",
                  mappings=list(3079, "red", 14487, "#00ff00", 33792, "rgb(64,64,255)", 63, "rgba(128,128,255,0.5)", 83710, "green"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"background-color: rgba(128,128,255,0.5); \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #00ff00; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: rgb(64,64,255); \">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"background-color: green; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=color, mapType=logic")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="logic",
                  mappings=list("v==3079", "pink", "v<1000", "red", "5000<=v<25000", "yellow", "v>50000", "green"))
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="text", mapType="logic",
                  mappings=list("is.null(v)", "red"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: pink; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: red; \">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: red; \">63</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: red; \"></td>\n    <td class=\"Total\" style=\"background-color: yellow; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"background-color: green; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=color, mapType=range")
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

    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="range",
                  mappings=list(0, "red", 5000, "orange", 30000, "yellow", 80000, "green"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: red; \">830</td>\n    <td class=\"Total\" style=\"background-color: red; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: red; \">63</td>\n    <td class=\"Total\" style=\"background-color: orange; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">33792</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: orange; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: yellow; \">49025</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">34685</td>\n    <td class=\"Total\" style=\"background-color: green; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  valueType=color, mapType=continuous")
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
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()

    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="continuous",
                  mappings=list(0, "red", 5000, "orange", 30000, "yellow", 80000, "green"))

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #ff6600; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #ff1b00; \">830</td>\n    <td class=\"Total\" style=\"background-color: #ff8100; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #ffe500; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: #ff0200; \">63</td>\n    <td class=\"Total\" style=\"background-color: #ffe600; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #ffc700; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: #ecf500; \">33792</td>\n    <td class=\"Total\" style=\"background-color: #a2d100; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: #ffb200; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: #ffb200; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: #9ecf00; \">49025</td>\n    <td class=\"Total\" style=\"background-color: #e7f300; \">34685</td>\n    <td class=\"Total\" style=\"background-color: #008000; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)

    # test color values
    fc <- function(cell) {
      if(is.null(cell$style)) return("-")
      value <- cell$style$getPropertyValue("background-color")
      if(is.null(value)) return("-")
      if(is.na(value)) return("na")
      value <- gsub("#", "", value)
      return(value)
    }
    totalColour <- paste(sapply(pt$allCells, fc), collapse="|")
    expect_identical(totalColour, "ff6600|ff1b00|ff8100|ffe500|ff0200|ffe600|ffc700|ecf500|a2d100|ffb200|-|ffb200|9ecf00|e7f300|008000")

    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  mapping function")
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

    redclr <- function(x, cell) {
      clr <- 255-floor(140*cell$columnNumber/3)
      return(paste0("#",
                    format(as.hexmode(255), width=2),
                    format(as.hexmode(clr), width=2),
                    format(as.hexmode(clr), width=2)))
    }
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", mappings=redclr)

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #ffd1d1; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">830</td>\n    <td class=\"Total\" style=\"background-color: #ff7373; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #ffd1d1; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">63</td>\n    <td class=\"Total\" style=\"background-color: #ff7373; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #ffd1d1; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">33792</td>\n    <td class=\"Total\" style=\"background-color: #ff7373; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: #ffd1d1; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \"></td>\n    <td class=\"Total\" style=\"background-color: #ff7373; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"background-color: #ffd1d1; \">49025</td>\n    <td class=\"Total\" style=\"background-color: #ffa2a2; \">34685</td>\n    <td class=\"Total\" style=\"background-color: #ff7373; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  mapping range (3 \"from\", 2 \"to\")")
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

    # note below there are 3 "from" values and 2 "to" values, and styleHigherValues has been disabled
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="range",
                  mappings=list(0, "red", 1000, "orange", 15000), styleHigherValues=FALSE)

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: red; \">830</td>\n    <td class=\"Total\" style=\"background-color: orange; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"background-color: red; \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: orange; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("map styling:  mapping range (extended lower")
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

    # note below there are 3 "from" values and 2 "to" values, and styleHigherValues has been disabled
    pt$mapStyling(cells=pt$allCells, styleProperty="background-color", valueType="color", mapType="range",
                  mappings=list(500, "red", 1000, "orange", 15000), styleLowerValues=TRUE, styleHigherValues=FALSE)

    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: red; \">830</td>\n    <td class=\"Total\" style=\"background-color: orange; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"background-color: red; \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\" style=\"background-color: orange; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



