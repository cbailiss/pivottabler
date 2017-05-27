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

context("BASIC TESTS")

scenarios <- testScenarios("bhmtrains basic pivot total", runAllForReleaseVersion=TRUE)
for(i in 1:length(scenarios)) {
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
    # prepStr(pt$print(asString=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales               3079                 830   3909  \nCrossCountry                     22865                  63  22928  \nLondon Midland                   14487               33792  48279  \nVirgin Trains                     8594                       8594  \nTotal                            49025               34685  83710  "

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(pt$print(asCharacter=TRUE), str)
  })
}


context("SMOKE TESTS")


scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot values", runAllForReleaseVersion=TRUE)
for(i in 1:length(scenarios)) {
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
    # cat(paste(as.vector(pt$cells$asMatrix()), sep=", ", collapse=", "))
    res <- c(3079, 22865, 14487, 8594, 49025, 830, 63, 33792, NA, 34685, 3909, 22928, 48279, 8594, 83710)

    expect_equal(pt$cells$asMatrix(), matrix(res, nrow=5))
  })
}



scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot html", runAllForReleaseVersion=TRUE)
for(i in 1:length(scenarios)) {
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
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot values two levels")
for(i in 1:length(scenarios)) {
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
    # pt$renderPivot()
    # cat(paste(as.vector(pt$cells$asMatrix()), sep=", ", collapse=", "))
    res <- c(3079, 22133, 5638, 2137, 32987, NA, NA, 8849, 6457, 15306, NA, 732, NA, NA, 732, 3079, 22865, 14487, 8594, 49025, 830, 63, 5591, NA, 6484, NA, NA, 28201, NA, 28201, 830, 63, 33792, NA, 34685, 3909, 22928, 48279, 8594, 83710)

    expect_equal(pt$cells$asMatrix(), matrix(res, nrow=5))
  })
}


scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot html two levels")
for(i in 1:length(scenarios)) {
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
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot values two levels expanded")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType", expandExistingTotals=TRUE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # cat(paste(as.vector(pt$cells$asMatrix()), sep=", ", collapse=", "))
    res <- c(3079, 22133, 5638, 2137, 32987, NA, NA, 8849, 6457, 15306, NA, 732, NA, NA, 732, 3079, 22865, 14487, 8594, 49025, 830, 63, 5591, NA, 6484, NA, NA, 28201, NA, 28201, 830, 63, 33792, NA, 34685, 3909, 22196, 11229, 2137, 39471, NA, NA, 37050, 6457, 43507, NA, 732, NA, NA, 732, 3909, 22928, 48279, 8594, 83710)

    expect_equal(pt$cells$asMatrix(), matrix(res, nrow=5))
  })
}


scenarios <- testScenarios("smoke tests:  bhmtrains basic pivot html two levels expanded")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType", expandExistingTotals=TRUE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22196</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">11229</td>\n    <td class=\"Cell\">37050</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">39471</td>\n    <td class=\"Cell\">43507</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("smoke tests:  calculation filters")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    library(pivottabler)

    # get the date of each train and whether that date is a weekday or weekend
    trains <- bhmtrains %>%
      mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
             DayNumber=wday(GbttDateTime),
             WeekdayOrWeekend=ifelse(DayNumber %in% c(1,7), "Weekend", "Weekday"))

    # render the pivot table
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    weekendFilter <- PivotFilters$new(pt, variableName="WeekdayOrWeekend", values="Weekend")
    pt$defineCalculation(calculationName="WeekendTrains", summariseExpression=countFunction,
                         filters=weekendFilter, visible=FALSE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction, visible=FALSE)
    pt$defineCalculation(calculationName="WeekendTrainsPercentage",
                         type="calculation", basedOn=c("WeekendTrains", "TotalTrains"),
                         format="%.1f %%",
                         calculationExpression="values$WeekendTrains/values$TotalTrains*100")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # cat(paste(as.vector(pt$cells$asMatrix()), sep=", ", collapse=", "))
    # prepStr(as.character(pt$getHtml()))
    res <- c(26.6320233842157, 23.7480865952329, 18.7892593359564, 24.4240167558762, 22.5823559408465, 17.710843373494, NA, 22.8693181818182, NA, 22.7043390514632, 24.7377845996419, 23.6828332170272, 21.6450216450216, 24.4240167558762, 22.6328992951858)
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">26.6 %</td>\n    <td class=\"Cell\">17.7 %</td>\n    <td class=\"Cell\">24.7 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">23.7 %</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">23.7 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">18.8 %</td>\n    <td class=\"Cell\">22.9 %</td>\n    <td class=\"Cell\">21.6 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">24.4 %</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">24.4 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">22.6 %</td>\n    <td class=\"Cell\">22.7 %</td>\n    <td class=\"Cell\">22.6 %</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 296.5828)
    expect_equal(pt$cells$asMatrix(), matrix(res, nrow=5))
    expect_identical(as.character(pt$getHtml()), html)

  })
}


scenarios <- testScenarios("smoke tests:  ignoring parent groups")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

  	expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("smoke tests:  contradictory filters")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Express Passenger</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Ordinary Passenger</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("QUICK-PIVOT TESTS")


test_that("quick-pivot tests:  qpvt pivot 1", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory", "n()")
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qpvt pivot 2", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, c("=", "TOC"), c("TrainCategory", "PowerType"),
             c("Number of Trains"="n()", "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"))
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Number of Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Maximum Speed</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 505565)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qpvt pivot format 1 - single calc", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory", "mean(SchedSpeedMPH, na.rm=TRUE)", format="%.0f")
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">89</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">113</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">98</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">93</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">101</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1426.677, tolerance=1e-2)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qpvt pivot format 2 - two calcs basic", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
             c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
             formats=list("%.0f", "%.1f"))
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">89</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">98</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">93</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qpvt pivot format 3 - two calcs list", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
             c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
             formats=list(list(digits = 3, nsmall=1), "%.1f"))
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">89.7</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">89.0</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">89.5</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">112.9</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">100.0</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">112.9</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">97.6</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">90.8</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">92.9</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125.0</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125.0</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109.1</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">90.8</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">101.5</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qpvt pivot format 3 - two calcs function", {

  library(pivottabler)
  qf <- function(x) { return(paste0("**", sprintf("%.0f", x), "**"))}
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
             c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
             formats=list(qf, "%.1f"))
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">**90**</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">**89**</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">**90**</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">**113**</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">**100**</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">**113**</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">**98**</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">**91**</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">**93**</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">**125**</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">**125**</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">**109**</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">**91**</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">**101**</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
  expect_identical(as.character(pt$getHtml()), html)
})


test_that("quick-pivot tests:  qlpvt pivot 1", {

  library(pivottabler)
  lchr <- qlpvt(bhmtrains, "TOC", "TrainCategory", "n()", caption="my caption", label="mylabel")
  # prepStr(lchr)
  latex <- "\\begin{table}[h!]\n  \\centering\n  \\caption{my caption}\n  \\label{tab:mylabel}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  & 3079 & 830 & 3909\\\\\n    CrossCountry  & 22865 & 63 & 22928\\\\\n    London Midland  & 14487 & 33792 & 48279\\\\\n    Virgin Trains  & 8594 &  & 8594\\\\\n    Total  & 49025 & 34685 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

  expect_identical(lchr, latex)
})


context("BASIC LAYOUT TESTS")


scenarios <- testScenarios("basic layout tests:  empty pivot")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "    "
    html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  empty pivot plus data")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "    "
    html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  just a total")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "  TotalTrains  \n        83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 83710)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  two measures")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "  TotalTrains  MaxSchedSpeed  \n        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 83835)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows only")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "Arriva Trains Wales    \nCrossCountry           \nLondon Midland         \nVirgin Trains          \nTotal                  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"0\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows plus total")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     TotalTrains  \nArriva Trains Wales         3909  \nCrossCountry               22928  \nLondon Midland             48279  \nVirgin Trains               8594  \nTotal                      83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows plus two measures")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     TotalTrains  MaxSchedSpeed  \nArriva Trains Wales         3909             90  \nCrossCountry               22928            125  \nLondon Midland             48279            110  \nVirgin Trains               8594            125  \nTotal                      83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns only")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "  Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \n                                                                           "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

  	expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus total")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "  Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \n                 3909         22928           48279           8594  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus two totals")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "  Arriva Trains Wales         CrossCountry                London Midland              Virgin Trains               Total                       \n  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  \n         3909             90        22928            125        48279            110         8594            125        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"0\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows and columns only")
for(i in 1:length(scenarios)) {
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
    pt$evaluatePivot()
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales                                                \nCrossCountry                                                       \nLondon Midland                                                     \nVirgin Trains                                                      \nTotal                                                              "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

  	expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and calculation")
for(i in 1:length(scenarios)) {
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
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales               3079                 830   3909  \nCrossCountry                     22865                  63  22928  \nLondon Midland                   14487               33792  48279  \nVirgin Trains                     8594                       8594  \nTotal                            49025               34685  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and two calculations")
for(i in 1:length(scenarios)) {
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
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     Express Passenger           Ordinary Passenger          Total                       \n                     TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  TotalTrains  MaxSchedSpeed  \nArriva Trains Wales         3079             90          830             90         3909             90  \nCrossCountry               22865            125           63            100        22928            125  \nLondon Midland             14487            110        33792            100        48279            110  \nVirgin Trains               8594            125                                     8594            125  \nTotal                      49025            125        34685            100        83710            125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">TotalTrains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">MaxSchedSpeed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus total on row")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "             Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \nTotalTrains                 3909         22928           48279           8594  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167420)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  columns plus two totals on rows")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix())
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "               Arriva Trains Wales  CrossCountry  London Midland  Virgin Trains  Total  \nTotalTrains                   3909         22928           48279           8594  83710  \nMaxSchedSpeed                   90           125             110            125    125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 167995)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and calculation on rows")
for(i in 1:length(scenarios)) {
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
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                     Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales               3079                 830   3909  \nCrossCountry                     22865                  63  22928  \nLondon Midland                   14487               33792  48279  \nVirgin Trains                     8594                       8594  \nTotal                            49025               34685  83710  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("basic layout tests:  rows, columns and two calculations on rows")
for(i in 1:length(scenarios)) {
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
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(pt$print(asString=TRUE))
    str <- "                                    Express Passenger  Ordinary Passenger  Total  \nArriva Trains Wales  TotalTrains                 3079                 830   3909  \n                     MaxSchedSpeed                 90                  90     90  \nCrossCountry         TotalTrains                22865                  63  22928  \n                     MaxSchedSpeed                125                 100    125  \nLondon Midland       TotalTrains                14487               33792  48279  \n                     MaxSchedSpeed                110                 100    110  \nVirgin Trains        TotalTrains                 8594                       8594  \n                     MaxSchedSpeed                125                        125  \nTotal                TotalTrains                49025               34685  83710  \n                     MaxSchedSpeed                125                 100    125  "
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\">TotalTrains</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">MaxSchedSpeed</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(pt$print(asCharacter=TRUE), str)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("DATA GROUP TESTS")


scenarios <- testScenarios("data groups tests:  dplyr ignoring parent groups")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  adding data groups explicitly")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addColumnDataGroups("PowerType", fromData=FALSE, explicitListOfValues=list("DMU", "EMU"))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 500796)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  adding data groups that combine values")
for(i in 1:length(scenarios)) {
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
    pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list(
      "London Midland", "CrossCountry", "Other"=c("Arriva Trains Wales", "Virgin Trains")))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Other</th>\n    <td class=\"Cell\">5216</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">11673</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">12503</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

scenarios <- testScenarios("data groups tests:  formatting data groups")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(lubridate)
    trains <- mutate(bhmtrains,
       GbttDate=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
       GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%Y-%m")) # use numbers here for formatting as the names vary between locales/languages (i.e. the test will fail on some systems)
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">2016-12</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">2017-01</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">2017-02</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1291</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1291</td>\n    <td class=\"Cell\">1402</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1402</td>\n    <td class=\"Cell\">1216</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1216</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">7314</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">236</td>\n    <td class=\"Cell\">7550</td>\n    <td class=\"Cell\">7777</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">256</td>\n    <td class=\"Cell\">8033</td>\n    <td class=\"Cell\">7105</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">240</td>\n    <td class=\"Cell\">7345</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">3635</td>\n    <td class=\"Cell\">11967</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">15602</td>\n    <td class=\"Cell\">3967</td>\n    <td class=\"Cell\">13062</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">17029</td>\n    <td class=\"Cell\">3627</td>\n    <td class=\"Cell\">12021</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">15648</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">740</td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2877</td>\n    <td class=\"Cell\">728</td>\n    <td class=\"Cell\">2276</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3004</td>\n    <td class=\"Cell\">669</td>\n    <td class=\"Cell\">2044</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2713</td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">12980</td>\n    <td class=\"Cell\">14104</td>\n    <td class=\"Cell\">236</td>\n    <td class=\"Cell\">27320</td>\n    <td class=\"Cell\">13874</td>\n    <td class=\"Cell\">15338</td>\n    <td class=\"Cell\">256</td>\n    <td class=\"Cell\">29468</td>\n    <td class=\"Cell\">12617</td>\n    <td class=\"Cell\">14065</td>\n    <td class=\"Cell\">240</td>\n    <td class=\"Cell\">26922</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  sort by group into descending order")
for(i in 1:length(scenarios)) {
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
    pt$addRowDataGroups("TOC", dataSortOrder="desc")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  numerical sort by group into descending order")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    a <- c(7,4,6,1,8,3,2,9,5,10,12,11,0)
    b <- c(1,5,4,2,3,2,4,3,1,5,2,1,4)
    z <- a + b
    df <- data.frame(a, b, z)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("a", dataSortOrder="asc")
    pt$addRowDataGroups("b", dataSortOrder="desc")
    pt$defineCalculation(calculationName="z", summariseExpression="sum(z)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">0</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">6</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">7</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">8</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">9</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">10</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">11</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">12</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">24</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">4</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">20</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14</td>\n    <td class=\"Cell\">22</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">26</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">14</td>\n    <td class=\"Cell\">115</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 460)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  sort by value into descending order")
for(i in 1:length(scenarios)) {
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
    pt$addRowDataGroups("TOC", dataSortOrder="desc")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, orderBy="calculation", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  sort by level 2 value into descending order")
for(i in 1:length(scenarios)) {
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
    pt$sortColumnDataGroups(levelNumber=2, orderBy="calculation", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("CALCULATION TESTS")


scenarios <- testScenarios("calculation tests:  calculate dply summarise")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    library(dplyr)
    library(lubridate)

    # derive some additional data
    trains <- mutate(bhmtrains,
       ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
       ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta))

    # create the pivot table
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addRowDataGroups("TOC", totalCaption="All TOCs")
    pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                         summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MinArrivalDelay", caption="Min Arr. Delay",
                         summariseExpression="min(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="MaxArrivalDelay", caption="Max Arr. Delay",
                         summariseExpression="max(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="MeanArrivalDelay", caption="Mean Arr. Delay",
                         summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.1f")
    pt$defineCalculation(calculationName="MedianArrivalDelay", caption="Median Arr. Delay",
                         summariseExpression="median(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="IQRArrivalDelay", caption="Delay IQR",
                         summariseExpression="IQR(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="SDArrivalDelay", caption="Delay Std. Dev.",
                         summariseExpression="sd(ArrivalDelay, na.rm=TRUE)", format="%.1f")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sprintf("%.6f", sum(pt$cells$asMatrix()))
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Min Arr. Delay</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Max Arr. Delay</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Arr. Delay</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Median Arr. Delay</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Delay IQR</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Delay Std. Dev.</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">49</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">273</td>\n    <td class=\"Cell\">3.5</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">8.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">177</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">181</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">All TOCs</th>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">273</td>\n    <td class=\"Cell\">2.7</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">6.1</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("calculation tests:  calculate on rows dply summarise")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    library(dplyr)
    library(lubridate)

    # derive some additional data
    trains <- mutate(bhmtrains,
       ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
       ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta))

    # create the pivot table
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("TOC", totalCaption="All TOCs")
    pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                         summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MinArrivalDelay", caption="Min Arr. Delay",
                         summariseExpression="min(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="MaxArrivalDelay", caption="Max Arr. Delay",
                         summariseExpression="max(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="MeanArrivalDelay", caption="Mean Arr. Delay",
                         summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.1f")
    pt$defineCalculation(calculationName="MedianArrivalDelay", caption="Median Arr. Delay",
                         summariseExpression="median(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="IQRArrivalDelay", caption="Delay IQR",
                         summariseExpression="IQR(ArrivalDelay, na.rm=TRUE)")
    pt$defineCalculation(calculationName="SDArrivalDelay", caption="Delay Std. Dev.",
                         summariseExpression="sd(ArrivalDelay, na.rm=TRUE)", format="%.1f")
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sprintf("%.6f", sum(pt$cells$asMatrix()))
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">CrossCountry</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">London Midland</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Virgin Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">All TOCs</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total Trains</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Min Arr. Delay</th>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Max Arr. Delay</th>\n    <td class=\"Cell\">49</td>\n    <td class=\"Cell\">273</td>\n    <td class=\"Cell\">177</td>\n    <td class=\"Cell\">181</td>\n    <td class=\"Cell\">273</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Mean Arr. Delay</th>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">3.5</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\">2.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Median Arr. Delay</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Delay IQR</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Delay Std. Dev.</th>\n    <td class=\"Cell\">4.3</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">4.2</td>\n    <td class=\"Cell\">8.4</td>\n    <td class=\"Cell\">6.1</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("calculation tests:  deriving values from other calculations")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    library(dplyr)
    library(lubridate)

    # derive some additional data
    trains <- mutate(bhmtrains,
       ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
       ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta),
       DelayedByMoreThan5Minutes=ifelse(ArrivalDelay>5,1,0))

    # create the pivot table
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addRowDataGroups("TOC", totalCaption="All TOCs")
    pt$defineCalculation(calculationName="DelayedTrains", caption="Trains Arr. 5+ Mins Late",
                         summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm=TRUE)")
    pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                         summariseExpression=countFunction)
    pt$defineCalculation(calculationName="DelayedPercent", caption="% Trains Arr. 5+ Mins Late",
                         type="calculation", basedOn=c("DelayedTrains", "TotalTrains"),
                         format="%.1f %%",
                         calculationExpression="values$DelayedTrains/values$TotalTrains*100")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sprintf("%.6f", sum(pt$cells$asMatrix()))
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Trains Arr. 5+ Mins Late</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">% Trains Arr. 5+ Mins Late</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">372</td>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">9.5 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">2780</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">12.1 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">3561</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">7.4 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">770</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">9.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">All TOCs</th>\n    <td class=\"Cell\">7483</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">8.9 %</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix()), 182432.916225)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("calculation tests:  showing values only")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    library(dplyr)

    # perform the aggregation in R code explicitly
    trains <- bhmtrains %>%
      group_by(TrainCategory, TOC) %>%
      summarise(NumberOfTrains=n()) %>%
      ungroup()

    # display this pre-calculated data
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory", addTotal=FALSE)   #  <<  *** CODE CHANGE ***  <<
    pt$addRowDataGroups("TOC", addTotal=FALSE)                #  <<  *** CODE CHANGE ***  <<
    pt$defineCalculation(calculationName="TotalTrains", type="value", valueName="NumberOfTrains")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("calculation tests:  showing values plus totals")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    library(dplyr)

    # perform the aggregation in R code explicitly
    trains <- bhmtrains %>%
      group_by(TrainCategory, TOC) %>%
      summarise(NumberOfTrains=n()) %>%
      ungroup()

    # display this pre-calculated data
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains",  # <<  *** CODE CHANGE (AND BELOW) *** <<
                         type="value", valueName="NumberOfTrains",
                         summariseExpression="sum(NumberOfTrains)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  calcs first 1")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="NumberOfTrains", caption="Number of Trains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaximumSpeedMPH", caption="Maximum Speed (MPH)", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addColumnCalculationGroups()
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Number of Trains</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Maximum Speed (MPH)</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  calcs first 2")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="NumberOfTrains", caption="Number of Trains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaximumSpeedMPH", caption="Maximum Speed (MPH)", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Number of Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Maximum Speed (MPH)</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 505565)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


# Failed testing on win builder (R 3.4.0 alpha) - guess: something to do with NA matching or NA sorting
  # 2. Failure: specific tests:  checking NA matching (@testGeneral.R#772) ---------
  # digest::digest(as.character(pt$getHtml()), algo = "md5") not identical to "4de9b5984fc79813e347de07177f6d58".
  # 1/1 mismatches
  # x[1]: "b60fb6d08a52644b7e535c105a444579"
  # y[1]: "4de9b5984fc79813e347de07177f6d58"
# test_that("specific tests:  checking NA matching", {
#
#   checkDigestAvailable()
#
#   library(pivottabler)
#   pt <- PivotTable$new()
#   pt$addData(bhmtrains)
#   pt$addColumnDataGroups("TrainCategory")
#   pt$addRowDataGroups("TOC")
#   pt$addRowDataGroups("PowerType")
#   pt$addRowDataGroups("SchedSpeedMPH")    #    << **** CODE CHANGE **** <<
#   pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
#   pt$evaluatePivot()
#   # pt$renderPivot()
#   # sum(pt$cells$asMatrix(), na.rm=TRUE)
#   # prepStr(as.character(pt$getHtml()))
#
#   expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
#   expect_identical(as.character(pt$getHtml()), html), "4de9b5984fc79813e347de07177f6d58")
# })


context("VISUAL TOTALS TESTS")



scenarios <- testScenarios("visual totals")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Express Passenger"), visualTotals=TRUE)
    pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list("Arriva Trains Wales", "CrossCountry", "London Midland"), visualTotals=TRUE)
    pt$addRowDataGroups("PowerType", fromData=FALSE, explicitListOfValues=list("DMU"), visualTotals=TRUE)
    pt$addRowDataGroups("SchedSpeedMPH", fromData=FALSE, explicitListOfValues=list(90, 100), visualTotals=TRUE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\">2340</td>\n    <td class=\"Cell\">2340</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\">2450</td>\n    <td class=\"Cell\">2450</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4790</td>\n    <td class=\"Cell\">4790</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">4790</td>\n    <td class=\"Cell\">4790</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">18740</td>\n    <td class=\"Cell\">18740</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 149920)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("THEMING TESTS")



scenarios <- testScenarios("theming tests:  basic test")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    # define the colours
    orangeColors <- list(
      headerBackgroundColor = "rgb(237, 125, 49)",
      headerColor = "rgb(255, 255, 255)",
      cellBackgroundColor = "rgb(255, 255, 255)",
      cellColor = "rgb(0, 0, 0)",
      totalBackgroundColor = "rgb(248, 198, 165)",
      totalColor = "rgb(0, 0, 0)",
      borderColor = "rgb(198, 89, 17)"
    )
    # create the pivot table
    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getSimpleColoredTheme(parentPivot=pt, colors=orangeColors, fontName="Garamond, arial")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(as.character(pt$getCss()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
  	css <- ".Table {border-collapse: collapse; border: 2px solid rgb(198, 89, 17); }\r\n.ColumnHeader {font: 0.75em Garamond, arial; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.RowHeader {font: 0.75em Garamond, arial; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.Cell {font: 0.75em Garamond, arial; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font: 0.75em Garamond, arial; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); text-align: right; color: rgb(0, 0, 0); background-color: rgb(248, 198, 165); }\r\n"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


context("EMPTY GROUP TESTS")



scenarios <- testScenarios("empty data group test 1")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Freight"))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("empty data group test 2")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Freight"), visualTotals=TRUE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("empty data group test 3")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 236790)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("empty data group test 4")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"), visualTotals=TRUE)
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">33792</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">34685</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 138740)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("empty data group test 5")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"))
    pt$addRowDataGroups("TOC")
    pt$addColumnDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 306160)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("empty data group test 6")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"), visualTotals=TRUE)
    pt$addRowDataGroups("TOC")
    pt$addColumnDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Freight</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">33792</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">34685</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 208110)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("EXPORT TESTS")


scenarios <- testScenarios("export tests:  as Matrix (without row headings)")
for(i in 1:length(scenarios)) {
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
    # pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE)
    # sum(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE), na.rm=TRUE)
    # mean(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE), na.rm=TRUE)
    # min(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE), na.rm=TRUE)
    # max(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE)), sep=" ", collapse=" "))
  	text <- "3079 22133 5638 2137 32987 NA NA 8849 6457 15306 NA 732 NA NA 732 3079 22865 14487 8594 49025 830 63 5591 NA 6484 NA NA 28201 NA 28201 830 63 33792 NA 34685 3909 22928 48279 8594 83710"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(mean(pt$cells$asMatrix(), na.rm=TRUE), 16742)
    expect_equal(min(pt$cells$asMatrix(), na.rm=TRUE), 63)
    expect_equal(max(pt$cells$asMatrix(), na.rm=TRUE), 83710)
    expect_identical(paste(as.character(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE)), sep=" ", collapse=" "), text)
  })
}


scenarios <- testScenarios("export tests:  as Matrix (with row headings)")
for(i in 1:length(scenarios)) {
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
    # prepStr(paste(as.character(pt$asMatrix(includeHeaders=TRUE)), sep=" ", collapse=" "))
    text <- "  Arriva Trains Wales CrossCountry London Midland Virgin Trains Total Express Passenger DMU 3079 22133 5638 2137 32987  EMU   8849 6457 15306  HST  732   732  Total 3079 22865 14487 8594 49025 Ordinary Passenger DMU 830 63 5591  6484  EMU   28201  28201  Total 830 63 33792  34685 Total  3909 22928 48279 8594 83710"

    expect_identical(paste(as.character(pt$asMatrix(includeHeaders=TRUE)), sep=" ", collapse=" "), text)
  })
}


scenarios <- testScenarios("export tests:  as Data Frame")
for(i in 1:length(scenarios)) {
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

    # sum(pt$asDataFrame(), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "))
  	text <- "c(3079, 22133, 5638, 2137, 32987) c(NA, NA, 8849, 6457, 15306) c(NA, 732, NA, NA, 732) c(3079, 22865, 14487, 8594, 49025) c(830, 63, 5591, NA, 6484) c(NA, NA, 28201, NA, 28201) c(830, 63, 33792, NA, 34685) c(3909, 22928, 48279, 8594, 83710)"

    expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 502260)
    expect_identical(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "), text)
  })
}


scenarios <- testScenarios("export tests:  as Tidy Data Frame")
for(i in 1:length(scenarios)) {
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

    # sum(pt$asTidyDataFrame()$rawValue, na.rm=TRUE)
    # prepStr(paste(as.character(pt$asTidyDataFrame()), sep=" ", collapse=" "))
  	text <- "c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5) c(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8) c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE) c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4) c(1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 2, 2, 2, 3) c(2, 3, 4, 5, 2, 3, 5, 1, 2, 3, 4, 5, 2, 3, 5, 1, 2, 3, 4, 5, 2, 3, 5, 1, 2, 3, 4, 5, 2, 3, 5, 1, 2, 3, 4, 5, 2, 3, 5, 1) c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4) c(1, 1, 1, 1, 3, 3, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2) c(1, 2, 3, 4, 1, 2, 4, 4, 1, 2, 3, 4, 1, 2, 4, 4, 1, 2, 3, 4, 1, 2, 4, 4, 1, 2, 3, 4, 1, 2, 4, 4, 1, 2, 3, 4, 1, 2, 4, 4) c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) c(3079, NA, NA, 3079, 830, NA, 830, 3909, 22133, NA, 732, 22865, 63, NA, 63, 22928, 5638, 8849, NA, 14487, 5591, 28201, 33792, 48279, 2137, 6457, NA, 8594, NA, NA, NA, 8594, 32987, 15306, 732, 49025, 6484, 28201, 34685, 83710) c(3079, NA, NA, 3079, 830, NA, 830, 3909, 22133, NA, 732, 22865, 63, NA, 63, 22928, 5638, 8849, NA, 14487, 5591, 28201, 33792, 48279, 2137, 6457, NA, 8594, NA, NA, NA, 8594, 32987, 15306, 732, 49025, 6484, 28201, 34685, 83710)"

    expect_equal(sum(pt$asTidyDataFrame()$rawValue, na.rm=TRUE), 502260)
    expect_identical(paste(as.character(pt$asTidyDataFrame()), sep=" ", collapse=" "), text)
  })
}


context("LATEX TESTS")


scenarios <- testScenarios("latex tests:  basic latex table with spans")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
    R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
    C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
    V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
    pt$addColumnDataGroups("C2", addTotal=FALSE)
    pt$addRowDataGroups("R1", addTotal=FALSE)
    pt$addRowDataGroups("R2", addTotal=FALSE)
    pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
    pt$evaluatePivot()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|l|rrr|rrr|rrr|}\n    \\hline\n    \\multicolumn{2}{|c|}{} & \\multicolumn{3}{|c|}{n} & \\multicolumn{3}{|c|}{m} & \\multicolumn{3}{|c|}{e}\\\\\n    \\cline{3-11}\n    \\multicolumn{2}{|c|}{} & a & b & c & a & b & c & a & b & c\\\\\n    \\hline\n    \\multirow{3}{*}{p} & x  & 1 & 2 & 3 & 1 & 2 & 3 & 1 & 2 & 3\\\\\n    & y  & 4 & 5 & 6 & 4 & 5 & 6 & 4 & 5 & 6\\\\\n    & z  & 7 & 8 & 9 & 7 & 8 & 9 & 7 & 8 & 9\\\\\n    \\cline{1-11}\n    \\multirow{3}{*}{q} & x  & 1 & 2 & 3 & 1 & 2 & 3 & 1 & 2 & 3\\\\\n    & y  & 4 & 5 & 6 & 4 & 5 & 6 & 4 & 5 & 6\\\\\n    & z  & 7 & 8 & 9 & 7 & 8 & 9 & 7 & 8 & 9\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  no data")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|}\n    \\hline\n    (no data)\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  just rows")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addRowDataGroups("TOC")
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|r|}\n    \\hline\n    & \\\\\n    \\hline\n    Arriva Trains Wales  & \\\\\n    CrossCountry  & \\\\\n    London Midland  & \\\\\n    Virgin Trains  & \\\\\n    Total  & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  just columns")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n     &  &  & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows and columns")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  &  &  & \\\\\n    CrossCountry  &  &  & \\\\\n    London Midland  &  &  & \\\\\n    Virgin Trains  &  &  & \\\\\n    Total  &  &  & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows, columns and a measure")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  & 3079 & 830 & 3909\\\\\n    CrossCountry  & 22865 & 63 & 22928\\\\\n    London Midland  & 14487 & 33792 & 48279\\\\\n    Virgin Trains  & 8594 &  & 8594\\\\\n    Total  & 49025 & 34685 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  styling headers")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable", boldHeadings=TRUE, italicHeadings=TRUE)

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & \\textbf{\\textit{Express Passenger}} & \\textbf{\\textit{Ordinary Passenger}} & \\textbf{\\textit{Total}}\\\\\n    \\hline\n    \\textbf{\\textit{Arriva Trains Wales}}  & 3079 & 830 & 3909\\\\\n    \\textbf{\\textit{CrossCountry}}  & 22865 & 63 & 22928\\\\\n    \\textbf{\\textit{London Midland}}  & 14487 & 33792 & 48279\\\\\n    \\textbf{\\textit{Virgin Trains}}  & 8594 &  & 8594\\\\\n    \\textbf{\\textit{Total}}  & 49025 & 34685 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows, columns and two measures (on cols)")
for(i in 1:length(scenarios)) {
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
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rr|rr|rr|}\n    \\hline\n    & \\multicolumn{2}{|c|}{Express Passenger} & \\multicolumn{2}{|c|}{Ordinary Passenger} & \\multicolumn{2}{|c|}{Total}\\\\\n    \\cline{2-7}\n    & TotalTrains & MaxSchedSpeed & TotalTrains & MaxSchedSpeed & TotalTrains & MaxSchedSpeed\\\\\n    \\hline\n    Arriva Trains Wales  & 3079 & 90 & 830 & 90 & 3909 & 90\\\\\n    CrossCountry  & 22865 & 125 & 63 & 100 & 22928 & 125\\\\\n    London Midland  & 14487 & 110 & 33792 & 100 & 48279 & 110\\\\\n    Virgin Trains  & 8594 & 125 &  &  & 8594 & 125\\\\\n    Total  & 49025 & 125 & 34685 & 100 & 83710 & 125\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows, columns and two measures (on rows)")
for(i in 1:length(scenarios)) {
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
    pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|l|rrr|}\n    \\hline\n    \\multicolumn{2}{|c|}{} & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    \\multirow{2}{*}{Arriva Trains Wales} & TotalTrains  & 3079 & 830 & 3909\\\\\n    & MaxSchedSpeed  & 90 & 90 & 90\\\\\n    \\cline{1-5}\n    \\multirow{2}{*}{CrossCountry} & TotalTrains  & 22865 & 63 & 22928\\\\\n    & MaxSchedSpeed  & 125 & 100 & 125\\\\\n    \\cline{1-5}\n    \\multirow{2}{*}{London Midland} & TotalTrains  & 14487 & 33792 & 48279\\\\\n    & MaxSchedSpeed  & 110 & 100 & 110\\\\\n    \\cline{1-5}\n    \\multirow{2}{*}{Virgin Trains} & TotalTrains  & 8594 &  & 8594\\\\\n    & MaxSchedSpeed  & 125 &  & 125\\\\\n    \\cline{1-5}\n    \\multirow{2}{*}{Total} & TotalTrains  & 49025 & 34685 & 83710\\\\\n    & MaxSchedSpeed  & 125 & 100 & 125\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  just a total (on columns)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|r|}\n    \\hline\n    & TotalTrains\\\\\n    \\hline\n     & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  two totals (on columns)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains1", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="TotalTrains2", summariseExpression=countFunction)
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rr|}\n    \\hline\n    & TotalTrains1 & TotalTrains2\\\\\n    \\hline\n     & 83710 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  multiple levels on columns (but no rows)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
    R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
    C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
    V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("C0", addTotal=FALSE)
    pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
    pt$addColumnDataGroups("C2", addTotal=FALSE)
    pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|rrr|rrr|}\n    \\hline\n    & \\multicolumn{9}{|c|}{C}\\\\\n    \\cline{2-10}\n    & \\multicolumn{3}{|c|}{n} & \\multicolumn{3}{|c|}{m} & \\multicolumn{3}{|c|}{e}\\\\\n    \\cline{2-10}\n    & a & b & c & a & b & c & a & b & c\\\\\n    \\hline\n     & 24 & 30 & 36 & 24 & 30 & 36 & 24 & 30 & 36\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  multiple levels on columns (but no rows) with the calc on rows")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
    R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
    C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
    V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("C0", addTotal=FALSE)
    pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
    pt$addColumnDataGroups("C2", addTotal=FALSE)
    pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
    pt$addRowCalculationGroups()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|rrr|rrr|}\n    \\hline\n    & \\multicolumn{9}{|c|}{C}\\\\\n    \\cline{2-10}\n    & \\multicolumn{3}{|c|}{n} & \\multicolumn{3}{|c|}{m} & \\multicolumn{3}{|c|}{e}\\\\\n    \\cline{2-10}\n    & a & b & c & a & b & c & a & b & c\\\\\n    \\hline\n    V  & 24 & 30 & 36 & 24 & 30 & 36 & 24 & 30 & 36\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  just a total (on rows)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$addRowCalculationGroups()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|r|}\n    \\hline\n    & \\\\\n    \\hline\n    TotalTrains  & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  two totals (on rows)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$defineCalculation(calculationName="TotalTrains1", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="TotalTrains2", summariseExpression=countFunction)
    pt$addRowCalculationGroups()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|r|}\n    \\hline\n    & \\\\\n    \\hline\n    TotalTrains1  & 83710\\\\\n    TotalTrains2  & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  multiple levels on rows (but no columns)")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
    R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
    C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
    V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addRowDataGroups("R0", addTotal=FALSE)
    pt$addRowDataGroups("R1", addTotal=FALSE)
    pt$addRowDataGroups("R2", addTotal=FALSE)
    pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
    pt$addRowCalculationGroups()
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|l|l|r|}\n    \\hline\n    \\multicolumn{3}{|c|}{} & \\\\\n    \\hline\n    \\multirow{6}{*}{R} & \\multirow{3}{*}{p} & x  & 18\\\\\n    & & y  & 45\\\\\n    & & z  & 72\\\\\n    \\cline{2-4}\n    & \\multirow{3}{*}{q} & x  & 18\\\\\n    & & y  & 45\\\\\n    & & z  & 72\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  multiple levels on rows (but no columns) with the calculation on columns")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
    R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
    C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
    V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addRowDataGroups("R0", addTotal=FALSE)
    pt$addRowDataGroups("R1", addTotal=FALSE)
    pt$addRowDataGroups("R2", addTotal=FALSE)
    pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|l|l|r|}\n    \\hline\n    \\multicolumn{3}{|c|}{} & V\\\\\n    \\hline\n    \\multirow{6}{*}{R} & \\multirow{3}{*}{p} & x  & 18\\\\\n    & & y  & 45\\\\\n    & & z  & 72\\\\\n    \\cline{2-4}\n    & \\multirow{3}{*}{q} & x  & 18\\\\\n    & & y  & 45\\\\\n    & & z  & 72\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows/cols split 1")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable", fromRow=2, toRow=4)

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    CrossCountry  & 22865 & 63 & 22928\\\\\n    London Midland  & 14487 & 33792 & 48279\\\\\n    Virgin Trains  & 8594 &  & 8594\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows/cols split 2")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable", fromColumn=2, toColumn=3)

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rr|}\n    \\hline\n    & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  & 830 & 3909\\\\\n    CrossCountry  & 63 & 22928\\\\\n    London Midland  & 33792 & 48279\\\\\n    Virgin Trains  &  & 8594\\\\\n    Total  & 34685 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows/cols split 3")
for(i in 1:length(scenarios)) {
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
    ltx <- pt$getLatex(caption="My Table", label="mytable", fromRow=3, toRow=5, fromColumn=1, toColumn=2)

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
  	ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger\\\\\n    \\hline\n    London Midland  & 14487 & 33792\\\\\n    Virgin Trains  & 8594 & \\\\\n    Total  & 49025 & 34685\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}


context("FIND GROUP TESTS")


scenarios <- testScenarios("find groups tests:  simple:  variableNames")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
    groups <- pt$findColumnDataGroups(variableNames="TrainCategory")
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 3)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  simple:  variableValues")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
    groups <- pt$findColumnDataGroups(variableValues=list("PowerType"=c("DMU", "HST")))
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 3)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  simple:  exclude totals")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
    groups <- pt$findColumnDataGroups(variableNames="TrainCategory", totals="exclude")
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  simple:  only totals")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
    groups <- pt$findColumnDataGroups(variableNames="TrainCategory", totals="only")
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 1)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  simple:  includeDescendantGroups")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
    groups <- pt$findColumnDataGroups(
      variableValues=list("TrainCategory"="Ordinary Passenger"),
      includeDescendantGroup=TRUE)
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #FFFF00; \" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 4)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  combinations:  variableNames")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
    groups <- pt$findColumnDataGroups(matchMode="combinations",
                                      variableNames=c("TrainCategory", "PowerType"))
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 8)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  combinations:  variableValues")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
    groups <- pt$findColumnDataGroups(matchMode="combinations",
                                      variableValues=list("TrainCategory"="Express Passenger", "PowerType"=c("DMU", "HST")))
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("find groups tests:  combinations:  specific sub total")
for(i in 1:length(scenarios)) {
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
    highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
    groups <- pt$findColumnDataGroups(matchMode="combinations",
                                      variableValues=list("TrainCategory"="Express Passenger", "PowerType"="**"))
    groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # length(groups)
    # prepStr(as.character(pt$getHtml()))
  	html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" style=\"background-color: #00FFFF; \" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_equal(length(groups), 1)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("GET CELLS TESTS")


scenarios <- testScenarios("get cells tests:  whole rows (specifyCellsAsList=FALSE)")
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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


context("FIND CELLS TESTS")


scenarios <- testScenarios("find cells tests:  variableValues 1")
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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
for(i in 1:length(scenarios)) {
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


context("IRREGULAR LAYOUT TESTS")


scenarios <- testScenarios("irregular layout tests:  simple example")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$columnGroup$addChildGroup(variableName="TrainCategory", values="Express Passenger")
    pt$columnGroup$addChildGroup(variableName="PowerType", values="DMU")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">39471</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 176992)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  two-level example A")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    cg1 <- pt$columnGroup$addChildGroup(variableName="TrainCategory", values="Express Passenger")
    cg2 <- pt$columnGroup$addChildGroup(variableName="PowerType", values="DMU")
    cg1$addChildGroup(variableName="Status", values="A")
    cg1$addChildGroup(variableName="Status", values="R")
    cg2$addChildGroup(variableName="SchedSpeedMPH", values="100")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">100</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22270</td>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">11024</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14133</td>\n    <td class=\"Cell\">18</td>\n    <td class=\"Cell\">2723</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8359</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">47780</td>\n    <td class=\"Cell\">55</td>\n    <td class=\"Cell\">13747</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 123164)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  two-level example B")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    cg1 <- pt$columnGroup$addChildGroup(variableName="TrainCategory", values="Express Passenger")
    cg2 <- pt$columnGroup$addChildGroup(variableName="PowerType", values="DMU")
    cg1$addDataGroups("Status")
    cg2$addDataGroups("SchedSpeedMPH")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"6\">DMU</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">C</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">75</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">90</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">100</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">125</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">NA</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">121</td>\n    <td class=\"Cell\">3761</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">27</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22270</td>\n    <td class=\"Cell\">569</td>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">11024</td>\n    <td class=\"Cell\">11040</td>\n    <td class=\"Cell\">132</td>\n    <td class=\"Cell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14133</td>\n    <td class=\"Cell\">336</td>\n    <td class=\"Cell\">18</td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">6104</td>\n    <td class=\"Cell\">2399</td>\n    <td class=\"Cell\">2723</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8359</td>\n    <td class=\"Cell\">226</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2135</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">47780</td>\n    <td class=\"Cell\">1190</td>\n    <td class=\"Cell\">55</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6225</td>\n    <td class=\"Cell\">6160</td>\n    <td class=\"Cell\">13747</td>\n    <td class=\"Cell\">13175</td>\n    <td class=\"Cell\">164</td>\n    <td class=\"Cell\">39471</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 353984)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  two-level example C")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    cgrps <- pt$addColumnDataGroups("PowerType")
    add2Groups <- function(grp) {
      if(!grp$isTotal) {
        grp$addChildGroup(variableName="Status", values="A")
        grp$addChildGroup(variableName="Status", values="R")
      }
    }
    invisible(lapply(cgrps, add2Groups))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"7\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"5\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"2\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">A</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">R</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">21561</td>\n    <td class=\"Cell\">26</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">60</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5534</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8599</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5520</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2028</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">6331</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32141</td>\n    <td class=\"Cell\">33</td>\n    <td class=\"Cell\">14930</td>\n    <td class=\"Cell\">22</td>\n    <td class=\"Cell\">709</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6395</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">27331</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 498018)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  empty groups")
for(i in 1:length(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    cg1 <- pt$columnGroup$addChildGroup(variableName="TrainCategory", values="Express Passenger")
    cg2 <- pt$columnGroup$addChildGroup(variableName="PowerType", values="DMU")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$defineCalculation(calculationGroupName="calcGrp2", calculationName="MaxSpeedMPH",
                         summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    cg3 <- cg1$addChildGroup(caption="Count")
    cg4 <- cg2$addChildGroup(caption="Maximum Speed")
    cg3$addCalculationGroups("default")
    cg4$addCalculationGroups("calcGrp2")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Maximum Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">100</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 98615)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
