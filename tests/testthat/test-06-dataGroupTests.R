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


context("DATA GROUP TESTS")


scenarios <- testScenarios("data groups tests:  dplyr ignoring parent groups")
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

if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("data groups tests:  formatting data groups")
  for(i in 1:nrow(scenarios)) {
    if(!isDevelopmentVersion) break
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
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                           compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
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
}


scenarios <- testScenarios("data groups tests:  sort by group into descending order")
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


scenarios <- testScenarios("data groups tests:  sort by group into custom order")
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
    pt$addRowDataGroups("TOC", dataSortOrder="custom",
                        customSortOrder=c("Arriva Trains Wales", "London Midland", "CrossCountry"))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  numerical sort by group into descending order")
for(i in 1:nrow(scenarios)) {
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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


scenarios <- testScenarios("data groups tests:  sort by group into descending order using function")
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
    pt$addRowDataGroups("TOC", dataSortOrder="desc")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, orderBy="value", sortOrder="desc")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  sort by group into custom order using function")
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
    pt$addRowDataGroups("TOC", dataSortOrder="desc")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$sortRowDataGroups(levelNumber=1, orderBy="customByValue", sortOrder="asc",
                         customOrder=c("Arriva Trains Wales", "London Midland", "CrossCountry"))
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  sort by value into descending order using function")
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


scenarios <- testScenarios("data groups tests:  sort by level 2 value into descending order using function")
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


scenarios <- testScenarios("data groups tests:  row group headers (1 level)")
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
    pt$addRowDataGroups("TOC", header="Train Operating Company")
    pt$addRowDataGroups("PowerType", header="Power Type")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">Train Operating Company</th>\n    <th class=\"RowHeader\">Power Type</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("data groups tests:  row group headers (2 levels)")
  for(i in 1:nrow(scenarios)) {
    if(!isDevelopmentVersion) break
    evaluationMode <- scenarios$evaluationMode[i]
    processingLibrary <- scenarios$processingLibrary[i]
    description <- scenarios$description[i]
    countFunction <- scenarios$countFunction[i]

    test_that(description, {

      # derive the date of each train (from the arrival/dep times),
      # then the month of each train from the date of each train
      library(dplyr)
      library(lubridate)
      library(pivottabler)
      trains <- mutate(bhmtrains,
                       GbttDate=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
                       GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))
      trains <- filter(trains, GbttMonth>=make_date(year=2017, month=1, day=1))

      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
      pt$addData(trains)
      pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
      pt$addColumnDataGroups("PowerType")
      pt$addRowDataGroups("TOC", header="Train Company")
      pt$addRowDataGroups("TrainCategory", header="Train Category")
      pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
      pt$evaluatePivot()
      # pt$renderPivot(showRowGroupHeaders=TRUE)
      # sum(pt$cells$asMatrix(), na.rm=TRUE)
      # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Train Company</th>\n    <th class=\"RowHeader\" rowspan=\"2\">Train Category</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">January 2017</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">February 2017</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">1088</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1088</td>\n    <td class=\"Cell\">974</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">974</td>\n    <td class=\"Total\">2062</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">314</td>\n    <td class=\"Cell\">242</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">242</td>\n    <td class=\"Total\">556</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">1402</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">1402</td>\n    <td class=\"Total\">1216</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">1216</td>\n    <td class=\"Total\">2618</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">7755</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">256</td>\n    <td class=\"Total\">8011</td>\n    <td class=\"Cell\">7085</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">240</td>\n    <td class=\"Total\">7325</td>\n    <td class=\"Total\">15336</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">22</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">22</td>\n    <td class=\"Cell\">20</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">20</td>\n    <td class=\"Total\">42</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">7777</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">256</td>\n    <td class=\"Total\">8033</td>\n    <td class=\"Total\">7105</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">240</td>\n    <td class=\"Total\">7345</td>\n    <td class=\"Total\">15378</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">1956</td>\n    <td class=\"Cell\">3108</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">5064</td>\n    <td class=\"Cell\">1793</td>\n    <td class=\"Cell\">2879</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">4672</td>\n    <td class=\"Total\">9736</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <td class=\"Cell\">2011</td>\n    <td class=\"Cell\">9954</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">11965</td>\n    <td class=\"Cell\">1834</td>\n    <td class=\"Cell\">9142</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">10976</td>\n    <td class=\"Total\">22941</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3967</td>\n    <td class=\"Total\">13062</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">17029</td>\n    <td class=\"Total\">3627</td>\n    <td class=\"Total\">12021</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">15648</td>\n    <td class=\"Total\">32677</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <td class=\"Cell\">728</td>\n    <td class=\"Cell\">2276</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3004</td>\n    <td class=\"Cell\">669</td>\n    <td class=\"Cell\">2044</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2713</td>\n    <td class=\"Total\">5717</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">728</td>\n    <td class=\"Total\">2276</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">3004</td>\n    <td class=\"Total\">669</td>\n    <td class=\"Total\">2044</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">2713</td>\n    <td class=\"Total\">5717</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">13874</td>\n    <td class=\"Total\">15338</td>\n    <td class=\"Total\">256</td>\n    <td class=\"Total\">29468</td>\n    <td class=\"Total\">12617</td>\n    <td class=\"Total\">14065</td>\n    <td class=\"Total\">240</td>\n    <td class=\"Total\">26922</td>\n    <td class=\"Total\">56390</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 507510)
      expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
    })
  }
}


scenarios <- testScenarios("data groups tests:  caption templates 1 (no outlining)")
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
    pt$addColumnDataGroups("TrainCategory", caption="TC:  {value}")
    pt$addColumnDataGroups("PowerType", caption="PT:  {value}")
    pt$addRowDataGroups("TOC", caption="TOC:  {value}")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">TC:  Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">TC:  Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">TC:  Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">PT:  DMU</th>\n    <th class=\"ColumnHeader\">PT:  EMU</th>\n    <th class=\"ColumnHeader\">PT:  HST</th>\n    <th class=\"ColumnHeader\">PT:  Total</th>\n    <th class=\"ColumnHeader\">PT:  DMU</th>\n    <th class=\"ColumnHeader\">PT:  EMU</th>\n    <th class=\"ColumnHeader\">PT:  Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  Total</th>\n    <td class=\"Total\">32987</td>\n    <td class=\"Total\">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  caption templates 2 (partial outlining)")
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
    pt$addColumnDataGroups("PowerType", caption="PT:  {value}")
    pt$addRowDataGroups("TrainCategory", caption="TC:  {value}", outlineBefore=list(isEmpty=FALSE, caption="TC: {value}"))
    pt$addRowDataGroups("TOC", addTotal=FALSE, caption="TOC:  {value}")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">PT:  DMU</th>\n    <th class=\"ColumnHeader\">PT:  EMU</th>\n    <th class=\"ColumnHeader\">PT:  HST</th>\n    <th class=\"ColumnHeader\">PT:  Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">TC: Express Passenger</th>\n    <td class=\"OutlineCell\">32987</td>\n    <td class=\"OutlineCell\">15306</td>\n    <td class=\"OutlineCell\">732</td>\n    <td class=\"OutlineCell\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">TOC:  Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">TC: Ordinary Passenger</th>\n    <td class=\"OutlineCell\">6484</td>\n    <td class=\"OutlineCell\">28201</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">TOC:  Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  London Midland</th>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">33792</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TC:  Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">39471</td>\n    <td class=\"Total\">43507</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  caption templates 3 (full outlining)")
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
    pt$addColumnDataGroups("PowerType", caption="PT:  {value}")
    pt$addRowDataGroups("TrainCategory",
                        outlineBefore=list(isEmpty=FALSE, caption="TC: {value}"),
                        outlineTotal=list(isEmpty=FALSE, caption="TC: {value}"))
    pt$addRowDataGroups("TOC", addTotal=FALSE, caption="TOC:  {value}")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">PT:  DMU</th>\n    <th class=\"ColumnHeader\">PT:  EMU</th>\n    <th class=\"ColumnHeader\">PT:  HST</th>\n    <th class=\"ColumnHeader\">PT:  Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">TC: Express Passenger</th>\n    <td class=\"OutlineCell\">32987</td>\n    <td class=\"OutlineCell\">15306</td>\n    <td class=\"OutlineCell\">732</td>\n    <td class=\"OutlineCell\">49025</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">TOC:  Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">TC: Ordinary Passenger</th>\n    <td class=\"OutlineCell\">6484</td>\n    <td class=\"OutlineCell\">28201</td>\n    <td class=\"OutlineCell\"></td>\n    <td class=\"OutlineCell\">34685</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">TOC:  Arriva Trains Wales</th>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  CrossCountry</th>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">TOC:  London Midland</th>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">33792</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">TC: Total</th>\n    <td class=\"OutlineCell\">39471</td>\n    <td class=\"OutlineCell\">43507</td>\n    <td class=\"OutlineCell\">732</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  getRelatedOutlineGroups and removedRelatedOutlineGroups")
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
    pt$addRowDataGroups("TOC", outlineBefore=TRUE, outlineAfter=TRUE)
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    x <- pt$rowGroup$childGroups[[6]]$getRelatedOutlineGroups()
    y <- x[[1]]$caption
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # length(x)
    # y
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
    expect_equal(length(x), 3)
    expect_identical(y, "CrossCountry")

    pt$rowGroup$childGroups[[4]]$removeGroup(removedRelatedOutlineGroups=TRUE)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\"></th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 410548)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  normal group with multiple values")
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
    pt$addColumnDataGroups("PowerType")
    pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list(
      "London Midland", "CrossCountry", c("Arriva Trains Wales", "Virgin Trains")))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales Virgin Trains</th>\n    <td class=\"Cell\">5216</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">11673</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">12503</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">32987</td>\n    <td class=\"Total\">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  outline group with multiple values")
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
    pt$addRowDataGroups("TOC", outlineBefore=TRUE, fromData=FALSE, explicitListOfValues=list(
      "London Midland", "CrossCountry", c("Arriva Trains Wales", "Virgin Trains")))
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">London Midland</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">CrossCountry</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" colspan=\"2\">Arriva Trains Wales Virgin Trains</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5216</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">6046</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">11673</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">12503</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\"></th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data groups tests:  index and instanceIds")
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
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)

    cgrps <- pt$topColumnGroups
    rgrps <- pt$topRowGroups

    expect_equal(length(cgrps), 3)
    expect_equal(length(rgrps), 5)

    grps <- c(cgrps, rgrps)
    expect_equal(length(grps), 8)

    cix <- pt$columnGroup$getChildIndex(cgrps)
    expect_equal(cix, c(1, 2, 3))

    rix <- pt$rowGroup$getChildIndex(rgrps)
    expect_equal(rix, c(1, 2, 3, 4, 5))

    fx <- function(x) { x$instanceId }
    cid <- sapply(cgrps, fx)
    rid <- sapply(rgrps, fx)
    id <- c(cid, rid)
    expect_identical(any(duplicated(id)), FALSE)

    cix <- pt$columnGroup$findChildIndex(cid)
    expect_equal(cix, c(1, 2, 3))

    rix <- pt$rowGroup$findChildIndex(rid)
    expect_equal(rix, c(1, 2, 3, 4, 5))
  })
}


scenarios <- testScenarios("data groups tests:  navigation methods")
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
    pt$addRowDataGroups("PowerType")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">22865</td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)

    rglc <- pt$rowGroupLevelCount
    cglc <- pt$columnGroupLevelCount
    expect_equal(rglc, 2)
    expect_equal(cglc, 1)

    rgl1 <- pt$getRowGroupsByLevel(level=1)
    fx <- function(x) { x$caption }
    # rgl1 <- paste(sapply(rgl1, fx), collapse="|")
    # prepStr(rgl1)
    rgl1a <- "Arriva Trains Wales|CrossCountry|London Midland|Virgin Trains|Total"
    expect_identical(paste(sapply(rgl1, fx), collapse="|"), rgl1a)

    rgl2 <- pt$getRowGroupsByLevel(level=2)
    fx <- function(x) { x$caption }
    # rgl2 <- paste(sapply(rgl2, fx), collapse="|")
    # prepStr(rgl2)
    rgl2a <- "DMU|Total|DMU|HST|Total|DMU|EMU|Total|DMU|EMU|Total|"
    expect_identical(paste(sapply(rgl2, fx), collapse="|"), rgl2a)

    cgl1 <- pt$getColumnGroupsByLevel(level=1)
    fx <- function(x) { x$caption }
    # cgl1 <- paste(sapply(cgl1, fx), collapse="|")
    # prepStr(cgl1)
    cgl1a <- "Express Passenger|Ordinary Passenger|Total"
    expect_identical(paste(sapply(cgl1, fx), collapse="|"), cgl1a)

    rg <- pt$getLeafRowGroup(r=4)
    rga <- "HST"
    expect_identical(rg$caption, rga)

    cg <- pt$getLeafColumnGroup(c=2)
    cga <- "Ordinary Passenger"
    expect_identical(cg$caption, cga)
  })
}

