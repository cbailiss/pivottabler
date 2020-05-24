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


context("CALCULATION TESTS")

if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  calculate dply summarise")
  for(i in 1:nrow(scenarios)) {
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
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                           compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  calculate on rows dply summarise")
  for(i in 1:nrow(scenarios)) {
    if(!isDevelopmentVersion) break
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
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                           compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  deriving values from other calculations")
  for(i in 1:nrow(scenarios)) {
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
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                           compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
}



scenarios <- testScenarios("calculation tests:  showing values only")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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



scenarios <- testScenarios("calculation tests:  showing values plus derived totals")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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



scenarios <- testScenarios("calculation tests:  showing values plus pre-calculated totals")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    library(pivottabler)

    # perform the aggregation in R code explicitly
    trains <- bhmtrains %>%
      group_by(TrainCategory, TOC) %>%
      summarise(NumberOfTrains=n()) %>%
      ungroup()

    trainsTrainCat <- bhmtrains %>%
      group_by(TrainCategory) %>%
      summarise(NumberOfTrains=n()) %>%
      ungroup()

    trainsTOC <- bhmtrains %>%
      group_by(TOC) %>%
      summarise(NumberOfTrains=n()) %>%
      ungroup()

    trainsTotal <- bhmtrains %>%
      summarise(NumberOfTrains=n())

    # display this pre-calculated data
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains)
    pt$addTotalData(trainsTrainCat, variableNames="TrainCategory")
    pt$addTotalData(trainsTOC, variableNames="TOC")
    pt$addTotalData(trainsTotal, variableNames=NULL)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", type="value", valueName="NumberOfTrains")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  calcs first 1")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
    pt$defineCalculation(calculationName="NumberOfTrains", caption="Number of Trains", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="MaximumSpeedMPH", caption="Maximum Speed (MPH)", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
    pt$addRowCalculationGroups()
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


scenarios <- testScenarios("calculation tests:  filter overrides - % of row total")
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
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction,
                         caption="Count", visible=FALSE)
    filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="TOC")
    pt$defineCalculation(calculationName="TOCTotalTrains", filters=filterOverrides,
                         summariseExpression=countFunction, caption="TOC Total", visible=FALSE)
    pt$defineCalculation(calculationName="PercentageOfTOCTrains", type="calculation",
                         basedOn=c("CountTrains", "TOCTotalTrains"),
                         calculationExpression="values$CountTrains/values$TOCTotalTrains*100",
                         format="%.1f %%", caption="% of TOC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">78.8 %</td>\n    <td class=\"Cell\">21.2 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">99.7 %</td>\n    <td class=\"Cell\">0.3 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">30.0 %</td>\n    <td class=\"Cell\">70.0 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">100.0 %</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">58.6 %</td>\n    <td class=\"Cell\">41.4 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1000)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - % of grand total")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction, caption="Count", visible=FALSE)
    filterOverrides <- PivotFilterOverrides$new(pt, removeAllFilters=TRUE)
    pt$defineCalculation(calculationName="GrandTotalTrains", filters=filterOverrides, summariseExpression=countFunction, caption="Grand Total", visible=FALSE)
    pt$defineCalculation(calculationName="PercentageOfAllTrains", type="calculation", basedOn=c("CountTrains", "GrandTotalTrains"),
                         calculationExpression="values$CountTrains/values$GrandTotalTrains*100", format="%.1f %%", caption="% of All")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3.7 %</td>\n    <td class=\"Cell\">1.0 %</td>\n    <td class=\"Cell\">4.7 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">27.3 %</td>\n    <td class=\"Cell\">0.1 %</td>\n    <td class=\"Cell\">27.4 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">17.3 %</td>\n    <td class=\"Cell\">40.4 %</td>\n    <td class=\"Cell\">57.7 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">10.3 %</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">10.3 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">58.6 %</td>\n    <td class=\"Cell\">41.4 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 400)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - ratios/multiples")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction, caption="Count", visible=FALSE)
    filterOverrides <- PivotFilterOverrides$new(pt, removeAllFilters=TRUE)
    filterOverrides$add(variableName="TrainCategory", values="Express Passenger", action="replace")
    filterOverrides$add(variableName="TOC", values="CrossCountry", action="replace")
    pt$defineCalculation(calculationName="CrossCountryExpress", filters=filterOverrides, summariseExpression=countFunction, caption="CrossCountry Express Trains", visible=FALSE)
    pt$defineCalculation(calculationName="MultipleOfCCExpressTrains", type="calculation", basedOn=c("CountTrains", "CrossCountryExpress"),
                         calculationExpression="values$CountTrains/values$CrossCountryExpress", format="%.2f", caption="Multiple of CC Express")
    pt$evaluatePivot()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=3)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">0.13</td>\n    <td class=\"Cell\">0.04</td>\n    <td class=\"Cell\">0.17</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">1.00</td>\n    <td class=\"Cell\">0.00</td>\n    <td class=\"Cell\">1.00</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">0.63</td>\n    <td class=\"Cell\">1.48</td>\n    <td class=\"Cell\">2.11</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">0.38</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">0.38</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">2.14</td>\n    <td class=\"Cell\">1.52</td>\n    <td class=\"Cell\">3.66</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=3), 14.644)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - subsets of data")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    filterDMU <- PivotFilter$new(pt, variableName="PowerType", values="DMU")
    filterOverrides <- PivotFilterOverrides$new(pt, filter=filterDMU, action="intersect")
    pt$defineCalculation(calculationName="CountDMU", filters=filterOverrides, summariseExpression=countFunction, caption="DMU", visible=FALSE)
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction, caption="Count", visible=FALSE)
    pt$defineCalculation(calculationName="PercentageDMU", type="calculation", basedOn=c("CountTrains", "CountDMU"),
                         calculationExpression="values$CountDMU/values$CountTrains*100", format="%.1f %%", caption="% DMU")
    pt$evaluatePivot()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=3)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">100.0 %</td>\n    <td class=\"Cell\">100.0 %</td>\n    <td class=\"Cell\">100.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">96.8 %</td>\n    <td class=\"Cell\">100.0 %</td>\n    <td class=\"Cell\">96.8 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">38.9 %</td>\n    <td class=\"Cell\">16.5 %</td>\n    <td class=\"Cell\">23.3 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">24.9 %</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">24.9 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">67.3 %</td>\n    <td class=\"Cell\">18.7 %</td>\n    <td class=\"Cell\">47.2 %</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=3), 855.192)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - custom function 1")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    trains <- bhmtrains %>%
      mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
             GbttDate=as.Date(GbttDateTime))
    januaryDates <- seq(as.Date("2017-01-01"), as.Date("2017-01-07"), by="days")

    # comparison to yesterday
    # date filter function to return yesterday
    getYesterdayDateFilter <- function(pt, filters, cell) {
      # get the date filter
      filter <- filters$getFilter("GbttDate")
      if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
        # there is no filter on GbttDate in this cell
        # i.e. we are in one of the total cells that covers all dates,
        # so the concept of yesterday has no meaning, so block all dates
        newFilter <- PivotFilter$new(pt, variableName="GbttDate", type="NONE")
        filters$setFilter(newFilter, action="replace")
      }
      else {
        # get the date value and subtract one day
        date <- filter$values
        date <- date - 1
        filter$values <- date
      }
    }

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("GbttDate", fromData=FALSE,
                        explicitListOfValues=as.list(januaryDates), visualTotals=TRUE)
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction,
                         caption="Current Day Count")
    filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getYesterdayDateFilter)
    pt$defineCalculation(calculationName="CountPreviousDayTrains", filters=filterOverrides,
                         summariseExpression=countFunction, caption="Previous Day Count")
    pt$defineCalculation(calculationName="Daily Change", type="calculation",
                         basedOn=c("CountTrains", "CountPreviousDayTrains"),
                         calculationExpression="values$CountTrains-values$CountPreviousDayTrains",
                         caption="Daily Change")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Previous Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Daily Change</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Previous Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Daily Change</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Previous Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Daily Change</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-01</th>\n    <td class=\"Cell\">297</td>\n    <td class=\"Cell\">486</td>\n    <td class=\"Cell\">-189</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">309</td>\n    <td class=\"Cell\">-95</td>\n    <td class=\"Cell\">511</td>\n    <td class=\"Cell\">795</td>\n    <td class=\"Cell\">-284</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-02</th>\n    <td class=\"Cell\">565</td>\n    <td class=\"Cell\">297</td>\n    <td class=\"Cell\">268</td>\n    <td class=\"Cell\">318</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">104</td>\n    <td class=\"Cell\">883</td>\n    <td class=\"Cell\">511</td>\n    <td class=\"Cell\">372</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-03</th>\n    <td class=\"Cell\">605</td>\n    <td class=\"Cell\">565</td>\n    <td class=\"Cell\">40</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">318</td>\n    <td class=\"Cell\">120</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">883</td>\n    <td class=\"Cell\">160</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-04</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">605</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">437</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">-1</td>\n    <td class=\"Cell\">1044</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-05</th>\n    <td class=\"Cell\">609</td>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">437</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1047</td>\n    <td class=\"Cell\">1044</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-06</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">609</td>\n    <td class=\"Cell\">-2</td>\n    <td class=\"Cell\">436</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">-2</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">1047</td>\n    <td class=\"Cell\">-4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-07</th>\n    <td class=\"Cell\">577</td>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">-30</td>\n    <td class=\"Cell\">433</td>\n    <td class=\"Cell\">436</td>\n    <td class=\"Cell\">-3</td>\n    <td class=\"Cell\">1010</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">-33</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">3867</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2714</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6581</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 39486)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - custom function 2")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    trains <- bhmtrains %>%
      mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
             GbttDate=as.Date(GbttDateTime))
    januaryDates <- seq(as.Date("2017-01-01"), as.Date("2017-01-07"), by="days")

    # three day rolling average
    # date filter function to a three day range of dates
    getThreeDayFilter <- function(pt, filters, cell) {
      # get the date filter
      filter <- filters$getFilter("GbttDate")
      if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
        # there is no filter on GbttDate in this cell
        # i.e. we are in one of the total cells that covers all dates,
        # so the concept of previous/next day has no meaning, so block all dates
        newFilter <- PivotFilter$new(pt, variableName="GbttDate", type="NONE")
        filters$setFilter(newFilter, action="replace")
      }
      else {
        # get the date value and create three day filter
        date <- filter$values
        newDates <- seq(date-1, date+1, by="days")
        filter$values <- newDates
      }
    }

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("GbttDate", fromData=FALSE,
                        explicitListOfValues=as.list(januaryDates), visualTotals=TRUE)
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction,
                         caption="Current Day Count")
    filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getThreeDayFilter)
    pt$defineCalculation(calculationName="ThreeDayCount", filters=filterOverrides,
                         summariseExpression=countFunction, caption="Three Day Total")
    pt$defineCalculation(calculationName="ThreeDayAverage", type="calculation",
                         basedOn="ThreeDayCount",
                         calculationExpression="values$ThreeDayCount/3",
                         format="%.1f", caption="Three Day Rolling Average")
    pt$evaluatePivot()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=1)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Rolling Average</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Rolling Average</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Three Day Rolling Average</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-01</th>\n    <td class=\"Cell\">297</td>\n    <td class=\"Cell\">1348</td>\n    <td class=\"Cell\">449.3</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">841</td>\n    <td class=\"Cell\">280.3</td>\n    <td class=\"Cell\">511</td>\n    <td class=\"Cell\">2189</td>\n    <td class=\"Cell\">729.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-02</th>\n    <td class=\"Cell\">565</td>\n    <td class=\"Cell\">1467</td>\n    <td class=\"Cell\">489.0</td>\n    <td class=\"Cell\">318</td>\n    <td class=\"Cell\">970</td>\n    <td class=\"Cell\">323.3</td>\n    <td class=\"Cell\">883</td>\n    <td class=\"Cell\">2437</td>\n    <td class=\"Cell\">812.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-03</th>\n    <td class=\"Cell\">605</td>\n    <td class=\"Cell\">1777</td>\n    <td class=\"Cell\">592.3</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">1193</td>\n    <td class=\"Cell\">397.7</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">2970</td>\n    <td class=\"Cell\">990.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-04</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">1821</td>\n    <td class=\"Cell\">607.0</td>\n    <td class=\"Cell\">437</td>\n    <td class=\"Cell\">1313</td>\n    <td class=\"Cell\">437.7</td>\n    <td class=\"Cell\">1044</td>\n    <td class=\"Cell\">3134</td>\n    <td class=\"Cell\">1044.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-05</th>\n    <td class=\"Cell\">609</td>\n    <td class=\"Cell\">1823</td>\n    <td class=\"Cell\">607.7</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">1311</td>\n    <td class=\"Cell\">437.0</td>\n    <td class=\"Cell\">1047</td>\n    <td class=\"Cell\">3134</td>\n    <td class=\"Cell\">1044.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-06</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">1793</td>\n    <td class=\"Cell\">597.7</td>\n    <td class=\"Cell\">436</td>\n    <td class=\"Cell\">1307</td>\n    <td class=\"Cell\">435.7</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">3100</td>\n    <td class=\"Cell\">1033.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-07</th>\n    <td class=\"Cell\">577</td>\n    <td class=\"Cell\">1503</td>\n    <td class=\"Cell\">501.0</td>\n    <td class=\"Cell\">433</td>\n    <td class=\"Cell\">1083</td>\n    <td class=\"Cell\">361.0</td>\n    <td class=\"Cell\">1010</td>\n    <td class=\"Cell\">2586</td>\n    <td class=\"Cell\">862.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">3867</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2714</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6581</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=1), 78457.3)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  filter overrides - custom function 3")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(dplyr)
    trains <- bhmtrains %>%
      mutate(GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
             GbttDate=as.Date(GbttDateTime)) %>%
      filter((as.Date("2017-01-01") <= GbttDate)&(GbttDate <= as.Date("2017-01-07")))
    januaryDates <- seq(as.Date("2017-01-01"), as.Date("2017-01-07"), by="days")

    # date filter function to all dates since 1st jan
    getCumulativeFilter <- function(pt, filters, cell) {
      # get the date filter
      filter <- filters$getFilter("GbttDate")
      if(is.null(filter)||(filter$type=="ALL")||(length(filter$values)>1)) {
        # there is no filter on GbttDate in this cell
        # i.e. we are in one of the total cells that covers all dates,
        # can allow this to just be the total
      }
      else {
        # get the date value and modify the filter
        date <- filter$values
        newDates <- seq(as.Date("2017-01-01"), date, by="days")
        filter$values <- newDates
      }
    }

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(trains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("GbttDate", fromData=FALSE,
                        explicitListOfValues=as.list(januaryDates))
    pt$defineCalculation(calculationName="CountTrains", summariseExpression=countFunction,
                         caption="Current Day Count")
    filterOverrides <- PivotFilterOverrides$new(pt, overrideFunction=getCumulativeFilter)
    pt$defineCalculation(calculationName="CumulativeCount", filters=filterOverrides,
                         summariseExpression=countFunction, caption="Cumulative Count")
    pt$evaluatePivot()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=1)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Cumulative Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Cumulative Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Current Day Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Cumulative Count</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-01</th>\n    <td class=\"Cell\">297</td>\n    <td class=\"Cell\">297</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">214</td>\n    <td class=\"Cell\">511</td>\n    <td class=\"Cell\">511</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-02</th>\n    <td class=\"Cell\">565</td>\n    <td class=\"Cell\">862</td>\n    <td class=\"Cell\">318</td>\n    <td class=\"Cell\">532</td>\n    <td class=\"Cell\">883</td>\n    <td class=\"Cell\">1394</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-03</th>\n    <td class=\"Cell\">605</td>\n    <td class=\"Cell\">1467</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">970</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">2437</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-04</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">2074</td>\n    <td class=\"Cell\">437</td>\n    <td class=\"Cell\">1407</td>\n    <td class=\"Cell\">1044</td>\n    <td class=\"Cell\">3481</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-05</th>\n    <td class=\"Cell\">609</td>\n    <td class=\"Cell\">2683</td>\n    <td class=\"Cell\">438</td>\n    <td class=\"Cell\">1845</td>\n    <td class=\"Cell\">1047</td>\n    <td class=\"Cell\">4528</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-06</th>\n    <td class=\"Cell\">607</td>\n    <td class=\"Cell\">3290</td>\n    <td class=\"Cell\">436</td>\n    <td class=\"Cell\">2281</td>\n    <td class=\"Cell\">1043</td>\n    <td class=\"Cell\">5571</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-01-07</th>\n    <td class=\"Cell\">577</td>\n    <td class=\"Cell\">3867</td>\n    <td class=\"Cell\">433</td>\n    <td class=\"Cell\">2714</td>\n    <td class=\"Cell\">1010</td>\n    <td class=\"Cell\">6581</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">3867</td>\n    <td class=\"Cell\">3867</td>\n    <td class=\"Cell\">2714</td>\n    <td class=\"Cell\">2714</td>\n    <td class=\"Cell\">6581</td>\n    <td class=\"Cell\">6581</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 88492)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("calculation tests:  custom function with calcFuncArgs")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
                     GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
                     GbttDate=make_date(year=year(GbttDateTime), month=month(GbttDateTime), day=day(GbttDateTime)),
                     GbttMonth=make_date(year=year(GbttDateTime), month=month(GbttDateTime), day=1),
                     ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
                     ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta),
                     DelayedByMoreThan5Minutes=ifelse(ArrivalDelay>5,1,0))

    # custom calculation function
    getWorstSingleDayPerformance <- function(pivotCalculator, netFilters, calcFuncArgs,
                                             format, fmtFuncArgs, baseValues, cell) {
      # get the data frame
      trains <- pivotCalculator$getDataFrame("trains")
      # apply the TOC and month filters coming from the headers in the pivot table
      filteredTrains <- pivotCalculator$getFilteredDataFrame(trains, netFilters)
      # calculate the percentage of trains more than five minutes late by date
      dateSummary <- filteredTrains %>%
        group_by(GbttDate) %>%
        summarise(DelayedPercent = sum(DelayedByMoreThan5Minutes, na.rm=TRUE) / n() * 100) %>%
        arrange(desc(DelayedPercent))
      # top value
      tv <- dateSummary$DelayedPercent[1]
      date <- dateSummary$GbttDate[1]
      if(calcFuncArgs$output=="day") {  #     <<  CODE CHANGES HERE  <<
        # build the return value
        value <- list()
        value$rawValue <- date
        value$formattedValue <- format(date, format="%a %d")
      }
      else if(calcFuncArgs$output=="performance") {  #     <<  CODE CHANGES HERE  <<
        # build the return value
        value <- list()
        value$rawValue <- tv
        value$formattedValue <- pivotCalculator$formatValue(tv, format=format)
      }
      return(value)
    }

    # create the pivot table
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(trains, "trains")
    pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
    pt$addRowDataGroups("TOC", totalCaption="All TOCs")
    pt$defineCalculation(calculationName="WorstSingleDay", caption="Day",
                         format="%.1f %%", type="function",
                         calculationFunction=getWorstSingleDayPerformance,
                         calcFuncArgs=list(output="day"))
    pt$defineCalculation(calculationName="WorstSingleDayPerf", caption="Perf",
                         format="%.1f %%", type="function",
                         calculationFunction=getWorstSingleDayPerformance,
                         calcFuncArgs=list(output="performance"))
    pt$evaluatePivot()
    # pt$renderPivot()
    # round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits=1)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">December 2016</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">January 2017</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">February 2017</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">Day</th>\n    <th class=\"ColumnHeader\">Perf</th>\n    <th class=\"ColumnHeader\">Day</th>\n    <th class=\"ColumnHeader\">Perf</th>\n    <th class=\"ColumnHeader\">Day</th>\n    <th class=\"ColumnHeader\">Perf</th>\n    <th class=\"ColumnHeader\">Day</th>\n    <th class=\"ColumnHeader\">Perf</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">Tue 27</td>\n    <td class=\"Cell\">42.9 %</td>\n    <td class=\"Cell\">Sun 29</td>\n    <td class=\"Cell\">18.8 %</td>\n    <td class=\"Cell\">Sun 12</td>\n    <td class=\"Cell\">18.8 %</td>\n    <td class=\"Total\">Tue 27</td>\n    <td class=\"Total\">42.9 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">Thu 01</td>\n    <td class=\"Cell\">35.4 %</td>\n    <td class=\"Cell\">Fri 06</td>\n    <td class=\"Cell\">19.4 %</td>\n    <td class=\"Cell\">Thu 23</td>\n    <td class=\"Cell\">27.9 %</td>\n    <td class=\"Total\">Thu 01</td>\n    <td class=\"Total\">35.4 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">Thu 01</td>\n    <td class=\"Cell\">26.9 %</td>\n    <td class=\"Cell\">Fri 06</td>\n    <td class=\"Cell\">17.2 %</td>\n    <td class=\"Cell\">Thu 23</td>\n    <td class=\"Cell\">12.1 %</td>\n    <td class=\"Total\">Thu 01</td>\n    <td class=\"Total\">26.9 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">Thu 01</td>\n    <td class=\"Cell\">33.0 %</td>\n    <td class=\"Cell\">Thu 12</td>\n    <td class=\"Cell\">21.4 %</td>\n    <td class=\"Cell\">Sat 11</td>\n    <td class=\"Cell\">25.5 %</td>\n    <td class=\"Total\">Thu 01</td>\n    <td class=\"Total\">33.0 %</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">All TOCs</th>\n    <td class=\"Total\">Thu 01</td>\n    <td class=\"Total\">29.5 %</td>\n    <td class=\"Total\">Fri 06</td>\n    <td class=\"Total\">16.3 %</td>\n    <td class=\"Total\">Thu 23</td>\n    <td class=\"Total\">17.1 %</td>\n    <td class=\"Total\">Thu 01</td>\n    <td class=\"Total\">29.5 %</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE), 0), 530)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
