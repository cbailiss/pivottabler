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


context("CALCULATION CUSTOM FORMATTING TESTS")

if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  formatting using base::sprintf() and base::format()")
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
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
      pt$addData(trains)
      pt$addRowDataGroups("TOC", totalCaption="All TOCs")
      pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                           summariseExpression=countFunction)
      pt$defineCalculation(calculationName="MeanArrivalDelay1", caption="Mean Arr. Delay 1",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.2f")
      pt$defineCalculation(calculationName="MeanArrivalDelay2", caption="Mean Arr. Delay 2",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format=list(digits=2, nsmall=2))
      pt$evaluatePivot()
      # pt$renderPivot()
      # sprintf("%.6f", sum(pt$cells$asMatrix()))
      # prepStr(as.character(pt$getHtml()))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 1</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 2</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">2.30</td>\n    <td class=\"Cell\">2.30</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">3.52</td>\n    <td class=\"Cell\">3.52</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">2.27</td>\n    <td class=\"Cell\">2.27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">2.98</td>\n    <td class=\"Cell\">2.98</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">All TOCs</th>\n    <td class=\"Total\">83710</td>\n    <td class=\"Total\">2.71</td>\n    <td class=\"Total\">2.71</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix()), 167447.538966)
      expect_identical(as.character(pt$getHtml()), html)
    })
  }
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  formatting using simple custom format function")
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

      # custom format function
      fmtAddComment <- function(x) {
        formattedNumber <- sprintf("%.1f", x)
        comment <- "-"
        if (x < 2.95) comment <- "Below 3:  "
        else if ((2.95 <= x) && (x < 3.05)) comment <- "Equals 3:  "
        else if (x >= 3.05) comment <- "Over 3:  "
        return(paste0(comment, " ", formattedNumber))
      }

      # create the pivot table
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
      pt$addData(trains)
      pt$addRowDataGroups("TOC", totalCaption="All TOCs")
      pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                           summariseExpression=countFunction)
      pt$defineCalculation(calculationName="MeanArrivalDelay1", caption="Mean Arr. Delay 1",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.1f")
      pt$defineCalculation(calculationName="MeanArrivalDelay2", caption="Mean Arr. Delay 2",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format=fmtAddComment)
      pt$evaluatePivot()
      # pt$renderPivot()
      # sprintf("%.6f", sum(pt$cells$asMatrix()))
      # prepStr(as.character(pt$getHtml()))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 1</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 2</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">Below 3:   2.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">3.5</td>\n    <td class=\"Cell\">Over 3:   3.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">Below 3:   2.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\">Equals 3:   3.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">All TOCs</th>\n    <td class=\"Total\">83710</td>\n    <td class=\"Total\">2.7</td>\n    <td class=\"Total\">Below 3:   2.7</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix()), 167447.538966)
      expect_identical(as.character(pt$getHtml()), html)
    })
  }
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("calculation tests:  formatting using simple custom format function with params")
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

      # custom format function
      fmtNumDP <- function(x, numDP) {
        formatCode <- paste0("%.", numDP, "f")
        formattedNumber <- sprintf(formatCode, x)
        return(formattedNumber)
      }

      # create the pivot table
      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
      pt$addData(trains)
      pt$addRowDataGroups("TOC", totalCaption="All TOCs")
      pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                           summariseExpression=countFunction)

      # define calculations - note the use of the same custom format function (fmtNumDP) but specifying different decimal places
      pt$defineCalculation(calculationName="MeanArrivalDelay1", caption="Mean Arr. Delay 1",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format=fmtNumDP, fmtFuncArgs=list(numDP=1))
      pt$defineCalculation(calculationName="MeanArrivalDelay2", caption="Mean Arr. Delay 2",
                           summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format=fmtNumDP, fmtFuncArgs=list(numDP=2))
      pt$evaluatePivot()
      # pt$renderPivot()
      # sprintf("%.6f", sum(pt$cells$asMatrix()))
      # prepStr(as.character(pt$getHtml()))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 1</th>\n    <th class=\"ColumnHeader\">Mean Arr. Delay 2</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3909</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">2.30</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22928</td>\n    <td class=\"Cell\">3.5</td>\n    <td class=\"Cell\">3.52</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">48279</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">2.27</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\">2.98</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">All TOCs</th>\n    <td class=\"Total\">83710</td>\n    <td class=\"Total\">2.7</td>\n    <td class=\"Total\">2.71</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix()), 167447.538966)
      expect_identical(as.character(pt$getHtml()), html)
    })
  }
}


scenarios <- testScenarios("cell setStyling()")
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
    cells <- pt$getCells(columnNumbers=2)
    colorText <- function(cell) {
      cell$setStyling(list(color="blue"))
    }
    invisible(lapply(cells, colorText))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\" style=\"color: blue; \">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\" style=\"color: blue; \">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\" style=\"color: blue; \">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\" style=\"color: blue; \"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\" style=\"color: blue; \">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
