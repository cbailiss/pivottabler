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


dtdata <- data.frame(SaleID=1:5, Colour=c("Red", "Red", "Green", "Green", "Green", "Green", "Red", "Green", "Red", "Green"),
                     SaleItem=c("Car", "Lorry", "Car", "Train", "Train", "Lorry", "Car", "Train", "Lorry", "Car"),
                     SaleModel=c("CA", "LA", "CB", "TA", "TB", "LB", "CB", "TC", "LD", "CE"),
                     SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                     SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                     SaleAmount=c(12.1,2.333333333,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                     stringsAsFactors=FALSE)



context("DATA GROUP CUSTOM FORMATTING TESTS")

scenarios <- testScenarios("data group formatting: format() as date")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDate", dataFormat="%d %b %Y")
    pt$defineCalculation(calculationName="TotalAmount", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Amount")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Dec 2017</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jan 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">15 May 2018</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">13.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">28 Jun 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">4.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">03 Sep 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">35.633333333</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 142.533333333)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data group formatting: custom format function (sales)")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)

    formatDate <- function(x) {
      salesDate <- as.Date("15 May 2018", "%d %b %Y")
      tag <- ""
      if (x==salesDate) tag <-" ***Sale***"
      return(paste0(format(x, "%d %b %Y"), tag))
    }

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDate", dataFormat=formatDate)
    pt$defineCalculation(calculationName="TotalAmount", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Amount")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Dec 2017</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jan 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">15 May 2018 ***Sale***</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">13.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">28 Jun 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">4.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">03 Sep 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">35.633333333</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 142.533333333)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("data group formatting: custom format function with params (sales)")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)

    formatDate <- function(x, salesDate) {
      tag <- ""
      if (x==salesDate) tag <-" ***Sale***"
      return(paste0(format(x, "%d %b %Y"), tag))
    }

    salesDate <- as.Date("28 Jun 2018", "%d %b %Y")

    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDate", dataFormat=formatDate, dataFmtFuncArgs=list(salesDate=salesDate))
    pt$defineCalculation(calculationName="TotalAmount", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Amount")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Dec 2017</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jan 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2.533333333</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">15 May 2018</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">13.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">28 Jun 2018 ***Sale***</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">4.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">03 Sep 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">13.2</td>\n    <td class=\"Cell\">2.533333333</td>\n    <td class=\"Cell\">9.3</td>\n    <td class=\"Cell\">6.2</td>\n    <td class=\"Cell\">4.4</td>\n    <td class=\"Cell\">35.633333333</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 142.533333333)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("data group formatting: custom format function (trains)")
  for(i in 1:nrow(scenarios)) {
    if(!isDevelopmentVersion) break
    evaluationMode <- scenarios$evaluationMode[i]
    processingLibrary <- scenarios$processingLibrary[i]
    description <- scenarios$description[i]
    countFunction <- scenarios$countFunction[i]

    test_that(description, {

      library(dplyr)
      library(lubridate)
      library(pivottabler)
      trains <- mutate(bhmtrains,
                       GbttDate=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
                       GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))

      # define a custom formatting function
      formatDate <- function(x) {
        base::format(x, format="%B %Y")
      }

      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
      pt$addData(trains)
      pt$addColumnDataGroups("GbttMonth", dataFormat=formatDate)
      pt$addColumnDataGroups("PowerType")
      pt$addRowDataGroups("TOC")
      pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
      pt$evaluatePivot()
      # pt$renderPivot()
      # sum(pt$cells$asMatrix(), na.rm=TRUE)
      # prepStr(as.character(pt$getHtml()))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">December 2016</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">January 2017</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">February 2017</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1291</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1291</td>\n    <td class=\"Cell\">1402</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1402</td>\n    <td class=\"Cell\">1216</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1216</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">7314</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">236</td>\n    <td class=\"Total\">7550</td>\n    <td class=\"Cell\">7777</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">256</td>\n    <td class=\"Total\">8033</td>\n    <td class=\"Cell\">7105</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">240</td>\n    <td class=\"Total\">7345</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">3635</td>\n    <td class=\"Cell\">11967</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">15602</td>\n    <td class=\"Cell\">3967</td>\n    <td class=\"Cell\">13062</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">17029</td>\n    <td class=\"Cell\">3627</td>\n    <td class=\"Cell\">12021</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">15648</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">740</td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2877</td>\n    <td class=\"Cell\">728</td>\n    <td class=\"Cell\">2276</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3004</td>\n    <td class=\"Cell\">669</td>\n    <td class=\"Cell\">2044</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2713</td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">12980</td>\n    <td class=\"Total\">14104</td>\n    <td class=\"Total\">236</td>\n    <td class=\"Total\">27320</td>\n    <td class=\"Total\">13874</td>\n    <td class=\"Total\">15338</td>\n    <td class=\"Total\">256</td>\n    <td class=\"Total\">29468</td>\n    <td class=\"Total\">12617</td>\n    <td class=\"Total\">14065</td>\n    <td class=\"Total\">240</td>\n    <td class=\"Total\">26922</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
      expect_identical(as.character(pt$getHtml()), html)
    })
  }
}

if (requireNamespace("lubridate", quietly = TRUE)) {

  scenarios <- testScenarios("data group formatting: custom format function with params (trains)")
  for(i in 1:nrow(scenarios)) {
    if(!isDevelopmentVersion) break
    evaluationMode <- scenarios$evaluationMode[i]
    processingLibrary <- scenarios$processingLibrary[i]
    description <- scenarios$description[i]
    countFunction <- scenarios$countFunction[i]

    test_that(description, {

      library(dplyr)
      library(lubridate)
      library(pivottabler)
      trains <- mutate(bhmtrains,
                       GbttDate=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
                       GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))

      # define a custom formatting function
      formatDate <- function(x, formatCode) {
        base::format(x, format=formatCode)
      }

      pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
      pt$addData(trains)
      pt$addColumnDataGroups("GbttMonth", dataFormat=formatDate, dataFmtFuncArgs=list(formatCode="%B %Y"))
      pt$addColumnDataGroups("PowerType")
      pt$addRowDataGroups("TOC")
      pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
      pt$evaluatePivot()
      # pt$renderPivot()
      # sum(pt$cells$asMatrix(), na.rm=TRUE)
      # prepStr(as.character(pt$getHtml()))
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">December 2016</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">January 2017</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">February 2017</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1291</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1291</td>\n    <td class=\"Cell\">1402</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1402</td>\n    <td class=\"Cell\">1216</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">1216</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">7314</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">236</td>\n    <td class=\"Total\">7550</td>\n    <td class=\"Cell\">7777</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">256</td>\n    <td class=\"Total\">8033</td>\n    <td class=\"Cell\">7105</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">240</td>\n    <td class=\"Total\">7345</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">3635</td>\n    <td class=\"Cell\">11967</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">15602</td>\n    <td class=\"Cell\">3967</td>\n    <td class=\"Cell\">13062</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">17029</td>\n    <td class=\"Cell\">3627</td>\n    <td class=\"Cell\">12021</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">15648</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">740</td>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2877</td>\n    <td class=\"Cell\">728</td>\n    <td class=\"Cell\">2276</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3004</td>\n    <td class=\"Cell\">669</td>\n    <td class=\"Cell\">2044</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2713</td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">12980</td>\n    <td class=\"Total\">14104</td>\n    <td class=\"Total\">236</td>\n    <td class=\"Total\">27320</td>\n    <td class=\"Total\">13874</td>\n    <td class=\"Total\">15338</td>\n    <td class=\"Total\">256</td>\n    <td class=\"Total\">29468</td>\n    <td class=\"Total\">12617</td>\n    <td class=\"Total\">14065</td>\n    <td class=\"Total\">240</td>\n    <td class=\"Total\">26922</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

      expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
      expect_identical(as.character(pt$getHtml()), html)
    })
  }
}


scenarios <- testScenarios("data group setStyling()")
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
    cgrps <- pt$addColumnDataGroups("PowerType")
    colorText <- function(grp) {
      if(isTRUE(grp$values=="DMU")) grp$setStyling(list(color="blue"))
      else if(isTRUE(grp$values=="EMU")) grp$setStyling(list(color="green"))
      else if(isTRUE(grp$values=="HST")) grp$setStyling(list(color="red"))
    }
    invisible(lapply(cgrps, colorText))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" style=\"color: blue; \">DMU</th>\n    <th class=\"ColumnHeader\" style=\"color: green; \">EMU</th>\n    <th class=\"ColumnHeader\" style=\"color: red; \">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\" style=\"color: blue; \">DMU</th>\n    <th class=\"ColumnHeader\" style=\"color: green; \">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">32987</td>\n    <td class=\"Total\">15306</td>\n    <td class=\"Total\">732</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">6484</td>\n    <td class=\"Total\">28201</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
