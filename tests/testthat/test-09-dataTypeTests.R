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




# DATA FOR DATA TYPE TESTS

# Note that there is a quirk (or bug) in data.table that means aggregating and grouping on the same column
# gives odd/wrong results.  See the following issue for details:
# https://github.com/Rdatatable/data.table/issues/3103
# To work around this, the SaleQuantityDupe column has been added below

dtdata <- data.frame(SaleID=1:5, Colour=c("Red", "Red", "Green", "Green", "Green", "Green", "Red", "Green", "Red", "Green"),
                     SaleItem=c("Car", "Lorry", "Car", "Train", "Train", "Lorry", "Car", "Train", "Lorry", "Car"),
                     SaleModel=c("CA", "LA", "CB", "TA", "TB", "LB", "CB", "TC", "LD", "CE"),
                     SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                     SaleDT=as.POSIXct(c("2018-05-15 09:12:34 UTC", "2018-01-23 13:23:54 UTC", "2018-09-03 23:59:59 UTC",
                                         "2017-12-25 11:47:19 UTC", "2018-06-28 18:00:00 UTC", "2018-06-23 12:34:15 UTC",
                                         "2018-03-31 17:05:23 UTC", "2018-09-01 15:54:23 UTC", "2018-02-02 10:06:16 UTC",
                                         "2016-11-25 18:12:11 UTC")),
                     IsNewCustomer=c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                     SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                     SaleQuantityDupe=as.integer(c(1,3,2,1,5,3,1,2,3,2)), # needed due to a quirk of data.table
                     SaleAmount=c(12.1,2.333333333,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                     Propensity=c(12,35,0,45,87,NA,Inf,-Inf,NaN,100),
                     stringsAsFactors=FALSE)

context("DATA GROUP DATA TYPE FORMAT TESTS:  NO FORMAT")

# integer

scenarios <- testScenarios("row/column data type tests (no format): integer")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleQuantityDupe")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# numeric

scenarios <- testScenarios("row/column data type tests (no format): numeric")
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
    pt$addRowDataGroups("SaleAmount")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0.2</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.1</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.333333333</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.9</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3.7</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5.6</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12.1</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("row/column data type tests (no format): logical")
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
    pt$addRowDataGroups("IsNewCustomer")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">FALSE</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">18</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">TRUE</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# Date

scenarios <- testScenarios("row/column data type tests (no format): date")
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
    pt$addRowDataGroups("SaleDate")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-12-25</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-01-23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-05-15</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-28</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-03</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# POSIXct

scenarios <- testScenarios("row/column data type tests (no format): POSIXct")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDT")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2016-11-25 18:12:11</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-12-25 11:47:19</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-01-23 13:23:54</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-02-02 10:06:16</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-03-31 17:05:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-05-15 09:12:34</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-23 12:34:15</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-28 18:00:00</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-01 15:54:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-03 23:59:59</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("DATA GROUP DATA TYPE FORMAT TESTS:  SPRINTF()")

# character - N/A
# date - N/A
# POSIXct - N/A

# integer

scenarios <- testScenarios("row/column data type tests (sprintf): integer")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleQuantityDupe", dataFormat="%i")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# numeric

scenarios <- testScenarios("row/column data type tests (sprintf): numeric")
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
    pt$addRowDataGroups("SaleAmount", dataFormat="%.1f")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0.2</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.1</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.3</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.5</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.9</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3.7</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5.6</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12.1</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("row/column data type tests (sprintf): logical")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("IsNewCustomer", dataFormat="%i")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">18</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# Date

scenarios <- testScenarios("row/column data type tests (format): date")
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
    pt$addRowDataGroups("SaleDate", dataFormat="%i")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">17525</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">17554</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">17666</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">17710</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">17777</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# POSIXct - this test fails on Travis (and probably CRAN) due to time zone issues

# scenarios <- testScenarios("row/column data type tests (format): posixct")
# for(i in 1:nrow(scenarios)) {
#   evaluationMode <- scenarios$evaluationMode[i]
#   processingLibrary <- scenarios$processingLibrary[i]
#   description <- scenarios$description[i]
#   countFunction <- scenarios$countFunction[i]
#
#   test_that(description, {
#
#     library(pivottabler)
#     pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
#     pt$addData(dtdata)
#     pt$addColumnDataGroups("SaleID")
#     pt$addRowDataGroups("SaleDT", dataFormat="%i")
#     pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
#     pt$evaluatePivot()
#     # pt$renderPivot()
#     # sum(pt$cells$asMatrix(), na.rm=TRUE)
#     # prepStr(as.character(pt$getHtml()))
#     html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1480097531</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1514202439</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1516713834</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1517565976</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1522512323</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1526371954</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1529753655</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1530205200</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1535813663</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1536015599</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"
#
#     expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
#     expect_identical(as.character(pt$getHtml()), html)
#   })
# }


context("DATA GROUP DATA TYPE FORMAT TESTS:  FORMAT()")

# integer - N/A
# character - N/A

# numeric

scenarios <- testScenarios("row/column data type tests (format): numeric")
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
    pt$addRowDataGroups("SaleAmount", dataFormat=list(digits=4, nsmall=2))
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0.20</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.10</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">1.50</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.333</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.50</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2.90</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">3.70</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">5.60</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12.10</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("row/column data type tests (format): logical")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("IsNewCustomer", dataFormat=c("Existing Customer", "New Customer"))
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Existing Customer</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">18</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">New Customer</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# Date

scenarios <- testScenarios("row/column data type tests (format): date")
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
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Dec 2017</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jan 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">15 May 2018</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">28 Jun 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">03 Sep 2018</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# POSIXct

scenarios <- testScenarios("row/column data type tests (format): posixct")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDT", dataFormat="%d %b %Y %H:%M:%S")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Nov 2016 18:12:11</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">25 Dec 2017 11:47:19</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jan 2018 13:23:54</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">02 Feb 2018 10:06:16</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">31 Mar 2018 17:05:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">15 May 2018 09:12:34</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">23 Jun 2018 12:34:15</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">28 Jun 2018 18:00:00</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">01 Sep 2018 15:54:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">03 Sep 2018 23:59:59</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("MEASURE DATA TYPE CALCULATION AND FORMAT TESTS:  NO FORMAT")

# integer

scenarios <- testScenarios("measure data type tests (no format): integer")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
    pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format="%.1f",
                         calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">8.5</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">12.3</td>\n    <td class=\"Cell\">6.1</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">20.8</td>\n    <td class=\"Cell\">3.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">0.4</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">4.833333333</td>\n    <td class=\"Cell\">0.8</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">5.933333333</td>\n    <td class=\"Cell\">0.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">1.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">18.5</td>\n    <td class=\"Cell\">1.2</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">17.133333333</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">35.633333333</td>\n    <td class=\"Cell\">1.5</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 255.255756842318)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# numeric

scenarios <- testScenarios("measure data type tests (no format): numeric")
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
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
    pt$defineCalculation(type="calculation", basedOn=c("SaleCount", "TotalSales"), format="%.1f",
                         calculationName="AvgSales", calculationExpression="values$TotalSales/values$SaleCount", caption="Avg")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">8.5</td>\n    <td class=\"Cell\">4.2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">12.3</td>\n    <td class=\"Cell\">6.1</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">20.8</td>\n    <td class=\"Cell\">5.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4.833333333</td>\n    <td class=\"Cell\">2.4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5.933333333</td>\n    <td class=\"Cell\">2.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">3.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">18.5</td>\n    <td class=\"Cell\">3.1</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">17.133333333</td>\n    <td class=\"Cell\">4.3</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">35.633333333</td>\n    <td class=\"Cell\">3.6</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 220.491111109383)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# character

scenarios <- testScenarios("measure data type tests (no format): charcater")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstModel", summariseExpression="min(SaleModel, na.rm=TRUE)", caption="First Model")
    pt$defineCalculation(calculationName="LastModel", summariseExpression="max(SaleModel, na.rm=TRUE)", caption="Last Model")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Model</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Model</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Model</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Model</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Model</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Model</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">CB</td>\n    <td class=\"Cell\">CE</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">CA</td>\n    <td class=\"Cell\">CB</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">CA</td>\n    <td class=\"Cell\">CE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">LB</td>\n    <td class=\"Cell\">LB</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">LA</td>\n    <td class=\"Cell\">LD</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">LA</td>\n    <td class=\"Cell\">LD</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">TA</td>\n    <td class=\"Cell\">TC</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">TA</td>\n    <td class=\"Cell\">TC</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">CB</td>\n    <td class=\"Cell\">TC</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">CA</td>\n    <td class=\"Cell\">LD</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">CA</td>\n    <td class=\"Cell\">TC</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("measure data type tests (no format): logical")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))", caption="First NC")
    pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))", caption="Last NC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">FALSE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# date

scenarios <- testScenarios("measure data type tests (no format): date")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDate, na.rm=TRUE)", caption="First Sale")
    pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDate, na.rm=TRUE)", caption="Last Sale")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2018-06-28</td>\n    <td class=\"Cell\">2018-09-03</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2018-01-23</td>\n    <td class=\"Cell\">2018-05-15</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">2018-01-23</td>\n    <td class=\"Cell\">2018-09-03</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2018-05-15</td>\n    <td class=\"Cell\">2018-05-15</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-01-23</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-05-15</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-09-03</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-09-03</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-09-03</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-05-15</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">2017-12-25</td>\n    <td class=\"Cell\">2018-09-03</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# POSIXct

scenarios <- testScenarios("measure data type tests (no format): posixct")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDT, na.rm=TRUE)", caption="First Sale")
    pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDT, na.rm=TRUE)", caption="Last Sale")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2016-11-25 18:12:11</td>\n    <td class=\"Cell\">2018-09-03 23:59:59</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2018-03-31 17:05:23</td>\n    <td class=\"Cell\">2018-05-15 09:12:34</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">2016-11-25 18:12:11</td>\n    <td class=\"Cell\">2018-09-03 23:59:59</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2018-06-23 12:34:15</td>\n    <td class=\"Cell\">2018-06-23 12:34:15</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2018-01-23 13:23:54</td>\n    <td class=\"Cell\">2018-02-02 10:06:16</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2018-01-23 13:23:54</td>\n    <td class=\"Cell\">2018-06-23 12:34:15</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2017-12-25 11:47:19</td>\n    <td class=\"Cell\">2018-09-01 15:54:23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">2017-12-25 11:47:19</td>\n    <td class=\"Cell\">2018-09-01 15:54:23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">2016-11-25 18:12:11</td>\n    <td class=\"Cell\">2018-09-03 23:59:59</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">2018-01-23 13:23:54</td>\n    <td class=\"Cell\">2018-05-15 09:12:34</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">2016-11-25 18:12:11</td>\n    <td class=\"Cell\">2018-09-03 23:59:59</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("MEASURE DATA TYPE FORMAT TESTS:  SPRINTF()")

# character - N/A
# date - N/A
# POSIXct - N/A

# integer

scenarios <- testScenarios("measure data type tests (sprintf): integer")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", format="%i", caption="Volume Sold")
    pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", caption="Total Sales")
    pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format="%.1f",
                         calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">8.5</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">12.3</td>\n    <td class=\"Cell\">6.1</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">20.8</td>\n    <td class=\"Cell\">3.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">0.4</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">4.833333333</td>\n    <td class=\"Cell\">0.8</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">5.933333333</td>\n    <td class=\"Cell\">0.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">1.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">18.5</td>\n    <td class=\"Cell\">1.2</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">17.133333333</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">35.633333333</td>\n    <td class=\"Cell\">1.5</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 255.255756842318)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# numeric

scenarios <- testScenarios("measure data type tests (sprintf): numeric")
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
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)", format="%.1f", caption="Total Sales")
    pt$defineCalculation(type="calculation", basedOn=c("SaleCount", "TotalSales"), format="%.1f",
                         calculationName="AvgSales", calculationExpression="values$TotalSales/values$SaleCount", caption="Avg")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">8.5</td>\n    <td class=\"Cell\">4.2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">12.3</td>\n    <td class=\"Cell\">6.1</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">20.8</td>\n    <td class=\"Cell\">5.2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">1.1</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4.8</td>\n    <td class=\"Cell\">2.4</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5.9</td>\n    <td class=\"Cell\">2.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">3.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">8.9</td>\n    <td class=\"Cell\">3.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">18.5</td>\n    <td class=\"Cell\">3.1</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">17.1</td>\n    <td class=\"Cell\">4.3</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">35.6</td>\n    <td class=\"Cell\">3.6</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 220.491111109383)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("measure data type tests (sprintf): logical")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))", format="%i", caption="First NC")
    pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))", format=c("No","Yes"), caption="Last NC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">No</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">Yes</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


context("MEASURE DATA TYPE FORMAT TESTS:  FORMAT()")

# integer - N/A
# character - N/A

# numeric

scenarios <- testScenarios("measure data type tests (format): numeric")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$defineCalculation(calculationName="TotalSales", summariseExpression="sum(SaleAmount, na.rm=TRUE)",
                         format=list(digits=4, nsmall=2), caption="Total Sales")
    pt$defineCalculation(type="calculation", basedOn=c("VolumeSold", "TotalSales"), format=list(digits=4, nsmall=2),
                         calculationName="AvgSales", calculationExpression="values$TotalSales/values$VolumeSold", caption="Avg")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Volume Sold</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Avg</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">8.50</td>\n    <td class=\"Cell\">2.125</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">12.30</td>\n    <td class=\"Cell\">6.15</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">20.80</td>\n    <td class=\"Cell\">3.467</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">1.10</td>\n    <td class=\"Cell\">0.3667</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">4.833</td>\n    <td class=\"Cell\">0.8056</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">5.933</td>\n    <td class=\"Cell\">0.6593</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.90</td>\n    <td class=\"Cell\">1.113</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">8.90</td>\n    <td class=\"Cell\">1.113</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\">18.50</td>\n    <td class=\"Cell\">1.233</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">17.13</td>\n    <td class=\"Cell\">2.142</td>\n    <td class=\"Cell\">23</td>\n    <td class=\"Cell\">35.63</td>\n    <td class=\"Cell\">1.549</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 255.255756842318)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# logical

scenarios <- testScenarios("measure data type tests (format): logical")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstIsNewCustomer", summariseExpression="as.logical(min(IsNewCustomer, na.rm=TRUE))",
                         format=c("No","Yes", ""), caption="First NC")
    pt$defineCalculation(calculationName="LastIsNewCustomer", summariseExpression="as.logical(max(IsNewCustomer, na.rm=TRUE))",
                         format=list(), caption="Last NC")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First NC</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last NC</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">Yes</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">FALSE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">No</td>\n    <td class=\"Cell\">TRUE</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# date

scenarios <- testScenarios("measure data type tests (format): date")
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
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDate, na.rm=TRUE)",
                         format="%d %b %Y", caption="First Sale")
    pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDate, na.rm=TRUE)",
                         format=list("%d %b %Y"), caption="Last Sale")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">28 Jun 2018</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">23 Jan 2018</td>\n    <td class=\"Cell\">15 May 2018</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">23 Jan 2018</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">15 May 2018</td>\n    <td class=\"Cell\">15 May 2018</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">23 Jan 2018</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">15 May 2018</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">15 May 2018</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">25 Dec 2017</td>\n    <td class=\"Cell\">03 Sep 2018</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}

# POSIXct

scenarios <- testScenarios("measure data type tests (format): posixct")
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
    pt$addData(dtdata)
    pt$addColumnDataGroups("Colour")
    pt$addRowDataGroups("SaleItem")
    pt$defineCalculation(calculationName="SaleCount", summariseExpression=countFunction, caption="Sale Count")
    pt$defineCalculation(calculationName="FirstSale", summariseExpression="min(SaleDT, na.rm=TRUE)",
                         format="%d %b %Y %H:%M:%S", caption="First Sale")
    pt$defineCalculation(calculationName="LastSale", summariseExpression="max(SaleDT, na.rm=TRUE)",
                         format=list("%d %b %Y %H:%M:%S"), caption="Last Sale")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Green</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Red</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">First Sale</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Last Sale</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Car</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">25 Nov 2016 18:12:11</td>\n    <td class=\"Cell\">03 Sep 2018 23:59:59</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">31 Mar 2018 17:05:23</td>\n    <td class=\"Cell\">15 May 2018 09:12:34</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">25 Nov 2016 18:12:11</td>\n    <td class=\"Cell\">03 Sep 2018 23:59:59</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Lorry</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">23 Jun 2018 12:34:15</td>\n    <td class=\"Cell\">23 Jun 2018 12:34:15</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">23 Jan 2018 13:23:54</td>\n    <td class=\"Cell\">02 Feb 2018 10:06:16</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">23 Jan 2018 13:23:54</td>\n    <td class=\"Cell\">23 Jun 2018 12:34:15</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Train</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">25 Dec 2017 11:47:19</td>\n    <td class=\"Cell\">01 Sep 2018 15:54:23</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">25 Dec 2017 11:47:19</td>\n    <td class=\"Cell\">01 Sep 2018 15:54:23</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">25 Nov 2016 18:12:11</td>\n    <td class=\"Cell\">03 Sep 2018 23:59:59</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">23 Jan 2018 13:23:54</td>\n    <td class=\"Cell\">15 May 2018 09:12:34</td>\n    <td class=\"Cell\">10</td>\n    <td class=\"Cell\">25 Nov 2016 18:12:11</td>\n    <td class=\"Cell\">03 Sep 2018 23:59:59</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 40)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
