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


context("EXPORT OPTIONS TESTS")


etdata <- data.frame(SaleID=1:5,
                     SaleDate=as.Date(c("2018-05-15", "2018-01-23", "2018-09-03", "2017-12-25", "2018-06-28")),
                     SaleDT=as.POSIXct(c("2018-05-15 09:12:34 UTC", "2018-01-23 13:23:54 UTC", "2018-09-03 23:59:59 UTC",
                                         "2017-12-25 11:47:19 UTC", "2018-06-28 18:00:00 UTC", "2018-06-23 12:34:15 UTC",
                                         "2018-03-31 17:05:23 UTC", "2018-09-01 15:54:23 UTC", "2018-02-02 10:06:16 UTC",
                                         "2016-11-25 18:12:11 UTC")),
                     SaleQuantity=as.integer(c(1,3,2,1,5,3,1,2,3,2)),
                     SaleAmount=c(12.1,2.333333333,5.6,3.7,1.5,1.1,0.2,3.7,2.5,2.9),
                     Propensity=c(12,35,0,45,87,NA,Inf,-Inf,NaN,100),
                     stringsAsFactors=FALSE)

scenarios <- testScenarios("export options:  skip values in headings")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    pt$addData(etdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("Propensity")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))))
    ltx <- pt$getLatex(caption="My Table", label="mytable", exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
    # prepStr(ltx)
    if (processingLibrary=="data.table") { # NaN and NA sort in different orders in dplyr and data.table
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">35</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">45</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">87</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"
      ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n      &  &  & 2 &  &  & 2\\\\\n    0  &  &  & 2 &  &  & 2\\\\\n    12  & 1 &  &  &  &  & 1\\\\\n    35  &  & 3 &  &  &  & 3\\\\\n    45  &  &  &  & 1 &  & 1\\\\\n    87  &  &  &  &  & 5 & 5\\\\\n    100  &  &  &  &  & 2 & 2\\\\\n      &  & 1 &  &  &  & 1\\\\\n      &  &  &  & 3 &  & 3\\\\\n      & 3 &  &  &  &  & 3\\\\\n    Total  & 4 & 4 & 4 & 4 & 7 & 23\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"
    }
    else {
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">35</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">45</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">87</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"
      ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n      &  &  & 2 &  &  & 2\\\\\n    0  &  &  & 2 &  &  & 2\\\\\n    12  & 1 &  &  &  &  & 1\\\\\n    35  &  & 3 &  &  &  & 3\\\\\n    45  &  &  &  & 1 &  & 1\\\\\n    87  &  &  &  &  & 5 & 5\\\\\n    100  &  &  &  &  & 2 & 2\\\\\n      &  & 1 &  &  &  & 1\\\\\n      & 3 &  &  &  &  & 3\\\\\n      &  &  &  & 3 &  & 3\\\\\n    Total  & 4 & 4 & 4 & 4 & 7 & 23\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"
    }
    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))), html)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("export options:  replace values in headings")
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
    pt$addData(etdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("Propensity")
    pt$defineCalculation(calculationName="VolumeSold", summariseExpression="sum(SaleQuantity, na.rm=TRUE)", caption="Volume Sold")
    pt$evaluatePivot()
    # pt$renderPivot(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))))
    ltx <- pt$getLatex(caption="My Table", label="mytable", exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
    # prepStr(ltx)
    if (processingLibrary=="data.table") { # NaN and NA sort in different orders in dplyr and data.table
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">-Infinity</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">35</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">45</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">87</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Infinity</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Not a Number</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Nothing</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"
      ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n    -Infinity  &  &  & 2 &  &  & 2\\\\\n    0  &  &  & 2 &  &  & 2\\\\\n    12  & 1 &  &  &  &  & 1\\\\\n    35  &  & 3 &  &  &  & 3\\\\\n    45  &  &  &  & 1 &  & 1\\\\\n    87  &  &  &  &  & 5 & 5\\\\\n    100  &  &  &  &  & 2 & 2\\\\\n    Infinity  &  & 1 &  &  &  & 1\\\\\n    Not a Number  &  &  &  & 3 &  & 3\\\\\n    Nothing  & 3 &  &  &  &  & 3\\\\\n    Total  & 4 & 4 & 4 & 4 & 7 & 23\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"
    }
    else {
      html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">-Infinity</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">0</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">12</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">35</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">45</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">87</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Infinity</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Nothing</th>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Not a Number</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">23</td>\n  </tr>\n</table>"
      ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n    -Infinity  &  &  & 2 &  &  & 2\\\\\n    0  &  &  & 2 &  &  & 2\\\\\n    12  & 1 &  &  &  &  & 1\\\\\n    35  &  & 3 &  &  &  & 3\\\\\n    45  &  &  &  & 1 &  & 1\\\\\n    87  &  &  &  &  & 5 & 5\\\\\n    100  &  &  &  &  & 2 & 2\\\\\n    Infinity  &  & 1 &  &  &  & 1\\\\\n    Nothing  & 3 &  &  &  &  & 3\\\\\n    Not a Number  &  &  &  & 3 &  & 3\\\\\n    Total  & 4 & 4 & 4 & 4 & 7 & 23\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"
    }
    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 92)
    expect_identical(as.character(pt$getHtml(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))), html)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("export options:  skip values in cells")
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
    pt$addData(etdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDT")
    pt$defineCalculation(calculationName="Propensity", summariseExpression="sum(Propensity)", caption="Propensity")
    pt$evaluatePivot()
    # pt$renderPivot(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2016-11-25 18:12:11</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-12-25 11:47:19</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">45</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">45</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-01-23 13:23:54</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">35</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">35</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-02-02 10:06:16</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-03-31 17:05:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-05-15 09:12:34</th>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">12</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-23 12:34:15</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-28 18:00:00</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">87</td>\n    <td class=\"Cell\">87</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-01 15:54:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-03 23:59:59</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">187</td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"
    ltx <- pt$getLatex(caption="My Table", label="mytable", exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n    2016-11-25 18:12:11  &  &  &  &  & 100 & 100\\\\\n    2017-12-25 11:47:19  &  &  &  & 45 &  & 45\\\\\n    2018-01-23 13:23:54  &  & 35 &  &  &  & 35\\\\\n    2018-02-02 10:06:16  &  &  &  &  &  & \\\\\n    2018-03-31 17:05:23  &  &  &  &  &  & \\\\\n    2018-05-15 09:12:34  & 12 &  &  &  &  & 12\\\\\n    2018-06-23 12:34:15  &  &  &  &  &  & \\\\\n    2018-06-28 18:00:00  &  &  &  &  & 87 & 87\\\\\n    2018-09-01 15:54:23  &  &  &  &  &  & \\\\\n    2018-09-03 23:59:59  &  &  & 0 &  &  & 0\\\\\n    Total  &  &  &  &  & 187 & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), NaN)
    expect_identical(as.character(pt$getHtml(exportOptions=list(skipNegInf=TRUE,skipPosInf=TRUE,skipNA=TRUE,skipNaN=TRUE))), html)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("export options:  replace values in cells")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(etdata)
    pt$addColumnDataGroups("SaleID")
    pt$addRowDataGroups("SaleDT")
    pt$defineCalculation(calculationName="Propensity", summariseExpression="sum(Propensity)", caption="Propensity")
    pt$evaluatePivot()
    # pt$renderPivot(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">1</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">2</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">3</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">4</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">5</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2016-11-25 18:12:11</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2017-12-25 11:47:19</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">45</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">45</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-01-23 13:23:54</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">35</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">35</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-02-02 10:06:16</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">Not a Number</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">Not a Number</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-03-31 17:05:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">Infinity</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">Infinity</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-05-15 09:12:34</th>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">12</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-23 12:34:15</th>\n    <td class=\"Cell\">Nothing</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">Nothing</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-06-28 18:00:00</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">87</td>\n    <td class=\"Cell\">87</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-01 15:54:23</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">-Infinity</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">-Infinity</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">2018-09-03 23:59:59</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">Nothing</td>\n    <td class=\"Cell\">Infinity</td>\n    <td class=\"Cell\">-Infinity</td>\n    <td class=\"Cell\">Not a Number</td>\n    <td class=\"Cell\">187</td>\n    <td class=\"Cell\">Nothing</td>\n  </tr>\n</table>"
    ltx <- pt$getLatex(caption="My Table", label="mytable", exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrrrrr|}\n    \\hline\n    & 1 & 2 & 3 & 4 & 5 & Total\\\\\n    \\hline\n    2016-11-25 18:12:11  &  &  &  &  & 100 & 100\\\\\n    2017-12-25 11:47:19  &  &  &  & 45 &  & 45\\\\\n    2018-01-23 13:23:54  &  & 35 &  &  &  & 35\\\\\n    2018-02-02 10:06:16  &  &  &  & Not a Number &  & Not a Number\\\\\n    2018-03-31 17:05:23  &  & Infinity &  &  &  & Infinity\\\\\n    2018-05-15 09:12:34  & 12 &  &  &  &  & 12\\\\\n    2018-06-23 12:34:15  & Nothing &  &  &  &  & Nothing\\\\\n    2018-06-28 18:00:00  &  &  &  &  & 87 & 87\\\\\n    2018-09-01 15:54:23  &  &  & -Infinity &  &  & -Infinity\\\\\n    2018-09-03 23:59:59  &  &  & 0 &  &  & 0\\\\\n    Total  & Nothing & Infinity & -Infinity & Not a Number & 187 & Nothing\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), NaN)
    expect_identical(as.character(pt$getHtml(exportOptions=list(exportNegInfAs="-Infinity",exportPosInfAs="Infinity",exportNAAs="Nothing",exportNaNAs="Not a Number"))), html)
    expect_identical(ltx, ltx2)
  })
}
