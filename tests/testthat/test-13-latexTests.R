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


context("LATEX TESTS")


scenarios <- testScenarios("latex tests:  basic latex table with spans")
for(i in 1:nrow(scenarios)) {
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|}\n    \\hline\n    (no data)\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  just rows")
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
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n     &  &  & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows and columns")
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
    ltx <- pt$getLatex(caption="My Table", label="mytable")

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  &  &  & \\\\\n    CrossCountry  &  &  & \\\\\n    London Midland  &  &  & \\\\\n    Virgin Trains  &  &  & \\\\\n    Total  &  &  & \\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
    expect_identical(ltx, ltx2)
  })
}


scenarios <- testScenarios("latex tests:  rows, columns and a measure")
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
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
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
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    ltx <- pt$getLatex(caption="My Table", label="mytable", fromRow=3, toRow=5, fromColumn=1, toColumn=2)

    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(ltx)
    ltx2 <- "\\begin{table}[h!]\n  \\centering\n  \\caption{My Table}\n  \\label{tab:mytable}\n  \\begin{tabular}{|l|rr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger\\\\\n    \\hline\n    London Midland  & 14487 & 33792\\\\\n    Virgin Trains  & 8594 & \\\\\n    Total  & 49025 & 34685\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(ltx, ltx2)
  })
}
