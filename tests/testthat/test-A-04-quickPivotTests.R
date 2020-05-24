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


context("QUICK-PIVOT TESTS")


test_that("quick-pivot tests:  qpvt pivot 1", {

  library(pivottabler)
  pt <- qpvt(bhmtrains, "TOC", "TrainCategory", "n()", compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
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
             c("Number of Trains"="n()", "Maximum Speed"="max(SchedSpeedMPH, na.rm=TRUE)"),
             compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # prepStr(as.character(pt$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">HST</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">DMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">EMU</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n    <th class=\"ColumnHeader\" colspan=\"1\"></th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Number of Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"5\">Maximum Speed</th>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">110</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">110</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">125</td>\n  </tr>\n</table>"

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 505565)
  expect_identical(as.character(pt$getHtml()), html)
})


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  qpvt pivot format 1 - single calc", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, "TOC", "TrainCategory", "mean(SchedSpeedMPH, na.rm=TRUE)", format="%.0f",
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">89</td>\n    <td class=\"Cell\">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">113</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">98</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">93</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">101</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1426.677, tolerance=1e-2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  qpvt pivot format 2 - two calcs basic", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
               c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
               formats=list("%.0f", "%.1f"), compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">89</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">100</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">113</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">98</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">93</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">91</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  qpvt pivot format 3 - two calcs list", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
               c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
               formats=list(list(digits = 3, nsmall=1), "%.1f"), compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">89.7</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">89.0</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">89.5</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">112.9</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">100.0</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">112.9</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">97.6</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">90.8</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">92.9</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">125.0</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">125.0</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">109.1</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">90.8</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">101.5</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  qpvt pivot format 3 - two calcs function", {

    library(pivottabler)
    qf <- function(x) { return(paste0("**", sprintf("%.0f", x), "**"))}
    pt <- qpvt(bhmtrains, "TOC", "TrainCategory",
               c("Mean Speed"="mean(SchedSpeedMPH, na.rm=TRUE)", "Std Dev Speed"="sd(SchedSpeedMPH, na.rm=TRUE)"),
               formats=list(qf, "%.1f"), compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Mean Speed</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Std Dev Speed</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">**90**</td>\n    <td class=\"Cell\">2.1</td>\n    <td class=\"Cell\">**89**</td>\n    <td class=\"Cell\">3.8</td>\n    <td class=\"Cell\">**90**</td>\n    <td class=\"Cell\">2.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">**113**</td>\n    <td class=\"Cell\">12.5</td>\n    <td class=\"Cell\">**100**</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">**113**</td>\n    <td class=\"Cell\">12.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">**98**</td>\n    <td class=\"Cell\">7.4</td>\n    <td class=\"Cell\">**91**</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">**93**</td>\n    <td class=\"Cell\">8.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">**125**</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">**125**</td>\n    <td class=\"Cell\">0.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">**109**</td>\n    <td class=\"Cell\">14.2</td>\n    <td class=\"Cell\">**91**</td>\n    <td class=\"Cell\">8.1</td>\n    <td class=\"Cell\">**101**</td>\n    <td class=\"Cell\">15.1</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 1521.537, tolerance=1e-2)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


test_that("quick-pivot tests:  qlpvt pivot 1", {

  library(pivottabler)
  lchr <- qlpvt(bhmtrains, "TOC", "TrainCategory", "n()", caption="my caption", label="mylabel",
                compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
  # prepStr(lchr)
  latex <- "\\begin{table}[h!]\n  \\centering\n  \\caption{my caption}\n  \\label{tab:mylabel}\n  \\begin{tabular}{|l|rrr|}\n    \\hline\n    & Express Passenger & Ordinary Passenger & Total\\\\\n    \\hline\n    Arriva Trains Wales  & 3079 & 830 & 3909\\\\\n    CrossCountry  & 22865 & 63 & 22928\\\\\n    London Midland  & 14487 & 33792 & 48279\\\\\n    Virgin Trains  & 8594 &  & 8594\\\\\n    Total  & 49025 & 34685 & 83710\\\\\n    \\hline\n  \\end{tabular}\n\\end{table}"

  expect_identical(lchr, latex)
})


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  totals base", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()",
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  no totals", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()", totals=NULL,
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  1 total out of 3 (chr)", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()", totals="TOC",
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  2 totals out of 3 (chr)", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()", totals=c("TOC", "TrainCategory"),
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  1 total out of 3 (list)", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()", totals=list("TOC"="All TOCs"),
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">All TOCs</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


if(isDevelopmentVersion) {
  test_that("quick-pivot tests:  2 totals out of 3 (list)", {

    library(pivottabler)
    pt <- qpvt(bhmtrains, c("TOC", "PowerType"), "TrainCategory", "n()",
               totals=list("TOC"="All TOCs", "TrainCategory"="All Categories"),
               compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE, noDataGroupNBSP=TRUE))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">All Categories</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">22133</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Cell\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Cell\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\" rowspan=\"1\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">All TOCs</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
