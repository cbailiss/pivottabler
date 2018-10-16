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


context("IRREGULAR LAYOUT TESTS")


scenarios <- testScenarios("irregular layout tests:  simple example")
for(i in 1:nrow(scenarios)) {
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
for(i in 1:nrow(scenarios)) {
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
for(i in 1:nrow(scenarios)) {
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
for(i in 1:nrow(scenarios)) {
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
