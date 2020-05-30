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


context("IRREGULAR LAYOUT TESTS")


scenarios <- testScenarios("irregular layout tests:  simple example")
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


scenarios <- testScenarios("irregular layout tests:  adding and removing individual groups")
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
    grps <- pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$rowGroup$addChildGroup(caption="Test 1a", insertAtIndex=3, isEmpty=TRUE)
    pt$rowGroup$addChildGroup(caption="Test 1b", insertAtIndex=4, isEmpty=TRUE, mergeEmptySpace="doNotMerge")
    pt$rowGroup$addChildGroup(caption="Test 1c", insertAtIndex=5)
    pt$rowGroup$addChildGroup(caption="Test 2", insertBefore=grps[[3]])
    pt$rowGroup$addChildGroup(caption="Test 3", insertAfter=grps[[3]])
    grp <- pt$rowGroup$addChildGroup(caption="Test 4", insertAfter=grps[[4]])
    grp$removeGroup()
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Test 1a</th>\n    <td class=\"Cell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Test 1b</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Test 1c</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Test 2</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Test 3</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 837100)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  combine pivot 1")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode, compatibility=list(noDataGroupNBSP=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    # Rows: TOC breakdown
    grp1 <- pt$addRowGroup(caption="By TOC")
    grp1$addDataGroups("TOC", addTotal=FALSE)
    # Rows: Power Type breakdown
    grp2 <- pt$addRowGroup(caption="By Power Type")
    grp2$addDataGroups("PowerType", addTotal=FALSE)
    # Rows: Total
    grp3 <- pt$addRowGroup(caption="Total")
    # Row Group Headings
    pt$setRowDataGroupHeader(levelNumber=1, header="Breakdown")
    pt$setRowDataGroupHeader(levelNumber=2, header="Subset")
    # Finish...
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getStandardTableTheme(pt)
    pt$evaluatePivot()
    # pt$renderPivot(showRowGroupHeaders=TRUE)
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml(showRowGroupHeaders=TRUE)))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"LeftColumnHeader\">Breakdown</th>\n    <th class=\"LeftColumnHeader\">Subset</th>\n    <th class=\"CentreColumnHeader\">Express Passenger</th>\n    <th class=\"CentreColumnHeader\">Ordinary Passenger</th>\n    <th class=\"CentreColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"4\">By TOC</th>\n    <th class=\"LeftCell\">Arriva Trains Wales</th>\n    <td class=\"RightCell\">3079</td>\n    <td class=\"RightCell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">CrossCountry</th>\n    <td class=\"RightCell\">22865</td>\n    <td class=\"RightCell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">London Midland</th>\n    <td class=\"RightCell\">14487</td>\n    <td class=\"RightCell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Virgin Trains</th>\n    <td class=\"RightCell\">8594</td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\" rowspan=\"3\">By Power Type</th>\n    <th class=\"LeftCell\">DMU</th>\n    <td class=\"RightCell\">32987</td>\n    <td class=\"RightCell\">6484</td>\n    <td class=\"Total\">39471</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">EMU</th>\n    <td class=\"RightCell\">15306</td>\n    <td class=\"RightCell\">28201</td>\n    <td class=\"Total\">43507</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">HST</th>\n    <td class=\"RightCell\">732</td>\n    <td class=\"RightCell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"LeftCell\">Total</th>\n    <th class=\"LeftCell\"></th>\n    <td class=\"RightCell\">49025</td>\n    <td class=\"RightCell\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml(showRowGroupHeaders=TRUE)), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  combine pivot 2")
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
    # Rows: TOC breakdown
    grp1a <- pt$addRowGroup(caption="By TOC", isOutline=TRUE, isEmpty=TRUE, sortAnchor="next", styleDeclarations=list(color="blue"))
    grp1b <- pt$addRowGroup()
    grp1b$addDataGroups("TOC", addTotal=FALSE)
    # Rows: Power Type breakdown
    grp2a <- pt$addRowGroup(caption="By Power Type", isOutline=TRUE, isEmpty=TRUE, sortAnchor="next", styleDeclarations=list(color="blue"))
    grp2b <- pt$addRowGroup()
    grp2b$addDataGroups("PowerType", addTotal=FALSE)
    # Rows: Total
    grp3 <- pt$addRowGroup(caption="Total", isOutline=TRUE, styleDeclarations=list(color="blue"))
    # Finish...
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">By TOC</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\"></th>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">By Power Type</th>\n    <td class=\"OutlineCell\" colspan=\"3\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\"></th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">32987</td>\n    <td class=\"Cell\">6484</td>\n    <td class=\"Total\">39471</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">15306</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">43507</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">732</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" colspan=\"2\">Total</th>\n    <td class=\"OutlineCell\">49025</td>\n    <td class=\"OutlineCell\">34685</td>\n    <td class=\"OutlineCell\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  combine pivot 3")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    gender <- c("F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M")
    age <- c(19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22)
    grade <- c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E")
    counts <- c(6, 16, 56, 37, 213, 14, 21, 61, 45, 191, 30, 54, 74, 82, 246, 91, 46, 29, 71, 296, 3, 6, 21, 35, 162, 14, 11, 29, 22, 204, 15, 30, 49, 75, 253, 45, 22, 30, 30, 319)

    df <- data.frame(gender, age, grade, counts)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("grade")
    pt$addRowGroup(caption="Age", isEmpty=TRUE, isOutline=TRUE, styleDeclarations=list(color="blue"))
    pt$addRowDataGroups("age", atLevel=1)
    pt$addRowGroup(caption="Gender", isEmpty=TRUE, isOutline=TRUE, styleDeclarations=list(color="blue"))
    pt$addRowDataGroups("gender", atLevel=1)
    pt$defineCalculation(calculationName="GradeCounts", summariseExpression="sum(counts)")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">B</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">D</th>\n    <th class=\"ColumnHeader\">E</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \">Age</th>\n    <td class=\"OutlineCell\" colspan=\"6\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">19</th>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">22</td>\n    <td class=\"Cell\">77</td>\n    <td class=\"Cell\">72</td>\n    <td class=\"Cell\">375</td>\n    <td class=\"Total\">555</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">20</th>\n    <td class=\"Cell\">28</td>\n    <td class=\"Cell\">32</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Cell\">395</td>\n    <td class=\"Total\">612</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">21</th>\n    <td class=\"Cell\">45</td>\n    <td class=\"Cell\">84</td>\n    <td class=\"Cell\">123</td>\n    <td class=\"Cell\">157</td>\n    <td class=\"Cell\">499</td>\n    <td class=\"Total\">908</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">22</th>\n    <td class=\"Cell\">136</td>\n    <td class=\"Cell\">68</td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">615</td>\n    <td class=\"Total\">979</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">218</td>\n    <td class=\"Total\">206</td>\n    <td class=\"Total\">349</td>\n    <td class=\"Total\">397</td>\n    <td class=\"Total\">1884</td>\n    <td class=\"Total\">3054</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \">Gender</th>\n    <td class=\"OutlineCell\" colspan=\"6\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">F</th>\n    <td class=\"Cell\">141</td>\n    <td class=\"Cell\">137</td>\n    <td class=\"Cell\">220</td>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">946</td>\n    <td class=\"Total\">1679</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">M</th>\n    <td class=\"Cell\">77</td>\n    <td class=\"Cell\">69</td>\n    <td class=\"Cell\">129</td>\n    <td class=\"Cell\">162</td>\n    <td class=\"Cell\">938</td>\n    <td class=\"Total\">1375</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">218</td>\n    <td class=\"Total\">206</td>\n    <td class=\"Total\">349</td>\n    <td class=\"Total\">397</td>\n    <td class=\"Total\">1884</td>\n    <td class=\"Total\">3054</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 24432)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("irregular layout tests:  combine pivot 4")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    gender <- c("F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M")
    age <- c(19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22)
    grade <- c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E", "A", "B", "C", "D", "E")
    counts <- c(6, 16, 56, 37, 213, 14, 21, 61, 45, 191, 30, 54, 74, 82, 246, 91, 46, 29, 71, 296, 3, 6, 21, 35, 162, 14, 11, 29, 22, 204, 15, 30, 49, 75, 253, 45, 22, 30, 30, 319)

    df <- data.frame(gender, age, grade, counts)

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(df)
    pt$addColumnDataGroups("grade")
    pt$addRowGroup(caption="Age", isEmpty=TRUE, isOutline=TRUE, styleDeclarations=list(color="blue"))
    pt$addRowDataGroups("age", atLevel=1, addTotal=FALSE)
    pt$addRowGroup(caption="Gender", isEmpty=TRUE, isOutline=TRUE, styleDeclarations=list(color="blue"))
    pt$addRowDataGroups("gender", atLevel=1, addTotal=FALSE)
    pt$defineCalculation(calculationName="GradeCounts", summariseExpression="sum(counts)")
    pt$addRowGroup(caption="Total", isOutline=TRUE, isTotal=TRUE, isLevelTotal=TRUE,
                   styleDeclarations=list(color="blue"), cellStyleDeclarations=list(color="blue"))
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">B</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">D</th>\n    <th class=\"ColumnHeader\">E</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \">Age</th>\n    <td class=\"OutlineCell\" colspan=\"6\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">19</th>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">22</td>\n    <td class=\"Cell\">77</td>\n    <td class=\"Cell\">72</td>\n    <td class=\"Cell\">375</td>\n    <td class=\"Total\">555</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">20</th>\n    <td class=\"Cell\">28</td>\n    <td class=\"Cell\">32</td>\n    <td class=\"Cell\">90</td>\n    <td class=\"Cell\">67</td>\n    <td class=\"Cell\">395</td>\n    <td class=\"Total\">612</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">21</th>\n    <td class=\"Cell\">45</td>\n    <td class=\"Cell\">84</td>\n    <td class=\"Cell\">123</td>\n    <td class=\"Cell\">157</td>\n    <td class=\"Cell\">499</td>\n    <td class=\"Total\">908</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">22</th>\n    <td class=\"Cell\">136</td>\n    <td class=\"Cell\">68</td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">101</td>\n    <td class=\"Cell\">615</td>\n    <td class=\"Total\">979</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \">Gender</th>\n    <td class=\"OutlineCell\" colspan=\"6\">&nbsp;</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">F</th>\n    <td class=\"Cell\">141</td>\n    <td class=\"Cell\">137</td>\n    <td class=\"Cell\">220</td>\n    <td class=\"Cell\">235</td>\n    <td class=\"Cell\">946</td>\n    <td class=\"Total\">1679</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">M</th>\n    <td class=\"Cell\">77</td>\n    <td class=\"Cell\">69</td>\n    <td class=\"Cell\">129</td>\n    <td class=\"Cell\">162</td>\n    <td class=\"Cell\">938</td>\n    <td class=\"Total\">1375</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \">Total</th>\n    <td class=\"OutlineCell\" style=\"color: blue; \">218</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">206</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">349</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">397</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">1884</td>\n    <td class=\"OutlineCell\" style=\"color: blue; \">3054</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 18324)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
