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


context("VISUAL TOTALS TESTS")



scenarios <- testScenarios("visual totals")
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
    pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Express Passenger"), visualTotals=TRUE)
    pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list("Arriva Trains Wales", "CrossCountry", "London Midland"), visualTotals=TRUE)
    pt$addRowDataGroups("PowerType", fromData=FALSE, explicitListOfValues=list("DMU"), visualTotals=TRUE)
    pt$addRowDataGroups("SchedSpeedMPH", fromData=FALSE, explicitListOfValues=list(90, 100), visualTotals=TRUE)
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"3\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">2989</td>\n    <td class=\"Cell\">2989</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">CrossCountry</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">10961</td>\n    <td class=\"Cell\">10961</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"4\">London Midland</th>\n    <th class=\"RowHeader\" rowspan=\"3\">DMU</th>\n    <th class=\"RowHeader\" rowspan=\"1\">90</th>\n    <td class=\"Cell\">2340</td>\n    <td class=\"Cell\">2340</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">100</th>\n    <td class=\"Cell\">2450</td>\n    <td class=\"Cell\">2450</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">4790</td>\n    <td class=\"Cell\">4790</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">4790</td>\n    <td class=\"Cell\">4790</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <th class=\"RowHeader\" rowspan=\"1\"></th>\n    <td class=\"Cell\">18740</td>\n    <td class=\"Cell\">18740</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 149920)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
