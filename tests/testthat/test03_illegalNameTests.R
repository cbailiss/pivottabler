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

context("ILLEGAL NAME TESTS")

scenarios <- testScenarios("illegal names tests", runAllForReleaseVersion=TRUE)
illegalNameTestData <- data.frame("Sale ID"=1:5, "Colour"=c("Red", "Red", "Green", "Red", "Green"), "Sale Item"=c("Car", "Lorry", "Car", "Train", "Train"), "Sale Amount"=c(12.1,2.3,5.6,3.7,1.5))
colnames(illegalNameTestData) <- c("Sale.ID", "Colour Name", "Sale$Item", "Sale+Amount")

for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE, explicitHeaderSpansOfOne=TRUE))
    pt$addData(illegalNameTestData)
    pt$addColumnDataGroups("Sale$Item")
    pt$addRowDataGroups("Colour Name")
    pt$defineCalculation(calculationName="Sale Count", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="$Sale$Count$", summariseExpression=countFunction)
    pt$defineCalculation(calculationName="Total+Sales", summariseExpression="sum(`Sale+Amount`)")
    pt$defineCalculation(type="calculation", basedOn=c("$Sale$Count$", "Total+Sales"), format="%.1f",
                         calculationName="!Avg!Sales!", calculationExpression="values$`Total+Sales`/values$`$Sale$Count$`")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Car</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Lorry</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Train</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">$Sale$Count$</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total+Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">!Avg!Sales!</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">$Sale$Count$</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total+Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">!Avg!Sales!</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">$Sale$Count$</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total+Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">!Avg!Sales!</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Sale Count</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">$Sale$Count$</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total+Sales</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">!Avg!Sales!</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Green</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">5.6</td>\n    <td class=\"Cell\">5.6</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1.5</td>\n    <td class=\"Cell\">1.5</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">7.1</td>\n    <td class=\"Cell\">3.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Red</th>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">12.1</td>\n    <td class=\"Cell\">12.1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3.7</td>\n    <td class=\"Cell\">3.7</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">18.1</td>\n    <td class=\"Cell\">6.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">17.7</td>\n    <td class=\"Cell\">8.8</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">2.3</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">5.2</td>\n    <td class=\"Cell\">2.6</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">25.2</td>\n    <td class=\"Cell\">5.0</td>\n  </tr>\n</table>"

    expect_equal(round(sum(pt$cells$asMatrix(), na.rm=TRUE), digits = 3), 194.373)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
