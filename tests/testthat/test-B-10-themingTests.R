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


context("THEMING TESTS")



scenarios <- testScenarios("theming tests:  legacy test")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    # define the colours
    orangeColors <- list(
      headerBackgroundColor = "rgb(237, 125, 49)",
      headerColor = "rgb(255, 255, 255)",
      cellBackgroundColor = "rgb(255, 255, 255)",
      cellColor = "rgb(0, 0, 0)",
      totalBackgroundColor = "rgb(248, 198, 165)",
      totalColor = "rgb(0, 0, 0)",
      borderColor = "rgb(198, 89, 17)"
    )
    # create the pivot table
    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- getSimpleColoredTheme(parentPivot=pt, colors=orangeColors, fontName="Garamond, arial")
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(as.character(pt$getCss()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(198, 89, 17); }\r\n.ColumnHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.RowHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.Cell {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.OutlineColumnHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.OutlineRowHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.OutlineCell {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); font-weight: bold; }\r\n.Total {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(248, 198, 165); }\r\n"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


scenarios <- testScenarios("theming tests:  basic test")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    # define the theme
    simpleGreenTheme <- list(
      fontName="Helvetica, arial",
      fontSize="0.75em",
      headerBackgroundColor = "rgb(112, 173, 71)",
      headerColor = "rgb(255, 255, 255)",
      cellBackgroundColor="rgb(255, 255, 255)",
      cellColor="rgb(0, 0, 0)",
      outlineCellBackgroundColor = "rgb(182, 216, 158)",
      outlineCellColor = "rgb(0, 0, 0)",
      totalBackgroundColor = "rgb(182, 216, 158)",
      totalColor="rgb(0, 0, 0)",
      borderColor = "rgb(84, 130, 53)"
    )
    # create the pivot table
    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$theme <- simpleGreenTheme
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(as.character(pt$getCss()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(84, 130, 53); }\r\n.ColumnHeader {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(112, 173, 71); }\r\n.RowHeader {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(112, 173, 71); }\r\n.Cell {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.OutlineColumnHeader {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(112, 173, 71); }\r\n.OutlineRowHeader {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(112, 173, 71); }\r\n.OutlineCell {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(182, 216, 158); font-weight: bold; }\r\n.Total {font-family: Helvetica, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(84, 130, 53); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(182, 216, 158); }\r\n"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


scenarios <- testScenarios("theming tests:  styling data groups")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory", styleDeclarations=list("color"="red", "font-weight"="bold", "background-color"="yellow"))
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(as.character(pt$getCss()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \">Express Passenger</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.OutlineColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.OutlineRowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.OutlineCell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; background-color: #F8F8F8; font-weight: bold; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


scenarios <- testScenarios("theming tests:  styling cells")
for(i in 1:nrow(scenarios)) {
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    pt$styles$addStyle(styleName="NewHeadingStyle1", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "background-color"="Gold",
      "xl-wrap-text"="wrap"
    ))
    pt$styles$addStyle(styleName="CellStyle1", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "background-color"="Yellow",
      "text-align"="right"
    ))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains1", summariseExpression=countFunction,
                         headingBaseStyleName="NewHeadingStyle1", cellBaseStyleName="CellStyle1")
    pt$defineCalculation(calculationName="TotalTrains2", summariseExpression=countFunction,
                         headingStyleDeclarations=list("color"="red", "font-weight"="bold"),
                         cellStyleDeclarations=list("color"="blue"))
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    # prepStr(as.character(pt$getCss()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"NewHeadingStyle1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \">TotalTrains2</th>\n    <th class=\"NewHeadingStyle1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \">TotalTrains2</th>\n    <th class=\"NewHeadingStyle1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \">TotalTrains2</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"CellStyle1\">3079</td>\n    <td class=\"Cell\" style=\"color: blue; \">3079</td>\n    <td class=\"CellStyle1\">830</td>\n    <td class=\"Cell\" style=\"color: blue; \">830</td>\n    <td class=\"CellStyle1\">3909</td>\n    <td class=\"Total\" style=\"color: blue; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"CellStyle1\">22865</td>\n    <td class=\"Cell\" style=\"color: blue; \">22865</td>\n    <td class=\"CellStyle1\">63</td>\n    <td class=\"Cell\" style=\"color: blue; \">63</td>\n    <td class=\"CellStyle1\">22928</td>\n    <td class=\"Total\" style=\"color: blue; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"CellStyle1\">14487</td>\n    <td class=\"Cell\" style=\"color: blue; \">14487</td>\n    <td class=\"CellStyle1\">33792</td>\n    <td class=\"Cell\" style=\"color: blue; \">33792</td>\n    <td class=\"CellStyle1\">48279</td>\n    <td class=\"Total\" style=\"color: blue; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"CellStyle1\">8594</td>\n    <td class=\"Cell\" style=\"color: blue; \">8594</td>\n    <td class=\"CellStyle1\"></td>\n    <td class=\"Cell\" style=\"color: blue; \"></td>\n    <td class=\"CellStyle1\">8594</td>\n    <td class=\"Total\" style=\"color: blue; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"CellStyle1\">49025</td>\n    <td class=\"Total\" style=\"color: blue; \">49025</td>\n    <td class=\"CellStyle1\">34685</td>\n    <td class=\"Total\" style=\"color: blue; \">34685</td>\n    <td class=\"CellStyle1\">83710</td>\n    <td class=\"Total\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"
    css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.OutlineColumnHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px; text-align: center; }\r\n.OutlineRowHeader {font-family: Arial; font-size: 0.75em; border: 1px solid lightgray; vertical-align: middle; font-weight: bold; background-color: #F2F2F2; padding: 2px 8px 2px 2px; text-align: left; }\r\n.OutlineCell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; background-color: #F8F8F8; font-weight: bold; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.NewHeadingStyle1 {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: Gold; }\r\n.CellStyle1 {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; background-color: Yellow; text-align: right; }\r\n"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


scenarios <- testScenarios("theming tests:  styling basics 1")
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
    pt$addColumnDataGroups("Status")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$setStyling(rowNumbers=2:3, declarations=list("background-color"="yellow"))
    pt$setStyling(rFrom=4, rTo=5, columnNumbers=5:7, declarations=list("background-color"="pink"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">R</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">R</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\">815</td>\n    <td class=\"Cell\">15</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">22270</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">569</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">26</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">60</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">1</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">63</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">14133</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">336</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">18</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">32851</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">914</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">27</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">33792</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">8359</td>\n    <td class=\"Cell\">226</td>\n    <td class=\"Cell\">9</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\" style=\"background-color: pink; \"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \"></td>\n    <td class=\"Cell\" style=\"background-color: pink; \"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">47780</td>\n    <td class=\"Total\">1190</td>\n    <td class=\"Total\">55</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\" style=\"background-color: pink; \">33726</td>\n    <td class=\"Total\" style=\"background-color: pink; \">931</td>\n    <td class=\"Total\" style=\"background-color: pink; \">28</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("theming tests:  styling basics 2")
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
    pt$addColumnDataGroups("Status")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    pt$setStyling(rowNumbers=c(2, 4), columnNumbers=c(1, 3), declarations=list("background-color"="pink"))
    pt$setStyling(columnNumbers=5:7, declarations=list("background-color"="yellow"))
    pt$setStyling(rFrom=1, rTo=2, cFrom=8, cTo=9, declarations=list("background-color"="lightgreen"))
    pt$setStyling(cells=pt$getCell(5, 9), declarations=list("background-color"="lightblue"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">R</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">A</th>\n    <th class=\"ColumnHeader\">C</th>\n    <th class=\"ColumnHeader\">R</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3018</td>\n    <td class=\"Cell\">59</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Total\">3079</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">815</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">15</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">830</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: pink; \">22270</td>\n    <td class=\"Cell\">569</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">26</td>\n    <td class=\"Total\">22865</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">60</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">2</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">1</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">63</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">14133</td>\n    <td class=\"Cell\">336</td>\n    <td class=\"Cell\">18</td>\n    <td class=\"Total\">14487</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">32851</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">914</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">27</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: pink; \">8359</td>\n    <td class=\"Cell\">226</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">9</td>\n    <td class=\"Total\">8594</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Cell\" style=\"background-color: yellow; \"></td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">47780</td>\n    <td class=\"Total\">1190</td>\n    <td class=\"Total\">55</td>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">33726</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">931</td>\n    <td class=\"Total\" style=\"background-color: yellow; \">28</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\" style=\"background-color: lightblue; \">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}


scenarios <- testScenarios("theming tests:  applying styling multiple times to the same cell", runAllForReleaseVersion=TRUE)
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode,
                         compatibility=list(totalStyleIsCellStyle=TRUE))
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    grps <- pt$rowGroup$childGroups
    pt$setStyling(groups=grps, declarations=list("font-weight"="normal"))
    pt$setStyling(groups=grps, declarations=list("color"="blue"))
    cells <- pt$getCells(rowNumbers=4)
    pt$setStyling(cells=cells, declarations=list("font-weight"="bold"))
    pt$setStyling(cells=cells, declarations=list("color"="green"))
    pt$setStyling(2, 1, declarations=list("color"="red"))
    pt$setStyling(2, 1, declarations=list("font-weight"="bold"))
    # pt$renderPivot()
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; color: blue; \">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; color: blue; \">CrossCountry</th>\n    <td class=\"Cell\" style=\"color: red; font-weight: bold; \">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; color: blue; \">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; color: blue; \">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: bold; color: green; \">8594</td>\n    <td class=\"Cell\" style=\"font-weight: bold; color: green; \"></td>\n    <td class=\"Cell\" style=\"font-weight: bold; color: green; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" style=\"font-weight: normal; color: blue; \">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"

    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("theming tests:  inheriting cell style names from row groups")
for(i in 1:nrow(scenarios)) {
  if(!isDevelopmentVersion) break
  evaluationMode <- scenarios$evaluationMode[i]
  processingLibrary <- scenarios$processingLibrary[i]
  description <- scenarios$description[i]
  countFunction <- scenarios$countFunction[i]

  test_that(description, {

    library(pivottabler)
    pt <- PivotTable$new(processingLibrary=processingLibrary, evaluationMode=evaluationMode)
    style <- pt$styles$copyStyle(styleName="Cell", newStyleName="RedCell")
    style$setPropertyValue(property="color", value="red")
    style <- pt$styles$copyStyle(styleName="Cell", newStyleName="BlueCell")
    style$setPropertyValue(property="color", value="blue")
    pt$addData(bhmtrains)
    pt$addColumnDataGroups("TrainCategory")
    grps <- pt$addRowDataGroups("TOC")
    grps[[2]]$cellBaseStyleName <- "RedCell"
    grps <- pt$addRowDataGroups("PowerType") # D/EMU = Diesel/Electric Multiple Unit, HST=High Speed Train
    grps[[4]]$cellBaseStyleName <- "BlueCell"
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"RedCell\">22133</td>\n    <td class=\"RedCell\">63</td>\n    <td class=\"RedCell\">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"BlueCell\">732</td>\n    <td class=\"BlueCell\"></td>\n    <td class=\"BlueCell\">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"RedCell\">22865</td>\n    <td class=\"RedCell\">63</td>\n    <td class=\"RedCell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("theming tests:  inheriting cell style declarations from row groups")
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
    grps[[2]]$cellStyle <- list("color"="red")
    grps <- pt$addRowDataGroups("PowerType") # D/EMU = Diesel/Electric Multiple Unit, HST=High Speed Train
    grps[[4]]$cellStyle <- list("color"="blue")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\">Express Passenger</th>\n    <th class=\"ColumnHeader\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">3079</td>\n    <td class=\"Total\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">CrossCountry</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\" style=\"color: red; \">22133</td>\n    <td class=\"Cell\" style=\"color: red; \">63</td>\n    <td class=\"Total\" style=\"color: red; \">22196</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\" style=\"color: blue; \">732</td>\n    <td class=\"Cell\" style=\"color: blue; \"></td>\n    <td class=\"Total\" style=\"color: blue; \">732</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\" style=\"color: red; \">22865</td>\n    <td class=\"Total\" style=\"color: red; \">63</td>\n    <td class=\"Total\" style=\"color: red; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">London Midland</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5638</td>\n    <td class=\"Cell\">5591</td>\n    <td class=\"Total\">11229</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8849</td>\n    <td class=\"Cell\">28201</td>\n    <td class=\"Total\">37050</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">14487</td>\n    <td class=\"Total\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"3\">Virgin Trains</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">2137</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">2137</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">6457</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">6457</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <td class=\"Total\">8594</td>\n    <td class=\"Total\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Total</th>\n    <th class=\"RowHeader\">&nbsp;</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
    expect_identical(as.character(pt$getHtml()), html)
  })
}



scenarios <- testScenarios("theming tests:  intersecting styles from rows, columns, calcs and cells")
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
    cgrps <- pt$addColumnDataGroups("PowerType")
    colorText <- function(grp) {
      if(isTRUE(grp$values=="DMU")) grp$cellStyle <- list(color="blue")
      else if(isTRUE(grp$values=="EMU")) grp$cellStyle <- list(color="green")
      else if(isTRUE(grp$values=="HST")) grp$cellStyle <- list(color="red")
    }
    invisible(lapply(cgrps, colorText))
    rgrps <- pt$addRowDataGroups("TOC", atLevel=1, addTotal=FALSE)
    colorText <- function(grp) {
      if(isTRUE(grp$values=="Arriva Trains Wales")) grp$cellStyle <- list("background-color"="aliceblue")
      else if(isTRUE(grp$values=="CrossCountry")) grp$cellStyle <- list("background-color"="cornsilk")
      else if(isTRUE(grp$values=="London Midland")) grp$cellStyle <- list("background-color"="lightgreen")
      else if(isTRUE(grp$values=="Virgin Trains")) grp$cellStyle <- list("background-color"="lavenderblush")
    }
    invisible(lapply(rgrps, colorText))
    pt$addRowGroup(caption="Total", isOutline=TRUE, isTotal=TRUE, isLevelTotal=TRUE,
                   styleDeclarations=list(color="blue"), cellStyleDeclarations=list("background-color"="plum"))
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction, caption="Train Count",
                         format = "%.0f", cellStyleDeclarations=list("font-family"="serif", "font-style"="italic"))
    pt$defineCalculation(calculationName="AvgSchSpeed", summariseExpression="mean(SchedSpeedMPH, na.rm=TRUE)",
                         caption="Avg Speed", format = "%.0f",
                         cellStyleDeclarations=list("font-family"="Lucida Console", "xl-value-format"="#,##0"))
    pt$addRowCalculationGroups()
    pt$evaluatePivot()
    pt$setStyling(rFrom=4, rTo=5, cFrom=6, cTo=8, declarations=list("background-color"="yellow", "font-weight"="bold"))
    # pt$renderPivot()
    # sum(pt$cells$asMatrix(), na.rm=TRUE)
    # prepStr(as.character(pt$getHtml()))
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"2\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"4\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"3\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">HST</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">DMU</th>\n    <th class=\"ColumnHeader\">EMU</th>\n    <th class=\"ColumnHeader\">Total</th>\n    <th class=\"ColumnHeader\">&nbsp;</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Train Count</th>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: blue; font-family: serif; font-style: italic; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: green; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: red; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: serif; font-style: italic; \">3079</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: blue; font-family: serif; font-style: italic; \">830</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: green; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: serif; font-style: italic; \">830</td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: serif; font-style: italic; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Avg Speed</th>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: blue; font-family: Lucida Console; \">90</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: green; font-family: Lucida Console; \"></td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: red; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: Lucida Console; \">90</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: blue; font-family: Lucida Console; \">89</td>\n    <td class=\"Cell\" style=\"background-color: aliceblue; color: green; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: Lucida Console; \">89</td>\n    <td class=\"Total\" style=\"background-color: aliceblue; font-family: Lucida Console; \">90</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">CrossCountry</th>\n    <th class=\"RowHeader\">Train Count</th>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: blue; font-family: serif; font-style: italic; \">22133</td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: green; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: red; font-family: serif; font-style: italic; \">732</td>\n    <td class=\"Total\" style=\"background-color: cornsilk; font-family: serif; font-style: italic; \">22865</td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: blue; font-family: serif; font-style: italic; \">63</td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: green; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: cornsilk; font-family: serif; font-style: italic; \">63</td>\n    <td class=\"Total\" style=\"background-color: cornsilk; font-family: serif; font-style: italic; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Avg Speed</th>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: blue; font-family: Lucida Console; \">113</td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: green; font-family: Lucida Console; \"></td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: red; font-family: Lucida Console; \">125</td>\n    <td class=\"Total\" style=\"background-color: cornsilk; font-family: Lucida Console; \">113</td>\n    <td class=\"Cell\" style=\"background-color: cornsilk; color: blue; font-family: Lucida Console; \">100</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: green; font-family: Lucida Console; font-weight: bold; \"></td>\n    <td class=\"Total\" style=\"background-color: yellow; font-family: Lucida Console; font-weight: bold; \">100</td>\n    <td class=\"Total\" style=\"background-color: yellow; font-family: Lucida Console; font-weight: bold; \">113</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">London Midland</th>\n    <th class=\"RowHeader\">Train Count</th>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: blue; font-family: serif; font-style: italic; \">5638</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: green; font-family: serif; font-style: italic; \">8849</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: red; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; font-family: serif; font-style: italic; \">14487</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: blue; font-family: serif; font-style: italic; \">5591</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: green; font-family: serif; font-style: italic; font-weight: bold; \">28201</td>\n    <td class=\"Total\" style=\"background-color: yellow; font-family: serif; font-style: italic; font-weight: bold; \">33792</td>\n    <td class=\"Total\" style=\"background-color: yellow; font-family: serif; font-style: italic; font-weight: bold; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Avg Speed</th>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: blue; font-family: Lucida Console; \">92</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: green; font-family: Lucida Console; \">101</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: red; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: lightgreen; font-family: Lucida Console; \">98</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: blue; font-family: Lucida Console; \">76</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; color: green; font-family: Lucida Console; \">94</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; font-family: Lucida Console; \">91</td>\n    <td class=\"Total\" style=\"background-color: lightgreen; font-family: Lucida Console; \">93</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\">Virgin Trains</th>\n    <th class=\"RowHeader\">Train Count</th>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: blue; font-family: serif; font-style: italic; \">2137</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: green; font-family: serif; font-style: italic; \">6457</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: red; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: serif; font-style: italic; \">8594</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: blue; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: green; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: serif; font-style: italic; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: serif; font-style: italic; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Avg Speed</th>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: blue; font-family: Lucida Console; \">125</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: green; font-family: Lucida Console; \">125</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: red; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: Lucida Console; \">125</td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: blue; font-family: Lucida Console; \"></td>\n    <td class=\"Cell\" style=\"background-color: lavenderblush; color: green; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: Lucida Console; \"></td>\n    <td class=\"Total\" style=\"background-color: lavenderblush; font-family: Lucida Console; \">125</td>\n  </tr>\n  <tr>\n    <th class=\"OutlineRowHeader\" style=\"color: blue; \" rowspan=\"2\">Total</th>\n    <th class=\"RowHeader\">Train Count</th>\n    <td class=\"Total\" style=\"background-color: plum; color: blue; font-family: serif; font-style: italic; \">32987</td>\n    <td class=\"Total\" style=\"background-color: plum; color: green; font-family: serif; font-style: italic; \">15306</td>\n    <td class=\"Total\" style=\"background-color: plum; color: red; font-family: serif; font-style: italic; \">732</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: serif; font-style: italic; \">49025</td>\n    <td class=\"Total\" style=\"background-color: plum; color: blue; font-family: serif; font-style: italic; \">6484</td>\n    <td class=\"Total\" style=\"background-color: plum; color: green; font-family: serif; font-style: italic; \">28201</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: serif; font-style: italic; \">34685</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: serif; font-style: italic; \">83710</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Avg Speed</th>\n    <td class=\"Total\" style=\"background-color: plum; color: blue; font-family: Lucida Console; \">108</td>\n    <td class=\"Total\" style=\"background-color: plum; color: green; font-family: Lucida Console; \">111</td>\n    <td class=\"Total\" style=\"background-color: plum; color: red; font-family: Lucida Console; \">125</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: Lucida Console; \">109</td>\n    <td class=\"Total\" style=\"background-color: plum; color: blue; font-family: Lucida Console; \">78</td>\n    <td class=\"Total\" style=\"background-color: plum; color: green; font-family: Lucida Console; \">94</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: Lucida Console; \">91</td>\n    <td class=\"Total\" style=\"background-color: plum; font-family: Lucida Console; \">101</td>\n  </tr>\n</table>"

    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 505332.016147847)
    expect_identical(as.character(pt$getHtml()), html)
  })
}
