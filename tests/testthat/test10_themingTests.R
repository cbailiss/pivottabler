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


context("THEMING TESTS")



scenarios <- testScenarios("theming tests:  basic test")
for(i in 1:nrow(scenarios)) {
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
    html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Total\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Total\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Total\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Total\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Total\">49025</td>\n    <td class=\"Total\">34685</td>\n    <td class=\"Total\">83710</td>\n  </tr>\n</table>"
    css <- ".Table {border-collapse: collapse; border: 2px solid rgb(198, 89, 17); }\r\n.ColumnHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.RowHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.Cell {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(248, 198, 165); }\r\n"


    expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
    expect_identical(as.character(pt$getHtml()), html)
    expect_identical(pt$getCss(), css)
  })
}


# The code for the following tests runs OK in R-Studio
# But errors when used with testthat on my PC:
#   Error PivotDataGroup.R:294:  object of type 'closure' is not subsettable
# The error is something to do with:
#   styleDeclarations=list("color"="red", "font-weight"="bold", "background-color"="yellow")

# scenarios <- testScenarios("theming tests:  styling data groups")
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
#     pt$addData(bhmtrains)
#     pt$addColumnDataGroups("TrainCategory", styleDeclarations=list("color"="red", "font-weight"="bold", "background-color"="yellow"))
#     pt$addRowDataGroups("TOC")
#     pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
#     pt$evaluatePivot()
#     # pt$renderPivot()
#     # sum(pt$cells$asMatrix(), na.rm=TRUE)
#     # prepStr(as.character(pt$getHtml()))
#     # prepStr(as.character(pt$getCss()))
#     html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \" colspan=\"1\">Express Passenger</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \" colspan=\"1\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; background-color: yellow; \" colspan=\"1\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"Cell\">3079</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"Cell\">22865</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"Cell\">14487</td>\n    <td class=\"Cell\">33792</td>\n    <td class=\"Cell\">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"Cell\">8594</td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"Cell\">49025</td>\n    <td class=\"Cell\">34685</td>\n    <td class=\"Cell\">83710</td>\n  </tr>\n</table>"
#     css <- ".Table {border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n"
#
#     expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
#     expect_identical(as.character(pt$getHtml()), html)
#     expect_identical(pt$getCss(), css)
#   })
# }
#
#
# scenarios <- testScenarios("theming tests:  styling cells")
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
#     pt$styles$addStyle(styleName="NewHeadingStyle1", list(
#       "font-family"="Arial",
#       "font-size"="0.75em",
#       padding="2px",
#       border="1px solid lightgray",
#       "vertical-align"="middle",
#       "text-align"="center",
#       "font-weight"="bold",
#       "background-color"="Gold",
#       "xl-wrap-text"="wrap"
#     ))
#     pt$styles$addStyle(styleName="CellStyle1", list(
#       "font-family"="Arial",
#       "font-size"="0.75em",
#       padding="2px 2px 2px 8px",
#       border="1px solid lightgray",
#       "vertical-align"="middle",
#       "background-color"="Yellow",
#       "text-align"="right"
#     ))
#     pt$addData(bhmtrains)
#     pt$addColumnDataGroups("TrainCategory")
#     pt$addRowDataGroups("TOC")
#     pt$defineCalculation(calculationName="TotalTrains1", summariseExpression=countFunction,
#                          headingBaseStyleName="NewHeadingStyle1", cellBaseStyleName="CellStyle1")
#     pt$defineCalculation(calculationName="TotalTrains2", summariseExpression=countFunction,
#                          headingStyleDeclarations=list("color"="red", "font-weight"="bold"),
#                          cellStyleDeclarations=list("color"="blue"))
#     pt$evaluatePivot()
#     # pt$renderPivot()
#     # sum(pt$cells$asMatrix(), na.rm=TRUE)
#     # prepStr(as.character(pt$getHtml()))
#     # prepStr(as.character(pt$getCss()))
#     html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"2\" colspan=\"1\">&nbsp;</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Express Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Ordinary Passenger</th>\n    <th class=\"ColumnHeader\" colspan=\"2\">Total</th>\n  </tr>\n  <tr>\n    <th class=\"NewHeadingStyle1\" colspan=\"1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \" colspan=\"1\">TotalTrains2</th>\n    <th class=\"NewHeadingStyle1\" colspan=\"1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \" colspan=\"1\">TotalTrains2</th>\n    <th class=\"NewHeadingStyle1\" colspan=\"1\">TotalTrains1</th>\n    <th class=\"ColumnHeader\" style=\"color: red; font-weight: bold; \" colspan=\"1\">TotalTrains2</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Arriva Trains Wales</th>\n    <td class=\"CellStyle1\">3079</td>\n    <td class=\"Cell\" style=\"color: blue; \">3079</td>\n    <td class=\"CellStyle1\">830</td>\n    <td class=\"Cell\" style=\"color: blue; \">830</td>\n    <td class=\"CellStyle1\">3909</td>\n    <td class=\"Cell\" style=\"color: blue; \">3909</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">CrossCountry</th>\n    <td class=\"CellStyle1\">22865</td>\n    <td class=\"Cell\" style=\"color: blue; \">22865</td>\n    <td class=\"CellStyle1\">63</td>\n    <td class=\"Cell\" style=\"color: blue; \">63</td>\n    <td class=\"CellStyle1\">22928</td>\n    <td class=\"Cell\" style=\"color: blue; \">22928</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">London Midland</th>\n    <td class=\"CellStyle1\">14487</td>\n    <td class=\"Cell\" style=\"color: blue; \">14487</td>\n    <td class=\"CellStyle1\">33792</td>\n    <td class=\"Cell\" style=\"color: blue; \">33792</td>\n    <td class=\"CellStyle1\">48279</td>\n    <td class=\"Cell\" style=\"color: blue; \">48279</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Virgin Trains</th>\n    <td class=\"CellStyle1\">8594</td>\n    <td class=\"Cell\" style=\"color: blue; \">8594</td>\n    <td class=\"CellStyle1\"></td>\n    <td class=\"Cell\" style=\"color: blue; \"></td>\n    <td class=\"CellStyle1\">8594</td>\n    <td class=\"Cell\" style=\"color: blue; \">8594</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\" rowspan=\"1\">Total</th>\n    <td class=\"CellStyle1\">49025</td>\n    <td class=\"Cell\" style=\"color: blue; \">49025</td>\n    <td class=\"CellStyle1\">34685</td>\n    <td class=\"Cell\" style=\"color: blue; \">34685</td>\n    <td class=\"CellStyle1\">83710</td>\n    <td class=\"Cell\" style=\"color: blue; \">83710</td>\n  </tr>\n</table>"
#     css <- ".Table {border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.NewHeadingStyle1 {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: Gold; }\r\n.CellStyle1 {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; background-color: Yellow; text-align: right; }\r\n"
#
#     expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
#     expect_identical(as.character(pt$getHtml()), html)
#     expect_identical(pt$getCss(), css)
#   })
# }
