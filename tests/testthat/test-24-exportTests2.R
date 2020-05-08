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


context("EXPORT TESTS 2")



scenarios <- testScenarios("export tests 2:  rows(0) and cols(0)")
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
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "))
    text <- "83710"
    text2 <- "TotalTrains 83710"
    text3 <- "83710"

    expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 83710)
    expect_identical(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(1) and cols(1)")
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
    pt$addColumnDataGroups("TOC")
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "))
    text <- "c(3079, 830, 3909) c(22865, 63, 22928) c(14487, 33792, 48279) c(8594, NA, 8594) c(49025, 34685, 83710)"
    text2 <- " Express Passenger Ordinary Passenger Total Arriva Trains Wales 3079 830 3909 CrossCountry 22865 63 22928 London Midland 14487 33792 48279 Virgin Trains 8594  8594 Total 49025 34685 83710"
    text3 <- "3079 830 3909 22865 63 22928 14487 33792 48279 8594 NA 8594 49025 34685 83710"

    expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 334840)
    expect_identical(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(1) and cols(0)")
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
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "))
    text <- "c(49025, 34685, 83710)"
    text2 <- " Express Passenger Ordinary Passenger Total TotalTrains 49025 34685 83710"
    text3 <- "49025 34685 83710"

    expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 167420)
    expect_identical(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(0) and cols(1)")
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
    pt$addColumnDataGroups("TOC")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "))
    text <- "3909 22928 48279 8594 83710"
    text2 <- "Arriva Trains Wales 3909 CrossCountry 22928 London Midland 48279 Virgin Trains 8594 Total 83710"
    text3 <- "3909 22928 48279 8594 83710"

    expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 167420)
    expect_identical(paste(as.character(pt$asDataFrame()), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix()), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(2) and cols(2)")
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
    pt$addColumnDataGroups("TOC")
    pt$addColumnDataGroups("TrainCategory")
    pt$addRowDataGroups("TOC")
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(separator=" | "), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "))
    text <- "c(3079, NA, 3079, NA, NA, NA, NA, NA, NA, NA, NA, 3079) c(NA, 830, 830, NA, NA, NA, NA, NA, NA, NA, NA, 830) c(3079, 830, 3909, NA, NA, NA, NA, NA, NA, NA, NA, 3909) c(NA, NA, NA, 22865, NA, 22865, NA, NA, NA, NA, NA, 22865) c(NA, NA, NA, NA, 63, 63, NA, NA, NA, NA, NA, 63) c(NA, NA, NA, 22865, 63, 22928, NA, NA, NA, NA, NA, 22928) c(NA, NA, NA, NA, NA, NA, 14487, NA, 14487, NA, NA, 14487) c(NA, NA, NA, NA, NA, NA, NA, 33792, 33792, NA, NA, 33792) c(NA, NA, NA, NA, NA, NA, 14487, 33792, 48279, NA, NA, 48279) c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 8594, 8594, 8594) c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 8594, 8594, 8594) c(3079, 830, 3909, 22865, 63, 22928, 14487, 33792, 48279, 8594, 8594, 83710)"
    text2 <- "  Arriva Trains Wales   CrossCountry   London Midland   Virgin Trains  Total   Express Passenger Ordinary Passenger Total Express Passenger Ordinary Passenger Total Express Passenger Ordinary Passenger Total Express Passenger Total  Arriva Trains Wales Express Passenger 3079  3079         3079  Ordinary Passenger  830 830         830  Total 3079 830 3909         3909 CrossCountry Express Passenger    22865  22865      22865  Ordinary Passenger     63 63      63  Total    22865 63 22928      22928 London Midland Express Passenger       14487  14487   14487  Ordinary Passenger        33792 33792   33792  Total       14487 33792 48279   48279 Virgin Trains Express Passenger          8594 8594 8594  Total          8594 8594 8594 Total  3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"
    text3 <- "3079 NA 3079 NA NA NA NA NA NA NA NA 3079 NA 830 830 NA NA NA NA NA NA NA NA 830 3079 830 3909 NA NA NA NA NA NA NA NA 3909 NA NA NA 22865 NA 22865 NA NA NA NA NA 22865 NA NA NA NA 63 63 NA NA NA NA NA 63 NA NA NA 22865 63 22928 NA NA NA NA NA 22928 NA NA NA NA NA NA 14487 NA 14487 NA NA 14487 NA NA NA NA NA NA NA 33792 33792 NA NA 33792 NA NA NA NA NA NA 14487 33792 48279 NA NA 48279 NA NA NA NA NA NA NA NA NA 8594 8594 8594 NA NA NA NA NA NA NA NA NA 8594 8594 8594 3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"

    expect_equal(sum(pt$asDataFrame(separator=" | "), na.rm=TRUE), 753390)
    expect_identical(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(2) and cols(0)")
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
    pt$addRowDataGroups("TOC")
    pt$addRowDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(separator=" | "), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "))
    text <- "c(3079, 830, 3909, 22865, 63, 22928, 14487, 33792, 48279, 8594, 8594, 83710)"
    text2 <- " Arriva Trains Wales   CrossCountry   London Midland   Virgin Trains  Total  Express Passenger Ordinary Passenger Total Express Passenger Ordinary Passenger Total Express Passenger Ordinary Passenger Total Express Passenger Total  TotalTrains 3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"
    text3 <- "3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"

    expect_equal(sum(pt$asDataFrame(separator=" | "), na.rm=TRUE), 251130)
    expect_identical(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "), text3)
  })
}



scenarios <- testScenarios("export tests 2:  rows(0) and cols(2)")
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
    pt$addColumnDataGroups("TOC")
    pt$addColumnDataGroups("TrainCategory")
    pt$defineCalculation(calculationName="TotalTrains", summariseExpression=countFunction)
    pt$evaluatePivot()
    # sum(pt$asDataFrame(separator=" | "), na.rm=TRUE)
    # prepStr(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "))
    # prepStr(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "))
    text <- "3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"
    text2 <- "Arriva Trains Wales Express Passenger 3079  Ordinary Passenger 830  Total 3909 CrossCountry Express Passenger 22865  Ordinary Passenger 63  Total 22928 London Midland Express Passenger 14487  Ordinary Passenger 33792  Total 48279 Virgin Trains Express Passenger 8594  Total 8594 Total  83710"
    text3 <- "3079 830 3909 22865 63 22928 14487 33792 48279 8594 8594 83710"

    expect_equal(sum(pt$asDataFrame(separator=" | "), na.rm=TRUE), 251130)
    expect_identical(paste(as.character(pt$asDataFrame(separator=" | ")), sep=" ", collapse=" "), text)
    expect_identical(paste(as.character(pt$asMatrix()), sep=" ", collapse=" "), text2)
    expect_identical(paste(as.character(pt$asDataMatrix(separator=" | ")), sep=" ", collapse=" "), text3)
  })
}



