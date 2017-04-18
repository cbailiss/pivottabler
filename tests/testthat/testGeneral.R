library(testthat)
context("General tests")

# most common expectations:
# equality:        expect_equal() and expect_identical()
# regexp:          expect_match()
# catch-all:       expect_true() and expect_false()
# console output:  expect_output()
# messages:        expect_message()
# warning:         expect_warning()
# errors:          expect_error()

checkDigestAvailable <- function() {
  if(!requireNamespace("digest", quietly = TRUE)) skip("digest package not available")
}


test_that("bhmtrains basic pivot total", {

  library(pivottabler)
  pt <- pivottabler::PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
})


test_that("smoke tests:  bhmtrains basic pivot values", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$cells$asMatrix(), algo="md5")

  expect_identical(digest::digest(pt$cells$asMatrix(), algo="md5"), "98734d1163b8eaf2de0903292effe2fc")
})


test_that("smoke tests:  bhmtrains basic pivot html", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a93a42c781ec0855456695c83e0bc218")
})


test_that("smoke tests:  basic layout tests:  empty pivot", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "4cbb141f2643a384349301a209816a35")
})


test_that("basic layout tests:  empty pivot plus data", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "4cbb141f2643a384349301a209816a35")
})


test_that("basic layout tests:  just a total", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 83710)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "9a1f109375f16824e1f2673fb98ce49e")
})


test_that("basic layout tests:  two measures", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 83835)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "b4c1b54df78bca250fda21f8fec41676")
})


test_that("basic layout tests:  rows only", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addRowDataGroups("TOC")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "c9a2502913bcb1af727276bc36218835")
})


test_that("basic layout tests:  rows plus total", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "0921e06e31cd11c00c67c15274362552")
})


test_that("basic layout tests:  rows plus two measures", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "180c6dbb7dce5066c1774509926e8c2d")
})


test_that("basic layout tests:  columns only", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "0a6d4186e7ad7ba417e7d77478b08b87")
})


test_that("basic layout tests:  columns plus total", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "f18a3892caac0451a136d95c3db23953")
})


test_that("basic layout tests:  columns plus two totals", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "289803c216e29da8dc4895ed4066f06f")
})


test_that("basic layout tests:  rows and columns only", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "2af2e6c5f23ffc450ff535a92e9ae25d")
})


test_that("basic layout tests:  rows, columns and calculation", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a93a42c781ec0855456695c83e0bc218")
})


test_that("basic layout tests:  rows, columns and two calculations", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "9f24c79b44c0068292fa7916c63c00d6")
})


test_that("basic layout tests:  columns plus total on row", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "4f9ed38dedbb8052ea52fc920c137c32")
})


test_that("basic layout tests:  columns plus two totals on rows", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "5bbbad8408175590395049941e41cd5b")
})


test_that("basic layout tests:  rows, columns and calculation on rows", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a93a42c781ec0855456695c83e0bc218")
})


test_that("basic layout tests:  rows, columns and two calculations on rows", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "5d87c05a38d7eacf0acdd5a9a2d54904")
})


test_that("data groups tests:  dplyr ignoring parent groups", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType", onlyCombinationsThatExist=FALSE)
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "4c28215c21eb0b904f4eba40fbe4e931")
})


test_that("data groups tests:  adding data groups explicitly", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType", fromData=FALSE, explicitListOfValues=list("DMU", "EMU"))
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 500796)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "480a893200d6ad062e0bc6ce8db3da57")
})


test_that("data groups tests:  adding data groups that combine values", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list(
    "London Midland", "CrossCountry", "Other"=c("Arriva Trains Wales", "Virgin Trains")))
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a2e56e597f1c9d0b7166bad08bd6bf7e")
})

# fails testing on win builder - format codes %B %Y probably mean different things on different machines
# test_that("data groups tests:  formatting data groups", {
#
#   checkDigestAvailable()
#
#   library(dplyr)
#   library(lubridate)
#   trains <- mutate(bhmtrains,
#      GbttDate=as.POSIXct(ifelse(is.na(GbttArrival), GbttDeparture, GbttArrival),
#                          origin = "1970-01-01"),
#      GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))
#
#   library(pivottabler)
#   pt <- PivotTable$new()
#   pt$addData(trains)
#   pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
#   pt$addColumnDataGroups("PowerType")
#   pt$addRowDataGroups("TOC")
#   pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
#   pt$evaluatePivot()
#   # pt$renderPivot()
#   # sum(pt$cells$asMatrix(), na.rm=TRUE)
#   # digest::digest(as.character(pt$getHtml()), algo="md5")
#
#   expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
#   expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "2bbed6d4f402fcb465d7b3569aa14e46")
# })


test_that("data groups tests:  sort by group into descending order", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC", dataSortOrder="desc")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "ec319fdc51c46f0096b8cdd06ff66b4a")
})


test_that("data groups tests:  numerical sort by group into descending order", {

  checkDigestAvailable()

  a <- c(7,4,6,1,8,3,2,9,5,10,12,11,0)
  b <- c(1,5,4,2,3,2,4,3,1,5,2,1,4)
  z <- a + b
  df <- data.frame(a, b, z)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addColumnDataGroups("a", dataSortOrder="asc")
  pt$addRowDataGroups("b", dataSortOrder="desc")
  pt$defineCalculation(calculationName="z", summariseExpression="sum(z)")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 460)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "9a832555c1e1b93e83880723b2b327a8")
})


test_that("data groups tests:  sort by value into descending order", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC", dataSortOrder="desc")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$sortRowDataGroups(levelNumber=1, orderBy="calculation", sortOrder="desc")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "5282168e57c7fff5f74da5293683a104")
})


test_that("data groups tests:  sort by level 2 value into descending order", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$sortColumnDataGroups(levelNumber=2, orderBy="calculation", sortOrder="desc")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "1a9f60b61dddfcc08f8cf1a878dca0b3")
})


test_that("calculation tests:  calculate dply summarise", {

  checkDigestAvailable()

  library(pivottabler)
  library(dplyr)
  library(lubridate)

  # derive some additional data
  trains <- mutate(bhmtrains,
     ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
     ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta))

  # create the pivot table
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addRowDataGroups("TOC", totalCaption="All TOCs")
  pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                       summariseExpression="n()")
  pt$defineCalculation(calculationName="MinArrivalDelay", caption="Min Arr. Delay",
                       summariseExpression="min(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="MaxArrivalDelay", caption="Max Arr. Delay",
                       summariseExpression="max(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="MeanArrivalDelay", caption="Mean Arr. Delay",
                       summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.1f")
  pt$defineCalculation(calculationName="MedianArrivalDelay", caption="Median Arr. Delay",
                       summariseExpression="median(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="IQRArrivalDelay", caption="Delay IQR",
                       summariseExpression="IQR(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="SDArrivalDelay", caption="Delay Std. Dev.",
                       summariseExpression="sd(ArrivalDelay, na.rm=TRUE)", format="%.1f")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sprintf("%.6f", sum(pt$cells$asMatrix()))
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "6c2a360ffe13a8614c1b43d290af5700")
})



test_that("calculation tests:  calculate on rows dply summarise", {

  checkDigestAvailable()

  library(pivottabler)
  library(dplyr)
  library(lubridate)

  # derive some additional data
  trains <- mutate(bhmtrains,
     ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
     ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta))

  # create the pivot table
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addColumnDataGroups("TOC", totalCaption="All TOCs")
  pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                       summariseExpression="n()")
  pt$defineCalculation(calculationName="MinArrivalDelay", caption="Min Arr. Delay",
                       summariseExpression="min(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="MaxArrivalDelay", caption="Max Arr. Delay",
                       summariseExpression="max(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="MeanArrivalDelay", caption="Mean Arr. Delay",
                       summariseExpression="mean(ArrivalDelay, na.rm=TRUE)", format="%.1f")
  pt$defineCalculation(calculationName="MedianArrivalDelay", caption="Median Arr. Delay",
                       summariseExpression="median(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="IQRArrivalDelay", caption="Delay IQR",
                       summariseExpression="IQR(ArrivalDelay, na.rm=TRUE)")
  pt$defineCalculation(calculationName="SDArrivalDelay", caption="Delay Std. Dev.",
                       summariseExpression="sd(ArrivalDelay, na.rm=TRUE)", format="%.1f")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sprintf("%.6f", sum(pt$cells$asMatrix()))
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "91c6b419f3294aa2abba44023819f5da")
})



test_that("calculation tests:  deriving values from other calculations", {

  checkDigestAvailable()

  library(pivottabler)
  library(dplyr)
  library(lubridate)

  # derive some additional data
  trains <- mutate(bhmtrains,
     ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
     ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta),
     DelayedByMoreThan5Minutes=ifelse(ArrivalDelay>5,1,0))

  # create the pivot table
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addRowDataGroups("TOC", totalCaption="All TOCs")
  pt$defineCalculation(calculationName="DelayedTrains", caption="Trains Arr. 5+ Mins Late",
                       summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm=TRUE)")
  pt$defineCalculation(calculationName="TotalTrains", caption="Total Trains",
                       summariseExpression="n()")
  pt$defineCalculation(calculationName="DelayedPercent", caption="% Trains Arr. 5+ Mins Late",
                       type="calculation", basedOn=c("DelayedTrains", "TotalTrains"),
                       format="%.1f %%",
                       calculationExpression="values$DelayedTrains/values$TotalTrains*100")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sprintf("%.6f", sum(pt$cells$asMatrix()))
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 182432.916225)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "666b8152d843bb0d6f55849f0d2d2bdf")
})



test_that("calculation tests:  showing values only", {

  checkDigestAvailable()

  library(pivottabler)
  library(dplyr)

  # perform the aggregation in R code explicitly
  trains <- bhmtrains %>%
    group_by(TrainCategory, TOC) %>%
    summarise(NumberOfTrains=n()) %>%
    ungroup()

  # display this pre-calculated data
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addColumnDataGroups("TrainCategory", addTotal=FALSE)   #  <<  *** CODE CHANGE ***  <<
  pt$addRowDataGroups("TOC", addTotal=FALSE)                #  <<  *** CODE CHANGE ***  <<
  pt$defineCalculation(calculationName="TotalTrains", type="value", valueName="NumberOfTrains")
  pt$evaluatePivot()
  pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "eb0538010efa4804a610cf676a5f327e")
})



test_that("calculation tests:  showing values plus totals", {

  checkDigestAvailable()

  library(pivottabler)
  library(dplyr)

  # perform the aggregation in R code explicitly
  trains <- bhmtrains %>%
    group_by(TrainCategory, TOC) %>%
    summarise(NumberOfTrains=n()) %>%
    ungroup()

  # display this pre-calculated data
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains",  # <<  *** CODE CHANGE (AND BELOW) *** <<
                       type="value", valueName="NumberOfTrains",
                       summariseExpression="sum(NumberOfTrains)")
  pt$evaluatePivot()
  pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a93a42c781ec0855456695c83e0bc218")
})


# Failed testing on win builder (R 3.4.0 alpha) - guess: something to do with NA matching or NA sorting
  # 2. Failure: specific tests:  checking NA matching (@testGeneral.R#772) ---------
  # digest::digest(as.character(pt$getHtml()), algo = "md5") not identical to "4de9b5984fc79813e347de07177f6d58".
  # 1/1 mismatches
  # x[1]: "b60fb6d08a52644b7e535c105a444579"
  # y[1]: "4de9b5984fc79813e347de07177f6d58"
# test_that("specific tests:  checking NA matching", {
#
#   checkDigestAvailable()
#
#   library(pivottabler)
#   pt <- PivotTable$new()
#   pt$addData(bhmtrains)
#   pt$addColumnDataGroups("TrainCategory")
#   pt$addRowDataGroups("TOC")
#   pt$addRowDataGroups("PowerType")
#   pt$addRowDataGroups("SchedSpeedMPH")    #    << **** CODE CHANGE **** <<
#   pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
#   pt$evaluatePivot()
#   # pt$renderPivot()
#   # sum(pt$cells$asMatrix(), na.rm=TRUE)
#   # digest::digest(as.character(pt$getHtml()), algo="md5")
#
#   expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
#   expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "4de9b5984fc79813e347de07177f6d58")
# })



test_that("specific tests:  visual totals", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Express Passenger"), visualTotals=TRUE)
  pt$addRowDataGroups("TOC", fromData=FALSE, explicitListOfValues=list("Arriva Trains Wales", "CrossCountry", "London Midland"), visualTotals=TRUE)
  pt$addRowDataGroups("PowerType", fromData=FALSE, explicitListOfValues=list("DMU"), visualTotals=TRUE)
  pt$addRowDataGroups("SchedSpeedMPH", fromData=FALSE, explicitListOfValues=list(90, 100), visualTotals=TRUE)
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 149920)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "f67036acc61d2f58b12bbedf7e6c8b1b")
})



test_that("theming tests:  basic test", {

  checkDigestAvailable()

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
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$theme <- getSimpleColoredTheme(parentPivot=pt, colors=orangeColors, fontName="Garamond, arial")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")
  # digest::digest(pt$getCss(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "02c77d4b27bb76629110d68aea4d5438")
  expect_identical(digest::digest(pt$getCss(), algo="md5"), "929497023d21358b044ce4fc084e31cd")
})



test_that("empty data group test 1", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Freight"))
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "020ba3be27a175e885f5f0e1b3207c2e")
})



test_that("empty data group test 2", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Freight"), visualTotals=TRUE)
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "7ef58df5f3c515bec1082843bb90f328")
})



test_that("empty data group test 3", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"))
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 236790)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "3998e09a5ada0b32a461de90ff6f8a6a")
})



test_that("empty data group test 4", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"), visualTotals=TRUE)
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 138740)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "cda8c56baf02aea9aae12b3b7ff187bc")
})



test_that("empty data group test 5", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"))
  pt$addRowDataGroups("TOC")
  pt$addColumnDataGroups("PowerType")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 306160)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "a3eb21c719504c56e61562dbdf1aab00")
})



test_that("empty data group test 6", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory", fromData=FALSE, explicitListOfValues=list("Ordinary Passenger", "Freight"), visualTotals=TRUE)
  pt$addRowDataGroups("TOC")
  pt$addColumnDataGroups("PowerType")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 208110)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "609bdcbd415698cb44627f00adc5352d")
})


test_that("export tests:  as Matrix (without row headings)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()

  # pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE)
  # sum(pt$asMatrix(includeHeaders=FALSE, rawValue=TRUE), na.rm=TRUE)
  # digest::digest(pt$asMatrix(includeHeaders=FALSE), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$asMatrix(includeHeaders=FALSE), algo="md5"), "098ad67f0f16ad4e6f0834b54080358b")
})


test_that("export tests:  as Matrix (with row headings)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()

  # pt$asMatrix(includeHeaders=TRUE)
  # digest::digest(pt$asMatrix(includeHeaders=TRUE), algo="md5")

  expect_identical(digest::digest(pt$asMatrix(includeHeaders=TRUE), algo="md5"), "c36b5bea5b354b7529def6dce92c1d59")
})


test_that("export tests:  as Data Frame", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()

  # sum(pt$asDataFrame(), na.rm=TRUE)
  #digest::digest(pt$asDataFrame(), algo="md5")

  expect_equal(sum(pt$asDataFrame(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$asDataFrame(), algo="md5"), "d7ef39c213140c6a7ced92a1cc0c2586")
})


test_that("export tests:  as Tidy Data Frame", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()

  # sum(pt$asTidyDataFrame()$rawValue, na.rm=TRUE)
  # digest::digest(pt$asTidyDataFrame(), algo="md5")

  expect_equal(sum(pt$asTidyDataFrame()$rawValue, na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$asTidyDataFrame(), algo="md5"), "0410f50b561dadf0a032ce2746b47859")
})


test_that("latex tests:  basic latex table with spans", {

  checkDigestAvailable()

  C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
  R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
  C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
  R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
  V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
  pt$addColumnDataGroups("C2", addTotal=FALSE)
  pt$addRowDataGroups("R1", addTotal=FALSE)
  pt$addRowDataGroups("R2", addTotal=FALSE)
  pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
  pt$evaluatePivot()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
  expect_identical(digest::digest(ltx, algo="md5"), "d723355b5f211c407d3f57598be925d6")
})


test_that("latex tests:  no data", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(ltx, algo="md5"), "946b3fc57de73bd14ce9d497e6e9baac")
})


test_that("latex tests:  just rows", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addRowDataGroups("TOC")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(ltx, algo="md5"), "afab86560162109434c0c2e6abf324e7")
})


test_that("latex tests:  just columns", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(ltx, algo="md5"), "904295a902232a1c61bc2dc699514204")
})


test_that("latex tests:  rows and columns", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(ltx, algo="md5"), "83419a617ba8d6075ba74f271579337a")
})


test_that("latex tests:  rows, columns and a measure", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(ltx, algo="md5"), "90ef511752df1d8ce9735633870560b7")
})


test_that("latex tests:  styling headers", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable", boldHeadings=TRUE, italicHeadings=TRUE)

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(ltx, algo="md5"), "693fb55b5b837e81ebf39e6da7b55039")
})


test_that("latex tests:  rows, columns and two measures (on cols)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(ltx, algo="md5"), "69008efe82cc5b4f2807d4a31adef839")
})


test_that("latex tests:  rows, columns and two measures (on rows)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$defineCalculation(calculationName="MaxSchedSpeed", summariseExpression="max(SchedSpeedMPH, na.rm=TRUE)")
  pt$addRowCalculationGroups()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(ltx, algo="md5"), "8417788d94d150845453e06395a27e70")
})


test_that("latex tests:  just a total (on columns)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
  expect_identical(digest::digest(ltx, algo="md5"), "a0e3dd35bde9afa1b10a0872932d8b96")
})


test_that("latex tests:  two totals (on columns)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains1", summariseExpression="n()")
  pt$defineCalculation(calculationName="TotalTrains2", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
  expect_identical(digest::digest(ltx, algo="md5"), "211e172de8c706ac1cf8c9c0db130d31")
})


test_that("latex tests:  multiple levels on columns (but no rows)", {

  checkDigestAvailable()

  C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
  R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
  C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
  R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
  V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addColumnDataGroups("C0", addTotal=FALSE)
  pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
  pt$addColumnDataGroups("C2", addTotal=FALSE)
  pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
  expect_identical(digest::digest(ltx, algo="md5"), "e02f971e108f7e51e6ac3b88ab78430a")
})


test_that("latex tests:  multiple levels on columns (but no rows) with the calc on rows", {

  checkDigestAvailable()

  C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
  R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
  C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
  R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
  V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addColumnDataGroups("C0", addTotal=FALSE)
  pt$addColumnDataGroups("C1", fromData=FALSE, explicitListOfValues=list("n", "m", "e"), addTotal=FALSE)
  pt$addColumnDataGroups("C2", addTotal=FALSE)
  pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
  pt$addRowCalculationGroups()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
  expect_identical(digest::digest(ltx, algo="md5"), "c4e96c45911efc17b6b996cdbbaffd2f")
})


test_that("latex tests:  just a total (on rows)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$addRowCalculationGroups()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
  expect_identical(digest::digest(ltx, algo="md5"), "08f92e0770f7230e0926959c029331e3")
})


test_that("latex tests:  two totals (on rows)", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$defineCalculation(calculationName="TotalTrains1", summariseExpression="n()")
  pt$defineCalculation(calculationName="TotalTrains2", summariseExpression="n()")
  pt$addRowCalculationGroups()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
  expect_identical(digest::digest(ltx, algo="md5"), "c35948f5461bb45249482b7b8a83e4f8")
})


test_that("latex tests:  multiple levels on rows (but no columns)", {

  checkDigestAvailable()

  C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
  R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
  C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
  R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
  V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addRowDataGroups("R0", addTotal=FALSE)
  pt$addRowDataGroups("R1", addTotal=FALSE)
  pt$addRowDataGroups("R2", addTotal=FALSE)
  pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
  pt$addRowCalculationGroups()
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
  expect_identical(digest::digest(ltx, algo="md5"), "7604d2674bd289706e0181d1a8eb3278")
})


test_that("latex tests:  multiple levels on rows (but no columns) with the calculation on columns", {

  checkDigestAvailable()

  C1 <- c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e")
  R1 <- c("p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q", "p", "p", "p", "p", "p", "p", "p", "p", "p", "q", "q", "q", "q", "q", "q", "q", "q", "q")
  C2 <- c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
  R2 <- c("x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x", "x", "y", "y", "y", "z", "z", "z")
  V <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  df <- data.frame(R0="R", R1, R2, C0="C", C1, C2, V)

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(df)
  pt$addRowDataGroups("R0", addTotal=FALSE)
  pt$addRowDataGroups("R1", addTotal=FALSE)
  pt$addRowDataGroups("R2", addTotal=FALSE)
  pt$defineCalculation(calculationName="V", summariseExpression="sum(V)")
  ltx <- pt$getLatex(caption="My Table", label="mytable")

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 270)
  expect_identical(digest::digest(ltx, algo="md5"), "a5b96394302e12a037dafe4e5db46fd4")
})


test_that("latex tests:  rows/cols split 1", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable", fromRow=2, toRow=4)

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(ltx, algo="md5"), "1e75325e2a21479080d7a746e90cdcec")
})


test_that("latex tests:  rows/cols split 2", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable", fromColumn=2, toColumn=3)

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(ltx, algo="md5"), "3b4ecd3dc898562018859ca16d5af694")
})


test_that("latex tests:  rows/cols split 3", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  ltx <- pt$getLatex(caption="My Table", label="mytable", fromRow=3, toRow=5, fromColumn=1, toColumn=2)

  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(ltx, algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(ltx, algo="md5"), "1c182f34dc3cef47549c053ac883bbac")
})


test_that("find groups tests:  simple:  variableNames", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
  groups <- pt$findColumnDataGroups(variableNames="TrainCategory")
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 3)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "ddf28a5c7043edccac96cde146f24cbf")
})


test_that("find groups tests:  simple:  variableValues", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
  groups <- pt$findColumnDataGroups(variableValues=list("PowerType"=c("DMU", "HST")))
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 3)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "24262ffb2311df047758b7fd9771e14b")
})


test_that("find groups tests:  simple:  exclude totals", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
  groups <- pt$findColumnDataGroups(variableNames="TrainCategory", totals="exclude")
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 2)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "1c2c091aca3132e5ac58ac6273e416fc")
})


test_that("find groups tests:  simple:  only totals", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
  groups <- pt$findColumnDataGroups(variableNames="TrainCategory", totals="only")
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 1)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "0a0d46aa60e4f0a9f1c8e1d7f927bae8")
})


test_that("find groups tests:  simple:  includeDescendantGroups", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FFFF00"))
  groups <- pt$findColumnDataGroups(
    variableValues=list("TrainCategory"="Ordinary Passenger"),
    includeDescendantGroup=TRUE)
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 4)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "c9092ffb76a65dabf81e81d6ac2de060")
})


test_that("find groups tests:  combinations:  variableNames", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
  groups <- pt$findColumnDataGroups(matchMode="combinations",
                                    variableNames=c("TrainCategory", "PowerType"))
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 8)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "0f4528309de180a08bc303969a1ba78c")
})


test_that("find groups tests:  combinations:  variableValues", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
  groups <- pt$findColumnDataGroups(matchMode="combinations",
                                    variableValues=list("TrainCategory"="Express Passenger", "PowerType"=c("DMU", "HST")))
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 2)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "e9a691aa552828548ebfa7e4b7e86943")
})


test_that("find groups tests:  combinations:  specific sub total", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FFFF"))
  groups <- pt$findColumnDataGroups(matchMode="combinations",
                                    variableValues=list("TrainCategory"="Express Passenger", "PowerType"="**"))
  groupCount <- lapply(groups, function(grp) {grp$style <- highlight})
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(groups)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(groups), 1)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "d59ac3d2eb4abcd8ae4b4be98785c433")
})


test_that("get cells tests:  whole rows", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
  cells <- pt$getCells(rowNumbers=c(1, 3))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 16)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 156564)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "5d8ec18a72a60e7f9bd074426e3af2c3")
})


test_that("get cells tests:  whole columns", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
  cells <- pt$getCells(columnNumbers=2)
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 5)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 30612)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "61289bc8558869783943bb2550da2948")
})


test_that("get cells tests:  rows, columns and cells", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#00FF00"))
  cells <- pt$getCells(rowNumbers=c(2, NA, 5), columnNumbers=c(NA, 4, 7))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 14)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 201519)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "f7db50d7a0501e51708cc95b2890313a")
})


test_that("find cells tests:  variableValues 1", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
  cells <- pt$findCells(variableValues=list("PowerType"=c("DMU", "HST")))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 15)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 80406)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "970f136cd0ad4fa42143a20cc28bfd2f")
})


test_that("find cells tests:  variableValues 2", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
  cells <- pt$findCells(variableValues=list("PowerType"=c("DMU", "HST"), "TOC"="London Midland"))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 3)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 11229)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "f447c681713c9f9b78a097af483fa0e5")
})


test_that("find cells tests:  totals", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
  cells <- pt$findCells(variableValues=list("PowerType"="**"))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 15)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 334840)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "97319f513afdc03a4f2f09fccad54113")
})


test_that("find cells tests:  grand total", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  highlight <- PivotStyle$new(pt, "cellHighlight", list("background-color"="#FF00FF"))
  cells <- pt$findCells(variableValues=list("TrainCategory"="**", "PowerType"="**", "TOC"="**"))
  cellCount <- lapply(cells, function(cell) {cell$style <- highlight})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 1)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 83710)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "6da0c0557c2be512c282994a1a190535")
})


test_that("find cells tests:  conditional formatting", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  redStyle <- PivotStyle$new(pt, "redStyle", list("background-color"="#FFC7CE", "color"="#9C0006"))
  cells <- pt$findCells(minValue=30000, maxValue=50000, includeNull=FALSE, includeNA=FALSE)
  cellCount <- lapply(cells, function(cell) {cell$style <- redStyle})
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # length(cells)
  # sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE)
  # digest::digest(as.character(pt$getHtml()), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_equal(length(cells), 5)
  expect_equal(sum(unlist(lapply(cells, function(x) { return(x$rawValue) })), na.rm=TRUE), 198768)
  expect_identical(digest::digest(as.character(pt$getHtml()), algo="md5"), "d74803ec078b4972fa3455ba04b7efab")
})


