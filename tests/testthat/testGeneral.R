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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "69abeaf38dc735daeddeae4d94d3ed59")
})


test_that("smoke tests:  basic layout tests:  empty pivot", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "cc73c556d32becd5b2658c8cfa03ed52")
})


test_that("basic layout tests:  empty pivot plus data", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "cc73c556d32becd5b2658c8cfa03ed52")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 83710)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "c50ee91df312ff77cfa7220aef5dcd97")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 83835)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "f519336add21aeeef3b764e228a53cfe")
})


test_that("basic layout tests:  rows only", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addRowDataGroups("TOC")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "4535ececc842d2771f7246e86d70f2fb")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "ffb1fbc40f2638a360e0a97d00fc42fa")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "db60e2cf41541eae3f3ee72611e1e211")
})


test_that("basic layout tests:  columns only", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TOC")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "f520ed3e8bb111f39ed33694a1cbfbdf")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "34eafe5ded056555d4ec42c0e678715a")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "f798002d4005ca4b572b85ea44df1f5f")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "dfaf7ca61e381f99ec0e05810b245b8b")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "69abeaf38dc735daeddeae4d94d3ed59")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "06780240f4afc3784525836829f65a66")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167420)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "41f1c652d393e2910aea62716c8244ea")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 167995)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "1531ba5f10e9f075b55cb18b037f3cfd")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "69abeaf38dc735daeddeae4d94d3ed59")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 336380)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "e82ee929b0e423df8b48adc926a264d8")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "39fd4b80e8be158ae9a888dcbbddc43e")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 500796)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "e4738a1ff93776a3b5011442234947fd")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "5520c3f4363267de0e416e2d0f54f509")
})


test_that("data groups tests:  formatting data groups", {

  checkDigestAvailable()

  library(dplyr)
  library(lubridate)
  trains <- mutate(bhmtrains,
     GbttDate=as.POSIXct(ifelse(is.na(GbttArrival), GbttDeparture, GbttArrival),
                         origin = "1970-01-01"),
     GbttMonth=make_date(year=year(GbttDate), month=month(GbttDate), day=1))

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(trains)
  pt$addColumnDataGroups("GbttMonth", dataFormat=list(format="%B %Y"))
  pt$addColumnDataGroups("PowerType")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "2bbed6d4f402fcb465d7b3569aa14e46")
})


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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "2c5eef1aca9347004d48e73b2010f63d")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 460)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "c68ba30efb84bb22036d5384dfa14a34")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "82516909a522ba233e179e98bcce4448")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 502260)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "484cab0595571b25af22912ea9ea69ec")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "9f62fc2410dab693c723dd256d763f15")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 168438.858522)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "cd501b5bea3fb0b596837e0a87987640")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 182432.916225)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "56a286b5d6aa564261accf8660ddd843")
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
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 83710)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "fcda3bab7f63063453591d141c274c62")
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
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "69abeaf38dc735daeddeae4d94d3ed59")
})



test_that("specific tests:  checking NA matching", {

  checkDigestAvailable()

  library(pivottabler)
  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$addRowDataGroups("PowerType")
  pt$addRowDataGroups("SchedSpeedMPH")    #    << **** CODE CHANGE **** <<
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix(), na.rm=TRUE)
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 669680)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "4de9b5984fc79813e347de07177f6d58")
})



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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 149920)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "fecc86f450c27978d80b6234bde45c33")
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
  # digest::digest(pt$getHtml(), algo="md5")
  # digest::digest(pt$getCss(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "01a7b4884d7eb49eb83afe7c9d1819d5")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 167420)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "bf15028c080c7cb7bef8b30b9b46049a")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 0)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "dd134181eb368210c4b3d958d8a7e166")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 236790)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "47f847ac274dd2fa51b856314c6387d5")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 138740)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "4f5ece1b5f35befc62b15391f09c94f2")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 306160)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "44983d5292c294cabe5004a1d3e18433")
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
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix(), na.rm=TRUE), 208110)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "46c99dd69354dde35faf7283548df2be")
})
