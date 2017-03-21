library(pivottabler)
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

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())

  expect_equal(sum(pt$cells$asMatrix()), 334840)
})


test_that("smoke tests:  bhmtrains basic pivot values", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$cells$asMatrix(), algo="md5")

  expect_identical(digest::digest(pt$cells$asMatrix(), algo="md5"), "e81b6ca05b770cbb304c36a44d7ce013")
})


test_that("smoke tests:  bhmtrains basic pivot html", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "3109a0a30f29510724e957ad8e2dce93")
})


test_that("smoke tests:  basic layout tests:  empty pivot", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "cc73c556d32becd5b2658c8cfa03ed52")
})


test_that("basic layout tests:  empty pivot plus data", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$evaluatePivot()
  # pt$renderPivot()
  # digest::digest(pt$getHtml(), algo="md5")

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "cc73c556d32becd5b2658c8cfa03ed52")
})


test_that("basic layout tests:  just a total", {

  checkDigestAvailable()

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

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "3109a0a30f29510724e957ad8e2dce93")
})


test_that("basic layout tests:  rows, columns and two calculations", {

  checkDigestAvailable()

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
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "b58e200be995448504d42a04ab7585b3")
})


test_that("basic layout tests:  columns plus total on row", {

  checkDigestAvailable()

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

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$addRowCalculationGroups()
  pt$evaluatePivot()
  # pt$renderPivot()
  # sum(pt$cells$asMatrix())
  # digest::digest(pt$getHtml(), algo="md5")

  expect_equal(sum(pt$cells$asMatrix()), 334840)
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "3109a0a30f29510724e957ad8e2dce93")
})


test_that("basic layout tests:  rows, columns and two calculations on rows", {

  checkDigestAvailable()

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
  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "472ca35d840705c54574786c577cb5fa")
})









