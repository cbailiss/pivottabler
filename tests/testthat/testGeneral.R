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
  pt$addLeafColumnDataGroup("TrainCategory")
  pt$addLeafRowDataGroup("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  pt$cells$asMatrix()

  expect_equal(sum(pt$cells$asMatrix()), 338980)
})

test_that("bhmtrains basic pivot values", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addLeafColumnDataGroup("TrainCategory")
  pt$addLeafRowDataGroup("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  pt$cells$asMatrix()

  expect_identical(digest::digest(pt$cells$asMatrix(), algo="md5"), "8153e0f25d156f8762da2fb5cc25d6d3")
})

test_that("bhmtrains basic pivot html", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addLeafColumnDataGroup("TrainCategory")
  pt$addLeafRowDataGroup("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  pt$cells$asMatrix()

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "0056b2f67598d2f1c9bb0c401d14eee2")
})

