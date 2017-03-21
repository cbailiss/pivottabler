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
  pt$cells$asMatrix()

  expect_equal(sum(pt$cells$asMatrix()), 334840)
})

test_that("bhmtrains basic pivot values", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  pt$cells$asMatrix()

  expect_identical(digest::digest(pt$cells$asMatrix(), algo="md5"), "e81b6ca05b770cbb304c36a44d7ce013")
})

test_that("bhmtrains basic pivot html", {

  checkDigestAvailable()

  pt <- PivotTable$new()
  pt$addData(bhmtrains)
  pt$addColumnDataGroups("TrainCategory")
  pt$addRowDataGroups("TOC")
  pt$defineCalculation(calculationName="TotalTrains", summariseExpression="n()")
  pt$evaluatePivot()
  pt$cells$asMatrix()

  expect_identical(digest::digest(pt$getHtml(), algo="md5"), "3109a0a30f29510724e957ad8e2dce93")
})

