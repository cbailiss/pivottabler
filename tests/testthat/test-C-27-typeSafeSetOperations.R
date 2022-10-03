library(testthat)

context("INTERSECTION TESTS")

test_that("typeSafeIntersect numeric", {

  a <- c(1, 2, 3, 4, 5)
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, c(2, 4))
})

test_that("typeSafeIntersect character", {#

  a <- c("a", "b", "c", "d", "e")
  b <- c("b", "d", "f", "s", "t")
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, c("b", "d"))
})

test_that("typeSafeIntersect date", {

  a <- seq(as.Date("2017-01-01"), as.Date("2017-01-07"), by="days")
  b <- c(as.Date("2017-01-02"), as.Date("2017-01-06"), as.Date("2017-01-12"))
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, c(as.Date("2017-01-02"), as.Date("2017-01-06")))
})

test_that("typeSafeIntersect POSIXct", {

  a <- c(as.POSIXct("2015-10-19 10:00"), as.POSIXct("2015-10-19 10:30"),
         as.POSIXct("2015-10-19 11:00"), as.POSIXct("2015-10-19 11:30"))
  b <- c(as.POSIXct("2015-10-19 10:00"), as.POSIXct("2015-10-20 10:00"),
         as.POSIXct("2015-10-19 11:00"), as.POSIXct("2015-10-20 11:00"))
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, c(as.POSIXct("2015-10-19 10:00"),
                       as.POSIXct("2015-10-19 11:00")))
})

test_that("typeSafeIntersect POSIXct", {

  a <- c(as.POSIXlt("2015-10-19 10:00"), as.POSIXlt("2015-10-19 10:30"),
         as.POSIXlt("2015-10-19 11:00"), as.POSIXlt("2015-10-19 11:30"))
  b <- c(as.POSIXlt("2015-10-19 10:00"), as.POSIXlt("2015-10-20 10:00"),
         as.POSIXlt("2015-10-19 11:00"), as.POSIXlt("2015-10-20 11:00"))
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, c(as.POSIXlt("2015-10-19 10:00"),
                       as.POSIXlt("2015-10-19 11:00")))
})

test_that("typeSafeIntersect logical", {

  a <- c(TRUE, FALSE)
  b <- FALSE
  z <- typeSafeIntersect(a, b)

  expect_setequal(z, FALSE)
})

test_that("typeSafeIntersect empty 1", {

  a <- numeric(0)
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeIntersect(a, b)

  expect_equal(z, numeric(0))
})

test_that("typeSafeIntersect empty 2", {

  a <- c(2, 4, 6, 8, 9)
  b <- numeric(0)
  z <- typeSafeIntersect(a, b)

  expect_equal(z, numeric(0))
})

test_that("typeSafeIntersect empty 3", {

  a <- numeric(0)
  b <- numeric(0)
  z <- typeSafeIntersect(a, b)

  expect_equal(z, numeric(0))
})

test_that("typeSafeIntersect null 1", {

  a <- NULL
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeIntersect(a, b)

  expect_equal(z, NULL)
})

test_that("typeSafeIntersect null 2", {

  a <- c(2, 4, 6, 8, 9)
  b <- NULL
  z <- typeSafeIntersect(a, b)

  expect_equal(z, NULL)
})

test_that("typeSafeIntersect null 3", {

  a <- NULL
  b <- NULL
  z <- typeSafeIntersect(a, b)

  expect_equal(z, NULL)
})

context("UNION TESTS")

test_that("typeSafeUnion numeric", {

  a <- c(1, 2, 3, 4, 5)
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c(1, 2, 3, 4, 5, 6, 8, 9))
})

test_that("typeSafeUnion character", {

  a <- c("a", "b", "c", "d", "e")
  b <- c("b", "d", "f", "s", "t")
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c("a", "b", "c", "d", "e", "f", "s", "t"))
})

test_that("typeSafeUnion date", {

  a <- seq(as.Date("2017-01-01"), as.Date("2017-01-03"), by="days")
  b <- c(as.Date("2017-01-02"), as.Date("2017-01-06"), as.Date("2017-01-12"))
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c(as.Date("2017-01-01"), as.Date("2017-01-02"),
                       as.Date("2017-01-03"), as.Date("2017-01-06"),
                       as.Date("2017-01-12")))
})

test_that("typeSafeUnion POSIXct", {

  a <- c(as.POSIXct("2015-10-19 10:00"), as.POSIXct("2015-10-19 10:30"),
         as.POSIXct("2015-10-19 11:00"), as.POSIXct("2015-10-19 11:30"))
  b <- c(as.POSIXct("2015-10-19 10:00"), as.POSIXct("2015-10-20 10:00"),
         as.POSIXct("2015-10-19 11:00"), as.POSIXct("2015-10-20 11:00"))
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c(as.POSIXct("2015-10-19 10:00"),
                       as.POSIXct("2015-10-19 10:30"),
                       as.POSIXct("2015-10-19 11:00"),
                       as.POSIXct("2015-10-19 11:30"),
                       as.POSIXct("2015-10-20 10:00"),
                       as.POSIXct("2015-10-20 11:00")))
})

test_that("typeSafeUnion POSIXct", {

  a <- c(as.POSIXlt("2015-10-19 10:00"), as.POSIXlt("2015-10-19 10:30"),
         as.POSIXlt("2015-10-19 11:00"), as.POSIXlt("2015-10-19 11:30"))
  b <- c(as.POSIXlt("2015-10-19 10:00"), as.POSIXlt("2015-10-20 10:00"),
         as.POSIXlt("2015-10-19 11:00"), as.POSIXlt("2015-10-20 11:00"))
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c(as.POSIXlt("2015-10-19 10:00"),
                       as.POSIXlt("2015-10-19 10:30"),
                       as.POSIXlt("2015-10-19 11:00"),
                       as.POSIXlt("2015-10-19 11:30"),
                       as.POSIXlt("2015-10-20 10:00"),
                       as.POSIXlt("2015-10-20 11:00")))
})

test_that("typeSafeUnion logical", {

  a <- c(TRUE)
  b <- c(FALSE)
  z <- typeSafeUnion(a, b)

  expect_setequal(z, c(TRUE, FALSE))
})

test_that("typeSafeUnion empty 1", {

  a <- numeric(0)
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeUnion(a, b)

  expect_equal(z, c(2, 4, 6, 8, 9))
})

test_that("typeSafeUnion empty 2", {

  a <- c(2, 4, 6, 8, 9)
  b <- numeric(0)
  z <- typeSafeUnion(a, b)

  expect_equal(z, c(2, 4, 6, 8, 9))
})

test_that("typeSafeUnion empty 3", {

  a <- numeric(0)
  b <- numeric(0)
  z <- typeSafeUnion(a, b)

  expect_equal(z, numeric(0))
})

test_that("typeSafeUnion null 1", {

  a <- NULL
  b <- c(2, 4, 6, 8, 9)
  z <- typeSafeUnion(a, b)

  expect_equal(z, c(2, 4, 6, 8, 9))
})

test_that("typeSafeUnion null 2", {

  a <- c(2, 4, 6, 8, 9)
  b <- NULL
  z <- typeSafeUnion(a, b)

  expect_equal(z, c(2, 4, 6, 8, 9))
})

test_that("typeSafeUnion null 3", {

  a <- c(2, 4, 6, 8, 9)
  b <- NULL
  z <- NULL

  expect_equal(z, NULL)
})

context("UNLIST TESTS")

test_that("typeSafeUnlist empty", {

  a <- numeric(0)
  z <- typeSafeUnlist(a)

  expect_equal(z, numeric(0))
})

test_that("typeSafeUnlist null", {

  a <- NULL
  z <- typeSafeUnlist(a)

  expect_equal(z, NULL)
})

test_that("typeSafeUnlist logical", {

  a <- list(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
  z <- typeSafeUnlist(a)

  expect_equal(z, c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("typeSafeUnlist numeric", {

  a <- list(1, 2, 3, 4, 5.5, 6)
  z <- typeSafeUnlist(a)

  expect_equal(z, c(1, 2, 3, 4, 5.5, 6))
})

test_that("typeSafeUnlist character", {

  a <- list("a", "b", "b", "c")
  z <- typeSafeUnlist(a)

  expect_equal(z, c("a", "b", "b", "c"))
})

test_that("typeSafeUnlist Date", {

  a <- list(as.Date("2010-01-01"), as.Date("2010-01-03"), as.Date("2010-01-12"))
  z <- typeSafeUnlist(a)

  expect_equal(z, c(as.Date("2010-01-01"), as.Date("2010-01-03"),
                    as.Date("2010-01-12")))
})

test_that("typeSafeUnlist Date", {

  a <- list(as.POSIXct("2010-01-01 08:53:22"), as.POSIXct("2010-01-01 12:34:56"),
            as.POSIXct("2010-01-12 01:23:45"))
  z <- typeSafeUnlist(a)

  expect_equal(z, c(as.POSIXct("2010-01-01 08:53:22"),
                    as.POSIXct("2010-01-01 12:34:56"),
                    as.POSIXct("2010-01-12 01:23:45")))
})

test_that("typeSafeUnlist Date", {

  a <- list(as.POSIXlt("2010-01-01 08:53:22"), as.POSIXlt("2010-01-01 12:34:56"),
            as.POSIXlt("2010-01-12 01:23:45"))
  z <- typeSafeUnlist(a)

  expect_equal(z, c(as.POSIXlt("2010-01-01 08:53:22"),
                    as.POSIXlt("2010-01-01 12:34:56"),
                    as.POSIXlt("2010-01-12 01:23:45")))
})
