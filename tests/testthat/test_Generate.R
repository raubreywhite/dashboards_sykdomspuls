context("GenFakeDataRaw")

test_that("Basic Raw", {
  library(data.table)

  expect_equal(TRUE, ValidateDataRaw(GenFakeDataRaw()))
})

context("GenFakeDataClean")

test_that("Basic Clean", {
  library(data.table)

  expect_equal(TRUE, ValidateDataClean(GenFakeDataClean()))
})

context("GenerateFakeResultsFull")

test_that("Basic Results", {
  library(data.table)

  testthat::expect_equal(TRUE, ValidateResultsFull(GenFakeResultsFull()))
})
