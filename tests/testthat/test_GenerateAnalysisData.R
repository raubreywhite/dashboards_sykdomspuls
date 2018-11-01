context("GenerateAnalysisResults")

test_that("Basic weekly", {
  library(data.table)

  d <- GenerateAnalysisResults(granularity = "weekly")
  expect_equal(TRUE, ValidateAnalysisResults(d, granularity = "weekly"))
})

test_that("Basic weekly with municip", {
  library(data.table)

  d <- GenerateAnalysisResults(granularity = "weekly", loc = "municip0301")
  expect_equal(TRUE, ValidateAnalysisResults(d, granularity = "weekly"))
})


test_that("Basic daily", {
  library(data.table)

  d <- GenerateAnalysisResults(granularity = "daily")
  expect_equal(TRUE, ValidateAnalysisResults(d, granularity = "daily"))
})
