context("QuasipoissonAlgorithm")
test_that("Sandefjord significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)
  if (interactive()) {
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "results", "formatted_sandefjord.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithInfluensa", "consult")
  res <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = F
  )

  significantByThreshold <- res[n > threshold2]
  significantByConfidenceIntervals <- res[cumL1 > 0]
  expect_equal(significantByThreshold, significantByConfidenceIntervals)
})

test_that("Sandefjord significantByThreshold vs significantByZScore", {
  library(data.table)
  if (interactive()) {
    BASE <- "/packages/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "results", "formatted_sandefjord.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithInfluensa", "consult")
  res <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = F
  )

  significantByThreshold <- res[n > threshold2]
  significantByZScore <- res[zscore > 2]
  expect_equal(significantByThreshold, significantByZScore)
})


test_that("Sandefjord weekly - restrict datasetTrain vs not", {
  library(data.table)
  if (interactive()) {
    BASE <- "/packages/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "results", "formatted_sandefjord.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithInfluensa", "consult")

  resAll <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d[date >= "2017-01-01"],
    isDaily = F
  )

  resRestricted <- QuasipoissonTrainPredictData(
    datasetTrain = d[date <= "2017-01-01"],
    datasetPredict = d[date >= "2017-01-01"],
    isDaily = F
  )

  expect_false(isTRUE(all.equal(resAll, resRestricted)))
})

test_that("Sandefjord daily - restrict datasetTrain vs not", {
  library(data.table)
  if (interactive()) {
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "results", "formatted_sandefjord.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithInfluensa", "consult")

  resAll <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d[date >= "2017-01-01"],
    isDaily = T
  )

  resRestricted <- QuasipoissonTrainPredictData(
    datasetTrain = d[date <= "2017-01-01"],
    datasetPredict = d[date >= "2017-01-01"],
    isDaily = T
  )

  expect_false(isTRUE(all.equal(resAll, resRestricted)))
})

test_that("Sandefjord daily - restrict datasetPredict vs not", {
  library(data.table)
  if (interactive()) {
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "results", "formatted_sandefjord.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithInfluensa", "consult")

  resAll <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = T
  )

  resRestricted <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d[date >= "2017-01-01"],
    isDaily = T
  )

  resAll <- resAll[date %in% resRestricted$date]

  expect_equal(resAll, resRestricted)
})


test_that("meraker has issues with a very large trend", {
  library(data.table)
  if (interactive()) {
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE, "data", "formatted_meraker.RDS"))
  setnames(d, "value", "n")
  setnames(d, "consultWithoutInfluensa", "consult")
  res2006_2010 <- QuasipoissonTrainPredictData(
    datasetTrain = d[date <= "2010-12-31"],
    datasetPredict = d[date <= "2007-01-01"],
    isDaily = F
  )
  res2013_2016 <- QuasipoissonTrainPredictData(
    datasetTrain = d[date >= "2013-01-01" & date <= "2016-12-31"],
    datasetPredict = d[date <= "2007-01-01"],
    isDaily = F
  )

  expect_lt(mean(res2006_2010$threshold2) * 1.25, mean(res2013_2016$threshold2))
})
