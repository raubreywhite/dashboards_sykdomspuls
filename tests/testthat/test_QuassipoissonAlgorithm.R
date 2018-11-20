context("QuasipoissonAlgorithm")
test_that("Sandefjord significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)

  d <- GenerateFakeDataAnalysis()
  res <- sykdomspuls::QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = T
  )
  significantByThreshold <- res[n > threshold2]
  significantByConfidenceIntervals <- res[cumL1 > 0]
  testthat::expect_equal(significantByThreshold, significantByConfidenceIntervals)
})

test_that("Sandefjord significantByThreshold vs significantByZScore", {
  library(data.table)

  d <- GenerateFakeDataAnalysis()
  res <- sykdomspuls::QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = F
  )
  significantByThreshold <- res[n > threshold2]
  significantByConfidenceIntervals <- res[cumL1 > 0]
  testthat::expect_equal(significantByThreshold, significantByConfidenceIntervals)

})


test_that("Sandefjord weekly - restrict datasetTrain vs not", {
  library(data.table)

  d <- GenerateFakeDataAnalysis()
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

  testthat::expect_false(isTRUE(all.equal(resAll, resRestricted)))
})

test_that("Sandefjord daily - restrict datasetTrain vs not", {
  library(data.table)

  d <- GenerateFakeDataAnalysis()
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

  testthat::expect_false(isTRUE(all.equal(resAll, resRestricted)))
})

test_that("Sandefjord daily - restrict datasetPredict vs not", {
  library(data.table)

  d <- GenerateFakeDataAnalysis()
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

  testthat::expect_equal(resAll, resRestricted)
})

