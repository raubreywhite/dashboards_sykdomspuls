context("AnalyseYearLine")

test_that("significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)

  d <- sykdomspuls::GenFakeDataAnalysis()
  res <- sykdomspuls::QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = F
  )

  significantByThreshold <- res[n > threshold2]
  significantByConfidenceIntervals <- res[cumL1 > 0]
  expect_equal(significantByThreshold, significantByConfidenceIntervals)
})
