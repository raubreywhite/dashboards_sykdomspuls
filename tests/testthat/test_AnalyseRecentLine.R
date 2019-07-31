context("AnalyseRecentLine")

test_that("significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)

  d <- GenFakeDataAnalysis()
  res <- sykdomspuls::QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = T
  )

  significantByThreshold <- res[n > threshold2]
  significantByConfidenceIntervals <- res[cumL1 > 0]
  testthat::expect_equal(significantByThreshold, significantByConfidenceIntervals)
})
