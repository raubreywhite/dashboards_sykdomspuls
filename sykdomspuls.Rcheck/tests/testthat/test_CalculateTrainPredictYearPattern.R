context("CalculateTrainPredictYearPattern")

test_that("2006-2017", {
  res <- CalculateTrainPredictYearPattern(yearMin=2006, yearMax=2017, numPerYear1 = 1)

  expectedRes <- list()
  expectedRes[[1]] <- list(
    "yearTrainMin"=2006,
    "yearTrainMax"=2010,
    "yearPredictMin"=2006,
    "yearPredictMax"=2011
  )

  expectedRes[[2]] <- list(
    "yearTrainMin"=2007,
    "yearTrainMax"=2011,
    "yearPredictMin"=2012,
    "yearPredictMax"=2013
  )

  expectedRes[[3]] <- list(
    "yearTrainMin"=2009,
    "yearTrainMax"=2013,
    "yearPredictMin"=2014,
    "yearPredictMax"=2015
  )

  expectedRes[[4]] <- list(
    "yearTrainMin"=2011,
    "yearTrainMax"=2015,
    "yearPredictMin"=2016,
    "yearPredictMax"=2016
  )

  expectedRes[[5]] <- list(
    "yearTrainMin"=2012,
    "yearTrainMax"=2016,
    "yearPredictMin"=2017,
    "yearPredictMax"=2017
  )

  expect_equal(res,expectedRes)
})
