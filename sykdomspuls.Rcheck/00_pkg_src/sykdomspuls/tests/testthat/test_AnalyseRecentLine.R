context("AnalyseRecentLine")

test_that("Sandefjord significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)
  if(interactive()){
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"results","formatted_sandefjord.RDS"))
  #setnames(d,"influensa","value")
  setnames(d,"consultWithInfluensa","consult")
  res <- AnalyseRecentLine(d,v=1)

  significantByThreshold <- res[n>threshold2]
  significantByConfidenceIntervals <- res[cumL1>0]
  expect_equal(significantByThreshold,significantByConfidenceIntervals)
})
