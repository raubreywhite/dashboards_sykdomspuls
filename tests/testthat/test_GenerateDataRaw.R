context("GenFakeDataRaw")

test_that("Basic", {
  library(data.table)

  expect_equal(TRUE, ValidateDataRaw(GenFakeDataRaw()))
})
