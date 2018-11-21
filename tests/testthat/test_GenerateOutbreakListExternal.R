context("GenerateOutbreakListExternal")

test_that("Basic example", {
  library(data.table)

  df <- GenFakeResultsFull(granularity = "weekly", syndrome="gastro",xmunicipEnd = "municip0301")
  dk <- GenFakeResultsFull(granularity = "weekly", syndrome="gastro",xmunicipEnd = "municip0301")

  df[,status:="Normal"]
  dk[,status:="Normal"]

  dk[.N, status := "High"]

  alerts <- data.table(email = "richardaubrey.white@fhi.no", location = "municip0301")

  res <- GenerateOutbreakListExternal(
    df = df,
    dk = dk,
    saveFiles = NULL,
    alerts = alerts
  )

  testthat::expect_equal(nrow(res), 1)
})
