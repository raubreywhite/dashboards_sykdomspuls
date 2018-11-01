context("GenerateOutbreakListExternal")

test_that("Basic example", {
  library(data.table)

  df <- GenerateAnalysisResults(granularity = "weekly", loc="Norge")
  dk <- GenerateAnalysisResults(granularity = "weekly", loc="municip0301")

  df[.N,status:="High"]
  dk[.N,status:="High"]

  alerts <- data.table(email="richardaubrey.white@fhi.no",location="Norge")

  res <- GenerateOutbreakListExternal(
    df = df,
    dk = dk,
    saveFiles = NULL,
    alerts = alerts)

  expect_equal(nrow(res),1)
})

test_that("Basic example 2", {
  library(data.table)

  dk <- df <- vector("list")
  for(con in CONFIG$SYNDROMES_ALERT_EXTERNAL){
    df[[con]] <- GenerateAnalysisResults(granularity = "weekly", loc="Norge", type = con)
    dk[[con]] <- GenerateAnalysisResults(granularity = "weekly", loc="municip0301", type = con)

    df[[con]][.N,status:="High"]
    dk[[con]][.N,status:="High"]
  }
  df <- rbindlist(df)
  dk <- rbindlist(dk)

  alerts <- data.table(email="richardaubrey.white@fhi.no",location="Norge")

  res <- GenerateOutbreakListExternal(
    df = df,
    dk = dk,
    saveFiles = NULL,
    alerts = alerts)

  expect_equal(nrow(res),length(CONFIG$SYNDROMES_ALERT_EXTERNAL))
})
