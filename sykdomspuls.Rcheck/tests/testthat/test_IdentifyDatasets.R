context("IdentifyDatasets")
library(data.table)

test_that("One of each", {
  res <- IdentifyDatasets(
    raw="partially_formatted_2017_05_09.txt",
    clean="done_2017_05_09.txt")

  expect_equal(res,data.table("id"="2017_05_09",
                              "raw"="partially_formatted_2017_05_09.txt",
                              "isRaw"=TRUE,
                              "clean"="done_2017_05_09.txt",
                              "isClean"=TRUE,
                              key="id"))
})

test_that("No clean", {
  res <- IdentifyDatasets(
    raw="partially_formatted_2017_05_09.txt",
    clean=character(0))

  expect_equal(res,data.table("id"="2017_05_09",
                              "raw"="partially_formatted_2017_05_09.txt",
                              "isRaw"=TRUE,
                              "clean"=as.character(NA),
                              "isClean"=as.logical(NA),
                              key="id"))
})

