context("CONFIG")

test_that("All CONFIG$SYNDROMES have names", {
  n <- CONFIG$SYNDROMES
  n <- n[n!=""]
  expect_equal(length(n),length(CONFIG$SYNDROMES))
})

test_that("All CONFIG$SYNDROMES have CONFIG$SYNDROMES_SHORT", {
  expect_equal(sum(!CONFIG$SYNDROMES %in% CONFIG$SYNDROMES_SHORT),0)
})

test_that("All CONFIG$SYNDROMES_SHORT have names", {
  n <- CONFIG$SYNDROMES_SHORT
  n <- n[n!=""]
  expect_equal(length(n),length(CONFIG$SYNDROMES_SHORT))
})
