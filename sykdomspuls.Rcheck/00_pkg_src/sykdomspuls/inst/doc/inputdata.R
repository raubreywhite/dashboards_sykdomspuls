## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(sykdomspuls)

## ------------------------------------------------------------------------
print(as.character(sykdomspuls::VARS$REQ_DATA_RAW_STRUCTURAL))

## ------------------------------------------------------------------------
print(as.character(sykdomspuls::CONFIG$SYNDROMES))
print(as.character(sykdomspuls::VARS$REQ_DATA_RAW_OTHER))

## ------------------------------------------------------------------------
print(as.character(sykdomspuls::VARS$REQ_DATA_RAW_ALL))

## ------------------------------------------------------------------------
d <- GenFakeDataRaw()
print(d)

## ------------------------------------------------------------------------
print(ValidateDataRaw(d))

