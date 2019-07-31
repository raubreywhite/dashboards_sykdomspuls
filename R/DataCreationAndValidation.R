#' Generate fake raw data
#' @param xmunicipEnd municipality
#' @import data.table
#' @export GenFakeDataRaw
GenFakeDataRaw <- function(xmunicipEnd = "municip5054") {
  municipEnd <- NULL
  municip <- NULL
  consult <- NULL
  syndromeOrConsult <- NULL
  age <- NULL

  m <- fhidata::norway_municip_merging
  skeleton <- unique(m[municip_code_current == xmunicipEnd & year <= lubridate::year(lubridate::today()), c("municip_code_original", "year")])
  setnames(skeleton,"municip_code_original","municip")

  data <- vector("list", length = nrow(skeleton))
  for (i in 1:length(data)) {
    minDate <- as.Date(sprintf("%s-01-01", skeleton$year[i]))
    maxDate <- as.Date(sprintf("%s-12-31", skeleton$year[i]))
    data[[i]] <- expand.grid(
      date = seq.Date(minDate, maxDate, 1),
      "age" = names(CONFIG$AGES),
      "Kontaktype" = c("Telefonkontakt", "Legekontakt"),
      "Praksis" = c("Fastlege", "Legevakt"),
      stringsAsFactors = F
    )
    setDT(data[[i]])
    data[[i]][, municip := skeleton$municip[i]]
    data[[i]][, municip := skeleton$municip[i]]

    data[[i]][, consult := rpois(.N, 50)]
    for (j in unique(CONFIG$SYNDROMES[syndromeOrConsult == "syndrome"]$syndrome)) {
      data[[i]][, (j) := rpois(.N, 5)]
      data[[i]][, consult := consult + get(j)]
    }
  }
  data <- rbindlist(data)
  setcolorder(data, VARS$REQ_DATA_RAW)
  data <- data[age != "Totalt"]

  return(data)
}

#' Validate raw data
#' @param d Dataset to validate
#' @export ValidateDataRaw
ValidateDataRaw <- function(d) {
  # names(d) must contain all required variables
  n <- VARS$REQ_DATA_RAW[!VARS$REQ_DATA_RAW %in% names(d)]
  if (length(n) > 0) {
    for (i in n) {
      fhi::DashboardMsg(sprintf("%s not in names(d)", i))
    }
    return(FALSE)
  }

  # there must not be any extra variables in names(d)
  n <- names(d)[!names(d) %in% VARS$REQ_DATA_RAW]
  if (sum(!names(d) %in% VARS$REQ_DATA_RAW) > 0) {
    for (i in n) {
      fhi::DashboardMsg(sprintf("%s not in VARS$REQ_DATA_RAW", i))
    }
    fhi::DashboardMsg("Variables in names(d) not in VARS$REQ_DATA_RAW", type = "warn")
  }

  return(TRUE)
}

#' Generate fake clean data
#' @param syndrome Syndrome to validate
#' @param xmunicipEnd municipality
#' @export GenFakeDataClean
GenFakeDataClean <- function(syndrome = "influensa", xmunicipEnd = "municip5054") {
  granularityGeo <- NULL

  d <- GenFakeDataRaw(xmunicipEnd = xmunicipEnd)
  d <- CleanData(d, syndrome = syndrome, removeMunicipsWithoutConsults = T)
  d <- d[granularity_geo == "municip"]

  return(d)
}

#' Validate clean data
#' @param d Dataset to validate
#' @export ValidateDataClean
ValidateDataClean <- function(d) {
  # names(d) must contain all required variables
  if (sum(!VARS$REQ_DATA_CLEAN %in% names(d)) > 0) {
    return(FALSE)
  }

  # there must not be any extra variables in names(d)
  if (sum(!names(d) %in% VARS$REQ_DATA_CLEAN) > 0) {
    return(FALSE)
  }

  return(TRUE)
}


#' Generate fake data for analysis
#' @param syndrome Syndrome
#' @param xage Age
#' @param xmunicipEnd municipality
#' @import data.table
#' @export GenFakeDataAnalysis
GenFakeDataAnalysis <- function(syndrome = "influensa", xage = "Totalt", xmunicipEnd = "municip5054") {
  age <- NULL

  d <- GenFakeDataClean(syndrome = syndrome, xmunicipEnd = xmunicipEnd)[age == xage]

  setnames(d, "consult_with_influenza", "denominator")
  return(d)
}

#' Generate fake analysis results
#' @param granularity daily/weekly
#' @param syndrome Syndrome
#' @param xage Age
#' @param xmunicipEnd municipality
#' @import data.table
#' @export GenFakeResultsFull
GenFakeResultsFull <- function(granularity = "weekly", syndrome = "influensa", xage = "Totalt", xmunicipEnd = "municip5054") {
  age <- NULL

  d <- GenFakeDataClean(syndrome = syndrome, xmunicipEnd = xmunicipEnd)[age == xage]

  stack <- data.table(
    tag = syndrome,
    denominator = "consult_with_influenza",
    location_code = xmunicipEnd,
    age = xage,
    granularity_time = granularity,
    granularity_geo = "municip",
    stringsAsFactors = F,
    weeklyDenominatorFunction = "sum",
    v = 1,
    file = "test.RDS",
    uuid="34234233"
  )

  res <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = stack$granularity_time == "daily",
    v = 1,
    weeklyDenominatorFunction = ifelse(stack$weeklyDenominatorFunction=="sum",sum,mean),
    uuid=stack$uuid
  )

  res <- clean_post_analysis(res=res, stack = stack)

  return(res)
}


#' Validate the analysis results
#' @param d Results data.tabled
#' @import data.table
#' @export ValidateResultsFull
ValidateResultsFull <- function(d) {
  reqVars <- VARS$REQ_RESULTS_FULL

  if (sum(!reqVars %in% names(d)) > 0) {
    return(FALSE)
  }
  if (sum(!names(d) %in% reqVars) > 0) {
    return(FALSE)
  }

  return(TRUE)
}
