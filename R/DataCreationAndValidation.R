#' Generate fake raw data
#' @import data.table
#' @export GenFakeDataRaw
GenFakeDataRaw <- function() {
  m <- GenNorwayMunicipMerging()
  skeleton <- unique(m[municipEnd == "municip5054", c("municip", "year")])

  data <- vector("list", length = nrow(skeleton))
  for (i in 1:length(data)) {
    minDate <- as.Date(sprintf("%s-01-01", skeleton$year[i]))
    maxDate <- as.Date(sprintf("%s-12-31", skeleton$year[i]))
    data[[i]] <- expand.grid(
      date = seq.Date(minDate, maxDate, 1),
      "age" = CONFIG$AGES,
      "Kontakttype" = c("Telefonkontakt", "Legekontakt"),
      "Praksis" = c("Fastlege", "Legevakt"),
      stringsAsFactors = F
    )
    setDT(data[[i]])
    data[[i]][, municip := skeleton$municip[i]]
    data[[i]][, municip := skeleton$municip[i]]

    data[[i]][, consult := rpois(.N, 50)]
    for (j in CONFIG$SYNDROMES) {
      data[[i]][, (j) := rpois(.N, 5)]
      data[[i]][, consult := consult + get(j)]
    }
  }
  data <- rbindlist(data)
  setcolorder(data, VARS$REQ_DATA_RAW_ALL)

  return(data)
}

#' Validate raw data
#' @param d Dataset to validate
#' @export ValidateDataRaw
ValidateDataRaw <- function(d) {
  # names(d) must contain all required variables
  if (sum(!VARS$REQ_DATA_RAW_ALL %in% names(d)) > 0) {
    return(FALSE)
  }

  # there must not be any extra variables in names(d)
  if (sum(!names(d) %in% VARS$REQ_DATA_RAW_ALL) > 0) {
    return(FALSE)
  }

  return(TRUE)
}

#' Generate fake analysis results
#' @param granularity "daily"/"weekly"
#' @param loc Location code
#' @param type Syndrome
#' @import data.table
#' @importFrom RAWmisc Year WeekN
#' @export GenerateAnalysisResults
GenerateAnalysisResults <- function(granularity = "weekly", loc = "Norge", type = CONFIG$SYNDROMES_ALERT_EXTERNAL[1]) {
  HelligdagIndikator <- NULL
  failed <- NULL
  age <- NULL
  location <- NULL
  locationName <- NULL

  set.seed(4)

  from <- "2010-01-01"
  to <- "2011-12-31"

  data <- data.table(date = seq.Date(as.Date(from), as.Date(to), by = 1))


  if (tolower(granularity) == "weekly") {
    data[, year := RAWmisc::Year(date)]
    data[, week := RAWmisc::WeekN(date)]
    data <- unique(data[, c("year", "week")])
    data <- AddXToWeekly(data)
    data <- AddWkyrAndDisplayDateToWeekly(data)


    reqVars <- c(
      variablesAlgorithmWeekly,
      variablesAlgorithmBasic,
      variablesAlgorithmProduced,
      variablesPostProcessing
    )
  } else {
    reqVars <- c(
      variablesAlgorithmDaily,
      variablesAlgorithmBasic,
      variablesAlgorithmProduced,
      variablesPostProcessing
    )
  }

  for (i in variablesAlgorithmBasic) {
    data[, (i) := rpois(.N, 10)]
  }
  data[, HelligdagIndikator := sample(x = c(0, 1), size = .N, replace = T)]

  for (i in variablesAlgorithmProduced) {
    data[, (i) := rpois(.N, 10)]
  }
  data[, failed := NULL]
  data[, failed := FALSE]

  data[, age := CONFIG$AGES[1]]
  data[, type := type]
  data[, location := loc]
  data[, locationName := GetLocationName(loc, norwayLocations = norwayLocations)]
  DetermineStatus(data)

  AddCounty(data = data, loc = loc)

  return(data)
}


#' Validate the analysis results
#' @param d Results data.table
#' @param granularity "daily"/"weekly"
#' @import data.table
#' @export ValidateAnalysisResults
ValidateAnalysisResults <- function(d, granularity = "weekly") {
  if (tolower(granularity) == "weekly") {
    reqVars <- c(
      variablesAlgorithmWeekly,
      variablesAlgorithmBasic,
      variablesAlgorithmProduced,
      variablesPostProcessing
    )
  } else {
    reqVars <- c(
      variablesAlgorithmDaily,
      variablesAlgorithmBasic,
      variablesAlgorithmProduced,
      variablesPostProcessing
    )
  }

  optionalVars <- variablesMunicip

  if (sum(!reqVars %in% names(d)) > 0) {
    return(FALSE)
  }
  if (sum(!names(d) %in% c(reqVars, optionalVars)) > 0) {
    return(FALSE)
  }

  return(TRUE)
}
