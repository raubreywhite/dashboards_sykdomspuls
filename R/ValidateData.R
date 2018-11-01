#' ValidateDataClean
#' @param d a
#' @param granularity a
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


#' GenerateAnalysisResults
#' @param granularity a
#' @param loc a
#' @param type a
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
