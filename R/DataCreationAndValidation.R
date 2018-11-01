#' GenFakeDataRaw
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

#' ValidateDataRaw
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
