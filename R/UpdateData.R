#' Identify all raw/clean datasets that are available
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
#' @import data.table
#' @import fhi
#' @export IdentifyAllDatasets
IdentifyAllDatasets <-
  function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
             clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
    # variables used in data.table functions in this function
    id <- isRaw <- isClean <- NULL
    # end

    raw <- data.table(raw)
    clean <- data.table(clean)

    raw[, id := gsub(".txt", "", gsub("partially_formatted_", "", raw))]
    raw[, isRaw := TRUE]
    clean[, id := gsub(".txt", "", gsub("done_", "", clean))]
    clean[, isClean := TRUE]
    res <- merge(raw, clean, by = "id", all = TRUE)
    setorder(res, id)

    return(res)
  }

#' Delete old datasets
#'
#' Delete all old raw/clean datasets except the latest
#'
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
#' @import data.table
#' @import fhi
#' @export DeleteOldDatasets
DeleteOldDatasets <-
  function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
             clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
    res <- IdentifyAllDatasets(raw = raw, clean = clean)
    if (nrow(res) > 0) {
      res <- res[-nrow(res)]
    }
    for (i in 1:nrow(res)) {
      unlink(file.path(fhi::DashboardFolder("data_raw"), res[i]$raw))
      unlink(file.path(
        fhi::DashboardFolder("data_clean"),
        sprintf("*%s*", res[i]$id)
      ))
    }
  }

#' Identify the latest raw/clean datasets
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
#' @import data.table
#' @import fhi
#' @export IdentifyDatasets
IdentifyDatasets <-
  function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
             clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
    res <- IdentifyAllDatasets(raw = raw, clean = clean)
    if (nrow(res) > 0) {
      res <- res[nrow(res)]
    }

    return(res)
  }

#' test
#' @export LatestRawID
LatestRawID <- function() {
  f <- IdentifyDatasets()
  return(max(f$id))
}

#' Delete the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @import fhi
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <-
  function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
    try(unlink(file), TRUE)
    # try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
  }

#' Create the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @import fhi
#' @export CreateLatestDoneFile
CreateLatestDoneFile <-
  function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
    try(file.create(file), TRUE)
    # try(file.create(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
  }

#' Latest datasets
#'
#' Returns the names of the four datasets corresponding to a
#' date/syndrome.
#'
#' @param clean A date/ID
#' @param SYNDROME Syndrome that you are interested in
#' @import fhi
#' @export LatestDatasets
LatestDatasets <-
  function(clean = LatestRawID(), SYNDROME = "influensa") {
    return(
      list(
        "everyone_everyone" = paste0(clean, "_", SYNDROME, "_cleaned_everyone_everyone.RDS"),
        "everyone_fastlege" = paste0(clean, "_", SYNDROME, "_cleaned_everyone_fastlege.RDS"),
        "legekontakt_everyone" = paste0(clean, "_", SYNDROME, "_cleaned_legekontakt_everyone.RDS"),
        "legekontakt_fastlege" = paste0(clean, "_", SYNDROME, "_cleaned_legekontakt_fastlege.RDS"),
        "date" = clean
      )
    )
  }

#' Top level function call for cleaning of data
#'
#' This function checks if there is new stable data and then cleans it.
#'
#' @import data.table
#' @import fhi
#' @importFrom RAWmisc IsFileStable
#' @export UpdateData
UpdateData <- function() {
  # variables used in data.table functions in this function
  isClean <- NULL
  Kontaktype <- NULL
  respiratory <- NULL
  # end

  files <- IdentifyDatasets()
  if (!fhi::DashboardIsDev()) {
    files <- files[is.na(isClean)]
  }
  if (nrow(files) == 0) {
    fhi::DashboardMsg("No new data")
    return(FALSE)
  }
  if (!RAWmisc::IsFileStable(fhi::DashboardFolder("data_raw", files$raw))) {
    fhi::DashboardMsg(sprintf("Unstable file %s", files$raw))
    return(FALSE)
  }

  fhi::DashboardMsg(sprintf("Cleaning file %s", files$raw))
  EmailNotificationOfNewData(files$id)

  d <- fread(fhi::DashboardFolder("data_raw", files$raw))
  d[, date := data.table::as.IDate(date)]
  d[, respiratory := NULL]

  for (i in 1:nrow(CONFIG$SYNDROMES)) {
    conf <- CONFIG$SYNDROMES[i]
    fhi::DashboardMsg(sprintf("Processing %s/%s: %s", i, nrow(CONFIG$SYNDROMES), conf$tag))


    res <- CleanData(copy(d[Kontaktype %in% conf$contactType[[1]]]),
      syndrome = conf$syndrome
    )
    saveRDS(res, file = fhi::DashboardFolder(
      "data_clean",
      sprintf(
        "%s_%s_cleaned.RDS",
        files$id,
        conf$tag
      )
    ))
  }

  fhi::DashboardMsg("New data is now formatted and ready")
  return(TRUE)
}
