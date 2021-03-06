#' Identify all raw/clean datasets that are available
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
#' @import data.table
#' @export IdentifyAllDatasets
IdentifyAllDatasets <-
  function(raw = list.files(fd::path("data_raw"), "^partially_formatted_"),
             clean = list.files(fd::path("data_clean"), "done_")) {
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
#' @export DeleteOldDatasets
DeleteOldDatasets <-
  function(raw = list.files(fd::path("data_raw"), "^partially_formatted_"),
             clean = list.files(fd::path("data_clean"), "done_")) {
    res <- IdentifyAllDatasets(raw = raw, clean = clean)
    if (nrow(res) > 0) {
      res <- res[-nrow(res)]
    }
    for (i in 1:nrow(res)) {
      unlink(file.path(fd::path("data_raw"), res[i]$raw))
      unlink(file.path(
        fd::path("data_clean"),
        sprintf("*%s*", res[i]$id)
      ))
    }
  }

#' Identify the latest raw/clean datasets
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
#' @import data.table
#' @export IdentifyDatasets
IdentifyDatasets <-
  function(raw = list.files(fd::path("data_raw"), "^partially_formatted_"),
             clean = list.files(fd::path("data_clean"), "done_")) {
    res <- IdentifyAllDatasets(raw = raw, clean = clean)
    if (nrow(res) > 0) {
      res <- res[nrow(res)]
    }

    return(res)
  }

#' test
#' @param hyphen Use a hyphen or underscore?
#' @export
LatestRawID <- function(hyphen = F) {
  f <- IdentifyDatasets()
  if (hyphen) f$id <- gsub("_", "-", f$id)
  return(max(f$id))
}

#' latest_date
#' @export
latest_date <- function() {
  LatestRawID(hyphen = T)
}


#' Delete the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <-
  function(file = fd::path("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
    try(unlink(file), TRUE)
    # try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
  }

#' Create the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @export CreateLatestDoneFile
CreateLatestDoneFile <-
  function(file = fd::path("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
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
#' @export UpdateData
UpdateData <- function() {
  # variables used in data.table functions in this function
  isClean <- NULL
  Kontaktype <- NULL
  respiratory <- NULL
  # end

  files <- IdentifyDatasets()
  if (!fd::config$is_dev) {
    files <- files[is.na(isClean)]
  }
  if (nrow(files) == 0) {
    fd::msg("No new data")
    return(FALSE)
  }
  if (!fhi::file_stable(fd::path("data_raw", files$raw))) {
    fd::msg(sprintf("Unstable file %s", files$raw))
    return(FALSE)
  }

  fd::msg(sprintf("Cleaning file %s", files$raw))
  EmailNotificationOfNewData(files$id)

  d <- fread(fd::path("data_raw", files$raw))
  d[, date := data.table::as.IDate(date)]
  d[, respiratory := NULL]

  d[, influensa_all := influensa]


  for (i in 1:nrow(CONFIG$SYNDROMES)) {
    conf <- CONFIG$SYNDROMES[i]
    fd::msg(sprintf("Processing %s/%s: %s", i, nrow(CONFIG$SYNDROMES), conf$tag))


    res <- CleanData(
      d = copy(d[Kontaktype %in% conf$contactType[[1]]]),
      syndrome = conf$syndrome
    )
    for (j in names(CONFIG$AGES)) {
      saveRDS(res[age == j], file = fd::path(
        "data_clean",
        sprintf(
          "%s_%s_%s_cleaned.RDS",
          files$id,
          conf$tag,
          j
        )
      ))
    }
  }
  gc()
  fd::msg("New data is now formatted and ready")
  return(TRUE)
}
