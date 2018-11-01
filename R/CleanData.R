#' test
#' @param raw a
#' @param clean a
#' @import data.table
#' @import fhi
#' @export IdentifyAllDatasets
IdentifyAllDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
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

#' test
#' @param raw a
#' @param clean a
#' @import data.table
#' @import fhi
#' @export DeleteOldDatasets
DeleteOldDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                              clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
  res <- IdentifyAllDatasets(raw = raw, clean = clean)
  if (nrow(res) > 0) {
    res <- res[-nrow(res)]
  }
  for (i in 1:nrow(res)) {
    unlink(file.path(fhi::DashboardFolder("data_raw"), res[i]$raw))
    unlink(file.path(fhi::DashboardFolder("data_clean"), sprintf("*%s*", res[i]$id)))
  }
}

#' test
#' @param raw a
#' @param clean a
#' @import data.table
#' @import fhi
#' @export IdentifyDatasets
IdentifyDatasets <- function(raw = list.files(fhi::DashboardFolder("data_raw"), "^partially_formatted_"),
                             clean = list.files(fhi::DashboardFolder("data_clean"), "done_")) {
  res <- IdentifyAllDatasets(raw = raw, clean = clean)
  if (nrow(res) > 0) res <- res[nrow(res)]

  return(res)
}

#' test
#' @export LatestRawID
LatestRawID <- function() {
  f <- IdentifyDatasets()
  return(max(f$id))
}

#' test
#' @param file a
#' @import fhi
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
  try(unlink(file), TRUE)
  # try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' test
#' @param file a
#' @import fhi
#' @export CreateLatestDoneFile
CreateLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
  try(file.create(file), TRUE)
  # try(file.create(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' test
#' @param clean a
#' @param SYNDROME a
#' @import fhi
#' @export LatestDatasets
LatestDatasets <- function(clean = LatestRawID(), SYNDROME = "influensa") {
  return(list(
    "everyone_everyone" = paste0(clean, "_", SYNDROME, "_cleaned_everyone_everyone.RDS"),
    "everyone_fastlege" = paste0(clean, "_", SYNDROME, "_cleaned_everyone_fastlege.RDS"),
    "legekontakt_everyone" = paste0(clean, "_", SYNDROME, "_cleaned_legekontakt_everyone.RDS"),
    "legekontakt_fastlege" = paste0(clean, "_", SYNDROME, "_cleaned_legekontakt_fastlege.RDS"),
    "date" = clean
  ))
}


#' GetPopulation
#' Mostly a function used by the package maintainer
#' to generate new population files as necessary
#' @param L a
#' @param U a
#' @param municip a
#' @param saveFiles a
#' @param yearsCopiedAtTail a
#' @import fhi
#' @import data.table
#' @importFrom lubridate today
#' @import httr
#' @import jsonlite
#' @export GetPopulation
GetPopulation <- function(
                          L = c(0, 5, 15, 20, 30, 65),
                          U = c(4, 14, 19, 29, 64, 9999),
                          municip = stringr::str_extract(readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "sykdomspuls"))$municip, "[0-9][0-9][0-9][0-9]$"),
                          saveFiles = file.path("/git", "dashboards_sykdomspuls", "inst", "extdata", "pop.RDS"),
                          yearsCopiedAtTail = 5) {
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  # end

  municip <- municip[nchar(municip) == 4 & municip != "9999"]
  ages <- c(formatC(0:104, width = 3, flag = "0"), "105+")

  lastYear <- data.table::year(lubridate::today())
  if (data.table::month(lubridate::today()) < 3) {
    lastYear <- lastYear - 1
  }
  years <- as.character(c((data.table::year(lubridate::today()) - 15):lastYear))

  retval <- vector("list", length(years) + yearsCopiedAtTail)
  for (i in 1:length(years)) {
    useYear <- years[i]
    print(useYear)

    b <- paste0('
  {
    "query": [
      {
        "code": "Region",
        "selection": {
          "filter": "item",
          "values": [
  ', paste0('"', municip, '"', collapse = ","), '
          ]
        }
      },
      {
        "code": "Kjonn",
        "selection": {
          "filter": "item",
          "values": [
            "1",
            "2"
          ]
        }
      },
      {
        "code": "Alder",
        "selection": {
          "filter": "item",
          "values": [
  ', paste0('"', ages, '"', collapse = ","), '
          ]
        }
      },
      {
        "code": "ContentsCode",
        "selection": {
          "filter": "item",
          "values": [
            "Personer1"
          ]
        }
      },
      {
        "code": "Tid",
        "selection": {
          "filter": "item",
          "values": [
  ', paste0('"', useYear, '"', collapse = ","), '
          ]
        }
      }
    ],
    "response": {
      "format": "json-stat"
    }
  }

    ')

    x <- httr::POST("http://data.ssb.no/api/v0/no/table/07459",
      body = b, encode = "json"
    )

    y <- jsonlite::fromJSON(httr::content(x, "text"))
    d1 <- municip # unlist(y$dataset$dimension$Region$category$label)
    d2 <- unlist(y$dataset$dimension$Kjonn$category$label)
    d3 <- unlist(y$dataset$dimension$Alder$category$label)
    d4 <- unlist(y$dataset$dimension$Tid$category$label)
    res <- expand.grid(d4, d3, d2, d1)
    res$value <- as.character(y$dataset$value)
    res <- data.table(res)
    res[, value := as.numeric(value)]
    res[, age := as.numeric(stringr::str_extract(Var2, "^[0-9]*"))]
    res <- res[, c("Var1", "Var4", "value", "age"), with = F]
    setnames(res, c("year", "municip", "pop", "agecont"))
    res[, age := paste0(L[1], "-", U[1])]
    for (j in 2:(length(L) - 1)) {
      res[agecont >= L[j] & agecont <= U[j], age := paste0(L[j], "-", U[j])]
    }
    res[agecont >= L[length(L)], age := paste0(L[length(L)], "+")]

    res[, municip := paste0("municip", municip)]
    res <- res[, .(pop = sum(pop)), by = .(year, municip, age)]
    x <- res[, .(pop = sum(pop)), by = .(year, municip)]
    x[, age := "Totalt"]
    res <- rbind(res, x)
    res[, year := as.character(year)]

    retval[[i]] <- copy(res)
  }
  for (j in 1:yearsCopiedAtTail) {
    res[, year := as.character(as.numeric(year) + 1)]
    retval[[i + j]] <- copy(res)
  }
  saveRDS(retval, saveFiles)
}

#' test
#' @param d a
#' @param SYNDROME a
#' @param population a
#' @param hellidager a
#' @param testIfHelligdagIndikatorFileIsOutdated a
#' @param removeMunicipsWithoutConsults a
#' @import data.table
#' @importFrom lubridate today
#' @export FormatData
FormatData <- function(d, SYNDROME,
                       population = readRDS(system.file("extdata", "pop.RDS", package = "sykdomspuls")),
                       hellidager = fread(system.file("extdata", "DatoerMedHelligdager.txt", package = "sykdomspuls"))[, c("Dato", "HelligdagIndikator"), with = FALSE],
                       testIfHelligdagIndikatorFileIsOutdated = TRUE,
                       removeMunicipsWithoutConsults = FALSE) {
  # variables used in data.table functions in this function
  . <- NULL
  municip <- NULL
  age <- NULL
  datex <- NULL
  yrwk <- NULL
  municipEnd <- NULL
  consult <- NULL
  consultWithInfluensa <- NULL
  consultWithoutInfluensa <- NULL
  influensa <- NULL
  pop <- NULL
  error <- NULL
  # end

  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  SYNDROME_AND_INFLUENSA <- unique(c(SYNDROME, "influensa"))

  d <- d[municip != "municip9999",
    lapply(.SD, sum),
    by = .(age, date, municip),
    .SDcols = c(SYNDROME_AND_INFLUENSA, "consult")
  ]

  dateMin <- min(d$date)
  dateMax <- max(d$date)
  if (removeMunicipsWithoutConsults) {
    d[, total := sum(consult, na.rm = T), by = municip]
    d <- d[is.finite(total)]
    d <- d[total > 0]
    d[, total := NULL]
    skeleton <- data.table(expand.grid(unique(norwayMunicipMerging[municipEnd %in% unique(d$municip) | municip %in% unique(d$municip)]$municip), unique(d$age), seq.Date(dateMin, dateMax, 1)))
  } else {
    skeleton <- data.table(expand.grid(unique(norwayMunicipMerging$municip), unique(d$age), seq.Date(dateMin, dateMax, 1)))
  }
  setnames(skeleton, c("municip", "age", "date"))
  skeleton[, date := data.table::as.IDate(date)]
  data <- merge(skeleton, d, by = c("municip", "age", "date"), all.x = TRUE)

  for (i in c(SYNDROME_AND_INFLUENSA, "consult")) {
    data[is.na(get(i)), (i) := 0]
  }


  total <- data[municip != "municip9999",
    lapply(.SD, sum),
    by = .(date, municip),
    .SDcols = c(SYNDROME_AND_INFLUENSA, "consult")
  ]
  total[, age := "Totalt"]
  data <- rbind(total, data[age != "Ukjent"])

  dates <- unique(data[, "date", with = F])
  dates[, datex := date]
  dates[, yrwk := format.Date(datex, "%G-%V")] # Week-based year, instead of normal year (%Y)
  dates[, year := as.numeric(format.Date(date, "%G"))]
  dates <- dates[year >= 2006]

  # delete last day of data if it is not a sunday
  if (format.Date(max(dates$datex), "%u") != 7) {
    dates <- dates[yrwk != max(yrwk)]
  }
  dates[, datex := NULL]
  dates[, yrwk := NULL]
  data <- merge(data, dates, by = "date")

  # POPULATION

  if (FALSE) {
    lastYr <- as.numeric(population[[length(population) - 2]]$year[1])

    population[[length(population) - 1]] <- copy(population[[length(population) - 2]])
    population[[length(population)]] <- copy(population[[length(population) - 2]])

    population[[length(population) - 1]][, year := as.character(lastYr + 1)]
    population[[length(population)]][, year := as.character(lastYr + 2)]

    population <- rbindlist(population)
    population[, year := as.numeric(year)]

    dim(data)
    data <- merge(data, population, by = c("municip", "year", "age"), all.x = T)
    dim(data)
  } else {
    data[, pop := 1]
  }

  # KOMMUNE MERGING

  dim(data)
  data <- merge(data, norwayMunicipMerging[, c("municip", "year", "municipEnd")], by = c("municip", "year"), all.x = T)
  dim(data)
  data <- data[!is.na(municipEnd),
    lapply(.SD, sum),
    by = .(municipEnd, year, age, date),
    .SDcols = c(SYNDROME_AND_INFLUENSA, "consult", "pop")
  ]
  dim(data)
  setnames(data, "municipEnd", "municip")

  # merging in municipalitiy-fylke names
  data <- merge(data, norwayLocations[, c("municip", "county")], by = "municip")
  for (i in c(SYNDROME, "consult")) {
    data[is.na(get(i)), (i) := 0]
  }
  data[, consultWithInfluensa := as.numeric(consult)]
  data[, consultWithoutInfluensa := consultWithInfluensa - influensa]
  data[, consult := NULL]
  data[, pop := as.numeric(pop)]

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, municip := as.character(municip)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated & lubridate::today() > max(hellidager$date)) {
    error("HELLIGDAGER NEEDS UPDATING")
  }
  dim(data)
  data <- merge(data, hellidager, by = "date")
  dim(data)

  data[, year := NULL]

  if (SYNDROME != "influensa") {
    data[, influensa := NULL]
  }

  setcolorder(data, c(
    "date",
    "HelligdagIndikator",
    "county",
    "municip",
    "age",
    SYNDROME,
    "consultWithInfluensa",
    "consultWithoutInfluensa",
    "pop"
  ))
  setnames(data, SYNDROME, "value")

  setorder(data, municip, age, date)
  setkey(data, municip, age, date)

  return(data)
}

#' test
#' @param ageStrings a
#' @import stringr
#' @export GetAgesLU
GetAgesLU <- function(ageStrings) {
  ageStrings <- ageStrings[ageStrings != "Ukjent"]
  ageStrings <- gsub("\\+", "", ageStrings)
  ageStrings <- stringr::str_split(ageStrings, "-")
  L <- c()
  for (i in 1:length(ageStrings)) L[i] <- as.numeric(ageStrings[[i]][1])
  L <- as.numeric(L)
  L <- sort(L)
  U <- L[2:length(L)] - 1
  U <- c(U, 99999)
  return(list(
    L = L,
    U = U
  ))
}

#' test
#' @import data.table
#' @import fhi
#' @importFrom RAWmisc IsFileStable
#' @export UpdateData
UpdateData <- function() {
  # variables used in data.table functions in this function
  isClean <- NULL
  Kontaktype <- NULL
  # end

  files <- IdentifyDatasets()
  files <- files[is.na(isClean)]
  if (nrow(files) == 0) {
    cat(sprintf("%s/%s/R/SYKDOMSPULS No new data", Sys.time(), Sys.getenv("COMPUTER")), "\n")
    return(FALSE)
  } else {
    cat(sprintf("%s/%s/R/SYKDOMSPULS Updating data", Sys.time(), Sys.getenv("COMPUTER")), "\n")
    if (Sys.getenv("COMPUTER") == "smhb") EmailNotificationOfNewData(files$id, isTest = FALSE)
    for (i in 1:nrow(files)) {
      if (!RAWmisc::IsFileStable(fhi::DashboardFolder("data_raw", files[i]$raw))) {
        cat(sprintf("%s/%s/R/SYKDOMSPULS Unstable file %s", Sys.time(), Sys.getenv("COMPUTER"), files[i]$raw), "\n")
        return(FALSE)
      }
      cat(sprintf("%s/%s/R/SYKDOMSPULS Cleaning file %s", Sys.time(), Sys.getenv("COMPUTER"), files[i]$raw), "\n")
      d <- fread(fhi::DashboardFolder("data_raw", files[i]$raw))
      d[, date := data.table::as.IDate(date)]

      for (SYNDROME in CONFIG$SYNDROMES) {
        cat(sprintf("%s/%s/R/SYKDOMSPULS Processing %s", Sys.time(), Sys.getenv("COMPUTER"), SYNDROME), "\n")
        res <- FormatData(d[Kontaktype == "Legekontakt"], SYNDROME = SYNDROME)
        saveRDS(res, file = fhi::DashboardFolder(
          "data_clean",
          sprintf(
            "%s_%s_cleaned_legekontakt_everyone.RDS",
            files[i]$id, SYNDROME
          )
        ))

        res <- FormatData(d, SYNDROME = SYNDROME)
        saveRDS(res, file = fhi::DashboardFolder(
          "data_clean",
          sprintf(
            "%s_%s_cleaned_everyone_everyone.RDS",
            files[i]$id, SYNDROME
          )
        ))
      }
    }

    cat(sprintf("%s/%s/R/SYKDOMSPULS New data is now formatted and ready", Sys.time(), Sys.getenv("COMPUTER")), "\n")
    return(TRUE)
  }
}
