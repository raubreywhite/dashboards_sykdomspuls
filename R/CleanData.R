#' Identify all raw/clean datasets that are available
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
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

#' Delete old datasets
#'
#' Delete all old raw/clean datasets except the latest
#'
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
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

#' Identify the latest raw/clean datasets
#' @param raw Folder containing raw data
#' @param clean Folder containing clean data
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

#' Delete the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @import fhi
#' @export DeleteLatestDoneFile
DeleteLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
  try(unlink(file), TRUE)
  # try(unlink(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' Create the latest done file
#' @param file Location of the latest done file (according to latest raw data file)
#' @import fhi
#' @export CreateLatestDoneFile
CreateLatestDoneFile <- function(file = fhi::DashboardFolder("data_clean", paste0("done_", LatestRawID(), ".txt"))) {
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
#' @import fhi
#' @import data.table
#' @importFrom lubridate today
#' @import httr
#' @import jsonlite
#' @export GetPopulation
GetPopulation <- function() {
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  # end
  popFiles <- c("Personer2005-2009.csv","Personer2010-2014.csv","Personer2015-2018.csv")
  pop <- vector("list",length=length(popFiles))
  for(i in seq_along(pop)){
    pop[[i]] <- fread(system.file("extdata",popFiles[i],package="sykdomspuls"))
    pop[[i]] <- melt.data.table(pop[[i]], id.vars=c("region","age"))
  }
  pop <- rbindlist(pop)
  pop[,municip:=sprintf("municip%s",stringr::str_extract(region,"^[0-9][0-9][0-9][0-9]"))]
  pop[,year:=as.numeric(stringr::str_extract(variable,"[0-9][0-9][0-9][0-9]$"))]
  pop[,agenum:=as.numeric(stringr::str_extract(age,"^[0-9]*"))]

  for(i in which(names(CONFIG$AGES)!="Totalt")){
    pop[agenum %in% CONFIG$AGES[[i]], age:=names(CONFIG$AGES)[i]]
  }

  pop <- pop[,.(
    pop=sum(value)
  ),keyby=.(
    municip,age,year
  )]

  total <- pop[,.(
    pop=sum(pop)
  ),keyby=.(
    municip,year
  )]
  total[,age:="Totalt"]

  pop <- rbind(pop,total)

  # Fixing broken parts in the population data
  # part 1
  pop2 <- pop[municip=="municip0710" & year <= 2017]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip0706"]
  pop2[,pop:=round(pop/3)]
  pop <- rbind(pop,pop2)

  pop2 <- pop[municip=="municip0710" & year <= 2017]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip0719"]
  pop2[,pop:=round(pop/3)]
  pop <- rbind(pop,pop2)

  pop2 <- pop[municip=="municip0710" & year <= 2017]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip0720"]
  pop2[,pop:=round(pop/3)]
  pop <- rbind(pop,pop2)

  # part 2
  pop2 <- pop[municip=="municip1756" & year <= 2012]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1723"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  pop2 <- pop[municip=="municip1756" & year <= 2012]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1729"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  # part 3
  pop2 <- pop[municip=="municip5046" & year <= 2018]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1901"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  pop2 <- pop[municip=="municip1756" & year <= 2018]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1915"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  # part 4
  pop2 <- pop[municip=="municip1505" & year <= 2008]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1503"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  pop2 <- pop[municip=="municip1505" & year <= 2008]
  pop2[,pop:=max(pop),by=age]
  pop2 <- pop2[year!=max(year)]
  pop2[,municip:="municip1556"]
  pop2[,pop:=round(pop/2)]
  pop <- rbind(pop,pop2)

  return(pop)
}

#' Format the raw data
#' @param d Raw data
#' @param SYNDROME Syndrome of interest
#' @param population Population file
#' @param hellidager Hellidager file
#' @param testIfHelligdagIndikatorFileIsOutdated Boolean. Test if the current date is older than the last hellidag recorded in the fiel?
#' @param removeMunicipsWithoutConsults Boolean. Remove municipalities that do not have any consultations?
#' @import data.table
#' @importFrom lubridate today
#' @export FormatData
FormatData <- function(d, SYNDROME,
                       population = GetPopulation(),
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

  if(SYNDROME %in% c("consultWithInfluensa","consultWithoutInfluensa")){
    SYNDROME_AND_INFLUENSA_AND_CONSULT <- unique(c("influensa", "consult"))
  } else {
    SYNDROME_AND_INFLUENSA_AND_CONSULT <- unique(c(SYNDROME, "influensa", "consult"))
  }

  d <- d[municip != "municip9999",
    lapply(.SD, sum),
    by = .(age, date, municip),
    .SDcols = SYNDROME_AND_INFLUENSA_AND_CONSULT
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

  for (i in SYNDROME_AND_INFLUENSA_AND_CONSULT) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[municip != "municip9999",
    lapply(.SD, sum),
    by = .(date, municip),
    .SDcols = SYNDROME_AND_INFLUENSA_AND_CONSULT
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

  # KOMMUNE MERGING

  dim(data)
  data <- merge(data, norwayMunicipMerging[, c("municip", "year", "municipEnd")], by = c("municip", "year"), all.x = T)
  dim(data)
  data <- data[!is.na(municipEnd)]

  n1 <- nrow(data)
  data <- merge(data, population, by=c("municip","age","year"))
  n2 <- nrow(data)

  if(n1!=n2) fhi::DashboardMsg("Population file not merging correctly", type="err")

  data <- data[!is.na(municipEnd),
    lapply(.SD, sum),
    keyby = .(municipEnd, year, age, date),
    .SDcols = c(SYNDROME_AND_INFLUENSA_AND_CONSULT, "pop")
  ]
  dim(data)
  setnames(data, "municipEnd", "municip")

  # merging in municipalitiy-fylke names
  data <- merge(data, norwayLocations[, c("municip", "county")], by = "municip")
  for (i in SYNDROME_AND_INFLUENSA_AND_CONSULT) {
    data[is.na(get(i)), (i) := 0]
  }
  data[, consultWithInfluensa := as.numeric(consult)]
  data[, consultWithoutInfluensa := consultWithInfluensa - influensa]
  data[, consult := NULL]

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

  setcolorder(data, unique(c(
    "date",
    "HelligdagIndikator",
    "county",
    "municip",
    "age",
    SYNDROME,
    "consultWithInfluensa",
    "consultWithoutInfluensa",
    "pop"
  )))

  if(SYNDROME %in% c("consultWithInfluensa","consultWithoutInfluensa")){
    data[,value:=get(SYNDROME)]
  } else {
    setnames(data, SYNDROME, "value")
  }

  setorder(data, municip, age, date)
  setkey(data, municip, age, date)

  return(data)
}

#' Not used.
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
  # end

  files <- IdentifyDatasets()
  if(!fhi::DashboardIsDev()) files <- files[is.na(isClean)]
  if (nrow(files) == 0) {
    fhi::DashboardMsg("No new data")
    return(FALSE)
  } else {
    fhi::DashboardMsg("Updating data")
    if (Sys.getenv("COMPUTER") == "smhb") EmailNotificationOfNewData(files$id, isTest = FALSE)
    for (i in 1:nrow(files)) {
      if (!RAWmisc::IsFileStable(fhi::DashboardFolder("data_raw", files[i]$raw))) {
        fhi::DashboardMsg(sprintf("Unstable file %s",files[i]$raw))
        return(FALSE)
      }
      fhi::DashboardMsg(sprintf("Cleaning file %s",files[i]$raw))
      d <- fread(fhi::DashboardFolder("data_raw", files[i]$raw))
      d[, date := data.table::as.IDate(date)]

      for (SYNDROME in CONFIG$SYNDROMES) {
        fhi::DashboardMsg(sprintf("Processing %s",SYNDROME))
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

    fhi::DashboardMsg("New data is now formatted and ready")
    return(TRUE)
  }
}
