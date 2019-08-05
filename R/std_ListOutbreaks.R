#' Generates the internal outbreak list
#' @param df Data inside resYearLine.RDS
#' @param dk Data inside resYearLineMunicip.RDS
#' @param saveFiles Where are you saving the outbreak lists?
#' @param useType Use type or tag as variable name
#' @import data.table
#' @export GenerateOutbreakListInternal
GenerateOutbreakListInternal <- function(df = readRDS(fd::path("results", sprintf("%s/resYearLine.RDS", LatestRawID()))),
                                         dk = readRDS(fd::path("results", sprintf("%s/resYearLineMunicip.RDS", LatestRawID()))),
                                         saveFiles = c(
                                           fd::path("results", sprintf("%s/outbreaks.RDS", LatestRawID()))
                                         ),
                                         useType = FALSE) {
  # variables used in data.table functions in this function
  . <- NULL
  status <- NULL
  wkyr <- NULL
  age <- NULL
  county <- NULL
  location <- NULL
  locationName <- NULL
  zscore <- NULL
  tag <- NULL
  cumE1 <- NULL
  meanZScore <- NULL
  sumCum <- NULL
  sumCumNorge <- NULL
  countyName <- NULL
  # end

  counties <- unique(df[, c("location_code", "location_name"), with = F])
  setnames(counties, c("county_code", "county_name"))

  df <- df[, c("yrwk", "age", "tag", "location_name", "status", "zscore", "cumE1"), with = F]
  dk <- dk[, c("yrwk", "age", "tag", "location_code", "location_name", "status", "county_code", "zscore", "cumE1"), with = F]
  dk <- merge(dk, counties, by = "county_code")

  setorder(df, status, -yrwk, -age)
  setorder(dk, status, -yrwk, -age, county_code, location_code)

  df[, location_name := sprintf("%s (%s)", location_name, formatC(zscore, digits = 2, format = "f"))]
  dk[, location_name := sprintf("%s (%s)", location_name, formatC(zscore, digits = 2, format = "f"))]

  df[status != "High", location_name := ""]
  dk[status != "High", location_name := ""]

  df[, status := NULL]
  dk[, status := NULL]

  dk[, location_code := NULL]


  df1 <- df[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df1[, zscore := NULL]
  df1[, cumE1 := NULL]

  df2 <- df[location_name != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df3 <- df[stringr::str_detect(location_name, "Norge"), .(
    sumCumNorge = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag
  )]
  df <- merge(df1, df2, by = c("yrwk", "age", "tag"), all.x = T)
  df <- merge(df, df3, by = c("yrwk", "age", "tag"), all.x = T)
  df[is.na(meanZScore), meanZScore := 0]
  df[is.na(sumCum), sumCum := 0]
  df[is.na(sumCumNorge), sumCumNorge := 0]
  df[stringr::str_detect(location_name, "Norge"), sumCum := sumCumNorge]
  df[, sumCumNorge := NULL]
  df[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  df[meanZScore == "0.00", meanZScore := ""]
  df[, sumCum := round(sumCum)]
  df[sumCum == 0, sumCum := NA]

  dk1 <- dk[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    yrwk,
    age,
    tag,
    county_code,
    county_name
  )]
  dk1[, zscore := NULL]
  dk1[, cumE1 := NULL]

  dk2 <- dk[location_name != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    yrwk,
    age,
    tag,
    county_code
  )]
  dk <- merge(dk1, dk2, by = c("yrwk", "age", "tag", "county_code"), all.x = T)
  dk[is.na(meanZScore), meanZScore := 0]
  dk[is.na(sumCum), sumCum := 0]
  dk[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  dk[meanZScore == "0.00", meanZScore := ""]
  dk[, sumCum := round(sumCum)]
  dk[sumCum == 0, sumCum := NA]

  df[, location_name := gsub(", , ", "", location_name)]
  df[, location_name := gsub(", $", "", location_name)]
  df[, location_name := gsub("^, ", "", location_name)]
  setorder(df, tag, -yrwk, -age)
  setnames(df, "location_name", "High")

  df[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(df, tag, -yrwk, age)
  setcolorder(df, c("tag", "yrwk", "age", "High", "meanZScore", "sumCum"))

  dk[, location_name := gsub(", , ", "", location_name)]
  dk[, location_name := gsub(", $", "", location_name)]
  dk[, location_name := gsub("^, ", "", location_name)]
  dk[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(dk, tag, -yrwk, age, county_code)
  dk[, county_code := NULL]
  setcolorder(dk, c("tag", "yrwk", "age", "county_name", "location_name", "meanZScore", "sumCum"))
  setnames(dk, "location_name", "High")

  if (useType) {
    setnames(df, "tag", "type")
    setnames(dk, "tag", "type")
  }

  outbreaks <- list(df = df, dk = dk)
  if (!is.null(saveFiles)) {
    SaveRDS(outbreaks, saveFiles)
  }

  return(outbreaks)
}


#' Generates the external outbreak list
#' @param df Data inside resYearLine.RDS
#' @param dk Data inside resYearLineMunicip.RDS
#' @param saveFiles Where are you saving the outbreak lists?
#' @param alerts Excel sheet containing registration information
#' @import data.table
#' @export GenerateOutbreakListExternal
GenerateOutbreakListExternal <- function(df = readRDS(fd::path("results", sprintf("%s/resYearLine.RDS", LatestRawID()))),
                                         dk = readRDS(fd::path("results", sprintf("%s/resYearLineMunicip.RDS", LatestRawID()))),
                                         saveFiles = fd::path("results", sprintf("%s/outbreaks_alert_external.RDS", LatestRawID())),
                                         alerts = GetAlertsEmails()) {
  # variables used in data.table functions in this function
  . <- NULL
  status <- NULL
  displayDay <- NULL
  email <- NULL
  age <- NULL
  tag <- NULL
  location <- NULL
  alertExternal <- NULL
  # end

  df <- df[date == max(date) & tag %in% CONFIG$SYNDROMES[alertExternal == T]$tag]
  dk <- dk[date == max(date) & tag %in% CONFIG$SYNDROMES[alertExternal == T]$tag]

  resultsk <- resultsf <- list()
  for (i in 1:nrow(alerts)) {
    resultsf[[i]] <- df[status %in% alerts$statuses[[i]] & stringr::str_detect(location_code, alerts$location[i])]
    resultsk[[i]] <- dk[status %in% alerts$statuses[[i]] & stringr::str_detect(location_code, alerts$location[i])]

    resultsf[[i]][, email := alerts$email[i]]
    resultsk[[i]][, email := alerts$email[i]]
  }

  resultsf <- rbindlist(resultsf)
  resultsk <- rbindlist(resultsk)

  resultsf[, age := factor(age, levels = names(CONFIG$AGES))]
  setorder(resultsf, tag, location_code, age)

  resultsk[, age := factor(age, levels = names(CONFIG$AGES))]
  setorder(resultsk, tag, location_code, age)

  results <- rbind(resultsf, resultsk, fill = T)

  if (!is.null(saveFiles)) {
    SaveRDS(results, saveFiles)
  } else {
    return(results)
  }
}
