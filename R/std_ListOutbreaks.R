#' Generates the external outbreak list
#' Generates the internal outbreak list
#' @param df Data inside resYearLine.RDS
#' @param dk Data inside resYearLineMunicip.RDS
#' @param saveFiles Where are you saving the outbreak lists?
#' @param useType Use type or tag as variable name
#' @import data.table
#' @export
GenerateOutbreakListAPI <- function(df = readRDS(fd::path("results", sprintf("%s/resYearLine.RDS", LatestRawID()))),
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

  counties <- unique(df[, c("location", "locationName"), with = F])
  setnames(counties, c("county", "countyName"))

  df <- df[, c("wkyr", "age", "tag", "locationName", "status", "zscore", "cumE1"), with = F]
  dk <- dk[, c("wkyr", "age", "tag", "location", "locationName", "status", "county", "zscore", "cumE1"), with = F]
  dk <- merge(dk, counties, by = "county")

  setorder(df, status, -wkyr, -age)
  setorder(dk, status, -wkyr, -age, county, location)

  df[, locationName := sprintf("%s (%s)", locationName, formatC(zscore, digits = 2, format = "f"))]
  dk[, locationName := sprintf("%s (%s)", locationName, formatC(zscore, digits = 2, format = "f"))]

  df[status != "High", locationName := ""]
  dk[status != "High", locationName := ""]

  df[, status := NULL]
  dk[, status := NULL]

  dk[, location := NULL]


  df1 <- df[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    wkyr,
    age,
    tag
  )]
  df1[, zscore := NULL]
  df1[, cumE1 := NULL]

  df2 <- df[locationName != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    wkyr,
    age,
    tag
  )]
  df3 <- df[stringr::str_detect(locationName, "Norge"), .(
    sumCumNorge = sum(cumE1)
  ), by = .(
    wkyr,
    age,
    tag
  )]
  df <- merge(df1, df2, by = c("wkyr", "age", "tag"), all.x = T)
  df <- merge(df, df3, by = c("wkyr", "age", "tag"), all.x = T)
  df[is.na(meanZScore), meanZScore := 0]
  df[is.na(sumCum), sumCum := 0]
  df[is.na(sumCumNorge), sumCumNorge := 0]
  df[stringr::str_detect(locationName, "Norge"), sumCum := sumCumNorge]
  df[, sumCumNorge := NULL]
  df[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  df[meanZScore == "0.00", meanZScore := ""]
  df[, sumCum := round(sumCum)]
  df[sumCum == 0, sumCum := NA]

  dk1 <- dk[, lapply(
    .SD, paste0,
    collapse = ", "
  ), by = .(
    wkyr,
    age,
    tag,
    county,
    countyName
  )]
  dk1[, zscore := NULL]
  dk1[, cumE1 := NULL]

  dk2 <- dk[locationName != "", .(
    meanZScore = mean(zscore),
    sumCum = sum(cumE1)
  ), by = .(
    wkyr,
    age,
    tag,
    county
  )]
  dk <- merge(dk1, dk2, by = c("wkyr", "age", "tag", "county"), all.x = T)
  dk[is.na(meanZScore), meanZScore := 0]
  dk[is.na(sumCum), sumCum := 0]
  dk[, meanZScore := formatC(meanZScore, digits = 2, format = "f")]
  dk[meanZScore == "0.00", meanZScore := ""]
  dk[, sumCum := round(sumCum)]
  dk[sumCum == 0, sumCum := NA]

  df[, locationName := gsub(", , ", "", locationName)]
  df[, locationName := gsub(", $", "", locationName)]
  df[, locationName := gsub("^, ", "", locationName)]
  setorder(df, tag, -wkyr, -age)
  setnames(df, "locationName", "High")

  df[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(df, tag, -wkyr, age)
  setcolorder(df, c("tag", "wkyr", "age", "High", "meanZScore", "sumCum"))

  dk[, locationName := gsub(", , ", "", locationName)]
  dk[, locationName := gsub(", $", "", locationName)]
  dk[, locationName := gsub("^, ", "", locationName)]
  dk[, age := factor(age, levels = c("Totalt", "0-4", "5-14", "15-19", "20-29", "30-64", "65+"))]
  setorder(dk, tag, -wkyr, age, county)
  dk[, county := NULL]
  setcolorder(dk, c("tag", "wkyr", "age", "countyName", "locationName", "meanZScore", "sumCum"))
  setnames(dk, "locationName", "High")

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
