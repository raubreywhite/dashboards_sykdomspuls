#' SaveShinyAppDataToDB
#' @export SaveShinyAppDataToDB
SaveShinyAppDataToDB <- function() {
  displayDay <- NULL

  db <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
    host = "db",
    port = 3306,
    user = "root",
    password = "example"
  )
  a <- RMariaDB::dbGetQuery(db, "SHOW DATABASES LIKE 'sykdomspuls';")
  if (nrow(a) == 0) {
    a <- RMariaDB::dbExecute(db, "CREATE DATABASE sykdomspuls;")
  }
  RMariaDB::dbExecute(db, "USE sykdomspuls")

  GLOBAL <- new.env(parent = emptyenv())
  CONFIG_OLD <- ConvertConfigForAPI()
  GLOBAL$weeklyTypes <- GLOBAL$dailyTypes <- CONFIG_OLD$SYNDROMES[CONFIG_OLD$SYNDROMES %in% CONFIG_OLD$SYNDROMES_ALERT_INTERNAL]
  GLOBAL$weeklyAges <- GLOBAL$dailyAges <- CONFIG_OLD$AGES

  resYearLine <- readRDS(fhi::DashboardFolder("results", sprintf("%s/resYearLine.RDS", LatestRawID())))[type %in% GLOBAL$weeklyTypes]
  GLOBAL$dateMax <- max(resYearLine$displayDay)
  GLOBAL$dateMinRestrictedRecent <- GLOBAL$dateMax - 365
  GLOBAL$dateMinRestrictedLine <- GLOBAL$dateMax - 365 * 15

  outbreaks <- readRDS(fhi::DashboardFolder("results", sprintf("%s/outbreaks.RDS", LatestRawID())))
  outbreaksDF <- outbreaks[["df"]][tag %in% GLOBAL$weeklyTypes]
  outbreaksDK <- outbreaks[["dk"]][tag %in% GLOBAL$weeklyTypes]

  resRecentLine <- readRDS(fhi::DashboardFolder("results", sprintf("%s/resRecentLine.RDS", LatestRawID())))[
    date >= GLOBAL$dateMinRestrictedRecent & date <= GLOBAL$dateMax &
      tag %in% GLOBAL$weeklyTypes
  ]
  resYearLineMunicip <- readRDS(fhi::DashboardFolder("results", sprintf("%s/resYearLineMunicip.RDS", LatestRawID())))[
    displayDay >= GLOBAL$dateMinRestrictedLine & displayDay <= GLOBAL$dateMax &
      tag %in% GLOBAL$weeklyTypes
  ]

  dplyr::copy_to(
    dest = db,
    df = resRecentLine,
    name = "resRecentLine",
    temporary = FALSE,
    overwrite = TRUE
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resRecentLine` ADD INDEX `ind1` (`tag`(10),`location`(10),`age`(10))"
  )

  dplyr::copy_to(
    dest = db,
    df = resYearLine,
    name = "resYearLine",
    temporary = FALSE,
    overwrite = TRUE
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resYearLine` ADD INDEX `ind1` (`tag`(10),`location`(10),`age`(10))"
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resYearLine` ADD INDEX `ind2` (`tag`(10),`age`(10))"
  )

  dplyr::copy_to(
    dest = db,
    df = resYearLineMunicip,
    name = "resYearLineMunicip",
    temporary = FALSE,
    overwrite = TRUE
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resYearLineMunicip` ADD INDEX `ind1` (`tag`(10),`location`(10),`age`(10))"
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resYearLineMunicip` ADD INDEX `ind2` (`tag`(10),`age`(10))"
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `resYearLineMunicip` ADD INDEX `ind3` (`tag`(10),`age`(10),`county`(10))"
  )

  dplyr::copy_to(
    dest = db,
    df = outbreaksDF,
    name = "outbreaksDF",
    temporary = FALSE,
    overwrite = TRUE
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `outbreaksDF` ADD INDEX `ind1` (`tag`(10),`wkyr`(10))"
  )

  dplyr::copy_to(
    dest = db,
    df = outbreaksDK,
    name = "outbreaksDK",
    temporary = FALSE,
    overwrite = TRUE
  )
  RMariaDB::dbExecute(
    db,
    "ALTER TABLE `outbreaksDK` ADD INDEX `ind1` (`tag`(10),`wkyr`(10))"
  )


  GLOBAL$dailyCounties <- unique(resRecentLine$location)
  names(GLOBAL$dailyCounties) <- unique(resRecentLine$locationName)
  GLOBAL$weeklyCounties <- unique(resYearLine$location)
  names(GLOBAL$weeklyCounties) <- unique(resYearLine$locationName)
  GLOBAL$weeklyWkyr <- rev(unique(resYearLine$wkyr))
  GLOBAL$outbreakswkyr <- unique(outbreaks$dk$wkyr)

  GLOBAL$municipToCounty <- unique(resYearLineMunicip[, c("location", "locationName", "county"), with = F])

  GLOBAL$resRecentLineStack <- unique(resRecentLine[, c("type", "location", "age"), with = F])
  GLOBAL$resYearLineStack <- unique(resYearLine[, c("type", "location", "age"), with = F])
  GLOBAL$resYearLineMunicipStack <- unique(resYearLineMunicip[, c("type", "location", "age", "county"), with = F])

  GLOBAL$weeklyValues <- c(
    "Konsultasjoner" = "consults",
    "1 uke eksess" = "excess1"
  )

  saveRDS(GLOBAL, "/data_app/sykdomspuls/GLOBAL.RDS")

  RMariaDB::dbDisconnect(db)
}
