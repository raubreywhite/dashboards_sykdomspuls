#' variablesAlgorithmDaily
#' @export variablesAlgorithmDaily
variablesAlgorithmDaily <- c("date")

#' variablesAlgorithmWeekly
#' @export variablesAlgorithmWeekly
variablesAlgorithmWeekly <- c("displayDay", "wkyr", "x", "year","week")

#' variablesAlgorithmBasic
#' @export variablesAlgorithmBasic
variablesAlgorithmBasic <- c("n","consult","pop","HelligdagIndikator")

#' variablesAlgorithmProduced
#' @export variablesAlgorithmProduced
variablesAlgorithmProduced <- c("threshold0","threshold2","threshold4","threshold6","cumE1","cumL1","cumU1","zscore","failed")

#' variablesPostProcessing
#' @export variablesPostProcessing
variablesPostProcessing <- c("age","type","location","locationName","status")

#' variablesMunicip
#' @export variablesMunicip
variablesMunicip <- c("county")



#' CONFIG
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$VERSION <- 1
CONFIG$VERSIONS <- 1:2
CONFIG$SYNDROMES_DOCTOR <- c("Influensa"="influensa")
CONFIG$SYNDROMES_ALL <- c("Mage-tarm diagnose"="gastro",
                          "\u00D8vre-luftvei diagnose"="respiratoryinternal",
                          "\u00D8vre-luftvei diagnose"="respiratoryexternal",
                          "Lungebetennelse diagnose"="lungebetennelse",
                          "Bronkitt diagnose"="bronkitt")

CONFIG$SYNDROMES <- c(CONFIG$SYNDROMES_DOCTOR, CONFIG$SYNDROMES_ALL)

CONFIG$SYNDROMES_ALERT_INTERNAL <- c(
  "influensa",
  "gastro",
  "respiratoryinternal",
  "lungebetennelse",
  "bronkitt")

CONFIG$SYNDROMES_ALERT_EXTERNAL <- c(
  "gastro",
  "respiratoryexternal")

CONFIG$SYNDROMES_SHORT <- c(
  "Influensa"="influensa",
  "Mage-tarm"="gastro",
  "Luftvei"="respiratoryinternal",
  "Luftvei"="respiratoryexternal",
  "Lungebet"="lungebetennelse",
  "Bronkitt"="bronkitt"
  )

# remove any excess short syndromes
CONFIG$SYNDROMES_SHORT <- CONFIG$SYNDROMES_SHORT[CONFIG$SYNDROMES_SHORT %in% CONFIG$SYNDROMES]

CONFIG$AGES <- c(
  "Totalt",
  "0-4",
  "5-14",
  "15-19",
  "20-29",
  "30-64",
  "65+"
)

#' norwayLocations
#' @export norwayLocations
norwayLocations <- GenNorwayLocations()

#' norwayMunicipMerging
#' @export norwayMunicipMerging
norwayMunicipMerging <- GenNorwayMunicipMerging()

#' displayDays
#' @export displayDays
displayDays <- data.table(day = seq.Date(as.Date("2000-01-01"), as.Date("2030-01-01"), by = "days"))
displayDays[, wkyr := format.Date(day, format = "%G-%V")]
displayDays <- displayDays[, .(displayDay = max(day)), by = .(wkyr)]
setkey(displayDays,wkyr)
