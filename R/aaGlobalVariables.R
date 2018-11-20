#' Variables produced by the daily algorithm
#'
#' A variable containing the variables produced by the daily algorithm.
#' The variabels are as follows:
#'
#' @format A vector with one variable:
#' \describe{
#'   \item{date}{Date of the observation}
#' }


#' Configuration of analyses
#'
#' This environment holds a number of important variables for
#' configuring the analyses that will be run.
#'
#' @format An environment containing 9 variables:
#' \describe{
#'   \item{VERSION}{The version that we are currently running.}
#'   \item{VERSIONS}{All available versions.}
#'   \item{SYNDROMES_DOCTOR}{Syndromes for which we will only count consultations where a doctor was seen.}
#'   \item{SYNDROMES_ALL}{Syndromes for which we will count consultations where a doctor was seen and over the phone.}
#'   \item{SYNDROMES}{A combined vector of SYNDROMES_DOCTOR and SYNDROMES_ALL.}
#'   \item{SYNDROMES_ALERT_INTERNAL}{Syndromes for the internal FHI dashboard/alerts.}
#'   \item{SYNDROMES_ALERT_EXTERNAL}{Syndromes for the external dashboard/alerts.}
#'   \item{SYNDROMES_SHORT}{The short names of the syndromes.}
#'   \item{AGES}{The age groups that we run analyses on.}
#' }
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$VERSION <- 1
CONFIG$VERSIONS <- 1:2
CONFIG$SYNDROMES <- data.table(
  tag=c(
    "influensa",
    "gastro",
    "respiratoryinternal",
    "respiratoryexternal",
    "lungebetennelse",
    "bronkitt",
    "consultWithInfluensa",
    "consultWithoutInfluensa"
  ),
  syndrome=c(
    "influensa",
    "gastro",
    "respiratoryinternal",
    "respiratoryexternal",
    "lungebetennelse",
    "bronkitt",
    "consultWithInfluensa",
    "consultWithoutInfluensa"
  ),
  alertInternal=c(
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    FALSE
  ),
  alertExternal=c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
  contactType=list(
    "Legekontakt",
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt"),
    c("Legekontakt","Telefonkontakt")
  ),
  syndromeOrConsult=c(
    "syndrome",
    "syndrome",
    "syndrome",
    "syndrome",
    "syndrome",
    "syndrome",
    "consult",
    "consult"
  ),
  denominator=c(
    "consultWithInfluensa",
    "consultWithoutInfluensa",
    "consultWithoutInfluensa",
    "consultWithoutInfluensa",
    "consultWithoutInfluensa",
    "consultWithoutInfluensa",
    "pop",
    "pop"
  ),
  weeklyDenominatorFunction=c(
    sum,
    sum,
    sum,
    sum,
    sum,
    sum,
    mean,
    mean
  ),
  namesLong=c(
    "Influensa",
    "Mage-tarm diagnose",
    "\u00D8vre-luftvei diagnose",
    "\u00D8vre-luftvei diagnose",
    "Lungebetennelse diagnose",
    "Bronkitt diagnose",
    "consultWithInfluensa",
    "consultWithoutInfluensa"
  ),
  namesShort=c(
    "Influensa",
    "Mage-tarm",
    "Luftvei",
    "Luftvei",
    "Lungebet",
    "Bronkitt",
    "ConsWithInf",
    "ConsWOInf"
  )
)

CONFIG$tagsWithLong <- CONFIG$SYNDROMES$tag
names(CONFIG$tagsWithLong) <- CONFIG$SYNDROMES$namesLong


CONFIG$AGES <- list(
  "Totalt"=c(0:105),
  "0-4"=c(0:4),
  "5-14"=c(5:14),
  "15-19"=c(15:19),
  "20-29"=c(20:29),
  "30-64"=c(30:64),
  "65+"=c(65:105)
)

CONFIG$smallMunicips <- c(
  "municip1151",
  "municip1835",
  "municip1252",
  "municip1739"
)

#' Global variables used for defining formats of data structures
#'
#' This environment holds a number of variables that are used for
#' defining the formats of data structures.
#'
#' @format An environment containing 3 variables:
#' \describe{
#'   \item{REQ_DATA_RAW_STRUCTURAL}{Required columns for raw data that are structural in nature (i.e. these form the skeleton).}
#'   \item{REQ_DATA_RAW_OTHER}{Required columns for raw data that are not structural in nature and are not listed in `CONFIG$SYNDROMES`.}
#'   \item{REQ_DATA_RAW_ALL}{The combination of `REQ_DATA_RAW_STRUCTURAL` and `REQ_DATA_RAW_OTHER`}
#' }
#' @export VARS
VARS <- new.env(parent = emptyenv())

VARS$REQ_DATA_RAW <- c(
  "age",
  "date",
  "Kontaktype",
  "Praksis",
  "municip",
  unique(CONFIG$SYNDROMES[syndromeOrConsult=="syndrome"]$syndrome),
  "consult"
)

VARS$REQ_DATA_CLEAN <- c(
  "granularityGeo",
  "county",
  "location",
  "age",
  "date",
  "HelligdagIndikator",
  "n",
  "consultWithoutInfluensa",
  "consultWithInfluensa",
  "pop"
)

VARS$REQ_DATA_ANALYSIS <- c(
  "age",
  "date",
  "municip",
  "n",
  "consultWithInfluensa",
  "consultWithoutInfluensa",
  "pop"
)

VARS$REQ_RESULTS_BASIC <- c(
  "wkyr",
  "year",
  "week",
  "x",
  "date",
  "displayDay",
  "HelligdagIndikator",
  "n",
  "denominator",
  "threshold0",
  "threshold2",
  "threshold4",
  "threshold6",
  "zscore",
  "cumE1",
  "cumL1",
  "cumU1",
  "failed"
)

VARS$REQ_RESULTS_FULL <- c(
  "tag",
  "type",
  "county",
  "location",
  "locationName",
  "age",
  "status",
  VARS$REQ_RESULTS_BASIC,
  "file"
)

#' norwayLocations
#' @export norwayLocations
norwayLocations <- readRDS(system.file("createddata","norwayLocations.RDS",package="sykdomspuls"))

#' norwayMunicipMerging
#' @export norwayMunicipMerging
norwayMunicipMerging <- readRDS(system.file("createddata","norwayMunicipMerging.RDS",package="sykdomspuls"))

#' displayDays
#' @export displayDays
displayDays <- data.table(day = seq.Date(as.IDate("2000-01-01"), as.IDate("2030-01-01"), by = "days"))
displayDays[, wkyr := format.Date(day, format = "%G-%V")]
displayDays <- displayDays[, .(displayDay = as.IDate(max(day))), by = .(wkyr)]
setkey(displayDays, wkyr)

#' PB
#' @export PB
PB <- new.env(parent = emptyenv())
PB$i <- 0
PB$pb <- RAWmisc::ProgressBarCreate(max = 1)
