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
#' @format An environment containing 7 variables:
#' \describe{
#'   \item{verbose}{Verbose?.}
#'   \item{VERSION}{The version that we are currently running.}
#'   \item{VERSIONS}{All available versions.}
#'   \item{SYNDROMES}{A data.table of all the syndromes/analyses that will be run}
#'   \item{tagsWithLong}{A convenience vector used to switch between tags and pretty names}
#'   \item{AGES}{The age groups that we run analyses on.}
#'   \item{smallMunicips}{Small municipalities that need to be censored.}
#' }
#' @export CONFIG
CONFIG <- new.env(parent = emptyenv())
CONFIG$verbose <- FALSE
CONFIG$VERSION <- 1
CONFIG$VERSIONS <- 1:2

CONFIG$SYNDROMES <- rbind(
  data.table(
    tag = "influensa",
    syndrome = "influensa",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list("Legekontakt"),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "Influensa",
    namesShort = "Influensa"
  ),
  data.table(
    tag = "gastro",
    syndrome = "gastro",
    alertInternal = TRUE,
    alertExternal = TRUE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "Mage-tarm diagnose",
    namesShort = "Mage-tarm"
  ),
  data.table(
    tag = "respiratoryinternal",
    syndrome = "respiratoryinternal",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "\u00D8vre-luftvei diagnose",
    namesShort = "Luftvei"
  ),
  data.table(
    tag = "respiratoryexternal",
    syndrome = "respiratoryexternal",
    alertInternal = FALSE,
    alertExternal = TRUE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "\u00D8vre-luftvei diagnose",
    namesShort = "Luftvei"
  ),
  data.table(
    tag = "lungebetennelse",
    syndrome = "lungebetennelse",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "Lungebetennelse diagnose",
    namesShort = "Lungebet"
  ),
  data.table(
    tag = "bronkitt",
    syndrome = "bronkitt",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "Bronkitt diagnose",
    namesShort = "Bronkitt"
  ),
  data.table(
    tag = "consultWithInfluensa",
    syndrome = "consultWithInfluensa",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "consult",
    denominator = "pop",
    weeklyDenominatorFunction = "mean",
    namesLong = "consultWithInfluensa",
    namesShort = "ConsWithInf"
  ),
  data.table(
    tag = "consultWithoutInfluensa",
    syndrome = "consultWithoutInfluensa",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "consult",
    denominator = "pop",
    weeklyDenominatorFunction = "mean",
    namesLong = "consultWithoutInfluensa",
    namesShort = "ConsWOInf"
  ),
  data.table(
    tag = "skabb",
    syndrome = "skabb",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "Skabb diagnose",
    namesShort = "Skabb"
  ),
  data.table(
    tag = "emerg1",
    syndrome = "emerg1",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg1 diagnose",
    namesShort = "emerg1"
  ),
  data.table(
    tag = "emerg2",
    syndrome = "emerg2",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg2 diagnose",
    namesShort = "emerg2"
  ),
  data.table(
    tag = "emerg3",
    syndrome = "emerg3",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg3 diagnose",
    namesShort = "emerg3"
  ),
  data.table(
    tag = "emerg4",
    syndrome = "emerg4",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg4 diagnose",
    namesShort = "emerg4"
  ),
  data.table(
    tag = "emerg5",
    syndrome = "emerg5",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consultWithoutInfluensa",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg5 diagnose",
    namesShort = "emerg5"
  )
)

CONFIG$tagsWithLong <- CONFIG$SYNDROMES$tag
names(CONFIG$tagsWithLong) <- CONFIG$SYNDROMES$namesLong


CONFIG$AGES <- list(
  "Totalt" = c(0:105),
  "0-4" = c(0:4),
  "5-14" = c(5:14),
  "15-19" = c(15:19),
  "20-29" = c(20:29),
  "30-64" = c(30:64),
  "65+" = c(65:105)
)

CONFIG$smallMunicips <- c(
  "municip1151",
  "municip1835",
  "municip1252",
  "municip1739"
)

CONFIG$checkedOutOfDate <- FALSE
CONFIG$outOfDate <- list()
CONFIG$outOfDate[["norwayPopulation"]] <- TRUE
CONFIG$outOfDate[["norwayLocations"]] <- TRUE
CONFIG$outOfDate[["norwayMunicipMerging"]] <- TRUE

#' Global variables used for defining formats of data structures
#'
#' This environment holds a number of variables that are used for
#' defining the formats of data structures.
#'
#' @format An environment containing 5 variables:
#' \describe{
#'   \item{REQ_DATA_RAW}{Required columns for raw data.}
#'   \item{REQ_DATA_CLEAN}{Required columns for clean data.}
#'   \item{REQ_DATA_ANALYSIS}{Required columns for data analysis.}
#'   \item{REQ_RESULTS_BASIC}{Required columns for the results (basic, right after the analysis is run)}
#'   \item{REQ_RESULTS_FULL}{Required columns for the full results (i.e. those that will be saved and used further)}
#' }
#' @export VARS
VARS <- new.env(parent = emptyenv())

VARS$REQ_DATA_RAW <- c(
  "age",
  "date",
  "Kontaktype",
  "Praksis",
  "municip",
  unique(CONFIG$SYNDROMES[syndromeOrConsult == "syndrome"]$syndrome),
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
  "purpose",
  "v",
  "granularity_time",
  "tag",
  "type",
  # "county",
  "location",
  # "locationName",
  "age",
  "status",
  VARS$REQ_RESULTS_BASIC,
  "file",
  "uuid"
)


#' The last date for each isoweek
#' @export displayDays
displayDays <- data.table(day = seq.Date(as.IDate("2000-01-01"), as.IDate("2030-01-01"), by = "days"))
displayDays[, wkyr := format.Date(day, format = "%G-%V")]
displayDays <- displayDays[, .(displayDay = as.IDate(max(day))), by = .(wkyr)]
setkey(displayDays, wkyr)

#' Environment for progress bars
#' @export PB
PB <- new.env(parent = emptyenv())
PB$i <- 0
PB$pb <- RAWmisc::ProgressBarCreate(max = 1)
