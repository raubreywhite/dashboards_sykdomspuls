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

CONFIG$DB_DRIVER <- Sys.getenv("DB_DRIVER", "MySQL")
CONFIG$DB_SERVER <- Sys.getenv("DB_SERVER", "db")
CONFIG$DB_DB <- Sys.getenv("DB_DB", "sykdomspuls")
CONFIG$DB_PORT <- as.integer(Sys.getenv("DB_PORT", 3306))
CONFIG$DB_USER <- Sys.getenv("DB_USER", "root")
CONFIG$DB_PASSWORD <- Sys.getenv("DB_PASSWORD", "example")

CONFIG$DB_CONFIG <- list(
  driver = CONFIG$DB_DRIVER,
  server = CONFIG$DB_SERVER,
  port = CONFIG$DB_PORT,
  user = CONFIG$DB_USER,
  password = CONFIG$DB_PASSWORD,
  db = CONFIG$DB_DB
)

CONFIG$MEM <- rbind(
 data.table(
    tag = "influensa",
    syndrome = "influensa",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list("Legekontakt"),
    syndromeOrConsult = "syndrome",
    denominator = "consult_with_influenza",
    weeklyDenominatorFunction = "sum",
    namesLong = "Influensa",
    namesShort = "Influensa",
    excludeSeason = c("2009/2010")
  )
)


CONFIG$BAYESIAN <- CONFIG$STANDARD<- rbind(
  data.table(
    tag = "gastro",
    syndrome = "gastro",
    alertInternal = TRUE,
    alertExternal = TRUE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
    weeklyDenominatorFunction = "sum",
    namesLong = "\u00D8vre-luftvei diagnose",
    namesShort = "Luftvei"
  ),
  data.table(
    tag = "influensa",
    syndrome = "influensa",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list("Legekontakt"),
    syndromeOrConsult = "syndrome",
    denominator = "consult_with_influenza",
    weeklyDenominatorFunction = "sum",
    namesLong = "Influensa",
    namesShort = "Influensa"
  ),
  data.table(
    tag = "lungebetennelse",
    syndrome = "lungebetennelse",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
    weeklyDenominatorFunction = "sum",
    namesLong = "Bronkitt diagnose",
    namesShort = "Bronkitt"
  ),
  data.table(
    tag = "consult_with_influenza",
    syndrome = "consult_with_influenza",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "consult",
    denominator = "pop",
    weeklyDenominatorFunction = "mean",
    namesLong = "consult_with_influenza",
    namesShort = "ConsWithInf"
  ),
  data.table(
    tag = "consult_without_influenza",
    syndrome = "consult_without_influenza",
    alertInternal = FALSE,
    alertExternal = FALSE,
    websiteInternal = FALSE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "consult",
    denominator = "pop",
    weeklyDenominatorFunction = "mean",
    namesLong = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
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
    denominator = "consult_without_influenza",
    weeklyDenominatorFunction = "sum",
    namesLong = "emerg5 diagnose",
    namesShort = "emerg5"
  )

)




CONFIG$MODELS <- list("mem" = CONFIG$MEM, "standard"= CONFIG$STANDARD)


CONFIG$SYNDROMES <- data.table(tag = character(),
                               syndrome = character(),
                               syndromeOrConsult = character(),
                               namesLong = character(),
                               namesShort = character(),
                               alertInternal = logical(),
                               websiteInternal = logical(),
                               alertExternal = logical(),
                               contactType = list())
for(model in CONFIG$MODELS){
  for(i in 1:nrow(model)){
    config = model[i]
    sub_config = config[, names(CONFIG$SYNDROMES), with=FALSE]
    if(config$tag %in% CONFIG$SYNDROMES[, tag]){
      if(! isTRUE(all.equal(CONFIG$SYNDROMES[tag==config$tag], sub_config))){
        stop(paste("tag, syndromeOrConsult, namesLong, namesShort, alertInternal, alertExternal, websiteInternal and contactType needs to be the same for the same syndrom across multiple models. It was not the same for", config$tag))
        }

    } else {
      CONFIG$SYNDROMES <- rbind(CONFIG$SYNDROMES, sub_config)

    }

  }
}


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
  "granularity_geo",
  "county_code",
  "location_code",
  "age",
  "date",
  "holiday",
  "n",
  "consult_without_influenza",
  "consult_with_influenza",
  "pop"
)

VARS$REQ_DATA_ANALYSIS <- c(
  "age",
  "date",
  "municip",
  "n",
  "consult_with_influenza",
  "consult_without_influenza",
  "pop"
)

VARS$REQ_RESULTS_BASIC <- c(
  "yrwk",
  "year",
  "week",
  "x",
  "date",
  "holiday",
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
  "failed",
  "uuid"
)

VARS$REQ_RESULTS_FULL <- c(
  "v",
  "granularity_time",
  "granularity_geo",
  "tag",
  "type",
  "county_code",
  "location_code",
  "location_name",
  "age",
  "status",
  VARS$REQ_RESULTS_BASIC,
  "file"
)

