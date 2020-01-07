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
    age = jsonlite::toJSON(list("Totalt" = c("Totalt"))),
    weeklyDenominatorFunction = "sum",
    multiplicative_factor = 100,
    namesLong = "Influensa",
    include_as_syndromes = TRUE,
    folder_name = "influensa",
    namesShort = "Influensa",
    excludeSeason = c("2009/2010"),
    mem_outputs = list(c("charts", "county_sheet", "region_sheet", "norway_sheet")),
    icpc2 = list("R80")
  ),
  data.table(
    tag = "influensa_all",
    syndrome = "influensa_all",
    alertInternal = TRUE,
    alertExternal = FALSE,
    websiteInternal = TRUE,
    contactType = list(c("Legekontakt", "Telefonkontakt")),
    syndromeOrConsult = "syndrome",
    denominator = "consult_with_influenza",
    age = jsonlite::toJSON(list(
      "0-4" = c("0-4"), "5-14" = c("5-14"),
      "15-64" = c("15-19", "20-29", "30-64"), "65+" = c("65+")
    )),
    weeklyDenominatorFunction = "sum",
    multiplicative_factor = 100,
    include_as_syndromes = TRUE,
    folder_name = "influensa",
    namesLong = "Influensa",
    namesShort = "Influensa",
    excludeSeason = c("2009/2010"),
    mem_outputs = list(c("n_doctors_sheet")),
    icpc2 = list("R80")
  ) ## ,
  ## data.table(
  ##   tag = "gastro",
  ##   syndrome = "gastro",
  ##   alertInternal = TRUE,
  ##   alertExternal = TRUE,
  ##   websiteInternal = TRUE,
  ##   age = jsonlite::toJSON(list("Totalt" = c("Totalt"), "0-4"=c("0-4"))),
  ##   icpc2 = list("D13"),
  ##   multiplicative_factor = 100,
  ##   mem_outputs=list(c("charts")),
  ##   contactType = list(c("Legekontakt", "Telefonkontakt")),
  ##   syndromeOrConsult = "syndrome",
  ##   denominator = "consult_without_influenza",
  ##   weeklyDenominatorFunction = "sum",
  ##   namesLong = "Mage-tarminfeksjoner",
  ##   namesShort = "Mage-tarm",
  ##   folder_name = "gastro",
  ##   excludeSeason = c(""),
  ##   include_as_syndromes = FALSE

  ## ),
  ## data.table(
  ##   tag = "gastro-N",
  ##   syndrome = "gastro",
  ##   alertInternal = TRUE,
  ##   alertExternal = TRUE,
  ##   websiteInternal = TRUE,
  ##   age = jsonlite::toJSON(list("Totalt" = c("Totalt"))),
  ##   icpc2 = list("D13"),
  ##   multiplicative_factor = 1,
  ##   mem_outputs=list(c("charts")),
  ##   contactType = list(c("Legekontakt", "Telefonkontakt")),
  ##   syndromeOrConsult = "syndrome",
  ##   denominator = "consult_without_influenza",
  ##   weeklyDenominatorFunction = "return_one",
  ##   namesLong = "Mage-tarminfeksjoner",
  ##   namesShort = "Mage-tarm",
  ##   excludeSeason = c(""),
  ##   folder_name = "gastro-N",
  ##   include_as_syndromes = FALSE
  ## )
)


CONFIG$BAYESIAN <- CONFIG$STANDARD <- rbind(
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
    namesLong = "Mage-tarminfeksjoner",
    namesShort = "Mage-tarm",
    include_as_syndromes = TRUE
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
    namesLong = "Luftveisinfeksjoner",
    namesShort = "Luftvei",
    include_as_syndromes = TRUE
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
    namesLong = "Luftveisinfeksjoner",
    namesShort = "Luftvei",
    include_as_syndromes = TRUE
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
    namesShort = "Influensa",
    include_as_syndromes = TRUE
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
    namesShort = "Lungebet",
    include_as_syndromes = TRUE
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
    namesShort = "Bronkitt",
    include_as_syndromes = TRUE
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
    namesShort = "ConsWithInf",
    include_as_syndromes = TRUE
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
    namesShort = "ConsWOInf",
    include_as_syndromes = TRUE
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
    namesShort = "Skabb",
    include_as_syndromes = TRUE
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
    namesShort = "emerg1",
    include_as_syndromes = TRUE
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
    namesShort = "emerg2",
    include_as_syndromes = TRUE
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
    namesShort = "emerg3",
    include_as_syndromes = TRUE
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
    namesShort = "emerg4",
    include_as_syndromes = TRUE
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
    namesShort = "emerg5",
    include_as_syndromes = TRUE
  )
)




CONFIG$MODELS <- list("mem" = CONFIG$MEM, "standard" = CONFIG$STANDARD)


CONFIG$SYNDROMES <- data.table(
  tag = character(),
  syndrome = character(),
  syndromeOrConsult = character(),
  namesLong = character(),
  namesShort = character(),
  alertInternal = logical(),
  websiteInternal = logical(),
  alertExternal = logical(),
  contactType = list()
)
for (model in CONFIG$MODELS) {
  for (i in 1:nrow(model)) {
    config <- model[i]
    if (config$include_as_syndromes) {
      sub_config <- config[, names(CONFIG$SYNDROMES), with = FALSE]
      if (config$tag %in% CONFIG$SYNDROMES[, tag]) {
        if (!isTRUE(all.equal(CONFIG$SYNDROMES[tag == config$tag], sub_config))) {
          stop(paste("tag, syndromeOrConsult, namesLong, namesShort, alertInternal, alertExternal, websiteInternal and contactType needs to be the same for the same syndrom across multiple models. It was not the same for", config$tag))
        }
      } else {
        CONFIG$SYNDROMES <- rbind(CONFIG$SYNDROMES, sub_config)
      }
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
