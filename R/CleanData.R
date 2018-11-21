#' Format the raw data
#' @param d Raw data
#' @param syndrome syndrome of interest
#' @param population Population dataset
#' @param hellidager Hellidager dataset
#' @param testIfHelligdagIndikatorFileIsOutdated Boolean. Test if the current date is older than the last hellidag recorded in the fiel?
#' @param removeMunicipsWithoutConsults Boolean. Remove municipalities that do not have any consultations?
#' @import data.table
#' @importFrom lubridate today
#' @export CleanData
CleanData <- function(d,
                      syndrome,
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
  n <- NULL
  granularityGeo <- NULL
  HelligdagIndikator <- NULL
  county <- NULL
  location <- NULL
  # end

  if (!ValidateDataRaw(d)) {
    fhi::DashboardMsg("RAW data not validated", type = "err")
  }

  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  d[, consultWithoutInfluensa := consult - influensa]
  setnames(d, "consult", "consultWithInfluensa")

  syndromeAndConsult <- unique(c(
    syndrome,
    "consultWithInfluensa",
    "consultWithoutInfluensa"
  ))

  d <- d[municip != "municip9999",
         lapply(.SD, sum),
         by = .(age, date, municip),
         .SDcols = syndromeAndConsult]

  dateMin <- min(d$date)
  dateMax <- max(d$date)
  if (removeMunicipsWithoutConsults) {
    d[, total := sum(consultWithInfluensa, na.rm = T), by = municip]
    d <- d[is.finite(total)]
    d <- d[total > 0]
    d[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        unique(norwayMunicipMerging[municipEnd %in% unique(d$municip) |
                                      municip %in% unique(d$municip)]$municip),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        unique(norwayMunicipMerging$municip),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  }
  setnames(skeleton, c("municip", "age", "date"))
  skeleton[, date := data.table::as.IDate(date)]
  data <-
    merge(skeleton,
          d,
          by = c("municip", "age", "date"),
          all.x = TRUE)

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[municip != "municip9999",
                lapply(.SD, sum),
                keyby = .(date, municip),
                .SDcols = syndromeAndConsult]
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
  data <-
    merge(data,
          norwayMunicipMerging[, c("municip", "year", "municipEnd")],
          by = c("municip", "year"),
          all.x = T)
  dim(data)
  data <- data[!is.na(municipEnd)]

  n1 <- nrow(data)
  data <- merge(data, population, by = c("municip", "age", "year"))
  n2 <- nrow(data)

  if (n1 != n2)
    fhi::DashboardMsg("Population file not merging correctly", type = "err")

  data <- data[!is.na(municipEnd),
               lapply(.SD, sum),
               keyby = .(municipEnd, year, age, date),
               .SDcols = c(syndromeAndConsult, "pop")]
  dim(data)
  setnames(data, "municipEnd", "municip")

  # merging in municipalitiy-fylke names
  data <-
    merge(data, norwayLocations[, c("municip", "county")], by = "municip")
  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, municip := as.character(municip)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated &
      lubridate::today() > max(hellidager$date)) {
    fhi::DashboardMsg("HELLIGDAGER NEEDS UPDATING",type="err")
  }
  dim(data)
  data <- merge(data, hellidager, by = "date")
  dim(data)

  data[, year := NULL]

  setnames(data, syndrome, "n")

  if (!"consultWithInfluensa" %in% names(data))
    data[, consultWithInfluensa := n]
  if (!"consultWithoutInfluensa" %in% names(data))
    data[, consultWithoutInfluensa := n]

  setcolorder(data, unique(
    c(
      "date",
      "HelligdagIndikator",
      "county",
      "municip",
      "age",
      "n",
      "consultWithoutInfluensa",
      "consultWithInfluensa",
      "pop"
    )
  ))

  setorder(data, municip, age, date)
  setkey(data, municip, age, date)

  data[, granularityGeo := "municip"]
  setnames(data, "municip", "location")

  # create fylke
  fylke <- data[, .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consultWithoutInfluensa = sum(consultWithoutInfluensa),
    consultWithInfluensa = sum(consultWithInfluensa),
    pop = sum(pop)
  ), keyby = .(county, age, date)]

  fylke[, location := county]
  fylke[, granularityGeo := "county"]

  # create national
  norge <- data[, .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consultWithoutInfluensa = sum(consultWithoutInfluensa),
    consultWithInfluensa = sum(consultWithInfluensa),
    pop = sum(pop)
  ), keyby = .(age, date)]

  norge[, location := "Norge"]
  norge[, county := location]
  norge[, granularityGeo := "national"]

  data <- rbind(data, fylke, norge)
  setcolorder(data, c("granularityGeo", "county", "location", "age", "date"))
  setorderv(data, c("granularityGeo", "county", "location", "age", "date"))
  setkey(data, location, age)

  if (!ValidateDataClean(data)) {
    fhi::DashboardMsg("Clean data not validated", type = "err")
  }

  return(data)
}


#' Create analysis stack and datasets for a tag
#'
#' Given one row from \code{CONFIG$SYNDROMES} we need
#' to generate an analysis stack and all relevant datasets
#' that can be directly sent to \code{QuasipoissonTrainPredictData}
#' without additional formatting.
#' @param conf A row from \code{CONFIG$SYNDROMES}
#' @export StackAndEfficientDataForAnalysis
StackAndEfficientDataForAnalysis <- function(conf) {
  . <- NULL
  granularityGeo <- NULL
  weeklyDenominatorFunction <- NULL
  v <- NULL
  tag <- NULL
  granularity <- NULL
  location <- NULL
  id <- NULL

  data <- readRDS(file = fhi::DashboardFolder(
    "data_clean",
    sprintf("%s_%s_cleaned.RDS",LatestRawID(),conf$tag)))

  counties <- unique(data[granularityGeo=="municip"]$county)
  municips <- unique(data[granularityGeo=="municip"]$location)
  locations <- c("Norge", counties, municips)

  ages <- unique(data$age)

  # setting control stack for counties
  analysesCounties <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location = c("Norge", counties),
      age = ages,
      granularity = c("Daily", "Weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesCounties[,weeklyDenominatorFunction := list(conf$weeklyDenominatorFunction)]
  analysesCounties[, v := sykdomspuls::CONFIG$VERSION]
  analysesCounties[, file := sprintf("%s_%s.RDS","resRecentLine",tag)]
  analysesCounties[granularity=="Weekly", file := sprintf("%s_%s.RDS","resYearLine",tag)]

  # setting control stack for municipalities
  analysesMunicips <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location = municips,
      age = ages,
      granularity = c("Weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesMunicips[,weeklyDenominatorFunction := list(conf$weeklyDenominatorFunction)]
  analysesMunicips[, v := sykdomspuls::CONFIG$VERSION]
  analysesMunicips[, file := sprintf("%s_%s.RDS","resYearLineMunicip",tag)]

  # control stack for comparison of models
  analysesComparison <-
    vector("list", length(sykdomspuls::CONFIG$VERSIONS))
  for (vx in sykdomspuls::CONFIG$VERSIONS) {
    temp <-
      analysesCounties[location == "Norge" & granularity == "Weekly"]
    temp[, v := vx]
    analysesComparison[[vx]] <- copy(temp)
  }
  analysesComparison <- rbindlist(analysesComparison)
  analysesComparison[, file := sprintf("%s_%s.RDS","resComparisons",tag)]

  analyses <- rbind(analysesCounties,analysesMunicips,analysesComparison)
  if(fhi::DashboardIsDev()){
    analyses[,id:=1:.N,by=.(file)]
    analyses <- analyses[id %in% 1:100 | location=="Norge"]
    analyses[,id:=NULL]
  }

  return(
    list(
      conf = conf,
      data = data,
      counties = counties,
      municips = municips,
      locations = locations,
      ages = ages,
      analyses = analyses,
      analysesCounties = analysesCounties,
      analysesMunicips = analysesMunicips,
      analysesComparison = analysesComparison
    )
  )
}
