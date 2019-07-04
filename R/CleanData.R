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
                      population = fhidata::norway_population_current,
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

  # fix population age categories
  for (i in which(names(CONFIG$AGES) != "Totalt")) {
    population[age %in% CONFIG$AGES[[i]], agex := names(CONFIG$AGES)[i]]
  }
  population[, age := NULL]
  setnames(population, "agex", "age")

  population <- population[, .(
    pop = sum(pop)
  ), keyby = .(
    location_code, age, year
  )]

  total <- population[, .(
    pop = sum(pop)
  ), keyby = .(
    location_code, year
  )]
  total[, age := "Totalt"]

  population <- rbind(population, total)
  # end population fix

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
    .SDcols = syndromeAndConsult
  ]

  dateMin <- min(d$date)
  dateMax <- max(d$date)
  if (removeMunicipsWithoutConsults) {
    d[, total := sum(consultWithInfluensa, na.rm = T), by = municip]
    d <- d[is.finite(total)]
    d <- d[total > 0]
    d[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        municip = unique(fhidata::norway_municip_merging[municip_code_current %in% unique(d$municip) |
          municip_code_original %in% unique(d$municip)]$municip_code_original),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        municip = unique(fhidata::norway_municip_merging$municip_code_original),
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
      all.x = TRUE
    )

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  total <- data[municip != "municip9999",
    lapply(.SD, sum),
    keyby = .(date, municip),
    .SDcols = syndromeAndConsult
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
  data <-
    merge(data,
          fhidata::norway_municip_merging[, c("municip_code_original", "year", "municip_code_current")],
          by.x = c("municip", "year"),
          by.y = c("municip_code_original", "year"),
      all.x = T
    )
  dim(data)
  data <- data[!is.na(municip_code_current)]

  data <- data[!is.na(municip_code_current),
    lapply(.SD, sum),
    keyby = .(municip_code_current, year, age, date),
    .SDcols = c(syndromeAndConsult)
  ]
  dim(data)
  setnames(data, "municip_code_current", "municip")

  # merge in population
  n1 <- nrow(data)
  data <- merge(data, population,
                by.x = c("municip", "age", "year"),
                by.y = c("location_code", "age", "year"))
  n2 <- nrow(data)

  if (n1 != n2) {
    fhi::DashboardMsg("Population file not merging correctly", type = "err")
  }

  # merging in municipalitiy-fylke names
  data[fhidata::norway_locations_current,on = "municip==municip_code", county:=county_code]

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, municip := as.character(municip)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated &
    lubridate::today() > max(hellidager$date)) {
    fhi::DashboardMsg("HELLIGDAGER NEEDS UPDATING", type = "err")
  }
  dim(data)
  data <- merge(data, hellidager, by = "date")
  dim(data)

  data[, year := NULL]

  setnames(data, syndrome, "n")

  if (!"consultWithInfluensa" %in% names(data)) {
    data[, consultWithInfluensa := n]
  }
  if (!"consultWithoutInfluensa" %in% names(data)) {
    data[, consultWithoutInfluensa := n]
  }

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
#' @param data a dataset
#' @param schema a schema
#' @export load_stack_schema
load_stack_schema <- function(conf, data, schema) {
  . <- NULL
  granularityGeo <- NULL
  weeklyDenominatorFunction <- NULL
  v <- NULL
  tag <- NULL
  granularity <- NULL
  location <- NULL
  id <- NULL

  counties <- unique(data[granularityGeo == "municip"]$county)
  municips <- unique(data[granularityGeo == "municip"]$location)
  locations <- c("Norge", counties, municips)

  ages <- unique(data$age)

  years <- CalculateTrainPredictYearPattern(
    yearMin = lubridate::isoyear(min(data$date)),
    yearMax = lubridate::isoyear(max(data$date)),
    numPerYear1 = 200)

  unlist(years)

  # setting control stack for counties
  analysesCounties <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location = c("Norge", counties),
      age = ages,
      year_index = 1:length(years),
      granularity_time = c("daily", "weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesCounties[, weeklyDenominatorFunction := conf$weeklyDenominatorFunction]
  analysesCounties[, v := sykdomspuls::CONFIG$VERSION]
  analysesCounties[, purpose := "production"]
  analysesCounties[, file := sprintf("%s_%s.RDS", "resRecentLine", tag)]
  analysesCounties[granularity_time == "weekly", file := sprintf("%s_%s.RDS", "resYearLine", tag)]

  # setting control stack for municipalities
  analysesMunicips <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location = municips,
      age = ages,
      year_index = 1:length(years),
      granularity_time = c("weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesMunicips[, weeklyDenominatorFunction := conf$weeklyDenominatorFunction]
  analysesMunicips[, v := sykdomspuls::CONFIG$VERSION]
  analysesMunicips[, purpose := "production"]
  analysesMunicips[, file := sprintf("%s_%s.RDS", "resYearLineMunicip", tag)]

  # control stack for comparison of models
  analysesComparison <-
    vector("list", length(sykdomspuls::CONFIG$VERSIONS))
  for (vx in sykdomspuls::CONFIG$VERSIONS) {
    temp <-
      analysesCounties[location == "Norge" & granularity_time == "weekly"]
    temp[, v := vx]
    analysesComparison[[vx]] <- copy(temp)
  }
  analysesComparison <- rbindlist(analysesComparison)
  analysesComparison[, file := sprintf("%s_%s.RDS", "resComparisons", tag)]
  analysesComparison[, purpose := "comparison"]

  analyses <- rbind(analysesCounties, analysesMunicips, analysesComparison)
  for(i in seq_along(years)){
    analyses[year_index==i,year_train_min:=years[[i]]$yearTrainMin]
    analyses[year_index==i,year_train_max:=years[[i]]$yearTrainMax]
    analyses[year_index==i,year_predict_min:=years[[i]]$yearPredictMin]
    analyses[year_index==i,year_predict_max:=years[[i]]$yearPredictMax]
  }
  analyses[,uuid:=replicate(.N, uuid::UUIDgenerate(F))]
  analyses[,year_index:=NULL]

  dates <- data[, "date"]
  dates[, year := RAWmisc::YearN(date)]
  dates[, date_min := min(date), by=year]
  dates[, date_max := max(date), by=year]
  dates <- unique(dates[,c("year","date_min","date_max")])

  analyses[dates, on="year_train_min==year", date_train_min:=date_min]
  analyses[dates, on="year_train_max==year", date_train_max:=date_max]

  analyses[dates, on="year_predict_min==year", date_predict_min:=date_min]
  analyses[dates, on="year_predict_max==year", date_predict_max:=date_max]

  # fix schema

  schema$dt <- analyses

  schema$identify_dt_that_exists_in_db()
  schema$get_data_dt()[year_predict_max>=max(year_predict_max)-1,exists_in_db:=FALSE]

}

#' Create analysis stack and datasets for a tag inside a list
#'
#' Given one row from \code{CONFIG$SYNDROMES} we need
#' to generate an analysis stack and all relevant datasets
#' that can be directly sent to \code{QuasipoissonTrainPredictData}
#' without additional formatting into a list for use in \code{pbmcapply}
#' @param conf A row from \code{CONFIG$SYNDROMES}
#' @export
StackAndEfficientDataForAnalysisInList <- function(conf) {
  stackAndData <- StackAndEfficientDataForAnalysis(conf = conf)
  stackStrata <- stackAndData$analysesStrata
  stack <- stackAndData$analyses
  data <- stackAndData$data

  retval <- vector("list", length = nrow(stack))
  for (i in seq_along(retval)) {
    retval[[i]] <- list("stack" = stack[i], "data" = data[.(stack$location[i], stack$age[i])])
  }
  retval
}



#' Create analysis stack and datasets for a tag inside a list
#'
#' Given one row from \code{CONFIG$SYNDROMES} we need
#' to generate an analysis stack and all relevant datasets
#' that can be directly sent to \code{QuasipoissonTrainPredictData}
#' without additional formatting into a list for use in \code{pbmcapply}
#' @param conf A row from \code{CONFIG$SYNDROMES}
#' @export
schema_and_data <- function(conf){
  stackAndData <- StackAndEfficientDataForAnalysis(conf = conf)
  stackStrata <- stackAndData$analysesStrata
  stack <- stackAndData$analyses
  data <- stackAndData$data

  #fd:::get_field_types(conn, stack)

  schema$stack_x$dt <- stack

  schema$stack_x$identify_dt_that_exists_in_db()
  schema$stack_x$get_data_dt()[year_predict_max>=max(year_predict_max)-1,exists_in_db:=FALSE]

  return(data)

}
