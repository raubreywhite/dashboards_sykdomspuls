#' Format the raw data
#' @param d Raw data
#' @param syndrome syndrome of interest
#' @param population Population dataset
#' @param hellidager Hellidager dataset
#' @param testIfHelligdagIndikatorFileIsOutdated Boolean. Test if the current date is older than the last hellidag recorded in the fiel?
#' @param removeMunicipsWithoutConsults Boolean. Remove municipalities that do not have any consultations?
#' @import data.table
#' @export CleanData
CleanData <- function(d,
                      syndrome,
                      population = fd::norway_population(),
                      hellidager = fhidata::norway_dates_holidays,
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
    fd::msg("RAW data not validated", type = "err", slack = TRUE)
  }

  if (!"IDate" %in% class(d$date)) {
    d[, date := data.table::as.IDate(date)]
  }

  d[, consult_without_influenza := consult - influensa]
  setnames(d, "consult", "consult_with_influenza")

  syndromeAndConsult <- unique(c(
    syndrome,
    "consult_with_influenza",
    "consult_without_influenza"
  ))

  d <- d[municip != "municip9999",
    lapply(.SD, sum),
    by = .(age, date, municip),
    .SDcols = syndromeAndConsult
  ]

  dateMin <- min(d$date)
  dateMax <- max(d$date)
  if (removeMunicipsWithoutConsults) {
    d[, total := sum(consult_with_influenza, na.rm = T), by = municip]
    d <- d[is.finite(total)]
    d <- d[total > 0]
    d[, total := NULL]
    skeleton <-
      data.table(expand.grid(
        municip = unique(fd::norway_municip_merging()[municip_code_current %in% unique(d$municip) |
          municip_code_original %in% unique(d$municip)]$municip_code_original),
        unique(d$age),
        seq.Date(dateMin, dateMax, 1)
      ))
  } else {
    skeleton <-
      data.table(expand.grid(
        municip = unique(fd::norway_municip_merging()$municip_code_original),
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
      fd::norway_municip_merging()[, c("municip_code_original", "year", "municip_code_current", "weighting")],
      by.x = c("municip", "year"),
      by.y = c("municip_code_original", "year"),
      all.x = T,
      allow.cartesian = T
    )
  dim(data)
  data <- data[!is.na(municip_code_current)]

  # apply the weighting
  for (i in syndromeAndConsult) {
    data[, (i) := get(i) * weighting]
  }

  data <- data[!is.na(municip_code_current),
    lapply(.SD, sum),
    keyby = .(municip_code_current, year, age, date),
    .SDcols = c(syndromeAndConsult)
  ]

  # apply ceiling to ensure we have whole numbers after weighting
  for (i in syndromeAndConsult) {
    data[, (i) := ceiling(get(i))]
  }
  dim(data)
  setnames(data, "municip_code_current", "municip")

  # merge in population
  n1 <- nrow(data)
  data <- merge(data, population,
    by.x = c("municip", "age", "year"),
    by.y = c("location_code", "age", "year")
  )
  n2 <- nrow(data)

  if (n1 != n2) {
    fd::msg("Population file not merging correctly", type = "err", slack = T)
  }

  # merging in municipalitiy-fylke names
  data[fd::norway_locations(), on = "municip==municip_code", county := county_code]

  for (i in syndromeAndConsult) {
    data[is.na(get(i)), (i) := 0]
  }

  data <- data[date >= data.table::as.IDate("2006-01-01")]
  data[, municip := as.character(municip)]

  setnames(hellidager, c("date", "HelligdagIndikator"))
  hellidager[, date := data.table::as.IDate(date)]
  if (testIfHelligdagIndikatorFileIsOutdated &
    lubridate::today() > max(hellidager$date)) {
    fd::msg("HELLIGDAGER NEEDS UPDATING", type = "err", slack = T)
  }
  data[hellidager, on = "date", HelligdagIndikator := HelligdagIndikator]
  data[is.na(HelligdagIndikator), HelligdagIndikator := FALSE]

  data[, year := NULL]

  setnames(data, syndrome, "n")

  if (!"consult_with_influenza" %in% names(data)) {
    data[, consult_with_influenza := n]
  }
  if (!"consult_without_influenza" %in% names(data)) {
    data[, consult_without_influenza := n]
  }

  setcolorder(data, unique(
    c(
      "date",
      "HelligdagIndikator",
      "county",
      "municip",
      "age",
      "n",
      "consult_without_influenza",
      "consult_with_influenza",
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
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(county, age, date)]

  fylke[, location := county]
  fylke[, granularityGeo := "county"]

  # create national
  norge <- data[, .(
    HelligdagIndikator = mean(HelligdagIndikator),
    n = sum(n),
    consult_without_influenza = sum(consult_without_influenza),
    consult_with_influenza = sum(consult_with_influenza),
    pop = sum(pop)
  ), keyby = .(age, date)]

  norge[, location := "norge"]
  norge[, county := location]
  norge[, granularityGeo := "national"]

  data <- rbind(data, fylke, norge)
  setcolorder(data, c("granularityGeo", "county", "location", "age", "date"))
  setorderv(data, c("granularityGeo", "county", "location", "age", "date"))
  setkey(data, location, age)

  setnames(data, c(
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
  ))

  if (!ValidateDataClean(data)) {
    fd::msg("Clean data not validated", type = "err")
  }

  return(data)
}
