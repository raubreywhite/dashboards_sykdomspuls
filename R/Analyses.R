#' PROJ
#' @param yearMin a
#' @param yearMax a
#' @param numPerYear1 a
#' @export CalculateTrainPredictYearPattern
CalculateTrainPredictYearPattern <- function(yearMin, yearMax, numPerYear1 = 1) {
  perYear1 <- seq(yearMax - numPerYear1 + 1, yearMax, by = 1)
  perYear2 <- c((yearMin + 6):(yearMax - numPerYear1))
  perFixed <- c(yearMin:(yearMin + 5))
  if (length(perYear2) %% 2 == 1) {
    perYear1 <- c(max(perYear2), perYear1)
    perYear2 <- perYear2[-length(perYear2)]
  }

  years <- list()
  years[[1]] <- list(
    yearTrainMin = min(perFixed),
    yearTrainMax = max(perFixed - 1),
    yearPredictMin = min(perFixed),
    yearPredictMax = max(perFixed)
  )
  index <- 2
  for (i in 1:length(perYear2)) {
    if (i %% 2 == 0) next
    years[[index]] <- list(
      yearTrainMin = perYear2[i] - 5,
      yearTrainMax = perYear2[i] - 1,
      yearPredictMin = perYear2[i],
      yearPredictMax = perYear2[i] + 1
    )
    index <- index + 1
  }
  for (i in 1:length(perYear1)) {
    years[[index]] <- list(
      yearTrainMin = perYear1[i] - 5,
      yearTrainMax = perYear1[i] - 1,
      yearPredictMin = perYear1[i],
      yearPredictMax = perYear1[i]
    )
    index <- index + 1
  }
  return(years)
}

#' AddXToWeekly
#' @param data d
#' @import data.table
#' @export AddXToWeekly
AddXToWeekly <- function(data) {
  week <- NULL
  x <- NULL

  data[week >= 30, x := week - 29]
  data[week < 30, x := week + 23]

  return(data)
}

#' AddWkyrAndDisplayDateToWeekly
#' @param data d
#' @import data.table
#' @export AddWkyrAndDisplayDateToWeekly
AddWkyrAndDisplayDateToWeekly <- function(data) {
  . <- NULL
  wkyr <- NULL
  year <- NULL
  week <- NULL
  day <- NULL

  data[, wkyr := paste0(year, "-", formatC(week, flag = "0", width = 2))]
  data <- merge(data, displayDays, by = "wkyr")

  return(data)
}

#' DetermineStatus
#' @param data d
#' @import data.table
#' @export DetermineStatus
DetermineStatus <- function(data) {
  status <- NULL
  n <- NULL
  threshold2 <- NULL
  threshold4 <- NULL

  # create "normal", "medium", "high" categories
  data[, status := "Normal"]
  data[n > 1 & n > threshold2, status := "Medium"]
  data[n > 1 & n > threshold4, status := "High"]
}

#' AddCounty
#' @param data d
#' @param loc a
#' @import data.table
#' @export AddCounty
AddCounty <- function(data, loc) {
  county <- GetCountyFromMunicip(loc, norwayLocations = norwayLocations)
  if (county != loc) {
    data[, county := county]
  }
}

#' AnalyseYearLine
#' @param data a
#' @param v a
#' @importFrom RAWmisc YearN WeekN
#' @import data.table
#' @export AnalyseYearLine
AnalyseYearLine <- function(data, v) {
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  consult <- NULL
  pop <- NULL
  HelligdagIndikator <- NULL
  threshold2 <- NULL
  x <- NULL
  wkyr <- NULL
  day <- NULL
  # end

  yearMax <- as.numeric(format.Date(max(data$date), "%G"))
  yearMin <- as.numeric(format.Date(min(data$date), "%G"))

  dataset <- data[, .(
    n = sum(value),
    consult = sum(consult),
    pop = sum(pop),
    HelligdagIndikator = mean(HelligdagIndikator)
  ), by = .(date)]

  dates <- dataset[, "date"]
  dates[, year := RAWmisc::YearN(date)]
  dates[, week := RAWmisc::WeekN(date)]

  years <- CalculateTrainPredictYearPattern(yearMin = yearMin, yearMax = yearMax, numPerYear1 = 1)
  res <- vector("list", length = length(years))

  for (i in 1:length(years)) {
    dateTrainMin <- min(dates[year == years[[i]]$yearTrainMin]$date)
    dateTrainMax <- max(dates[year == years[[i]]$yearTrainMax]$date)

    datePredictMin <- min(dates[year == years[[i]]$yearPredictMin]$date)
    datePredictMax <- max(dates[year == years[[i]]$yearPredictMax]$date)

    res[[i]] <- QuasipoissonTrainPredictData(
      datasetTrain = dataset[date >= dateTrainMin & date <= dateTrainMax],
      datasetPredict = dataset[date >= datePredictMin & date <= datePredictMax],
      isDaily = F, v = v
    )
  }
  res <- rbindlist(res)
  res <- res[!is.na(threshold2)]

  res <- res[, c(variablesAlgorithmWeekly, variablesAlgorithmBasic, variablesAlgorithmProduced), with = F]
  return(res)
}

#' PROJ
#' @param data a
#' @param v a
#' @importFrom RAWmisc YearN WeekN
#' @import data.table
#' @export AnalyseRecentLine
AnalyseRecentLine <- function(data, v) {
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  consult <- NULL
  pop <- NULL
  HelligdagIndikator <- NULL
  threshold2 <- NULL
  # end

  yearMax <- as.numeric(format.Date(max(data$date), "%G"))
  yearMin <- as.numeric(format.Date(min(data$date), "%G"))

  dataset <- data[, .(
    n = sum(value),
    consult = sum(consult),
    pop = sum(pop),
    HelligdagIndikator = mean(HelligdagIndikator)
  ), by = .(date)]

  dates <- dataset[, "date"]
  dates[, year := RAWmisc::YearN(date)]
  dates[, week := RAWmisc::WeekN(date)]

  years <- CalculateTrainPredictYearPattern(yearMin = yearMin, yearMax = yearMax, numPerYear1 = 1)
  res <- vector("list", length = length(years))

  for (i in 1:length(years)) {
    dateTrainMin <- min(dates[year == years[[i]]$yearTrainMin]$date)
    dateTrainMax <- max(dates[year == years[[i]]$yearTrainMax]$date)

    datePredictMin <- min(dates[year == years[[i]]$yearPredictMin]$date)
    datePredictMax <- max(dates[year == years[[i]]$yearPredictMax]$date)

    res[[i]] <- QuasipoissonTrainPredictData(
      datasetTrain = dataset[date >= dateTrainMin & date <= dateTrainMax],
      datasetPredict = dataset[date >= datePredictMin & date <= datePredictMax],
      isDaily = T, v = v
    )
  }
  res <- rbindlist(res)
  res <- res[!is.na(threshold2)]

  res <- res[, c(variablesAlgorithmDaily, variablesAlgorithmBasic, variablesAlgorithmProduced), with = F]

  return(res)
}

#' RunOneAnalysis
#' @param analysesStack a
#' @param analysisData a
#' @import data.table
#' @export RunOneAnalysis
RunOneAnalysis <- function(analysesStack, analysisData) {
  # variables used in data.table functions in this function
  age <- NULL
  type <- NULL
  location <- NULL
  locationName <- NULL
  status <- NULL
  n <- NULL
  threshold2 <- NULL
  threshold4 <- NULL
  # end

  if (analysesStack$type == "influensa") {
    setnames(analysisData, "consultWithInfluensa", "consult")
  } else {
    setnames(analysisData, "consultWithoutInfluensa", "consult")
  }

  retval <- NULL
  if (analysesStack$granularity == "Daily") {
    retval <- AnalyseRecentLine(
      data = analysisData,
      v = analysesStack$v
    )
  } else {
    retval <- AnalyseYearLine(
      data = analysisData,
      v = analysesStack$v
    )
  }

  retval[, age := analysesStack$age]
  retval[, type := analysesStack$type]
  retval[, location := analysesStack$location]
  retval[, locationName := GetLocationName(analysesStack$location, norwayLocations = norwayLocations)]

  # make threshold2 minimum of 2 and threshold4 minimum of 3
  retval[threshold2 < 2, threshold2 := 2]
  retval[threshold4 < 3, threshold4 := 3]

  # create "normal", "medium", "high" categories
  DetermineStatus(retval)

  # add county if this is a municipality
  AddCounty(data = retval, loc = analysesStack$location)

  # validate data
  if (!ValidateAnalysisResults(retval, granularity = analysesStack$granularity)) stop("Results in a bad format")

  return(retval)
}

#' PROB
#' @param location a
#' @param norwayLocations a
#' @import data.table
#' @export GetLocationName
GetLocationName <- function(location, norwayLocations) {
  locationName <- "Norge"

  if (location != "Norge") {
    if (sum(norwayLocations$municip == location) > 0) {
      locationName <- as.character(norwayLocations$municipName[norwayLocations$municip == location])
    } else if (sum(norwayLocations$county == location) > 0) {
      locationName <- as.character(norwayLocations$countyName[norwayLocations$county == location])
    }
  }

  return(locationName)
}

#' PROB
#' @param location a
#' @param norwayLocations a
#' @import data.table
#' @export GetCountyFromMunicip
GetCountyFromMunicip <- function(location, norwayLocations) {
  if (sum(norwayLocations$municip == location) > 0) {
    location <- as.character(norwayLocations$county[norwayLocations$municip == location])
  }

  return(location)
}
