#' Determining which years of training data will be used
#'
#' For a year to be "predicted" (i.e. have outbreaks detected)
#' it must use 5 years of training data. This function calculates
#' which years of training data are assigned to detect outbreaks.
#'
#' For reasons of efficiency, we change the training data set on January 1st.
#' That is, an entire year (or more) of outbreaks are detected off a fixed
#' set of training data.
#'
#' The current algorithm that we use sets the first 5 years of data as training data
#' for the first 6 years of outbreak detection. We then fix our 5 years of training data
#' and use it for sequences of 2 years of outbreak detection.
#'
#' When it approaches the current year, we want better calculations, so
#' we can specify that 5 years of training data should
#' only be used for 1 subsequent year of outbreak detection
#' through the \code{numPerYear1} parameter.
#'
#' @param yearMin The first year of data
#' @param yearMax The last year of data
#' @param numPerYear1 5 years of training data should
#' only be used for 1 subsequent year of outbreak detection
#' for the last \code{numPerYear1} years
#' @return A list containing a sequence of training years and prediction years
#' @examples
#' sykdomspuls::CalculateTrainPredictYearPattern(2000, 2015, 1)
#' 
#' sykdomspuls::CalculateTrainPredictYearPattern(2000, 2015, 3)
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

#' Adds seasonal week to dataset
#'
#' We often want to graph seasons. This function adds the seasonal week
#' to the dataset \code{data} as the variable \code{x}.
#'
#' @param data A data.table containing the variable \code{week}
#' @return A data.table with the extra variable \code{x}
#' @examples
#' library(data.table)
#' d <- data.table(week = 1:52)
#' AddXToWeekly(d)
#' @import data.table
#' @export AddXToWeekly
AddXToWeekly <- function(data) {
  week <- NULL
  x <- NULL

  data[week >= 30, x := week - 29]
  data[week < 30, x := week + 23]

  return(data)
}

#' Adds year-week and displayDate to dataset
#'
#' This function adds the year-week (i.e. YYYY-WW)
#' and the displayDate (the last day of the week, i.e. Sunday)
#' to the dataset \code{data} as the variables \code{wkyr} and \code{displayDay}.
#'
#' @param data A data.table containing the variables \code{year} and \code{week}
#' @return A data.table with the extra variables \code{wkyr} and \code{displayDay}.
#' @examples
#' library(data.table)
#' d <- data.table(year = 2015, week = 1:10)
#' AddWkyrAndDisplayDateToWeekly(d)
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

#' Convert n and thresholds to Normal/Medium/High
#'
#' Takes a data.table with the variables \code{n}, \code{threshold2}, and \code{threshold4}
#' and then creates a new variable called \code{status} with the values
#' Normal/Medium/High.
#'
#' Normal <= threshold2
#' threshold2 < Medium <= threshold4
#' threshold4 < High
#'
#' @param data A data.table containing the variables \code{n}, \code{threshold2}, and \code{threshold4}
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

#' Adds county to dataset
#'
#' Takes a location (either a municipality or county).
#' If the location is a county, nothing happens.
#' If the location is a municipality, it searches for the encompassing county
#' and adds this to the data.table \code{data} as a new variable \code{county}.
#'
#' @param data A data.table
#' @param loc A location (either a municipality or county)
#' @import data.table
#' @export AddCounty
AddCounty <- function(data, loc) {
  county <- GetCountyFromMunicip(loc, norwayLocations = norwayLocations)
  data[, county := county]
}

#' Run one analysis according to the analysis stack
#'
#' This function receives a generic dataset and a selection from the analysis stack.
#' The data is then reformatted according to the analysis stack and sent to the
#' appropriate analysis function.
#'
#' @param analysesStack The desired analysis. TODO: Validate
#' @param analysisData The generic dataset. TODO: Validate
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
  denominator <- NULL
  v <- NULL
  tag <- NULL
  # end

  analysisData[, denominator := get(analysesStack$denominator)]

  yearMax <- as.numeric(format.Date(max(analysisData$date), "%G"))
  yearMin <- as.numeric(format.Date(min(analysisData$date), "%G"))

  dataset <- copy(analysisData)

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
      isDaily = analysesStack$granularity == "Daily",
      v = v,
      weeklyDenominatorFunction = analysesStack$weeklyDenominatorFunction[[1]]
    )
  }
  res <- rbindlist(res)
  res <- res[!is.na(threshold2)]

  res[, age := analysesStack$age]
  res[, type := analysesStack$tag]
  res[, tag := analysesStack$tag]
  res[, location := analysesStack$location]
  res[, locationName := GetLocationName(analysesStack$location, norwayLocations = norwayLocations)]
  res[, file := analysesStack$file]

  # make threshold2 minimum of 2 and threshold4 minimum of 3
  res[threshold2 < 2, threshold2 := 2]
  res[threshold4 < 3, threshold4 := 3]

  # create "normal", "medium", "high" categories
  DetermineStatus(res)

  # add county if this is a municipality
  AddCounty(data = res, loc = analysesStack$location)

  # validate data
  if (!ValidateResultsFull(res)) stop("Results in a bad format")

  setcolorder(res, VARS$REQ_RESULTS_FULL)

  return(res)
}

#' Get location name from location code
#'
#' When given a location code, the pretty location name is returned
#'
#' @param location Location code
#' @param norwayLocations Dataset containing a map between locatino code and pretty location name
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

  return(locationName[1])
}

#' Finds county from municipality
#'
#' Takes a location (either a municipality or county).
#' If the location is a county, the county is returned.
#' If the location is a municipality, it searches for the encompassing county
#' and this is returned.
#'
#' @param location A location (either a municipality or county)
#' @param norwayLocations Dataset containing a map between locatino code and pretty location name
#' @import data.table
#' @export GetCountyFromMunicip
GetCountyFromMunicip <- function(location, norwayLocations) {
  if (sum(norwayLocations$municip == location) > 0) {
    location <- as.character(norwayLocations$county[norwayLocations$municip == location])
  }

  return(location)
}
