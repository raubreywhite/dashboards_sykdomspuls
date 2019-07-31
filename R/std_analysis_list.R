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
  if(numPerYear1 > yearMax-yearMin-5) numPerYear1 <- yearMax-yearMin-5
  perYear1 <- seq(yearMax - numPerYear1 + 1, yearMax, by = 1)
  perYear2 <- c((yearMin + 6):(yearMax - numPerYear1))
  perYear2 <- perYear2[!perYear2 %in% perYear1]

  perFixed <- c(yearMin:(yearMin + 5))

  perYear2 <- perYear2[!perYear2 %in% perFixed]
  perYear1 <- perYear1[!perYear1 %in% perFixed]

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
  for (i in seq_along(perYear2)) {
    if (i %% 2 == 0) next
    years[[index]] <- list(
      yearTrainMin = perYear2[i] - 5,
      yearTrainMax = perYear2[i] - 1,
      yearPredictMin = perYear2[i],
      yearPredictMax = perYear2[i] + 1
    )
    index <- index + 1
  }
  for (i in seq_along(perYear1)) {
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

  counties <- data[granularity_geo == "municip",unique(county_code)]
  municips <- data[granularity_geo == "municip",unique(location_code)]
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
      location_code = c("Norge", counties),
      age = ages,
      year_index = 1:length(years),
      granularity_time = c("daily", "weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesCounties[, weeklyDenominatorFunction := conf$weeklyDenominatorFunction]
  analysesCounties[, v := sykdomspuls::CONFIG$VERSION]
  analysesCounties[, file := sprintf("%s_%s.RDS", "resRecentLine", tag)]
  analysesCounties[granularity_time == "weekly", file := sprintf("%s_%s.RDS", "resYearLine", tag)]
  analysesCounties[,granularity_geo := "county"]
  analysesCounties[location_code=="Norge", granularity_geo := "national"]

  # setting control stack for municipalities
  analysesMunicips <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location_code = municips,
      age = ages,
      year_index = 1:length(years),
      granularity_time = c("weekly"),
      stringsAsFactors = FALSE
    )
  )
  analysesMunicips[, weeklyDenominatorFunction := conf$weeklyDenominatorFunction]
  analysesMunicips[, v := sykdomspuls::CONFIG$VERSION]
  analysesMunicips[, file := sprintf("%s_%s.RDS", "resYearLineMunicip", tag)]
  analysesMunicips[, granularity_geo := "municip"]

  analyses <- rbind(analysesCounties, analysesMunicips)
  for(i in seq_along(years)){
    analyses[year_index==i,year_train_min:=years[[i]]$yearTrainMin]
    analyses[year_index==i,year_train_max:=years[[i]]$yearTrainMax]
    analyses[year_index==i,year_predict_min:=years[[i]]$yearPredictMin]
    analyses[year_index==i,year_predict_max:=years[[i]]$yearPredictMax]
  }
  analyses[,uuid:=replicate(.N, uuid::UUIDgenerate(F))]
  analyses[,year_index:=NULL]

  dates <- data[, "date"]
  dates[, year := fhi::isoyear_n(date)]
  dates[, date_min := min(date), by=year]
  dates[, date_max := max(date), by=year]
  dates <- unique(dates[,c("year","date_min","date_max")])

  analyses[dates, on="year_train_min==year", date_train_min:=date_min]
  analyses[dates, on="year_train_max==year", date_train_max:=date_max]

  analyses[dates, on="year_predict_min==year", date_predict_min:=date_min]
  analyses[dates, on="year_predict_max==year", date_predict_max:=date_max]

  if(Sys.getenv("ONLY_RUN_LATEST_YEAR","FALSE")=="TRUE") analyses <- analyses[year_predict_max==max(year_predict_max)]

  # fix schema

  schema$dt <- analyses

  schema$identify_dt_that_exists_in_db()
  schema$get_data_dt()[year_predict_max>=max(year_predict_max)-1,exists_in_db:=FALSE]

}
