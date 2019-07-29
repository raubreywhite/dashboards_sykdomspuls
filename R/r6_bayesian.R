bayesian_stack_field_types = c(
  "granularity_time"="TEXT",
  "granularity_geo"="TEXT",
  "tag"="TEXT",
  "denominator"="TEXT",
  "location"="TEXT",
  "age"="TEXT",
  "weeklyDenominatorFunction"="TEXT",

  "year_train_min"="INTEGER",
  "year_train_max"="INTEGER",
  "year_predict_min"="INTEGER",
  "year_predict_max"="INTEGER",

  "date_train_min"="DATE",
  "date_train_max"="DATE",
  "date_predict_min"="DATE",
  "date_predict_max"="DATE",

  "uuid"="TEXT"
)

bayesian_stack_keys = c(
  "granularity_time",
  "granularity_geo",
  "tag",
  "location",
  "age",
  "year_train_min",
  "year_train_max",
  "year_predict_min",
  "year_predict_max"
)

bayesian_results_field_types = c(
  "v"="INTEGER",
  "granularity_time"="TEXT",
  "granularity_geo"="TEXT",
  "tag"="TEXT",
  "location"="TEXT",
  "age"="TEXT",
  "status"="TEXT",
  "wkyr"="TEXT",
  "year"="DOUBLE",
  "week"="DOUBLE",
  "x"="DOUBLE",
  "date"="DATE",
  "n"="INTEGER",
  "denominator"="INTEGER",
  "threshold0"="DOUBLE",
  "threshold2"="DOUBLE",
  "threshold4"="DOUBLE",
  "threshold6"="DOUBLE",
  "zscore"="DOUBLE",
  "cumE1"="DOUBLE",
  "cumL1"="DOUBLE",
  "cumU1"="DOUBLE",
  "failed"="TINYINT",
  "uuid"="TEXT",
  "locationName"="TEXT",
  "county"="TEXT"
)

bayesian_results_keys = c(
  "granularity_time",
  "granularity_geo",
  "tag",
  "location",
  "age",
  "year",
  "week",
  "date"
)

#' bayesian
#' @import R6
#' @export bayesian
bayesian <-  R6::R6Class(
  "bayesian",
  portable = FALSE,
  cloneable = FALSE,
  list(
    conf = NULL,
    stack_x = NULL,
    results_x = NULL,
    initialize = function(conf){
      conf <<- conf
    },
    run_all = function(){
      stack_x <<- fd::schema$new(
        db_config = CONFIG$DB_CONFIG,
        db_table = glue::glue("spuls_bayesian_analyses"),
        db_field_types = bayesian_stack_field_types,
        db_load_folder = "/xtmp/",
        keys = bayesian_stack_keys,
        check_fields_match = TRUE
      )

      results_x <<- fd::schema$new(
        db_config = CONFIG$DB_CONFIG,
        db_table = glue::glue("spuls_bayesian_results"),
        db_field_types = bayesian_results_field_types,
        db_load_folder = "/xtmp/",
        keys = bayesian_results_keys,
        check_fields_match = TRUE
      )

      stack_x$db_connect()
      results_x$db_connect()

      for(i in 1:nrow(CONFIG$BAYESIAN)) for(age in names(CONFIG$AGES))
        conf <- CONFIG$BAYESIAN[i]
        data <- readRDS(file = fd::path("data_clean", sprintf("%s_%s_%s_cleaned.RDS", LatestRawID(), conf$tag, age)))
        bayesian_load_stack_schema(conf, data, stack_x)


    }
  )
)



#' Create analysis stack and datasets for a tag
#'
#' Given one row from \code{CONFIG$SYNDROMES} we need
#' to generate an analysis stack and all relevant datasets
#' that can be directly sent to \code{QuasipoissonTrainPredictData}
#' without additional formatting.
#' @param conf A row from \code{CONFIG$SYNDROMES}
#' @param data a dataset
#' @param schema a schema
#' @export
bayesian_load_stack_schema <- function(conf, data, stack_x) {
  . <- NULL
  granularityGeo <- NULL
  weeklyDenominatorFunction <- NULL
  v <- NULL
  tag <- NULL
  granularity <- NULL
  location <- NULL
  id <- NULL

  counties <- unique(data[stringr::str_detect(location,"^county")]$location)

  ages <- unique(data$age)

  years <- CalculateTrainPredictYearPattern(
    yearMin = lubridate::isoyear(min(data$date)),
    yearMax = lubridate::isoyear(max(data$date)),
    numPerYear1 = 200)

  unlist(years)

  # setting control stack for counties
  analyses <- data.table(
    expand.grid(
      tag = conf$tag,
      denominator = conf$denominator,
      location = "^county",
      age = ages,
      year_index = 1:length(years),
      granularity_time = "weekly",
      stringsAsFactors = FALSE
    )
  )
  analyses[, weeklyDenominatorFunction := conf$weeklyDenominatorFunction]
  analyses[,granularity_geo := "county"]

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

  stack_x$dt <- analyses

  stack_x$identify_dt_that_exists_in_db()
  stack_x$get_data_dt()[year_predict_max>=max(year_predict_max)-1,exists_in_db:=FALSE]

}

bayesian_launch_analysis <- function(s,data){
  d <- data[stringr::str_detect(location,s$location)]

  setnames(d,s$denominator,"denominator")

  dataset_train <- d[date >= s$date_train_min & date <= s$date_train_max]
  dataset_predict <- d[date >= s$date_train_min & date <= s$date_train_max]

  weekly_denominator_function = ifelse(s$weeklyDenominatorFunction=="sum",sum,mean)
}

bayesian_analysis <- function(
  dataset_train,
  dataset_predict,
  weekly_denominator_function){

  dataset_train[, year := fhi::isoyear_n(date)] # Week-based year, instead of normal year (%Y)
  dataset_train[, week := fhi::isoweek_n(date)] # Week-based year, instead of normal year (%Y)

  dataset_predict[, year := fhi::isoyear_n(date)] # Week-based year, instead of normal year (%Y)
  dataset_predict[, week := fhi::isoweek_n(date)] # Week-based year, instead of normal year (%Y)

  # SET REGRESSION FORMULA:
  reg_formula <- n ~
    trend +
    sin(2 * pi * (week - 1) / 52) +
    cos(2 * pi * (week - 1) / 52) +
    HelligdagIndikator +
    (1|location)

  dataset_train <- FormatDatasetWeekly(
    dataset_train,
    weeklyDenominatorFunction = weekly_denominator_function,
    by_group = "location"
  )
  dataset_predict <- FormatDatasetWeekly(
    dataset_predict,
    weeklyDenominatorFunction = weekly_denominator_function,
    by_group = "location"
    )

  if (remove.pandemic.year == T) {
   dataset_train <- dataset_train[year != "2009"]
  }

  fit <- rstanarm::stan_glmer(
    as.formula(reg_formula),
    data=as.data.frame(dataset_train),
    offset = log(denominator),
    family = poisson,
    cores = 4
  )

  a <- rstanarm::posterior_predict(fit,dataset_predict)

  e <- apply(a,2,ecdf)
  convert_to_zscore.int <- function(i,value){
    retval <- qnorm(e[[i]](value))
    if(retval < -100) retval <- -100
    if(retval > 100) retval <- 100
    return(retval)
  }
  convert_to_zscore <- Vectorize(convert_to_zscore.int)
  dataset_predict[,zscore:=convert_to_zscore(1:.N,n)]
  dataset_predict[,threshold0:=apply(a,2,quantile, probs = pnorm(0))]
  dataset_predict[,threshold2:=apply(a,2, quantile, probs= pnorm(2))]
  dataset_predict[,threshold4:=apply(a,2, quantile, probs= pnorm(4))]
  dataset_predict[,threshold6:=apply(a,2, quantile, probs= pnorm(6))]

}





