#' quasip
#' @import R6
#' @export quasip
quasip <-  R6::R6Class(
  "quasip",
  portable = FALSE,
  cloneable = FALSE,
  list(
    conf = NULL,
    stack_x = NULL,
    results_x = NULL,
    initialize = function(conf, db_config){
      conf <<- conf

      stack_x <<- fd::schema$new(
        db_config = db_config,
        db_table = glue::glue("spuls_standard_analyses"),
        db_field_types = quasi_stack_field_types,
        db_load_folder = "/xtmp/",
        keys = quasi_stack_keys,
        check_fields_match = TRUE
      )

      results_x <<- fd::schema$new(
        db_config = db_config,
        db_table = glue::glue("spuls_standard_results"),
        db_field_types = quasi_results_field_types,
        db_load_folder = "/xtmp/",
        keys = quasi_results_keys,
        check_fields_match = TRUE
      )

      stack_x$db_connect()
      results_x$db_connect()
    },
    run = function(
      base_folder = fd::path("data_clean"),
      latest_id = sykdomspuls::LatestRawID()
      ){

      message(glue::glue("Running {conf$tag}"))
      connect_to_db()
      for(i in names(sykdomspuls::CONFIG$AGES)) quasi_run_age(
        age = i,
        conf = conf,
        base_folder = base_folder,
        latest_id = latest_id,
        stack_x = stack_x,
        results_x = results_x
        )
    },
    connect_to_db = function(){
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver="MySQL",
                             server = "db",
                             port = 3306,
                             user = "root",
                             password = "example"
      )

      fd:::use_db(conn, "sykdomspuls")

      stack_x$conn <- conn
      results_x$conn <- conn

    },
    save = function(){
      print("save QP")
      fhi::Log("save1Before")
      ResultsAggregateApply()
      fhi::Log("save1After")
    }
  )
)

quasi_stack_field_types = c(
  "granularity_time"="TEXT",
  "granularity_geo"="TEXT",
  "tag"="TEXT",
  "denominator"="TEXT",
  "location_code"="TEXT",
  "age"="TEXT",
  "weeklyDenominatorFunction"="TEXT",
  "v"="INTEGER",
  "file"="TEXT",
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

quasi_stack_keys = c(
  "granularity_time",
  "granularity_geo",
  "tag",
  "location_code",
  "age",
  "v",
  "year_train_min",
  "year_train_max",
  "year_predict_min",
  "year_predict_max"
)

quasi_results_field_types = c(
  "v"="INTEGER",
  "granularity_time"="TEXT",
  "granularity_geo"="TEXT",
  "tag"="TEXT",
  "location_code"="TEXT",
  "age"="TEXT",
  "status"="TEXT",
  "yrwk"="TEXT",
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
  "location_name"="TEXT",
  "county_code"="TEXT"
)

quasi_results_keys = c(
  "v",
  "granularity_time",
  "granularity_geo",
  "tag",
  "location_code",
  "age",
  "year",
  "week",
  "date"
)

quasi_run_age = function(
  age,
  conf,
  base_folder = fd::path("data_clean"),
  latest_id= sykdomspuls::LatestRawID(),
  stack_x,
  results_x
  ) {

  on.exit(function(){rm("data","res");gc()})

  message(glue::glue("Running {age}"))

  data <- readRDS(file = file.path(base_folder, glue::glue("{latest_id}_{conf$tag}_{age}_cleaned.RDS")))
  load_stack_schema(conf = conf, data=data, schema = stack_x)

  run_stack <- stack_x$get_data_dt()[exists_in_db==FALSE]
  run_stack <- split(run_stack,seq(nrow(run_stack)))

  res <- pbapply::pblapply(run_stack,function(x){
    run_data <- data[.(x$location)]
    setnames(run_data,x$denominator,"denominator")

    run_data_train <- run_data[date >= x$date_train_min & date <= x$date_train_max]
    run_data_predict <- run_data[date >= x$date_predict_min & date <= x$date_predict_max]

    retval <- QuasipoissonTrainPredictData(
      datasetTrain = run_data_train,
      datasetPredict = run_data_predict,
      isDaily = x$granularity_time == "daily",
      v = x$v,
      weeklyDenominatorFunction = ifelse(x$weeklyDenominatorFunction=="sum",sum,mean),
      uuid=x$uuid
    )
  })
  rm("data"); gc()
  res <- rbindlist(res)

  res <- clean_post_analysis(res=res, stack = stack_x$get_data_dt())

  results_x$db_upsert_load_data_infile(res[,names(results_x$db_field_types),with=F])
  stack_x$db_upsert_load_data_infile(stack_x$dt[uuid %in% unique(res$uuid),names(stack_x$db_field_types),with=F], drop_indexes=c("ind1","ind2","ind3","ind4","ind5"))

  rm("res"); gc()

  # add index
  try(DBI::dbExecute(
    results_x$conn,
    glue::glue(
      "ALTER TABLE `{tb}` ADD INDEX `ind1` (`granularity_time`(10),`tag`(10),`location_name`(10),`age`(10))",
      tb=results_x$db_table
    )
  ),TRUE)

  try(
    DBI::dbExecute(
      results_x$conn,
      glue::glue(
        "ALTER TABLE `{tb}` ADD INDEX `ind2` (`granularity_time`(10),`yrwk`(10))",
        tb=results_x$db_table
      )
    )
    ,TRUE)
  try(
    DBI::dbExecute(
      results_x$conn,
      glue::glue(
        "ALTER TABLE `{tb}` ADD INDEX `ind3` (`tag`(10), `age`(10), `granularity_time`(10), `county` (10))",
        tb=results_x$db_table
      )
    ) , TRUE
  )
  try(
    DBI::dbExecute(
      results_x$conn,
      glue::glue(
        "ALTER TABLE `{tb}` ADD INDEX `ind4` (`tag`(10), `age`(10), `granularity_time`(10), `granularity_geo` (10))",
        tb=results_x$db_table
      )
    )
    ,TRUE)
  try(
    DBI::dbExecute(
      results_x$conn,
      glue::glue(
        "ALTER TABLE `{tb}` ADD INDEX `ind5` (`yrwk`(10), `granularity_time`(10), `granularity_geo`(10), `tag` (10))",
        tb=results_x$db_table
      )
    )
    ,TRUE)

}
