#' quasipoission
#' @import R6
#' @export quasipoission
quasipoission <-  R6::R6Class(
  "quasipoission",
  portable = FALSE,
  cloneable = FALSE,
  list(
    conf = NULL,
    stack_field_types = c(
      "purpose"="TEXT",
      "granularity_time"="TEXT",
      "granularity_geo"="TEXT",
      "tag"="TEXT",
      "denominator"="TEXT",
      "location"="TEXT",
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
    ),
    stack_keys = c(
      "purpose",
      "granularity_time",
      "granularity_geo",
      "tag",
      "location",
      "age",
      "v",
      "year_train_min",
      "year_train_max",
      "year_predict_min",
      "year_predict_max"
    ),
    stack_x = NULL,
    results_field_types = c(
      "purpose"="TEXT",
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
    ),
    results_keys = c(
      "purpose",
      "v",
      "granularity_time",
      "granularity_geo",
      "tag",
      "location",
      "age",
      "year",
      "week",
      "date"
    ),
    results_x = NULL,
    initialize = function(conf, db_config){
      conf <<- conf

      stack_x <<- fd::schema$new(
        db_config = db_config,
        db_table = glue::glue("spuls_standard_analyses"),
        db_field_types = stack_field_types,
        db_load_folder = "/xtmp/",
        keys = stack_keys,
        check_fields_match = TRUE
      )

      results_x <<- fd::schema$new(
        db_config = db_config,
        db_table = glue::glue("spuls_standard_results"),
        db_field_types = results_field_types,
        db_load_folder = "/xtmp/",
        keys = results_keys,
        check_fields_match = TRUE
      )

      stack_x$db_connect()
      results_x$db_connect()
    },
    run_age = function(
      age,
      base_folder = fd::path("data_clean"),
      latest_id= sykdomspuls::LatestRawID()) {
      on.exit(function(){rm("data","res");gc()})

      message(glue::glue("Running {age}"))

      data <- readRDS(file = file.path(base_folder, sprintf("%s_%s_%s_cleaned.RDS", latest_id, conf$tag, age)))
      sykdomspuls::load_stack_schema(conf = conf, data=data, schema = stack_x)

      run_stack <- stack_x$get_data_dt()[exists_in_db==FALSE]
      run_stack <- split(run_stack,seq(nrow(run_stack)))

      res <- pbapply::pblapply(run_stack,function(x){
        uuid <- x$uuid

        run_data <- data[.(x$location)]
        setnames(run_data,x$denominator,"denominator")

        run_data_train <- run_data[date >= x$date_train_min & date <= x$date_train_max]
        run_data_predict <- run_data[date >= x$date_predict_min & date <= x$date_predict_max]

        retval <- QuasipoissonTrainPredictData(
          datasetTrain = run_data_train,
          datasetPredict = run_data_predict,
          isDaily = x$granularity_time == "daily",
          v = x$v,
          weeklyDenominatorFunction = ifelse(x$weeklyDenominatorFunction=="sum",sum,mean)
        )
        retval[,uuid:=uuid]
      })
      rm("data"); gc()
      res <- rbindlist(res)

      res <- clean_post_analysis(res=res, schema = stack_x)

      results_x$db_upsert_load_data_infile(res[,names(results_x$db_field_types),with=F])
      stack_x$db_upsert_load_data_infile(stack_x$dt[uuid %in% unique(res$uuid),names(stack_x$db_field_types),with=F])

      rm("res"); gc()
    },
    run = function(base_folder = fd::path("data_clean"), latest_id = sykdomspuls::LatestRawID()){
      message(glue::glue("Running {conf$tag}"))
      connect_to_db()
      for(i in names(sykdomspuls::CONFIG$AGES)) run_age(age = i, base_folder = base_folder, latest_id = latest_id)
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
