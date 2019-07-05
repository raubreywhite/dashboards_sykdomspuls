#' standard
#' @import R6
#' @export standard
standard <-  R6::R6Class(
  "standard",
  portable = FALSE,
  cloneable = FALSE,
  list(
    conf = NULL,
    db_config = NULL,
    models = list(),
    initialize = function(conf=NULL, db_config=NULL){
      conf <<- conf
      db_config <<- db_config

      for (i in 1:nrow(conf)) {
        models[[i]] <<- quasipoission$new(conf = conf[i], db_config = db_config)
      }
    },
    run_analysis = function(db_config = self$db_config){
      cl <- parallel::makeCluster(3L, file = "")
      doParallel::registerDoParallel(cl)
      base_folder <- fhi::DashboardFolder("data_clean")
      latest_id <- sykdomspuls::LatestRawID()
      foreach(i = 1:3, .packages = c("data.table"), .verbose = T, .export = "models") %dopar% {
        data.table::setDTthreads(1)
        models[[i]]$run(base_folder = base_folder, latest_id = latest_id)
      }
      stopCluster(cl)

      # add index
      try(DBI::dbExecute(
            std$models[[1]]$results_x$conn,
            glue::glue(
              "ALTER TABLE `{tb}` ADD INDEX `ind1` (`purpose`(10),`granularity_time`(10),`tag`(10),`location`(10),`age`(10))",
              tb=std$models[[1]]$results_x$db_table
              )
      ),TRUE)

    },
    save_external_api = function(){
      SaveRDS(ConvertConfigForAPI(), fd::path("results", "externalapi","config.RDS"))

      names_req <- c(
        "tag",
        "type",
        "location",
        "age",
        "status",
        "wkyr",
        "year",
        "week",
        "x",
        "date",
        "displayDay",
        "HelligdagIndikator",
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
        "file",
        "locationName",
        "county"
      )

      y <- models[[1]]$results_x$get_data_db(
        purpose=="production" &
          granularity_time=="weekly"
      )

      y[,type:=tag]
      y[,HelligdagIndikator:=0]
      y[,file:="x"]
      saveRDS(
        y[stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resYearLineMunicip.RDS")
      )

      saveRDS(
        y[!stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resYearLine.RDS")
      )

      y <- models[[1]]$results_x$get_data_db(
        purpose=="production" &
          granularity_time=="daily"
      )

      saveRDS(
        y[!stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resRecentLine.RDS")
      )
    }
  )
)
