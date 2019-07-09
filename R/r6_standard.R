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
    tags = list(),
    initialize = function(conf=NULL, db_config=NULL){
      conf <<- conf
      db_config <<- db_config

      for (i in 1:nrow(conf)) {
        tags[[i]] <<- quasipoission$new(conf = conf[i], db_config = db_config)
      }
    },
    run_analysis = function(db_config = self$db_config){
      cl <- parallel::makeCluster(3L, file = "")
      doParallel::registerDoParallel(cl)
      base_folder <- fd::path("data_clean")
      latest_id <- LatestRawID()

      foreach(i = 1:length(tags), .packages = c("data.table"), .verbose = T, .export = "tags") %dopar% {
        data.table::setDTthreads(1)
        tags[[i]]$run(base_folder = base_folder, latest_id = latest_id)
      }
      stopCluster(cl)

      fd::msg("Finished standard in parallel")
      # add index
      try(DBI::dbExecute(
        tags[[1]]$results_x$conn,
        glue::glue(
          "ALTER TABLE `{tb}` ADD INDEX `ind1` (`purpose`(10),`granularity_time`(10),`tag`(10),`location`(10),`age`(10))",
          tb=tags[[1]]$results_x$db_table
        )
      ),TRUE)

      try(DBI::dbExecute(
        tags[[1]]$results_x$conn,
        glue::glue(
          "ALTER TABLE `{tb}` ADD INDEX `ind2` (`purpose`(10),`granularity_time`(10),`wkyr`(10))",
          tb=tags[[1]]$results_x$db_table
        )
      ),TRUE)

    },
    save_internal_dashboard = function(){

      fd::msg("Saving GLOBAL.RDS for the internal dashboard")
      GLOBAL <- new.env(parent = emptyenv())
      CONFIG_OLD <- ConvertConfigForAPI()
      GLOBAL$weeklyTypes <- GLOBAL$dailyTypes <- CONFIG_OLD$SYNDROMES[CONFIG_OLD$SYNDROMES %in% CONFIG$SYNDROMES[websiteInternal == TRUE]$tag]
      GLOBAL$weeklyAges <- GLOBAL$dailyAges <- CONFIG_OLD$AGES

      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
          granularity_time=="daily"
        ) %>%
        dplyr::distinct(date) %>%
        dplyr::top_n(1L, date) %>%
        dplyr::collect()
      GLOBAL$dateMax <- val$date

      GLOBAL$dateMinRestrictedRecent <- GLOBAL$dateMax - 365
      GLOBAL$dateMinRestrictedLine <- GLOBAL$dateMax - 365 * 15

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
          granularity_time=="daily"
        ) %>%
        dplyr::distinct(location,locationName) %>%
        dplyr::collect()

      GLOBAL$dailyCounties <- unique(val$location)
      names(GLOBAL$dailyCounties) <- unique(val$locationName)
      GLOBAL$weeklyCounties <- unique(val$location)
      names(GLOBAL$weeklyCounties) <- unique(val$locationName)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
            granularity_time=="weekly"
        ) %>%
        dplyr::distinct(wkyr) %>%
        dplyr::collect()

      GLOBAL$weeklyWkyr <- rev(val$wkyr)
      GLOBAL$outbreakswkyr <- rev(val$wkyr)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
          granularity_time=="weekly"
        ) %>%
        dplyr::distinct(location,locationName,county) %>%
        dplyr::collect()

      GLOBAL$municipToCounty <- data.table(val)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
            granularity_time=="daily"
        ) %>%
        dplyr::distinct(tag,location,age) %>%
        dplyr::collect()
      setDT(val)
      setnames(val,"tag","type")

      GLOBAL$resRecentLineStack <- val

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
          granularity_time=="weekly"
        ) %>%
        dplyr::distinct(tag,location,age) %>%
        dplyr::collect()
      setDT(val)
      setnames(val,"tag","type")

      GLOBAL$resYearLineStack <- val[!stringr::str_detect(location,"^municip")]
      GLOBAL$resYearLineMunicipStack <- val[stringr::str_detect(location,"^municip")]

      GLOBAL$weeklyValues <- c(
        "Konsultasjoner" = "consults",
        "1 uke eksess" = "excess1"
      )

      saveRDS(GLOBAL, fd::path("data_app","GLOBAL.RDS"))
    },
    save_external_api = function(){
      fd::msg("Saving config for the external api")

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

      fd::msg("Saving daily data for the external api")

      d <- tags[[1]]$results_x$get_data_db(
        purpose=="production" &
          granularity_time=="daily"
      )
      d[,displayDay:=date]

      saveRDS(
        d[!stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resRecentLine.RDS")
      )

      fd::msg("Saving weekly data for the external api")

      d <- tags[[1]]$results_x$get_data_db(
        purpose=="production" &
          granularity_time=="weekly"
      )

      d[,type:=tag]
      d[,HelligdagIndikator:=0]
      d[,file:="x"]
      d[,displayDay:=date]
      saveRDS(
        d[stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resYearLineMunicip.RDS")
      )

      saveRDS(
        d[!stringr::str_detect(location,"^municip"),names_req,with=F],
        fd::path("results","externalapi","resYearLine.RDS")
      )

      fd::msg("Saving outbreaks for the external api")

      GenerateOutbreakListInternal(
        df = d[!stringr::str_detect(location,"^municip")],
        dk = d[stringr::str_detect(location,"^municip")],
        saveFiles = fd::path("results", "externalapi","outbreaks.RDS"),
        useType = TRUE
      )
    },
    email_external = function(){
      fd::msg("Generating external outbreak alerts")
      d <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          purpose=="production" &
          granularity_time=="weekly"
        ) %>%
        dplyr::top_n(1L, date) %>%
        dplyr::collect()
      setDT(d)

      GenerateOutbreakListExternal(
        df = d[!stringr::str_detect(location,"^municip")],
        dk = d[stringr::str_detect(location,"^municip")],
        saveFiles = fd::path("results", latest_date(), "outbreaks_alert_external.RDS")
      )

      fd::msg("Sending external emails")
      send_email <- fd::perform_weekly_action(
        file=fd::path("config", "last_emailed_utbrudd.RDS"),
        dev_always_performs = TRUE
        )

      if (send_email) {
        try(EmailExternal(), TRUE)
      }
    },
    email_internal = function(){
      try(EmailTechnicalNewResults(), TRUE)
    },
    run_all = function(){
      run_analysis()
      save_internal_dashboard()
      save_external_api()
      email_external()
      email_internal()
    }
  )
)
