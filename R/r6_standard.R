#' standard
#' @import R6
#' @export standard
standard <- R6::R6Class(
  "standard",
  portable = FALSE,
  cloneable = FALSE,
  list(
    conf = NULL,
    db_config = NULL,
    tags = list(),
    initialize = function(conf = NULL, db_config = NULL) {
      conf <<- conf
      db_config <<- db_config

      tags <<- apply(conf, 1, function(x) quasip$new(conf = x, db_config = db_config))
    },
    run_analysis = function(db_config = self$db_config) {
      fd::msg("Starting standard in parallel")

      cl <- parallel::makeCluster(3L, file = "")
      doParallel::registerDoParallel(cl)
      base_folder <- fd::path("data_clean")
      latest_id <- LatestRawID()

      foreach(i = 1:length(tags), .packages = c("data.table", "sykdomspuls"), .verbose = T, .export = "tags") %dopar% {
        data.table::setDTthreads(1)
        tags[[i]]$run(base_folder = base_folder, latest_id = latest_id)
      }
      stopCluster(cl)

      data.table::setDTthreads(parallel::detectCores() - 2)

      fd::msg("Finished standard in parallel")
    },
    save_internal_dashboard = function() {
      fd::msg("Saving GLOBAL.RDS for the internal dashboard")
      GLOBAL <- new.env(parent = emptyenv())
      CONFIG_OLD <- ConvertConfigForAPI()
      GLOBAL$weeklyTypes <- GLOBAL$dailyTypes <- CONFIG_OLD$SYNDROMES[CONFIG_OLD$SYNDROMES %in% CONFIG$SYNDROMES[websiteInternal == TRUE]$tag]
      GLOBAL$weeklyAges <- GLOBAL$dailyAges <- CONFIG_OLD$AGES

      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "daily"
        ) %>%
        dplyr::summarize(date = max(date, na.rm = T)) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()
      GLOBAL$dateMax <- val$date

      GLOBAL$dateMinRestrictedRecent <- GLOBAL$dateMax - 365
      GLOBAL$dateMinRestrictedLine <- GLOBAL$dateMax - 365 * 15

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "daily"
        ) %>%
        dplyr::distinct(location_code, location_name) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      GLOBAL$dailyCounties <- unique(val$location_code)
      names(GLOBAL$dailyCounties) <- unique(val$location_name)
      GLOBAL$weeklyCounties <- unique(val$location_code)
      names(GLOBAL$weeklyCounties) <- unique(val$location_name)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly"
        ) %>%
        dplyr::distinct(yrwk) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      GLOBAL$weeklyyrwk <- rev(val$yrwk)
      GLOBAL$outbreaksyrwk <- rev(val$yrwk)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly"
        ) %>%
        dplyr::distinct(location_code, location_name, county_code) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      GLOBAL$municipToCounty <- data.table(val)

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "daily"
        ) %>%
        dplyr::distinct(tag, location_code, age) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      setnames(val, "tag", "type")

      GLOBAL$resRecentLineStack <- val

      ###########################
      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly"
        ) %>%
        dplyr::distinct(tag, location_code, age) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      setnames(val, "tag", "type")

      GLOBAL$resYearLineStack <- val[!stringr::str_detect(location_code, "^municip")]
      GLOBAL$resYearLineMunicipStack <- val[stringr::str_detect(location_code, "^municip")]

      GLOBAL$weeklyValues <- c(
        "Konsultasjoner" = "consults",
        "1 uke eksess" = "excess1"
      )

      saveRDS(GLOBAL, fd::path("data_app", "GLOBAL.RDS"))
    },
    save_external_api = function() {
      fd::msg("Saving config for the external api")

      SaveRDS(ConvertConfigForAPI(), fd::path("results", "externalapi", "config.RDS"))

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
      x_tags <- conf[alertExternal == T]$tag

      fd::msg("Saving daily data for the external api")

      d <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "daily" &
            tag %in% x_tags
        ) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      d[, type := tag]
      d[, HelligdagIndikator := 0]
      d[, file := "x"]
      d[, displayDay := date]
      d[, location := location_code]
      d[, locationName := location_name]
      d[, county := county_code]
      d[, wkyr := yrwk]

      for (i in names(d)) {
        if (!i %in% names_req) d[, (i) := NULL]
      }
      saveRDS(
        d,
        fd::path("results", "externalapi", "resRecentLine.RDS")
      )

      rm("d")
      gc()

      fd::msg("Saving weekly municip data for the external api")

      dk <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly" &
            tag %in% x_tags &
            granularity_geo == "municip"
        ) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      dk[, type := tag]
      dk[, HelligdagIndikator := 0]
      dk[, file := "x"]
      dk[, displayDay := date]
      dk[, location := location_code]
      dk[, locationName := location_name]
      dk[, county := county_code]
      dk[, wkyr := yrwk]

      for (i in names(dk)) {
        if (!i %in% names_req) dk[, (i) := NULL]
      }
      saveRDS(
        dk,
        fd::path("results", "externalapi", "resYearLineMunicip.RDS")
      )

      fd::msg("Saving weekly not municip data for the external api")

      df <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly" &
            tag %in% x_tags &
            granularity_geo != "municip"
        ) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()
      df[, type := tag]
      df[, HelligdagIndikator := 0]
      df[, file := "x"]
      df[, displayDay := date]
      df[, location := location_code]
      df[, locationName := location_name]
      df[, county := county_code]
      df[, wkyr := yrwk]

      for (i in names(df)) {
        if (!i %in% names_req) df[, (i) := NULL]
      }
      saveRDS(
        df,
        fd::path("results", "externalapi", "resYearLine.RDS")
      )

      fd::msg("Saving outbreaks for the external api")

      GenerateOutbreakListInternal(
        df = df,
        dk = dk,
        saveFiles = fd::path("results", "externalapi", "outbreaks.RDS"),
        useType = TRUE
      )
    },
    email_external = function() {
      fd::msg("Generating external outbreak alerts")

      val <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::summarize(yrwk = max(yrwk, na.rm = T)) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      val <- val$yrwk

      d <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(
          granularity_time == "weekly" &
            yrwk == val
        ) %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      GenerateOutbreakListExternal(
        df = d[granularity_geo != "municip"],
        dk = d[granularity_geo == "municip"],
        saveFiles = fd::path("results", latest_date(), "outbreaks_alert_external.RDS")
      )

      fd::msg("Sending external emails")
      send_email <- fd::perform_weekly_action(
        file = fd::path("config", "email_external.txt"),
        dev_always_performs = TRUE
      )

      if (send_email) {
        try(EmailExternal(), TRUE)
      }
    },
    email_internal = function() {
      try(EmailTechnicalNewResults(), TRUE)
    },
    run_all = function() {
      run_analysis()
      save_internal_dashboard()
      save_external_api()
      email_external()
      email_internal()
    }
  )
)
