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

      cl <- parallel::makeCluster(3L, outfile = "")
      doParallel::registerDoParallel(cl)
      base_folder <- fd::path("data_clean")
      latest_id <- LatestRawID()

      foreach(i = 1:length(tags), .packages = c("data.table", "sykdomspuls"), .verbose = T, .export = "tags") %dopar% {
        Sys.sleep((i - 1) * 10)
        data.table::setDTthreads(1)
        tags[[i]]$run(base_folder = base_folder, latest_id = latest_id)
      }
      stopCluster(cl)

      data.table::setDTthreads(parallel::detectCores() - 2)

      fd::msg("Finished standard in parallel")
    },
    save_external_api = function() {
      fd::msg("Saving config for the external api")

      saveRDS(ConvertConfigForAPI(), fd::path("results", "externalapi", "config.RDS"), version = 2)

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
      d[location_code == "norge", location_code := "Norge"]

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
        fd::path("results", "externalapi", "resRecentLine.RDS"),
        version = 2
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
      dk[location_code == "norge", location_code := "Norge"]

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
        fd::path("results", "externalapi", "resYearLineMunicip.RDS"),
        version = 2
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
      df[location_code == "norge", location_code := "Norge"]

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
        fd::path("results", "externalapi", "resYearLine.RDS"),
        version = 2
      )

      fd::msg("Saving outbreaks for the external api")

      outbreaks <- GenerateOutbreakListAPI(
        df = df,
        dk = dk,
        saveFiles = NULL,
        useType = TRUE
      )
      saveRDS(outbreaks, fd::path("results", "externalapi", "outbreaks.RDS"), version = 2)
    },
    email_internal = function() {
      try(EmailTechnicalNewResults(), TRUE)
    },

    save_latest_data = function(years = 2) {
      latest_year_query <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::summarise(max(year, na.rm = TRUE)) %>%
        dplyr::collect()
      latest_year <- latest_year_query[[1]]
      data <- tags[[1]]$results_x$dplyr_tbl() %>%
        dplyr::filter(year > latest_year - years) %>%
        dplyr::collect()
      setDT(data)
      saveRDS(data, file = fd::path("results", latest_date(), "standard", "latest_data.RDS"))
    },
    restart_shiny_server = function() {
      system("touch /srv/shiny-server/sykdomspuls/restart.txt")
    },
    run_all = function() {
      run_analysis()
      save_latest_data(years = 2)
      save_external_api()
      restart_shiny_server()
    }
  )
)
