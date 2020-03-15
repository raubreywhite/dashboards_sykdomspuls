fd::initialize("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))
suppressMessages(library(foreach))

create_folders()

if(!fd::config$is_production) Sys.setenv(ONLY_RUN_LATEST_YEAR=TRUE)

if (!UpdateData()) {
   fd::msg("Have not run analyses and exiting")
   q(save = "no", status = 21)
}
DeleteOldDatasets()

for (model_name in names(sykdomspuls::CONFIG$MODELS)){
  fd::msg(paste("starting", model_name), slack = TRUE)

  conf <- sykdomspuls::CONFIG$MODELS[[model_name]]
  db_config <- fd::config$db_config

  model <- models()[[model_name]]$new(
    conf=conf,
    db_config=db_config
  )

  model$run_all()

  fd::msg(paste("Ending", model_name), slack = TRUE)
}

CreateLatestDoneFile()

# date_results <- fd::tbl("spuls_standard_results") %>%
#   dplyr::filter(granularity_time=="daily") %>%
#   dplyr::summarise(date=max(date, na.rm=T)) %>%
#   dplyr::collect() %>%
#   fd::latin1_to_utf8()
# date_results <- date_results$date
date_results <- fd::tbl("spuls_mem_results") %>%
  dplyr::summarise(date=max(date, na.rm=T)) %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

date_results <- fd::tbl("spuls_standard_results") %>%
  dplyr::filter(granularity_time=="daily") %>%
  dplyr::summarise(date=max(date, na.rm=T)) %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()
date_results <- date_results$date

fd::update_rundate(
  package="sykdomspuls",
  date_extraction = latest_date(),
  date_results = date_results,
  date_run = lubridate::today()
)

if(!fd::config$is_dev) quit(save="no", status=0)
