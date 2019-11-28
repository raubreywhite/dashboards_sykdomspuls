fhi::DashboardInitialiseOpinionated("sykdomspuls")
fd::initialize("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))
suppressMessages(library(foreach))

create_folders()

fhi::Log("numTags", nrow(CONFIG$SYNDROMES))
fhi::Log("versionAlgorithm", CONFIG$VERSION)
fhi::Log("versionPackage", packageDescription("sykdomspuls")$Version)

if(!fd::config$is_production) Sys.setenv(ONLY_RUN_LATEST_YEAR=TRUE)

fhi::Log("cleanBefore")
if (!UpdateData()) {
   fd::msg("Have not run analyses and exiting")
   q(save = "no", status = 21)
}
DeleteOldDatasets()
fhi::Log("cleanAfter")

fhi::Log("analyse1Before")
for (model_name in names(sykdomspuls::CONFIG$MODELS)){
  fd::msg(paste("starting", model_name), slack = TRUE)

  conf <- sykdomspuls::CONFIG$MODELS[[model_name]]
  db_config <- CONFIG$DB_CONFIG

  model <- models()[[model_name]]$new(
    conf=conf,
    db_config=db_config
  )

  model$run_all()

  fd::msg(paste("Ending", model_name), slack = TRUE)
}
fhi::Log("analyse1After")


## GENERATE LIST OF OUTBREAKS
#fhi::DashboardMsg("Secondary Analysis")
fhi::Log("analyse2Before")
#AnalysesSecondary()
fhi::Log("analyse2After")


# Done with analyses
#fhi::DashboardMsg("Analysing logs")
#AnalyseLogs()
fhi::Log("Done")
fhi::DashboardMsg("Finished analyses and exiting")

CreateLatestDoneFile()

date_results <- fd::tbl("spuls_standard_results") %>%
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
