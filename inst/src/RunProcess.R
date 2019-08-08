fhi::DashboardInitialiseOpinionated("sykdomspuls")
fd::initialize("sykdomspuls")
options(error=traceback)
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
   fhi::DashboardMsg("Have not run analyses and exiting")
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


# Done with analyses
fd::msg("Done with all analyses")

CreateLatestDoneFile()
cat("done", file = "/data_app/sykdomspuls/done.txt")

if (!fd::config$is_dev) quit(save = "no", status = 0)

## GENERATE LIST OF OUTBREAKS
fhi::DashboardMsg("Generate list of outbreaks")
fhi::Log("analyse2Before")
GenerateOutbreakListInternal()
GenerateOutbreakListInternal(
  saveFiles = fhi::DashboardFolder("results", "externalapi/outbreaks.RDS"),
  useType = TRUE
)
GenerateOutbreakListExternal()
AnalysesSecondary()
fhi::Log("analyse2After")


# Done with analyses
fhi::DashboardMsg("Done with all analyses")

CreateLatestDoneFile()
cat("done", file = "/data_app/sykdomspuls/done.txt")

## SENDING OUT EMAILS
EmailNotificationOfNewResults()
fhi::Log("Done")
## Saving log
log <- LogGet()
log[[length(log) + 1]] <- fhi::LogGet()
saveRDS(log, fhi::DashboardFolder("results", "log.RDS"))

AnalyseLogs()

fhi::DashboardMsg("Finished analyses and exiting")
if (!fhi::DashboardIsDev()) quit(save = "no", status = 0)
