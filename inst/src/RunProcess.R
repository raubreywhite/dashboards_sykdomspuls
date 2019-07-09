fhi::DashboardInitialiseOpinionated("sykdomspuls")
fd::initialize("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))
suppressMessages(library(foreach))

fs::dir_create(fd::path("results", "externalapi"))
fs::dir_create(fd::path("results", latest_date()))
fs::dir_create(fd::path("results", latest_date(), "standard"))
fs::dir_create(fd::path("results", latest_date(), "emerg"))
fs::dir_create(fd::path("results", latest_date(), "stats"))
fs::dir_create(fd::path("results", latest_date(), "skabb"))
fs::dir_create(fd::path("data_raw", "normomo"))

SaveRDS(ConvertConfigForAPI(), fd::path("results", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fd::path("data_app", "config.RDS"))

fhi::Log("numTags", nrow(CONFIG$SYNDROMES))
fhi::Log("versionAlgorithm", CONFIG$VERSION)
fhi::Log("versionPackage", packageDescription("sykdomspuls")$Version)


fhi::Log("cleanBefore")
if (!UpdateData()) {
  fhi::DashboardMsg("Have not run analyses and exiting")
  q(save = "no", status = 21)
}
DeleteOldDatasets()
fhi::Log("cleanAfter")


for (modelName in names(sykdomspuls::CONFIG$MODELS)){
  fhi::DashboardMsg(paste("starting", modelName))
  modelConfig <- sykdomspuls::CONFIG$MODELS[[modelName]]
  model <- models()[[modelName]]$new(conf=modelConfig,
                                                db_config=CONFIG$DB_CONFIG)
  model$run_analysis()
  fhi::DashboardMsg(paste("Ending", modelName))
}


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

fhi::DashboardMsg("Send data to DB")
fhi::Log("save2Before")
SaveShinyAppDataToDB()
fhi::Log("save2After")

# Done with analyses
fhi::DashboardMsg("Done with all analyses")

CreateLatestDoneFile()
cat("done", file = "/data_app/sykdomspuls/done.txt")

## SENDING OUT EMAILS
EmailNotificationOfNewResults()

## Saving log
log <- LogGet()
log[[length(log) + 1]] <- fhi::LogGet()
saveRDS(log, fhi::DashboardFolder("results", "log.RDS"))

fhi::DashboardMsg("Finished analyses and exiting")
if (!fhi::DashboardIsDev()) quit(save = "no", status = 0)
>>>>>>> richard-fork/master

## ## GENERATE LIST OF OUTBREAKS
## fhi::DashboardMsg("Generate list of outbreaks")
## fhi::Log("analyse2Before")
## GenerateOutbreakListInternal()
## GenerateOutbreakListInternal(
##   saveFiles = fhi::DashboardFolder("results", "externalapi/outbreaks.RDS"),
##   useType = TRUE
## )
## GenerateOutbreakListExternal()
## AnalysesSecondary()
## fhi::Log("analyse2After")

## fhi::DashboardMsg("Send data to DB")
## fhi::Log("save2Before")
## SaveShinyAppDataToDB()
## fhi::Log("save2After")

## # Done with analyses
## fhi::DashboardMsg("Done with all analyses")

## CreateLatestDoneFile()
## cat("done", file = "/data_app/sykdomspuls/done.txt")

## ## SENDING OUT EMAILS
## EmailNotificationOfNewResults()

## ## Saving log
## log <- LogGet()
## log[[length(log) + 1]] <- fhi::LogGet()
## saveRDS(log, fhi::DashboardFolder("results", "log.RDS"))

## fhi::DashboardMsg("Finished analyses and exiting")
## if (!fhi::DashboardIsDev()) quit(save = "no", status = 0)

## # dk = readRDS(fhi::DashboardFolder("results", "resYearLineMunicip.RDS"))
