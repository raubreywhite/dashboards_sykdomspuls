fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))

## if (!dir.exists(fhi::DashboardFolder("results", "externalapi"))) dir.create(fhi::DashboardFolder("results", "externalapi"))
## if (!dir.exists(fhi::DashboardFolder("results", LatestRawID()))) dir.create(fhi::DashboardFolder("results", LatestRawID()))
## if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")))
## if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "stats")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "stats")))
## if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "skabb")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "skabb")))
## if (!dir.exists(fhi::DashboardFolder("data_raw", "normomo"))) dir.create(fhi::DashboardFolder("data_raw", "normomo"))


SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("data_app", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "externalapi/config.RDS"))

fhi::Log("numTags", nrow(CONFIG$SYNDROMES))
fhi::Log("versionAlgorithm", CONFIG$VERSION)
fhi::Log("versionPackage", packageDescription("sykdomspuls")$Version)


## fhi::Log("cleanBefore")
## if (!UpdateData()) {
##   fhi::DashboardMsg("Have not run analyses and exiting")
##   q(save = "no", status = 21)
## }
## DeleteOldDatasets()
## fhi::Log("cleanAfter")


for (modelName in names(sykdomspuls::CONFIG$MODELS)){
  fhi::DashboardMsg(paste("starting", modelName))
  modelConfig <- sykdomspuls::CONFIG$MODELS[[modelName]]
  model <- sykdomspuls::models[[modelName]]$new()

  model$setup_db()
  
  for (i in 1:nrow(modelConfig)) {
    conf <- modelConfig[i]
    model$run(conf)
  }
  model$save()
  fhi::DashboardMsg(paste("Ending", modelName))
}

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
