fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))

if (!dir.exists(fhi::DashboardFolder("results", "externalapi"))) dir.create(fhi::DashboardFolder("results", "externalapi"))
if (!dir.exists(fhi::DashboardFolder("results", LatestRawID()))) dir.create(fhi::DashboardFolder("results", LatestRawID()))
if (!dir.exists(fhi::DashboardFolder("data_raw", "normomo"))) dir.create(fhi::DashboardFolder("data_raw", "normomo"))

SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("data_app", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "externalapi/config.RDS"))

fhi::Log("cleanBefore")
if (!UpdateData()) {
  fhi::DashboardMsg("Have not run analyses and exiting")
  q(save = "no", status = 21)
}
DeleteOldDatasets()
fhi::Log("cleanAfter")

fhi::Log("analyse1Before")
for (i in 1:nrow(sykdomspuls::CONFIG$SYNDROMES)) {
  conf <- sykdomspuls::CONFIG$SYNDROMES[i]
  fhi::DashboardMsg(conf$tag)

  stackAndData <- StackAndEfficientDataForAnalysisInList(conf = conf)

  res <- pbmclapply(stackAndData,
    function(x) RunOneAnalysis(analysesStack = x$stack, analysisData = x$data),
    mc.cores = parallel::detectCores()
  )

  res <- rbindlist(res)

  # adding in extra information
  AddLocationName(res)
  AddCounty(res)

  # cleaning on small municipalities
  res[location %in% CONFIG$smallMunicips & age != "Totalt", n := 0 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold2 := 5 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold4 := 10 ]

  fhi::DashboardMsg("Saving files", newLine = T)
  for (f in unique(res$file)) {
    fhi::DashboardMsg(sprintf("Saving file %s", f))
    saveRDS(res[file == f], file = fhi::DashboardFolder("results", sprintf("%s/%s", LatestRawID(), f)))
  }

  rm("res", "stackAndData")

  # displaying timing information
  timeElapsed <- as.numeric(difftime(Sys.time(), fhi::LogGet()$analyse1Before, units = "min"))
  timeTotal <- (timeElapsed / i) * nrow(sykdomspuls::CONFIG$SYNDROMES)
  timeRemaining <- timeTotal * (nrow(sykdomspuls::CONFIG$SYNDROMES) - i) / nrow(sykdomspuls::CONFIG$SYNDROMES)
  fhi::DashboardMsg(sprintf(
    "%s min total, %s min elapsed, %s min remaining",
    round(timeTotal),
    round(timeElapsed),
    round(timeRemaining)
  ))
}
fhi::Log("analyse1After")


# Append all the syndromes together
fhi::Log("save1Before")
ResultsAggregateApply()
fhi::Log("save1After")

## GENERATE LIST OF OUTBREAKS
fhi::DashboardMsg("Generate list of outbreaks")
fhi::Log("analyse2Before")
GenerateOutbreakListInternal()
GenerateOutbreakListInternal(
  saveFiles = fhi::DashboardFolder("results", "externalapi/outbreaks.RDS"),
  useType = TRUE
)
GenerateOutbreakListExternal()
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
if (file.exists(fhi::DashboardFolder("results", "log.RDS"))) {
  log <- readRDS(fhi::DashboardFolder("results", "log.RDS"))
} else {
  log <- vector("list")
}
log[[length(log) + 1]] <- fhi::LogGet()
saveRDS(log, fhi::DashboardFolder("results", "log.RDS"))

fhi::DashboardMsg("Finished analyses and exiting")
if (!fhi::DashboardIsDev()) quit(save = "no", status = 0)

# dk = readRDS(fhi::DashboardFolder("results", "resYearLineMunicip.RDS"))
