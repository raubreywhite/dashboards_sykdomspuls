fhi::DashboardInitialiseOpinionated("sykdomspuls", PACKAGE_DIR = ".")

suppressMessages(library(data.table))
suppressMessages(library(foreach))
suppressMessages(library(doSNOW))
suppressMessages(library(iterators))

SaveData(ConvertConfigForAPI(), fhi::DashboardFolder("results", "config.RDS"))
SaveData(ConvertConfigForAPI(), fhi::DashboardFolder("data_app", "config.RDS"))

if (!UpdateData()) {
  fhi::DashboardMsg("Have not run analyses and exiting")
  q(save = "no", status = 21)
}
DeleteOldDatasets()

if(!fhi::DashboardIsDev()){
  fhi::DashboardMsg("Registering cluster", newLine=T)
  cl <- makeCluster(parallel::detectCores())
  registerDoSNOW(cl)
}

for (i in 1:nrow(sykdomspuls::CONFIG$SYNDROMES)) {
  conf <- sykdomspuls::CONFIG$SYNDROMES[i]
  fhi::DashboardMsg(conf$tag)

  stackAndData <- StackAndEfficientDataForAnalysis(conf=conf)
  data <- stackAndData$data
  stack <- stackAndData$analyses

  if (i==1) {
    fhi::DashboardMsg("Initializing progress bar")
    PBInitialize(n=nrow(stack) * nrow(sykdomspuls::CONFIG$SYNDROMES))
  }

  fhi::DashboardMsg("Setting keys for binary search")
  setkeyv(data, c("location", "age"))

  res <- foreach(analysisIter = StackIterator(stack, data, PBIncrement), .noexport = c("data")) %dopar% {
    if (!fhi::DashboardIsDev()) {
      library(data.table)
      library(sykdomspuls)
    }

    exceptionalFunction <- function(err) {
      fhi::DashboardMsg(err,syscallsDepth = 10, newLine=T)
      fhi::DashboardMsg(analysisIter$stack, newLine=T)
    }

    analysesStack <- analysisIter$stack
    analysisData <- analysisIter$data

    retval <- tryCatch(
      RunOneAnalysis(analysesStack = analysesStack, analysisData = analysisData),
      error = exceptionalFunction
    )

    retval
  }
  res <- rbindlist(res)

  # cleaning on small municipalities
  res[location %in% CONFIG$smallMunicips & age != "Totalt", n := 0 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold2 := 5 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold4 := 10 ]

  fhi::DashboardMsg("Saving files", newLine=T)
  for(f in unique(res$file)){
    fhi::DashboardMsg(sprintf("Saving file %s",f))
    saveRDS(res[file==f], file=fhi::DashboardFolder("results", f))
  }

  rm("res", "data", "stackAndData")
}

if(!fhi::DashboardIsDev()){
  fhi::DashboardMsg("Stopping cluster", newLine=T)
  stopCluster(cl)
}

# Append all the syndromes together
ResultsAggregateApply()

## GENERATE LIST OF OUTBREAKS
fhi::DashboardMsg("Generate list of outbreaks")
GenerateOutbreakListInternal()
GenerateOutbreakListExternal()

# Done with analyses
fhi::DashboardMsg("Done with all analyses")

CreateLatestDoneFile()
cat("done", file = "/data_app/sykdomspuls/done.txt")

## SENDING OUT EMAILS
EmailNotificationOfNewResults()

fhi::DashboardMsg("Finished analyses and exiting")
quit(save = "no", status = 0)

dk = readRDS(fhi::DashboardFolder("results", "resYearLineMunicip.RDS"))
