fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))

if (!dir.exists(fhi::DashboardFolder("results", "externalapi"))) dir.create(fhi::DashboardFolder("results", "externalapi"))
if (!dir.exists(fhi::DashboardFolder("results", LatestRawID()))) dir.create(fhi::DashboardFolder("results", LatestRawID()))
if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")))
if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "stats")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "stats")))
if (!dir.exists(fhi::DashboardFolder("results", file.path(LatestRawID(), "skabb")))) dir.create(fhi::DashboardFolder("results", file.path(LatestRawID(), "skabb")))
if (!dir.exists(fhi::DashboardFolder("data_raw", "normomo"))) dir.create(fhi::DashboardFolder("data_raw", "normomo"))

SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("data_app", "config.RDS"))
SaveRDS(ConvertConfigForAPI(), fhi::DashboardFolder("results", "externalapi/config.RDS"))

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

conn <- DBI::dbConnect(odbc::odbc(),
                     driver="MySQL",
                     server = "db",
                     port = 3306,
                     user = "root",
                     password = "example"
)

fd:::use_db(conn, "sykdomspuls")

sch$stack_x$conn <- conn
#sch$stack_x$db_drop_table()
sch$stack_x$db_create_table()

sch$results_x$conn <- conn
#sch$results_x$db_drop_table()
sch$results_x$db_create_table()

field.types <- vapply(res, DBI::dbDataType, dbObj = db,
                      FUN.VALUE = character(1))

fhi::Log("analyse1Before")
for (i in 1:nrow(sykdomspuls::CONFIG$SYNDROMES)) {
  conf <- sykdomspuls::CONFIG$SYNDROMES[i]
  fhi::DashboardMsg(conf$tag)

  data <- readRDS(file = fhi::DashboardFolder("data_clean", sprintf("%s_%s_cleaned.RDS", LatestRawID(), conf$tag)))
  load_stack_schema(conf = conf, schema = sch$stack_x)
  #data <- schema_and_data(conf = conf)

  to_run <- split(
    sch$stack_x$get_data_dt()[exists_in_db==FALSE],
    seq(nrow(sch$stack_x$get_data_dt()[exists_in_db==FALSE]))
  )

  out <-
    foreach(i = 1:max_indx, .packages = c('data.table'), .combine = data.table::rbind ) %dopar% {
      Psubset<-DataP[indx==i,]
      # do some operations on Psubset
    }

  res <- pbmclapply(to_run,
                    function(x){
                      new_data <- data[.(x$location, x$age)]
                      uuid <- x$uuid

                      retval <- QuasipoissonTrainPredictData(
                        datasetTrain = new_data[date >= x$date_train_min & date <= x$date_train_max],
                        datasetPredict = new_data[date >= x$date_predict_min & date <= x$date_predict_max],
                        isDaily = x$granularity_time == "daily",
                        v = v,
                        weeklyDenominatorFunction = ifelse(x$weeklyDenominatorFunction=="sum",sum,mean),
                        denominator_string = x$denominator
                      )
                      retval[,uuid:=uuid]
                      return(retval)
                    },
                    mc.cores = parallel::detectCores()
  )

  rm("data"); gc()

  res <- rbindlist(res)

  clean_post_analysis(res=res, schema = sch$stack_x)

  try(DBI::dbRemoveTable(conn,"temporary_table"),TRUE)
  sch$results_x$db_upsert_load_data_infile(res[,names(sch$results_x$db_field_types),with=F])
  try(DBI::dbRemoveTable(conn,"temporary_table"),TRUE)
  sch$stack_x$db_upsert_load_data_infile(sch$stack_x$dt[uuid %in% unique(res$uuid),names(sch$stack_x$db_field_types),with=F])

  rm("res"); gc()

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

# dk = readRDS(fhi::DashboardFolder("results", "resYearLineMunicip.RDS"))
