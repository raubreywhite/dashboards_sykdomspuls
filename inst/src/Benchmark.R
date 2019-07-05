fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))
suppressMessages(library(foreach))

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

fhi::Log("analyse1Before")
models <- list()
for (i in seq_along(sykdomspuls::CONFIG$SYNDROMES)) {
  models[[i]] <- quasipoission$new(conf = sykdomspuls::CONFIG$SYNDROMES[i], db_config =list(
    driver="MySQL",
    server="db",
    db="sykdomspuls",
    port = 3306,
    user="root",
    password="example"
  ))
}

# 1 update takes 8.4 min
# 1 update on desktop takes 5.8m
# 1 full run takes ? min
data.table::setDTthreads(1)
future::plan(future::multisession)
a1 <- Sys.time()
models[[1]]$run()
a2 <- Sys.time()
a2 - a1
data.table::setDTthreads(ceiling(parallel::detectCores()/2))

data.table::setDTthreads(1)



a1 <- Sys.time()
cl <- parallel::makeCluster(4L, file = "")
doParallel::registerDoParallel(cl)
base_folder <- fhi::DashboardFolder("data_clean")
latest_id <- sykdomspuls::LatestRawID()
foreach(i = 1:3, .packages = c("data.table"), .verbose = T) %dopar% {
  future::plan(future::multisession)
  data.table::setDTthreads(1)
  models[[i]]$run(base_folder = base_folder, latest_id = latest_id)
}
stopCluster(cl)
a2 <- Sys.time()
a2 - a1
