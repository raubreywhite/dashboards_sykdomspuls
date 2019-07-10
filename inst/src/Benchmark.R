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

conf <- sykdomspuls::CONFIG$MODELS[["standard"]]
db_config <- CONFIG$DB_CONFIG

model <- models()[["standard"]]$new(
  conf=conf,
  db_config=db_config
)

tags <- model$tags

tags[[1]]$run_age(
  age = "Totalt",
  base_folder = fd::path("data_clean"),
  latest_id= sykdomspuls::LatestRawID()
)

model$run_all()

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
