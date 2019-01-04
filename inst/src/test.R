fhi::DashboardInitialiseOpinionated("sykdomspuls")

suppressMessages(library(data.table))
library(pbmcapply)
# A lazy and chatty sqrt function.
# An example of passing arguments to pbmclapply.
lazyChattySqrt <- function(num, name) {
  # Sleep randomly between 0 to 0.5 second
  Sys.sleep(runif(1, 0, 0.5))
  return(sprintf("Hello %s, the sqrt of %f is %f.", toString(name), num, sqrt(num)))
}
# Get the sqrt of 1-3 in parallel
chattyResult <- pbmclapply(1:300, lazyChattySqrt, "Bob", mc.cores = 1)


conf <- sykdomspuls::CONFIG$SYNDROMES[i]
fhi::DashboardMsg(conf$tag)

stackAndData <- StackAndEfficientDataForAnalysis(conf = conf, strataSize = 1)
stackStrata <- stackAndData$analysesStrata
stack <- stackAndData$analyses
data <- stackAndData$data

a0 <-Sys.time()
res <- pbmclapply(retval[1:500],
                  function(x) RunOneAnalysis(analysesStack = x$stack, analysisData = x$data),
                  mc.cores = parallel::detectCores())
a1 <-Sys.time()
a1 - a0


cl <- makeCluster(parallel::detectCores())
registerDoSNOW(cl)

if (i == 1) {
  fhi::DashboardMsg("Initializing progress bar")
  PBInitialize(n = round(length(stackStrata) * nrow(sykdomspuls::CONFIG$SYNDROMES)))
}
b0 <- Sys.time()
res <- foreach(
  analysisIter = StackIterator(stackStrata[1:500], stack, data, PBIncrement),
  .noexport = c("data"),
  .packages = c("data.table", "sykdomspuls")
) %dopar% {
  retval <- lapply(
    analysisIter,
    function(x) RunOneAnalysis(analysesStack = x$stack, analysisData = x$data)
  )

  rbindlist(retval)
}
b1 <- Sys.time()

a1 - a0 #49
b1 -b0
