#' Get Log
#' @export
LogGet <- function(){
  if (file.exists(fhi::DashboardFolder("results", "log.RDS"))) {
    log <- readRDS(fhi::DashboardFolder("results", "log.RDS"))
  } else {
    log <- vector("list")
  }

  return(log)
}
