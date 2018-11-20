#' PBInitialize
#' @param n a
#' @param flush a
#' @export PBInitialize
PBInitialize <- function(n, flush=fhi::DashboardIsDev()){
  PB$i <- 0
  PB$pb <- RAWmisc::ProgressBarCreate(max = n, flush=flush)
}

#' PBIncrement
#' @export PBIncrement
PBIncrement <- function(){
  PB$i <- PB$i + 1
  RAWmisc::ProgressBarSet(PB$pb, PB$i)
}

