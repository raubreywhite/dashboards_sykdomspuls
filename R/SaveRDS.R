#' Save data into multiple RDS files
#' @param data a
#' @param files a
#' @import fhi
#' @export SaveRDS
SaveRDS <- function(data, files) {
  for (f in files) {
    saveRDS(data, f)
  }
}
