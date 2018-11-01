#' test
#' @param data a
#' @param files a
#' @import fhi
#' @export SaveData
SaveData <- function(data, files) {
  for (f in files) {
    saveRDS(data, f)
  }
}
