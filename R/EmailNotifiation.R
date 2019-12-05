#' Email notification of new data
#' @param files The raw data file that is being analysed
#' @export EmailNotificationOfNewData
EmailNotificationOfNewData <- function(files) {
  tags <- paste0(CONFIG$SYNDROMES$tag, collapse = "</li><li>")
  files <- paste0(files, collapse = "</li><li>")

  html <- glue::glue("
    New Sykdomspulsen data has been received and signal processing has begun.

    New results should be available in around two hours.

    Tags being processed are:

    <li> {tags} </li>

    Files being processed are:

    <li> {files} </li>
    ")

  fd::mailgun(
    subject = "New Sykdomspuls data",
    html = html,
    bcc = fd::e_emails("sykdomspuls_data")
  )

  return(0)
}

#' Internal email notifying about new results
#' @export EmailTechnicalNewResults
EmailTechnicalNewResults <- function() {
  html <- glue::glue("
    New Sykdomspulsen results available at <a href='http://smhb.fhi.no/'>http://smhb.fhi.no/</a>
    ")

  fd::mailgun(
    subject = "Nye resultater til sykdomspulsen klar",
    html = html,
    bcc = fd::e_emails("sykdomspuls_data")
  )
}
