AlertsEmailConverter <- function(emails) {
  setDT(emails)
  emails[, statuses := vector("list", length = .N)]
  emails[, statuses := rep(list(c("High", "Medium")), .N)]
  emails[level == "high", statuses := rep(list(c("High")), .N)]

  return(emails)
}

#' Get the correct alerts email excel sheet
#'
#' If this function is run on a production machine,
#' then the production email file will be returned.
#' Otherwise it will return the test file.
#' @importFrom readxl read_excel
#' @export GetAlertsEmails
GetAlertsEmails <- function() {
  if (fhi::DashboardIsProduction()) {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx"))
  } else {
    retval <- readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx"))
  }

  retval <- AlertsEmailConverter(retval)

  return(retval)
}
