#' GetAlertsEmails
#' @importFrom readxl read_excel
#' @export GetAlertsEmails
GetAlertsEmails <- function(){
  if(fhi::DashboardIsProduction()){
    return(readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert.xlsx")))
  } else {
    return(readxl::read_excel(file.path("/etc", "gmailr", "emails_sykdomspuls_alert_test.xlsx")))
  }
}
