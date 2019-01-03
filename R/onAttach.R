.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: sykdomspuls")
  packageStartupMessage("Version 2019.01.03 at 02:10")
  packageStartupMessage("Developed by Richard White")
  packageStartupMessage("Department of Infectious Disease Epidemiology and Modelling")
  packageStartupMessage("Norwegian Institute of Public Health")
  packageStartupMessage("https://folkehelseinstituttet.github.io/dashboards_sykdomspuls/\n")

  CheckOutOfDate()

  for (i in seq_along(CONFIG$outOfDate)) {
    if (CONFIG$outOfDate[[i]]) {
      packageStartupMessage(sprintf("\u2716 %s is out of date.", names(CONFIG$outOfDate)[i]))
    } else {
      packageStartupMessage(sprintf("\u2713 %s is up to date.", names(CONFIG$outOfDate)[i]))
    }
  }
}
