.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: sykdomspuls")
  packageStartupMessage("Version 2018.12.19 at 14:10")
  packageStartupMessage("Developed by Richard White")
  packageStartupMessage("Department of Infectious Disease Epidemiology and Modelling")
  packageStartupMessage("Norwegian Institute of Public Health")
  packageStartupMessage("https://folkehelseinstituttet.github.io/dashboards_sykdomspuls/\n")

  if (max(norwayMunicipMerging$year) != RAWmisc::YearN(lubridate::today())) {
    packageStartupMessage("\u2716 norwayMunicipMerging is out of date. Regenerating.")
    norwayMunicipMerging <<- GenNorwayMunicipMerging()
  }
  packageStartupMessage("\u2713 norwayMunicipMerging is up to date.")
}
