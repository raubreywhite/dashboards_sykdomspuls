.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PACKAGE: sykdomspuls")
  packageStartupMessage("Version 2018.11.20 at 15:34")
  packageStartupMessage("Developed by Richard White")
  packageStartupMessage("Department of Infectious Disease Epidemiology and Modelling")
  packageStartupMessage("Norwegian Institute of Public Health")
  packageStartupMessage("https://folkehelseinstituttet.github.io/dashboards_sykdomspuls/\n")

  if(max(norwayMunicipMerging$year)!=RAWmisc::YearN(lubridate::today())){
    packageStartupMessage("\u2716 norwayMunicipMerging is out of date. Regenerating.")
    GenNorwayMunicipMerging()
    norwayMunicipMerging <<- readRDS(system.file("createddata","norwayMunicipMerging.RDS",package="sykdomspuls"))
  }
  packageStartupMessage("\u2713 norwayMunicipMerging is up to date")
}
