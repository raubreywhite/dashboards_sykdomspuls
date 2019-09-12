library(pool)
fd::initialize("sykdomspuls")

# load data in 'global' chunk so it can be shared by all users of the dashboard
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_DB", "sykdomspuls"),
  host = Sys.getenv("DB_SERVER", "db"),
  username =Sys.getenv("DB_USER", "root") ,
  password = Sys.getenv("DB_PASSWORD", "example")
)
# FUNCTIONS
Getlocation_name <- function(location) {
  location_name <- "Norge"
  locationHTML <- "Norge"

  if (location != "Norge") {
    location_name <- fhidata::norway_locations_long_current[location_code==location]$location_name
  }

  return(location_name)
}

GetCols <- function(){
  retval <- rev(fhiplot::warning_color)
  names(retval) <- NULL
  #retval <- c('#fc8d59','#ffffbf','#91cf60')
  return(retval)
}
