library(flexdashboard)
library(ggplot2)
library(ggrepel)
library(data.table)
library(fhi)
library(pool)

# load data in 'global' chunk so it can be shared by all users of the dashboard
CONFIG <- readRDS("/data_app/sykdomspuls/config.RDS")
GLOBAL <- readRDS("/data_app/sykdomspuls/GLOBAL.RDS")

GetLatestDataTime <- function() {
  if(file.exists("/data_app/sykdomspuls/done.txt")){
    return(file.info("/data_app/sykdomspuls/done.txt")$mtime[1])
  } else {
  	return(Sys.time())
  }
}
latestDataTime <- GetLatestDataTime()

pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "sykdomspuls",
  host = "db",
  username = "root",
  password = "example"
)
