library(pool)

# load data in 'global' chunk so it can be shared by all users of the dashboard
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "sykdomspuls",
  host = "db",
  username = "root",
  password = "example"
)
