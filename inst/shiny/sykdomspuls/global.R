library(pool)

# load data in 'global' chunk so it can be shared by all users of the dashboard
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_DB", "sykdomspuls"),
  host = Sys.getenv("DB_SERVER", "db"),
  username =Sys.getenv("DB_USER", "root") ,
  password = Sys.getenv("DB_PASSWORD", "example")
)
