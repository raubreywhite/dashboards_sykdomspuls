fhi::DashboardInitialiseOpinionated("sykdomspuls")
fd::initialize("sykdomspuls")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(pbmcapply))
suppressMessages(library(foreach))

fs::dir_create(fd::path("results", "externalapi"))
fs::dir_create(fd::path("results", latest_date()))
fs::dir_create(fd::path("results", latest_date(), "standard"))
fs::dir_create(fd::path("results", latest_date(), "emerg"))
fs::dir_create(fd::path("results", latest_date(), "stats"))
fs::dir_create(fd::path("results", latest_date(), "skabb"))
fs::dir_create(fd::path("data_raw", "normomo"))

conf <- sykdomspuls::CONFIG$MODELS[["standard"]]
db_config <- fd::config$db_config

model <- models()[["standard"]]$new(
  conf=conf,
  db_config=db_config
)

tags <- model$tags
conf=conf[1]
stack_x=tags[[1]]$stack_x

tags[[1]]$run_age(
  age = "Totalt",
  base_folder = fd::path("data_clean"),
  latest_id= sykdomspuls::LatestRawID()
)


val <- tags[[1]]$results_x$dplyr_tbl() %>%
  dplyr::filter(
    granularity_time=="daily"
  ) %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()
val



sql <- glue::glue("
          ALTER DATABASE sykdomspuls CHARACTER SET utf8 COLLATE utf8_general_ci;
          ")
a <- DBI::dbExecute(conn, sql)

sql <- glue::glue("
SET character_set_results = 'utf8',
character_set_client = 'utf8',
character_set_connection = 'utf8',
character_set_database = 'utf8',
character_set_server = 'utf8'
          ")
a <- DBI::dbExecute(conn, sql)
DBI::dbGetQuery(conn, "show variables like 'character_set_%'")


x <- data.table(a="øå")

DBI::dbRemoveTable(conn,"x")
DBI::writeTable(conn,"x",x,append=T)
DBI::dbReadTable(conn, "x")


conn <- tags[[1]]$results_x$conn

library(data.table)
conn <- DBI::dbConnect(odbc::odbc(),
                       driver = "MySQL",
                       server = "db",
                       port = 3306,
                       user = "root",
                       password = "example",
                       encoding = "utf8"
)
DBI::dbExecute(conn, 'set character set utf8')
DBI::dbExecute(conn, glue::glue({
  "USE sykdomspuls;"
}))

DBI::dbRemoveTable(conn,"x")
sql <- DBI::sqlCreateTable(conn,"x",x)
sql@.Data <- stringr::str_replace_all(sql@.Data,"TEXT","TEXT CHARACTER SET utf8")
sql@.Data <- paste0(sql@.Data,"COLLATE='utf8_unicode_ci'")
a <- DBI::dbExecute(conn, sql)
sql <- glue::glue("
INSERT INTO x
VALUES(_utf8'\u00C5')
")
a <- DBI::dbExecute(conn, sql)
a <- DBI::dbReadTable(conn, "x", encoding="utf8")
a


x <- data.table(a="\u00C5 ø")
DBI::dbWriteTable(conn,"x",x)
DBI::dbReadTable(conn, "x")

values <- DBI::sqlData(conn, x, row.names = F)



DBI::dbGetQuery(conn, "select CHARACTER_SET_NAME from information_schema.columns where COLUMN_NAME='a' and TABLE_NAME = 'x'")

x <- data.table(a="\u00C5")
DBI::dbRemoveTable(conn,"x")
sql <- DBI::sqlCreateTable(conn,"x",x)
a <- DBI::dbExecute(conn, sql)

sql <- glue::glue("
INSERT INTO x
VALUES('\u00C5')
")
values <- DBI::sqlData(conn, as.data.frame(x))
sql <- paste0(
"INSERT INTO x\n",
"VALUES('",values$a,"')"
)

library(dplyr)
y <- dplyr::tbl(conn,"x") %>% collect()
str(y)
setDT(y)
for(i in names(y)){
  try(if(Encoding(y[[i]])=="UTF-8") y[,(i):=iconv(get(i),from="latin1",to="utf8")],TRUE)
}
y
Encoding(y$a)

a <- DBI::dbExecute(conn, sql)
a <- DBI::dbReadTable(conn, "x", encoding="utf8")
a
Encoding(a$a) <- "latin1"
a

sql <- glue::glue("
          ALTER DATABASE sykdomspuls CHARACTER SET utf8 COLLATE utf8_general_ci;
          ")
a <- DBI::dbExecute(conn, sql)
sql <- glue::glue("
          ALTER TABLE x CHARACTER SET utf8 COLLATE utf8_general_ci;
          ")
a <- DBI::dbExecute(conn, sql)

sql <- glue::glue("
INSERT INTO x
VALUES('ø')
")
a <- DBI::dbExecute(conn, sql)
a <- DBI::dbReadTable(conn, "x")
a

sql <- glue::glue("
INSERT INTO x
VALUES('\u00C5')
")
a <- DBI::dbExecute(conn, sql)
a <- DBI::dbReadTable(conn, "x")
a

DBI::dbWriteTable(conn,"x",x,append=T)
a <- DBI::dbReadTable(conn, "x")
a

DBI::dbWriteTable(conn,"x",x,overwrite=T)
DBI::dbReadTable(conn, "x")

fwrite(x,file = "/xtmp/x123.csv")
write.csv(x,file = "/xtmp/x123.csv", row.names = F, fileEncoding = "utf-8")

sep <- ","
eol <- "\n"
quote <- '"'
skip <- 0
header <- T
path <- normalizePath("/xtmp/x123.csv", winslash = "/", mustWork = TRUE)

sql <- paste0(
  "LOAD DATA INFILE ", DBI::dbQuoteString(conn, path), "\n",
  "INTO TABLE ", DBI::dbQuoteIdentifier(conn, "x"), "\n",
  "CHARACTER SET utf8", "\n",
  "FIELDS TERMINATED BY ", DBI::dbQuoteString(conn, sep), "\n",
  "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(conn, quote), "\n",
  "LINES TERMINATED BY ", DBI::dbQuoteString(conn, eol), "\n",
  "IGNORE ", skip + as.integer(header), " LINES \n",
  "(", paste0(names(x), collapse = ","), ")"
)
DBI::dbExecute(conn, sql)
DBI::dbReadTable(conn, "x")

a <- DBI::dbReadTable(conn, "x")
a
Encoding(a$a) = "UTF-8"
a

sql <- glue::glue("
          ALTER DATABASE sykdomspuls CHARACTER SET utf8 COLLATE utf8_general_ci;
          ")
a <- DBI::dbExecute(conn, sql)

DBI::dbGetQuery(conn, "select CHARACTER_SET_NAME from information_schema.columns where COLUMN_NAME='a' and TABLE_NAME = 'x'")

conn <- tags[[1]]$results_x$conn
DBI::dbGetQuery(conn, "show variables like 'character_set_%'")

DBI::dbGetQuery(conn, "select hex(convert(unhex('c5') using ucs2));")

showMethods("dbWriteTable")


DBI::dbListTables(conn)
DBI::dbRemoveTable(conn,"spuls_standard_results")
DBI::dbRemoveTable(conn,"spuls_standard_analyses")

table <- "x"
fields <- tags[[1]]$results_field_types
fields[fields=="TEXT"] <- "TEXT CHARACTER SET utf8 COLLATE utf8_unicode_ci"
x <- DBI::sqlCreateTable(conn, table, fields)

CREATE TABLE table_name (
  column1 datatype,
  column2 datatype,
  column3 datatype,
  ....
);


microbenchmark::microbenchmark(counties <- unique(data[granularityGeo == "municip"]$county))
microbenchmark::microbenchmark(counties <- data[granularityGeo == "municip",unique(county)])

data <- data[.("Norge")]
data[,denominator:=consultWithoutInfluensa]
data[, year := as.numeric(format.Date(date, "%G"))] # Week-based year, instead of normal year (%Y)
data[, week := as.numeric(format.Date(date, "%V"))] # Week-based year, instead of normal year (%Y)

microbenchmark::microbenchmark(RAWmisc::WeekC(rep("2018-01-01",1000)))
microbenchmark::microbenchmark(fhi::isoweek_c(rep("2018-01-01",1000)))
microbenchmark::microbenchmark(fhi::isoweek_n(rep("2018-01-01",1000)))
microbenchmark::microbenchmark(fhi::isoyear_n(rep("2018-01-01",1000)))
microbenchmark::microbenchmark(fhi::isoyear_c(rep("2018-01-01",1000)))

fhi::Log("analyse1Before")
models <- list()
for (i in seq_along(sykdomspuls::CONFIG$SYNDROMES)) {
  models[[i]] <- quasipoission$new(conf = sykdomspuls::CONFIG$SYNDROMES[i], db_config =list(
    driver="MySQL",
    server="db",
    db="sykdomspuls",
    port = 3306,
    user="root",
    password="example"
  ))
}

# 1 update takes 8.4 min
# 1 update on desktop takes 5.8m
# 1 full run takes ? min
data.table::setDTthreads(1)
a1 <- Sys.time()
models[[1]]$run()
a2 <- Sys.time()
a2 - a1
data.table::setDTthreads(ceiling(parallel::detectCores()/2))

data.table::setDTthreads(1)



a1 <- Sys.time()
cl <- parallel::makeCluster(4L, file = "")
doParallel::registerDoParallel(cl)
base_folder <- fhi::DashboardFolder("data_clean")
latest_id <- sykdomspuls::LatestRawID()
foreach(i = 1:3, .packages = c("data.table"), .verbose = T) %dopar% {
  data.table::setDTthreads(1)
  models[[i]]$run(base_folder = base_folder, latest_id = latest_id)
}
stopCluster(cl)
a2 <- Sys.time()
a2 - a1
