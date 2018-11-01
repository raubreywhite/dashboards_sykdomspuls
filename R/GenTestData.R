GenTestData <- function() {
  # variables used in data.table functions in this function
  . <- NULL
  municip <- NULL
  consult <- NULL
  influensa <- NULL
  gastro <- NULL
  respiratory <- NULL
  age <- NULL
  value <- NULL
  consultWithoutInfluensa <- NULL
  HelligdagIndikator <- NULL
  consultWithInfluensa <- NULL
  # end

  print(1)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0301", "municip1503", "municip1505")]

  for (i in CONFIG$SYNDROMES) {
    d[get(i) < 10, (i) := 0]
    d[, (i) := get(i) + rpois(.N, 500)]
  }
  d[, consult := influensa + gastro + respiratory + 100 + rpois(.N, 500)]

  saveRDS(d, file = "/git/dashboards_sykdomspuls/tests/testthat/data/partially_formatted_2017_05_09.RDS")

  print(2)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0706", "municip0719", "municip0720", "municip0710")]

  for (i in CONFIG$SYNDROMES) {
    d[get(i) < 10, (i) := 0]
    d[, (i) := get(i) + rpois(.N, 500)]
  }
  d[, consult := influensa + gastro + respiratory + 100 + rpois(.N, 500)]

  saveRDS(d, file = "/git/dashboards_sykdomspuls/tests/testthat/data/partially_formatted_sandefjord.RDS")

  print(3)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip0301")]
  population <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "pop.RDS"))
  for (i in 1:length(population)) {
    if (!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
    SYNDROME = "influensa",
    population = population,
    hellidager = hellidager,
    testIfHelligdagIndikatorFileIsOutdated = FALSE
  )

  res <- res[age == "Totalt", .(
    value = sum(value),
    consultWithoutInfluensa = sum(consultWithoutInfluensa),
    HelligdagIndikator = mean(HelligdagIndikator)
  ),
  by = .(date, municip)
  ]

  res[, value := value + sample(c(-5:5), .N, replace = T)]
  res[value < 0, value := 0]
  res[, consultWithoutInfluensa := consultWithoutInfluensa + sample(c(-5:5), .N, replace = T)]
  res[, consultWithInfluensa := consultWithoutInfluensa + value]

  saveRDS(res, file = "/git/dashboards_sykdomspuls/tests/testthat/data/formatted_oslo.RDS")

  print(4)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip1711")]
  population <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "pop.RDS"))
  for (i in 1:length(population)) {
    if (!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
    SYNDROME = "gastro",
    population = population,
    hellidager = hellidager,
    testIfHelligdagIndikatorFileIsOutdated = FALSE
  )
  res <- res[age == "Totalt"]
  res[, value := value + sample(c(-5:5), .N, replace = T)]
  res[value < 0, value := 0]

  res[, consultWithoutInfluensa := consultWithoutInfluensa + sample(c(-5:5), .N, replace = T)]
  res[, consultWithInfluensa := consultWithoutInfluensa]

  saveRDS(res, file = "/git/dashboards_sykdomspuls/tests/testthat/data/formatted_meraker.RDS")

  print(5)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip5001", "municip1601")]
  population <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "pop.RDS"))
  for (i in 1:length(population)) {
    if (!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager <- readRDS(file.path("/git/dashboards_sykdomspuls/tests/testthat", "data", "hellidager.RDS"))

  res <- sykdomspuls::FormatData(d,
    SYNDROME = "influensa",
    population = population,
    hellidager = hellidager,
    testIfHelligdagIndikatorFileIsOutdated = FALSE
  )
  res <- res[municip == "municip5001"]
  res[, value := value + sample(c(-10:10), .N, replace = T)]
  res[value < 0, value := 0]
  res[, consultWithoutInfluensa := consultWithoutInfluensa + sample(c(-10:10), .N, replace = T)]
  res[, consultWithInfluensa := consultWithoutInfluensa + value]

  saveRDS(res, file = "/git/dashboards_sykdomspuls/tests/testthat/data/formatted_trondheim.RDS")

  print(6)
  set.seed(4)
  d <- fread("/docs/testing/dashboards/data_raw/sykdomspuls/partially_formatted_2017_12_11.txt")
  d <- d[municip %in% c("municip1723", "municip1756", "municip5053")]

  for (i in CONFIG$SYNDROMES) {
    d[get(i) < 10, (i) := 0]
    d[, (i) := get(i) + rpois(.N, 500)]
  }
  d[, consult := influensa + gastro + respiratory + 100 + rpois(.N, 500)]

  saveRDS(d, file = "/git/dashboards_sykdomspuls/tests/testthat/data/inderoy_partially_formatted_2017_05_09.RDS")
}
