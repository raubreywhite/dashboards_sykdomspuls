context("FormatData")
test_that("Basic Oslo", {
  library(data.table)
  d <- vector("list",7)
  for(i in 0:6){
    d[[i+1]] <- data.table(
      age=c("0-4","5-14","15-19","20-29","30-64","65+","Ukjent"),
      date=data.table::as.IDate(as.Date("2006-01-23")+i),
      Kontaktype=c("Legekontakt"),
      Praksis=c("Fastlege"),
      influensa=c(100),
      gastro=c(100),
      respiratory=c(100),
      respiratoryinternal=c(100),
      respiratoryexternal=c(100),
      lungebetennelse=c(100),
      bronkitt=c(100),
      consult=c(500),
      municip=c("municip0301")
    )
  }
  d <- rbindlist(d)


  population <- vector("list",3)
  population[[1]] <- data.table(
    year="2006",
    municip="municip0301",
    age=c("0-4","5-14","15-19","20-29","30-64","65+","Totalt"),
    pop=c(100,100,100,100,100,100,600)
  )

  hellidager=data.table(
    Dato=data.table::as.IDate(seq(as.Date("2006-01-23"),as.Date("2006-01-29"),by=1)),
    HelligdagIndikator=0
  )

  res <- FormatData(d,
                    SYNDROME="influensa",
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE,
                    removeMunicipsWithoutConsults=TRUE)
  res <- res[municip %in% unique(d$municip)]

  expectedRes <- data.table(expand.grid(
    date=data.table::as.IDate(seq(as.Date("2006-01-23"),as.Date("2006-01-29"),by=1)),
    age=c("0-4","5-14","15-19","20-29","30-64","65+","Totalt"),
    stringsAsFactors = FALSE
  ))
  expectedRes[,municip:="municip0301"]

  expectedRes[,value:=100]
  expectedRes[age=="Totalt",value:=700]

  expectedRes[,consultWithInfluensa:=500]
  expectedRes[age=="Totalt",consultWithInfluensa:=3500]
  expectedRes[,consultWithoutInfluensa:=consultWithInfluensa-value]
  expectedRes[,pop:=100]
  expectedRes[age=="Totalt",pop:=600]
  expectedRes[,county:="county03"]
  expectedRes[,HelligdagIndikator:=0]
  setcolorder(expectedRes,c("date","HelligdagIndikator","county","municip","age","value","consultWithInfluensa","consultWithoutInfluensa","pop"))
  #setorder(expectedRes,date,age)
  setkey(expectedRes,municip,age,date)

  res[,pop:=1]
  expectedRes[,pop:=1]

  testthat::expect_equal(res,expectedRes)
})

test_that("Oslo + kristiansand fake data", {
  library(data.table)
  if(interactive()){
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"data","partially_formatted_2017_05_09.RDS"))
  population <- readRDS(file.path(BASE,"data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path(BASE,"data","hellidager.RDS"))

  res <- FormatData(d,
                    SYNDROME="influensa",
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE,
                    removeMunicipsWithoutConsults=TRUE)
  res <- res[municip %in% unique(d$municip)]

  if(FALSE) if(interactive()) saveRDS(res,file="/git/dashboards_sykdomspuls/tests/testthat/results/formatted_2017_05_09.RDS")
  expectedRes <- readRDS(file.path(BASE,"results","formatted_2017_05_09.RDS"))

  res[,pop:=1]
  expectedRes[,pop:=1]

  expect_equal(res,expectedRes)
})


test_that("Sandefjord joining together", {
  library(data.table)
  if(interactive()){
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"data","partially_formatted_sandefjord.RDS"))
  population <- readRDS(file.path(BASE,"data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path(BASE,"data","hellidager.RDS"))

  res <- FormatData(d,
                    SYNDROME="influensa",
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE,
                    removeMunicipsWithoutConsults=TRUE)
  res <- res[municip %in% unique(d$municip)]

  if(FALSE) if(interactive()) saveRDS(res,file="/git/dashboards_sykdomspuls/tests/testthat/results/formatted_sandefjord.RDS")
  expectedRes <- readRDS(file.path(BASE,"results","formatted_sandefjord.RDS"))

  res[,pop:=1]
  expectedRes[,pop:=1]

  expect_equal(res,expectedRes)
})


test_that("Inderoy joining together", {
  library(data.table)
  if(interactive()){
    BASE <- "/git/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"data","inderoy_partially_formatted_2017_05_09.RDS"))
  population <- readRDS(file.path(BASE,"data","pop.RDS"))
  for(i in 1:length(population)){
    if(!is.null(population[[i]])) population[[i]] <- population[[i]][municip %in% unique(d$municip)]
  }
  hellidager=readRDS(file.path(BASE,"data","hellidager.RDS"))

  res <- FormatData(d,
                    SYNDROME="influensa",
                    population=population,
                    hellidager=hellidager,
                    testIfHelligdagIndikatorFileIsOutdated=FALSE,
                    removeMunicipsWithoutConsults=TRUE)
  res <- res[municip %in% "municip5053"]

  if(FALSE) if(interactive()) saveRDS(res,file="/git/dashboards_sykdomspuls/tests/testthat/results/formatted_inderoy.RDS")
  expectedRes <- readRDS(file.path(BASE,"results","formatted_inderoy.RDS"))

  res[,pop:=1]
  expectedRes[,pop:=1]

  expect_equal(res,expectedRes)
})
