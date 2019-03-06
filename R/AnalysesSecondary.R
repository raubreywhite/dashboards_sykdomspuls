#' AnalyseLog dfdf
#' dfd
#' @export AnalyseLog
AnalyseLog <- function() {
  log <- LogGet()
  if (length(log) == 0) return()

  log <- rbindlist(log, fill = T)
  log[is.na(version), version := 1]
  log[is.na(numTags), numTags := 8]
  log[, date := as.Date(format.Date(initialiseBefore, "%Y-%m-%d"))]

  log[, minCleaning := as.numeric(difftime(cleanAfter, cleanBefore, units = "min"))]
  log[, minAnalyse1 := as.numeric(difftime(analyse1After, analyse1Before, units = "min"))]
  log[, minAnalyse2 := as.numeric(difftime(analyse2After, analyse2Before, units = "min"))]
  log[, minSave1 := as.numeric(difftime(save1After, save1Before, units = "min"))]
  log[, minSave2 := as.numeric(difftime(save2After, save2Before, units = "min"))]
  log[, minTotal := as.numeric(difftime(save2After, initialiseBefore, units = "min"))]

  log[, minCleaningPerTag := minCleaning / numTags]
  log[, minAnalyse1PerTag := minAnalyse1 / numTags]
  log[, minAnalyse2PerTag := minAnalyse2 / numTags]
  log[, minSave1PerTag := minSave1 / numTags]
  log[, minSave2PerTag := minSave2 / numTags]
  log[, minTotalPerTag := minTotal / numTags]

  log[, id := 1:.N, by = date]
  log <- log[id == 1]

  long <- melt.data.table(log[, c(
    "date",
    "numTags",
    "minCleaning",
    "minAnalyse1",
    "minAnalyse2",
    "minSave1",
    "minSave2",
    "minTotal",
    "minTotalPerTag"
  )],
  id.vars = "date"
  )

  q <- ggplot(long[variable %in% c("numTags", "minAnalyse1", "minTotal", "minTotalPerTag")], aes(x = date, y = value))
  q <- q + geom_line()
  q <- q + facet_wrap(~variable, ncol = 1, scales = "free")
  q <- q + scale_x_date("")
  q <- q + scale_y_continuous("Number of minutes")
  return(q)
  # q <- q + expand_limits(y=0)
  # RAWmisc::saveA4(q,fhi::DashboardFolder("results", file.path(LatestRawID(),"stats","time.png")),landscape=F)
  # fhi::DashboardFolder("results", file.path(LatestRawID(),"stats"))
}

#' AnalyseStats1
#' @param resYearLine a
#' @param resYearLineMunicip a
#' @export
AnalyseStats1 <- function(
                          resYearLine = readRDS(fhi::DashboardFolder("results", sprintf("%s/resYearLine.RDS", LatestRawID()))),
                          resYearLineMunicip = readRDS(fhi::DashboardFolder("results", sprintf("%s/resYearLineMunicip.RDS", LatestRawID())))) {
  # variables used in data.table functions in this function

  resYearLine[, x_alerts2 := n > threshold2]
  resYearLine[, x_alerts4 := n > threshold4]
  resYearLine[, x_alerts2_ge5cases := n > threshold2 & n >= 5]
  resYearLine[, x_alerts4_ge5cases := n > threshold4 & n >= 5]

  resYearLineMunicip[, x_alerts2 := n > threshold2]
  resYearLineMunicip[, x_alerts4 := n > threshold4]
  resYearLineMunicip[, x_alerts2_ge5cases := n > threshold2 & n >= 5]
  resYearLineMunicip[, x_alerts4_ge5cases := n > threshold4 & n >= 5]

  stack <- expand.grid(
    data = c("resYearLine", "resYearLineMunicip"),
    xtag = unique(resYearLine$tag),
    xyear = unique(resYearLine$year),
    xage = unique(resYearLine$age),
    stringsAsFactors = F
  )

  res <- vector("list", length = nrow(stack))
  for (i in 1:nrow(stack)) {
    s <- stack[i, ]
    x <- melt.data.table(get(s$data)[tag == s$xtag & year == s$xyear & age == s$xage],
      id.vars = c("tag", "location", "year", "age", "n", "threshold0", "threshold2", "threshold4"),
      measure.vars = patterns("^x_alerts"),
      variable.factor = F
    )
    x[, type := location]
    x[stringr::str_detect(location, "county"), type := "Fylke"]
    x[stringr::str_detect(location, "municip"), type := "Kommune"]

    x[, casesOverT0 := n - threshold0]
    x[, casesOverT2 := n - threshold2]
    x[, casesOverT4 := n - threshold4]

    x[, casesOverT := casesOverT4]
    x[stringr::str_detect(variable, "alerts2"), casesOverT := casesOverT2]

    x <- x[, .(
      alertsN = sum(value),
      alertsProp = mean(value),
      meanCases = mean(n[value == T]),
      meanCasesOverT0 = mean(casesOverT0[value == T]),
      meanCasesOverT = mean(casesOverT[value == T])
    ), keyby = .(
      tag,
      location,
      year,
      age,
      variable,
      type
    )]
    res[[i]] <- x
  }
  res <- rbindlist(res)

  pd <- res[, .(
    meanAlertsN = mean(alertsN, na.rm = T),
    meanAlertsProp = mean(alertsProp, na.rm = T),
    meanCases = mean(meanCases, na.rm = T),
    meanCasesOverT0 = mean(meanCasesOverT0, na.rm = T),
    meanCasesOverT = mean(meanCasesOverT, na.rm = T)
  ), keyby = .(
    tag,
    variable,
    type,
    age
  )]

  pd[, age := factor(age, levels = names(CONFIG$AGES))]
  pd[, type := factor(type, levels = c("Norge", "Fylke", "Kommune"))]

  pd[, prettyVar := variable]
  pd[variable == "x_alerts2", prettyVar := "ZScore>2"]
  pd[variable == "x_alerts2_ge5cases", prettyVar := "ZScore>2 & Cases>=5"]
  pd[variable == "x_alerts4", prettyVar := "ZScore>4"]
  pd[variable == "x_alerts4_ge5cases", prettyVar := "ZScore>4 & Cases>=5"]

  pd <- pd[stringr::str_detect(prettyVar, "Cases>=5")]

  return(pd)
}

AnalyseEmerg <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/emerg.Rmd", package = "sykdomspuls"),
    output_file = "stats.pdf",
    output_dir = fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")),
    params = sprintf(
      "dev=%s,package_dir=\"%s\"",
      fhi::DashboardIsDev(),
      getwd()
    )
  )
}

AnalyseStats <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/stats.Rmd", package = "sykdomspuls"),
    output_file = "stats.pdf",
    output_dir = fhi::DashboardFolder("results", file.path(LatestRawID(), "stats")),
    params = sprintf(
      "dev=%s,package_dir=\"%s\"",
      fhi::DashboardIsDev(),
      getwd()
    )
  )
}

AnalyseSkabb <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/skabb.Rmd", package = "sykdomspuls"),
    output_file = "stats.pdf",
    output_dir = fhi::DashboardFolder("results", file.path(LatestRawID(), "skabb")),
    params = sprintf(
      "dev=%s,package_dir=\"%s\"",
      fhi::DashboardIsDev(),
      getwd()
    )
  )
}

#' AnalysesSecondary
#' @export
AnalysesSecondary <- function() {
  AnalyseEmerg()
  AnalyseStats()
  AnalyseSkabb()
}
