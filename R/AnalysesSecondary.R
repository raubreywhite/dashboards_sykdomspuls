#' AnalyseLog dfdf
#' dfd
#' @export AnalyseLog
AnalyseLog <- function() {
  log <- LogGet()
  if (length(log) == 0) return()

  log <- rbindlist(log, fill = T)

  if (!"versionAlgorithm" %in% names(log)) log[, versionAlgorithm := 1]
  log[is.na(versionAlgorithm), versionAlgorithm := 1]

  if (!"versionPackage" %in% names(log)) log[, versionPackage := "Unknown"]
  log[is.na(versionPackage), versionPackage := "Unknown"]

  if (!"numTags" %in% names(log)) log[, numTags := 8]
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
    "versionPackage",
    "numTags",
    "minCleaning",
    "minAnalyse1",
    "minAnalyse2",
    "minSave1",
    "minSave2",
    "minTotal",
    "minTotalPerTag"
  )],
  id.vars = c("date", "versionPackage"),
  variable.factor = FALSE
  )

  q <- ggplot(long[variable %in% c("numTags", "minAnalyse1", "minTotal", "minTotalPerTag")], aes(x = date, y = value))
  q <- q + geom_line()
  q <- q + facet_wrap(~variable, ncol = 1, scales = "free")
  q <- q + scale_x_date("")
  q1 <- q + scale_y_continuous("Number of minutes")

  pd <- long[variable %in% c(
    "minCleaning",
    "minAnalyse1",
    "minAnalyse2",
    "minSave1",
    "minSave2"
  )]
  pd[, variable := factor(variable, levels = c(
    "minSave2",
    "minSave1",
    "minAnalyse2",
    "minAnalyse1",
    "minCleaning"
  ))]
  levels(pd$variable) <- c(
    "Save results (SQL)",
    "Save results (files)",
    "Analyse (secondary)",
    "Analyse (main)",
    "Cleaning"
  )

  q <- ggplot(pd, aes(x = date, y = value, fill = variable))
  q <- q + geom_area(alpha = 0.7)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(nrow = 1, byrow = T, reverse = T))
  q <- q + scale_x_date("")
  q <- q + theme(legend.position = "bottom")
  q2a <- q + scale_y_continuous("Number of minutes")

  legend <- cowplot::get_legend(q2a)

  q <- ggplot(pd[date == max(date)], aes(x = date, y = value, fill = variable))
  q <- q + geom_col(alpha = 0.7)
  q <- q + scale_fill_brewer("", palette = "Set1", guide = guide_legend(nrow = 1, byrow = T, reverse = T))
  q <- q + scale_x_date("")
  q <- q + theme(legend.position = "bottom")
  q2b <- q + scale_y_continuous("Number of minutes")

  q2Top <- cowplot::plot_grid(q2a + theme(legend.position = "none"), q2b + theme(legend.position = "none"), align = "h", nrow = 1, rel_widths = c(0.8, 0.2))
  q2 <- cowplot::plot_grid(q2Top, legend, align = "v", ncol = 1, rel_heights = c(0.85, 0.15))

  return(list("overview" = q1, "details" = q2))
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
    output_file = "emerg.pdf",
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
    output_file = "skabb.pdf",
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
