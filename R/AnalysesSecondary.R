
#' AnalyseLog dfdf
#' dfd
#' @export
AnalyseLog <- function() {
  log <- LogGet()
  if (length(log) == 0) {
    return()
  }

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
  log[, minTotal := as.numeric(difftime(Done, initialiseBefore, units = "min"))]

  log[, minCleaningPerTag := minCleaning / numTags]
  log[, minAnalyse1PerTag := minAnalyse1 / numTags]
  log[, minAnalyse2PerTag := minAnalyse2 / numTags]
  log[, minTotalPerTag := minTotal / numTags]

  log[, id := 1:.N, by = date]
  log <- log[id == 1]
  cat(file = stderr(), as.character(log))
  long <- melt.data.table(log[, c(
    "date",
    "versionPackage",
    "numTags",
    "minCleaning",
    "minAnalyse1",
    "minAnalyse2",
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



AggregateAlertsCases <- function(data) {
  pop <- fd::norway_population()
  current_year <- max(data[, year])

  pop_cats <- list(c(0, 10000), c(10000, 100000), c(100000, 6000000))
  pop_names <- c(
    "Liten Kommune (<10,000)",
    "Medium Kommune (10,000 - 100,000)",
    "Stor Kommune (>100,000)", "Fylke", "Norge"
  )
  pop <- data.table(pop)[year == current_year, .(pop = sum(pop)), by = .(location_code, level)]
  pop[location_code == "norway", location_code := "Norge"]
  data <- data[pop, on = "location_code", nomatch = 0]
  data[, type := location_code]
  data[granularity_geo == "county", type := "Fylke"]
  data[granularity_geo == "municip", type := "Kommune"]
  data[granularity_geo == "national", type := "Norge"]
  data[, subtype := type]
  data[, order := 1]
  data[granularity_geo == "county", order := 2]
  data[subtype == "Kommune" & pop <= 10000, subtype := pop_names[1]]
  data[subtype == pop_names[1], order := 5]
  data[subtype == "Kommune" & pop <= 100000, subtype := pop_names[2]]
  data[subtype == pop_names[2], order := 4]
  data[subtype == "Kommune" & pop > 100000, subtype := pop_names[3]]
  data[subtype == pop_names[3], order := 3]
  data[, cases_over_t0 := n - threshold0]
  data[, cases_over_t2 := 0]
  data[status == "Medium", cases_over_t2 := n - threshold2]
  data[, cases_over_t4 := 0]
  data[status == "High", cases_over_t4 := n - threshold4]



  breaks <- c(2005, 2010, 2015, current_year - 1, current_year)
  labels <- c("2006-2010", "2011-2015", paste("2016", current_year - 1, sep = "-"), current_year)
  n_weeks <- c(260, 260, (current_year + 1 - 2016) * 52, max(data[ year == current_year, week]))

  data[, year_group := cut(year, breaks = breaks, labels = labels)]
  data[, weight := 0.1]
  for (pi in 1:length(pop_names)) {
    if (!(pop_names[pi] %in% c("Fylke", "Norge"))) {
      n_areas <- sum(pop[, pop] > pop_cats[[pi]][1] & pop[, pop] <= pop_cats[[pi]][2])
    } else if (pop_names[pi] == "Fylke") {
      n_areas <- nrow(pop[level == "county"])
    } else {
      n_areas <- 1
    }
    for (yi in 1:length(labels)) {
      data[year_group == labels[yi] & subtype == pop_names[pi], weight := 52 / (n_areas * n_weeks[yi])]
    }
  }




  z_2_expected <- (1 - pnorm(2, 0, 1)) * 52 * length(unique(data[, age]))
  z_4_expected <- (1 - pnorm(4, 0, 1)) * 52 * length(unique(data[, age]))

  z_2 <- data[status == "Medium"]
  z_2 <- dcast.data.table(tag + subtype + order ~ year_group, fun.aggregate = sum, value.var = "weight", data = z_2, drop = FALSE)
  z_2[, expected := (1 - pnorm(2, 0, 1)) * 52 * length(unique(data[, age]))]
  z_4 <- data[status == "High"]
  z_4 <- dcast.data.table(tag + subtype + order ~ year_group, fun.aggregate = sum, value.var = "weight", data = z_4, drop = FALSE)
  z_4[, expected := (1 - pnorm(2, 0, 1)) * 52 * length(unique(data[, age]))]

  setorder(z_2, order)
  setorder(z_4, order)

  # x[, subtype := factor(subtype)]
  return(list(z_2 = z_2, z_4 = z_4))
}

#' AnalyseStats1
#' @param resYearLine a
#' @param resYearLineMunicip a
#' @export
AnalyseStats1 <- function(
                          resYearLine = NULL,
                          resYearLineMunicip = NULL) {
  # variables used in data.table functions in this function

  conn <- DBI::dbConnect(odbc::odbc(),
    driver = CONFIG$DB_CONFIG$driver,
    server = CONFIG$DB_CONFIG$server,
    port = CONFIG$DB_CONFIG$port,
    user = CONFIG$DB_CONFIG$user,
    password = CONFIG$DB_CONFIG$password
  )
  fd::use_db(conn, CONFIG$DB_CONFIG$db)
  db <- dplyr::tbl(conn, "spuls_standard_results")
  data <- db %>%
    dplyr::filter(granularity_time == "weekly" & status != "Normal") %>%
    dplyr::collect()
  setDT(data)

  aggregated_alerts <- AggregateAlertsCases(data)

  data <- db %>%
    dplyr::filter(granularity_time == "weekly") %>%
    dplyr::select(granularity_geo, tag, zscore, failed, location_code, status, date, year) %>%
    dplyr::collect()
  setDT(data)
  return(list(
    "alert_summary" = aggregated_alerts, "resYearLine" = data[granularity_geo != "municip"],
    "resYearLineMunicip" = data[granularity_geo == "municip"]
  ))
}


AnalyseEmergLineList <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/emerg_linelist.Rmd", package = "sykdomspuls"),
    output_file = "emerg_linelist.pdf",
    output_dir = fhi::DashboardFolder("results", file.path(LatestRawID(), "emerg")),
    params = sprintf(
      "dev=%s,package_dir=\"%s\"",
      fhi::DashboardIsDev(),
      getwd()
    )
  )
}

AnalyseEmerg <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/emerg.Rmd", package = "sykdomspuls"),
    output_file = "emerg.pdf",
    output_dir = fd::path("results", file.path(latest_date(), "emerg")),
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
    output_dir = fd::path("results", file.path(latest_date(), "stats")),
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
    output_dir = fd::path("results", file.path(latest_date(), "skabb")),
    params = sprintf(
      "dev=%s,package_dir=\"%s\"",
      fhi::DashboardIsDev(),
      getwd()
    )
  )
}

#' AnalysesLogs
#' @export
AnalyseLogs <- function() {
  fhi::RenderExternally(
    input = system.file("extdata/logs.Rmd", package = "sykdomspuls"),
    output_file = "logs.pdf",
    output_dir = fd::path("results", file.path(latest_date(), "stats")),
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
  AnalyseSkabb()
  AnalyseStats()

  return()
}
