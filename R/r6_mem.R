#' Mem implementation
#'
#' @import data.table
#' @import ggplot2
#' @export
MeM <-  R6::R6Class(
  "MeM",
  portable = FALSE,
  list(
    conf = NULL,
    db_config = NULL,
    initialize = function(conf=NULL, db_config=NULL){
      conf <<- conf
      db_config <<- db_config
      mem_schema <<- get_mem_schema()
str    },
    run_all = function() {
      mem_schema$db_connect(db_config)
      for (i in 1:nrow(conf)) {
        current_conf <- conf[i]
        run_all_mem(current_conf, mem_schema)
        create_plots(current_conf)
      }
      try(DBI::dbExecute(
        mem_schema$results_x$conn,
        glue::glue(
          "ALTER TABLE `{tb}` ADD INDEX `ind1` (`tag`(10),`location`(10), `season`(10))",
          tb=mem_schema$results_x$db_table
        )
      ),TRUE)


    }
  )
)



#' run_all
#' @param conf A mem model configuration object
#' @export
run_all <- function(conf){

  data = get_mem_data(conf)


}


#'
#' run_all_mem
#'
#' Runs mem analysis for all available years for the whole
#' country and for each county
#'
#' @param conf A mem model configuration object
#' @param mem_schema a
#' @export
run_all_mem <- function(conf, mem_schema){


    data <- readRDS(file = fhi::DashboardFolder(
    "data_clean",
    sprintf("%s_%s_%s_cleaned.RDS", LatestRawID(), conf$tag, "Totalt")
  ))[granularity_geo != "municip"]


  data[, week:= fhi::isoweek_n(date)]
  data[, year:= fhi::isoyear_n(date)]
  data[, yrwk:= fhi::isoyearweek(date)]
  data[, season:= fhi::season(yrwk)]

  # National
  national <- data[location_code=="Norge",
    .(n=sum(n), consult_with_influenza=sum(consult_with_influenza)), by=.(year, week, yrwk, season)]

  mem_national_df = prepare_data_frame(national)
  mem_results = run_mem_model(mem_national_df, conf)

  national[mem_results,on="season", low:=low]
  national[mem_results,on="season", medium:=medium]
  national[mem_results,on="season", high:=high]
  national[mem_results,on="season", very_high:=very_high]

  national[, location_code:="norge"]

  # County

  counties <- data[granularity_geo=="county",
    .(n=sum(n), consult_with_influenza=sum(consult_with_influenza)), by=.(year, week, yrwk, season, location_code)]


  for(county in unique(counties[, location_code])){
    mem_df = prepare_data_frame(counties[location_code == county])
    mem_results = run_mem_model(mem_df, conf)
    mem_results[,location_code:=county]

    counties[mem_results,on=c("season","location_code"), low:=low]
    counties[mem_results,on=c("season","location_code"), medium:=medium]
    counties[mem_results,on=c("season","location_code"), high:=high]
    counties[mem_results,on=c("season","location_code"), very_high:=very_high]
  }

  out = rbind(national, counties)
  out[, tag:=conf$tag]
  out[, rate:= n /consult_with_influenza *100]
  out[, n:= NULL]
  out[, consult_with_influenza:= NULL]
  out[fhidata::days,on="yrwk", date:=mon]

  mem_schema$db_connect(CONFIG$DB_CONFIG)
  mem_schema$db_drop_all_rows()
  mem_schema$db_load_data_infile(out)
}


#' get_season
#'
#' get the influensa season
#'
#' @param date Date to get the current season for
#'
#' @export
get_season <- function(date){

  year = year(date)
  week = week(date)

  if(week < 40){
    season = paste(year - 1, year, sep="/")
  } else {
    season = paste(year, year + 1, sep="/")

  }
  return(season)
}

#' create MEM season plots
#'
#' @param conf A mem model configuration object
#' @param mem_schema a
#' @export
create_plots <- function(conf, mem_schema=NULL){

  current_season <- get_season(Sys.Date())
  if(is.null(mem_schema)){
    mem_schema<- get_mem_schema()
    mem_schema$db_connect(CONFIG$DB_CONFIG)
  }
  data <- mem_schema$get_data_db(season==current_season)


  folder <- fhi::DashboardFolder("results", sprintf("%s/%s", latest_date(),
                                                    paste("mem",conf$tag, sep="_")))
  if (!file.exists(folder)){
    dir.create(folder)
  }

  for(loc in unique(data[, location_code])){
    data_location = data[location_code==loc]
    title = paste(" Niv\u00E5 p\u00E5 influensaintensitet m\u00E5lt ved andel legebes\u00F8k for ILS ",
                  current_season,
                  "i",
                  fhi::get_location_name(loc))

    chart = fhiplot::make_influenza_threshold_chart(data_location, title)

    filename = paste(folder, "/", loc, ".png", sep="")
    ggsave(filename, chart, height=7, width=9)
  }

  latest_week <- max(data[year == max(data[, year]), week])
  if(latest_week > 20){
    weeks <- 40:latest_week
  } else {
    weeks <- c(40:52,1:latest_week)
  }
  counties <- fhidata::norway_map_counties
  data[, status:= ifelse(rate <= low, "Sv\u00E6rt lav",
                         ifelse(rate <= medium, "Lav",
                                ifelse( rate <= high, "Middels",
                                       ifelse (rate <= very_high, "H\u00F8y", "Sv\u00E6rt h\u00F8y")
                                       )
                                )
                         )]
  for(current_week in weeks){
    counties <- fhidata::norway_map_counties
    plot_data <- counties[data[week == current_week], on=.(location_code=location_code), nomatch=0]
    cnames_country <- aggregate(cbind(long, lat) ~ rate,
                                data=plot_data[!(location_code %in% c("county03", "county02"))],
                                FUN=function(x)mean(range(x)))
    cnames_country$rate = round(cnames_country$rate, 1)

    cnames_osl_ak <- aggregate(cbind(long, lat) ~ rate,
                               data=plot_data[location_code %in% c("county03", "county02")],
                               FUN=function(x)mean(range(x)))
    cnames_osl_ak$rate = round(cnames_osl_ak$rate, 1)

    map_plot <- ggplot() +
      geom_polygon(data = plot_data, aes( x = long, y = lat, group = group, fill=status),
                   color="black", size=0.1) +
      theme_void() +
      coord_quickmap() +
      scale_fill_manual("Niv\u00E5", values=c("Sv\u00E6rt lav"=fhiplot::vals$cols$map_sequential[["MS5"]],
                                          "Lav"=fhiplot::vals$cols$map_sequential[["MS4"]],
                                          "Middels"=fhiplot::vals$cols$map_sequential[["MS3"]],
                                          "H\u00F8y"=fhiplot::vals$cols$map_sequential[["MS2"]],
                                          "Sv\u00E6rt h\u00F8y"=fhiplot::vals$cols$map_sequential[["MS1"]]
                                          )) +
      ggrepel::geom_label_repel(data=cnames_country, aes(long, lat, label = rate), size=2)

    oslo_akershus <- ggplot() +
      geom_polygon(data = plot_data[location_code %in% c("county03", "county02")]
                 , aes( x = long, y = lat, group = group, fill=status),
                   color="black", size=0.1) +
      theme_void() +
      coord_quickmap() +
       scale_fill_manual("Niv\u00E5", values=c("Sv\u00E6rt lav"=fhiplot::vals$cols$map_sequential[["MS5"]],
                                          "Lav"=fhiplot::vals$cols$map_sequential[["MS4"]],
                                          "Middels"=fhiplot::vals$cols$map_sequential[["MS3"]],
                                          "H\u00F8y"=fhiplot::vals$cols$map_sequential[["MS2"]],
                                          "Sv\u00E6rt h\u00F8y"=fhiplot::vals$cols$map_sequential[["MS1"]]
                                          )) +
      geom_label(data=cnames_osl_ak, aes(long, lat, label = rate), size=2) +
      theme(legend.position = "none") +
      ggtitle("Oslo og Akershus") +
      theme(plot.title = element_text(size = 8,))

    map_plot <- map_plot +
      annotation_custom(
        ggplotGrob(oslo_akershus),
        xmin = 10, xmax = 35, ymin = 60, ymax = 65
      )
    filename = paste(folder, "/map_week", current_week, ".png", sep="")
    ggsave(filename, map_plot, height=7, width=5)
  }



}

prepare_data_frame <- function(data){

  useful_data <- data[week >= 40 | week <=20]

  useful_data[, rate := n /consult_with_influenza * 100]
  useful_data[, label := ifelse(week >= 40,
                                paste(year, year + 1, sep="/"),
                                paste(year - 1, year,  sep="/")
                                )
              ]
  out <- data.frame(week = c(40:52,1:20))

  for(lab in unique(useful_data[, label])){
    current_season <- useful_data[label==lab, .(label, week, rate)]
    if(nrow(current_season) == 33){
      out[lab] = current_season[, rate]
    }
  }
  rownames(out) <- c(40:52,1:20)
  return(out[, !(names(out) == "week")])
}

run_mem_model <- function(data, conf){
  out <- list()


  for(i in 5:ncol(data)){
    col <- names(data)[i]
    model_data <- data[, names(data)[1:i]]

    model_data <- data[, names(model_data)[!(names(model_data) %in% conf$excludeSeason)]]
    epi <- mem::memmodel(model_data)
    out[[col]] <- c(epi$epidemic.thresholds[1],
                    epi$epi.intervals[1,4],
                    epi$epi.intervals[2,4],
                    epi$epi.intervals[3,4]
                    )

  }

  mem_results <- data.frame(out)
  mem_results$val <- c("low","medium","high","very_high")
  mem_results <- reshape2::melt(mem_results, id="val")
  setDT(mem_results)
  mem_results[,season:=stringr::str_replace(variable,"\\.","/")]
  mem_results[,season:=stringr::str_remove(season,"X")]
  mem_results <- dcast.data.table(mem_results,season~val,value.var = "value")

  return(mem_results)

}



mem_results_field_types <- c(
  "tag"="TEXT",
  "location_code"="TEXT",
  "season"="TEXT",
  "yrwk"="TEXT",
  "year"="INTEGER",
  "week"="INTEGER",
  "date"="DATE",
  "rate"="DOUBLE",
  "low"="DOUBLE",
  "medium"="DOUBLE",
  "high"="DOUBLE",
  "very_high"="DOUBLE"
)

mem_results_keys <- c(
  "tag",
  "location_code",
  "year",
  "week"

)

#' get_mem_schema
#'
#' DB schema for mem_results
#'
#' @export
get_mem_schema <- function()
  return(fd::schema$new(
    db_table = "spuls_mem_results",
    db_field_types = mem_results_field_types,
    db_load_folder = "/xtmp/",
    keys = mem_results_keys
  )
)

