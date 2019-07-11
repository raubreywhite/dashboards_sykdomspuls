#' Mem implementation
#'
#' @import data.table
#' @import ggplot2
#' @export MeM
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
    },
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




#' @param conf A mem model configuration object
run_all <- function(conf){

  data = get_mem_data(conf)
  

}

add_columns <- function(data){
  data[, low:=as.numeric(NaN)]
  data[, medium:=as.numeric(NaN)]
  data[, high:=as.numeric(NaN)]
  data[, very_high:=as.numeric(NaN)]
  data[, season:= ""]
  
}

add_results_to_data <- function(data, mem_results){
 
  for(yea in unique(data[, year])){
    label0_20 = paste(yea - 2, yea - 1, sep="/")
    label40_52 = paste(yea - 1, yea, sep="/")
    
    data[year==yea & week<=20, season:= paste(yea - 1, yea, sep="/")]
    data[year==yea & week>=40, season:= paste(yea, yea + 1 , sep="/")]
    
    if(! is.null(mem_results[[label0_20]][1])){
      data[year==yea & week<=20, low:= mem_results[[label0_20]][1]]
    }
    if(! is.null(mem_results[[label0_20]][2])){
      data[year==yea & week<=20, medium:= mem_results[[label0_20]][2]]
    }
    if(! is.null(mem_results[[label0_20]][3])){
      data[year==yea & week<=20, high:= mem_results[[label0_20]][3]]
    }

    if(! is.null(mem_results[[label0_20]][4])){
      data[year==yea & week<=20, very_high:= mem_results[[label0_20]][4]]
    }
    if(! is.null(mem_results[[label40_52]][1])){
      data[year == yea & week>=40, low:= mem_results[[label40_52]][1]]
    }
    if(! is.null(mem_results[[label40_52]][2])){
      data[year == yea & week>=40, medium:= mem_results[[label40_52]][2]]
    }
    if(! is.null(mem_results[[label40_52]][3])){
      data[year == yea & week>=40, high:= mem_results[[label40_52]][3]]
    }
    if(! is.null(mem_results[[label40_52]][4])){
      data[year == yea & week>=40, very_high:= mem_results[[label40_52]][4]]
    }
     

  }
  return(data)
}

#'
#' run_all_mem
#'
#' Runs mem analysis for all available years for the whole
#' country and for each county
#'
#' @param conf A mem model configuration object
#' 
#' @export run_all_mem
run_all_mem <- function(conf, mem_schema){

  data <- readRDS(file = fhi::DashboardFolder(
    "data_clean",
    sprintf("%s_%s_cleaned.RDS", LatestRawID(), conf$tag)
  ))[granularityGeo != "municip"]

  
  data[, week:= lubridate::isoweek(date)]
  data[, year:= lubridate::isoyear(date)]

  # National
  national <- data[location=="Norge" & age == "Totalt",
    .(n=sum(n), consultWithInfluensa=sum(consultWithInfluensa)), by=.(year, week)]

  mem_national_df = prepare_data_frame(national)
  mem_results = run_mem_model(mem_national_df, conf)
  add_columns(national)
  national <- add_results_to_data(national, mem_results)
  national[, location:="norge"]

  # County

  counties <- data[granularityGeo=="county",
    .(n=sum(n), consultWithInfluensa=sum(consultWithInfluensa)), by=.(year, week, location)]
  add_columns(counties)
  for(county in unique(counties[, location])){
    mem_df = prepare_data_frame(counties[location == county])
    mem_results = run_mem_model(mem_df, conf)
    counties[location==county] <-add_results_to_data(counties[location==county], mem_results)
  }
  
  out = rbind(national, counties)
  out[, tag:=conf$tag]
  out[, rate:= n /consultWithInfluensa *100]
  out[, n:= NULL]
  out[, consultWithInfluensa:= NULL]
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
#' @export get_season
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
#' 
#' @export create_plots

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

  for(loc in unique(data[, location])){
    data_location = data[location==loc]
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
    plot_data <- counties[data[week == current_week], on=.(location_code=location), nomatch=0]
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
  
  useful_data[, rate := n /consultWithInfluensa * 100]
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

  return(out)

}



mem_results_field_types <- c(
  "tag"="TEXT",
  "location"="TEXT",
  "season"="TEXT",
#  "status"="TEXT",
#  "wkyr"="TEXT",
  "year"="INTEGER",
  "week"="INTEGER",
#  "date"="DATE",
#  "displayDay"="DATE",
  "rate"="DOUBLE",
#  "denominator"="INTEGER",
  "low"="DOUBLE",
  "medium"="DOUBLE",
  "high"="DOUBLE",
  "very_high"="DOUBLE"
#  "locationName"="TEXT",
#  "county"="TEXT"
)

mem_results_keys <- c(
  "tag",
  "location",
  "year",
  "week"

)

#' get_mem_schema
#'
#' DB schema for mem_results
#' 
#' @export get_mem_schema
get_mem_schema <- function()
  return(fd::schema$new(
    db_table = "spuls_mem_results",
    db_field_types = mem_results_field_types,
    db_load_folder = "/xtmp/",
    keys = mem_results_keys
  )
)

