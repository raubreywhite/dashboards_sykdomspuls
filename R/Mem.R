suppressMessages(library(data.table))
suppressMessages(library(lubridate))
suppressMessages(library(mem))

run_all <- function(conf){

  data = get_mem_data(conf)
  

}

add_columns <- function(data){
  data[, low:=as.numeric(NaN)]
  data[, medium:=as.numeric(NaN)]
  data[, high:=as.numeric(NaN)]
  data[, very_high:=as.numeric(NaN)]
  
}

add_results_to_data <- function(data, mem_results){
 
  for(yea in unique(data[, year])){
    label0_20 = paste(yea - 2, yea - 1, sep="/")
    label40_52 = paste(yea - 1, yea, sep="/")
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
#' @export run_all_mem
run_all_mem <- function(conf){

  data <- readRDS(file = fhi::DashboardFolder(
    "data_clean",
    sprintf("%s_%s_cleaned.RDS", LatestRawID(), conf$tag)
  ))[granularityGeo != "municip"]

  
  data[, week:= isoweek(date)]
  data[, year:= isoyear(date)]

  # National
  national <- data[location=="Norge" & age == "Totalt",
    .(n=sum(n), consultWithInfluensa=sum(consultWithInfluensa)), by=.(year, week)]

  mem_national_df = prepare_data_frame(national)
  mem_results = run_mem_model(mem_national_df)
  add_columns(national)
  national <- add_results_to_data(national, mem_results)
  national[, location:="Norge"]

  # County

  counties <- data[granularityGeo=="county",
    .(n=sum(n), consultWithInfluensa=sum(consultWithInfluensa)), by=.(year, week, location)]
  add_columns(counties)
  for(county in unique(counties[, location])){
    mem_df = prepare_data_frame(counties[location == county])
    mem_results = run_mem_model(mem_df)
    counties[location==county] <-add_results_to_data(counties[location==county], mem_results)
  }
  
  out = rbind(national, counties)
  out[, tag:=conf$tag]
  out[, rate:= n /consultWithInfluensa *100]
  out[, n:= NULL]
  out[, consultWithInfluensa:= NULL]
  mem_schema$db_connect(sykdomspuls::CONFIG$DB_CONFIG)
  mem_schema$db_load_data_infile(out)

  
}



prepare_data_frame <- function(data){
  
  useful_data <- data[week >= 40 | week <=20]
  
  useful_data[, rate := n /consultWithInfluensa *100]
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

run_mem_model <- function(data){
  out <- list()
  for(i in 5:ncol(data)){
    col = names(data)[i]
    model_data = data[, names(data)[1:i]]
    epi <- memmodel(model_data)
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

#' mem_schema
#'
#' DB schema for mem_results
#' 
#' @export mem_schema
mem_schema <- fd::schema$new(
  db_table = "mem_results",
  db_field_types = mem_results_field_types,
  db_load_file = "/xtmp/mem.csv",
  keys = mem_results_keys,
)

