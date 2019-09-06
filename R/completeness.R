
#' Calculate completeness
#'
#' @param location
#' @param year
#' @param granularity_time
#'
#' @import data.table
#' 
#' @export
calculate_completeness <- function(location, x_year, x_granularity_time="weekly", table=NULL){


  if(is.null(table)){
    table <- connect_db("spuls_standard_results")
  }
  cat(file=stderr(), location)
  results <- table %>% dplyr::filter(location_code == location &
                                       year==x_year &
                                       granularity_time == x_granularity_time &
                                       tag == "consult_without_influenza" &
                                       age == "Totalt"
                                     ) %>% dplyr::collect()
  setDT(results)
  results[, completeness:=n/threshold0*100]
  results[ completeness > 100, completeness:=100]
  
  return(results)
}


#' Calculate Confidence Interval
#'
#' @param location
#' @param year
#' @param granularity_time
#'
#' @import data.table
#' 
#' @export
calculate_confidence_interval <- function(data, last_weeks=NULL){
  setDT(data)
  table <- connect_db("spuls_standard_results")


  location <- data[1, location_code]
  yrwks <- data[, yrwk]
  x_granularity_time <- data[1, granularity_time]
  x_age <- data[1, age]
  
  results <- table %>% dplyr::filter(location_code == location &
                                       yrwk %in% yrwks &
                                       granularity_time == x_granularity_time &
                                       tag == "consult_without_influenza" &
                                       age == x_age
                                     ) %>% dplyr::collect()
  setDT(results)
  results[, completeness:=n/threshold0]
  results[ completeness > 1, completeness:=1]

  
  population = results[, n] / pmax(results[,completeness], 1e-5)
  population
  cis <- list()
  for(i in 1:nrow(data)){
    print(!is.null(last_weeks))
    if(!is.null(last_weeks) && (i < ( nrow(data) - last_weeks))){
      cis[[i]] <- list(yrwk=data[i, yrwk], phat=NA, low_p=NA, high_p=NA, low_n=NA, high_n=NA)
      next
    }
    denom <- data[i, denominator]
    pop <- population[i]
    if(pop > 0){
      if(pop < 100000){
 
        if(pop != denom){
          CI <- samplingbook::Sprop(m=data[i, n], n=denom, N=population[i])$ci$exact
        }else{
          CI <- c(data[i, n]/denom, data[i, n]/denom)
        }
        cis[[i]] <- list(yrwk=data[i, yrwk], phat=data[i, n]/denom,
                         low_p=CI[1], high_p=CI[2],
                         low_n=CI[1]*denom, high_n=CI[2]*denom)
        
      }else{
        CI <- asbio::ci.p(phat=data[i, n]/denom, n=denom, N=population[i], summarized=T, fpc=T)$ci
        cis[[i]] <- list(yrwk=data[i, yrwk], phat=CI[1],
                         low_p=CI[2], high_p=CI[3],
                         low_n=denom*CI[2], high_n=denom*CI[3])
      }
    }else{
      cis[[i]] <- list(yrwk=data[i, yrwk], phat=0, low_p=0, high_p=0, low_n=0, high_n=5)
      }

  }

  cis <- rbindlist(cis)
  cis[, completeness:=results[, completeness]]
  cis[low_n <0, lower_n:=0]
  ret <- data[cis, on="yrwk"]
  
  return(ret)
}


