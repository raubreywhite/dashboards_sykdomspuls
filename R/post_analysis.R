#' Normal <= threshold2
#' threshold2 < Medium <= threshold4
#' threshold4 < High
#'
#' @param data A data.table containing the variables \code{n}, \code{threshold2}, and \code{threshold4}
#' @import data.table
#' @export DetermineStatus
DetermineStatus <- function(data) {
  status <- NULL
  n <- NULL
  threshold2 <- NULL
  threshold4 <- NULL

  # create "normal", "medium", "high" categories
  data[, status := "Normal"]
  data[n > 1 & n > threshold2, status := "Medium"]
  data[n > 1 & n > threshold4, status := "High"]
}

#' clean_post_analysis
#' @param res a
#' @param schema a
#' @export clean_post_analysis
clean_post_analysis <- function(res,schema){
  res <- res[!is.na(threshold2) & !is.infinite(threshold2)]

  res[schema$stack_x$get_data_dt(),on="uuid", age:=age]
  res[schema$stack_x$get_data_dt(),on="uuid", type:=tag]
  res[schema$stack_x$get_data_dt(),on="uuid", tag:=tag]
  res[schema$stack_x$get_data_dt(),on="uuid", location:=location]
  res[schema$stack_x$get_data_dt(),on="uuid", file:=file]
  res[schema$stack_x$get_data_dt(),on="uuid", purpose:=purpose]
  res[schema$stack_x$get_data_dt(),on="uuid", granularity_time:=granularity_time]
  res[schema$stack_x$get_data_dt(),on="uuid", v:=v]

  # make threshold2 minimum of 2 and threshold4 minimum of 3
  res[threshold2 < 2, threshold2 := 2]
  res[threshold4 < 3, threshold4 := 3]

  # create "normal", "medium", "high" categories
  DetermineStatus(res)

  # adding in extra information
  # add location name
  res[fhidata::norway_locations_long_current,on="location==location_code", locationName:=location_code]
  res[is.na(locationName),locationName:="Norge"]

  # add county
  res[, county:= location]
  res[fhidata::norway_locations_current,on="location==municip_code", county:=county_code]

  # cleaning on small municipalities
  res[location %in% CONFIG$smallMunicips & age != "Totalt", n := 0 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold2 := 5 ]
  res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold4 := 10 ]

  return(res)
}
