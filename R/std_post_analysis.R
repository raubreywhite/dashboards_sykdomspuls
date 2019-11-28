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
#' @param stack a
#' @export clean_post_analysis
clean_post_analysis <- function(res, stack) {
  res <- res[!is.na(threshold2) & !is.infinite(threshold2)]

  res[stack, on = "uuid", age := age]
  res[stack, on = "uuid", type := tag]
  res[stack, on = "uuid", tag := tag]
  res[stack, on = "uuid", location_code := location_code]
  res[stack, on = "uuid", file := file]
  res[stack, on = "uuid", granularity_time := granularity_time]
  res[stack, on = "uuid", granularity_geo := granularity_geo]
  res[stack, on = "uuid", v := v]

  # make threshold2 minimum of 2.5 and threshold4 minimum of 3
  res[threshold2 < 2.5, threshold2 := 2.5]
  res[threshold4 < 3, threshold4 := 3]

  # create "normal", "medium", "high" categories
  DetermineStatus(res)

  # adding in extra information
  # add location name
  res[fd::norway_locations_long(), on = "location_code==location_code", location_name := location_name]
  res[is.na(location_name), location_name := "Norge"]

  # add county
  res[fd::norway_locations(), on = "location_code==municip_code", county_code := county_code]
  res[is.na(county_code), county_code := location_code]

  # cleaning on small municipalities
  res[location_code %in% CONFIG$smallMunicips & age != "Totalt", n := 0 ]
  res[location_code %in% CONFIG$smallMunicips & age != "Totalt", threshold2 := 5 ]
  res[location_code %in% CONFIG$smallMunicips & age != "Totalt", threshold4 := 10 ]

  return(res)
}
