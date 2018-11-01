#' GenNorwayLocations
#' @importFrom readxl read_excel
#' @export GenNorwayLocations
GenNorwayLocations <- function() {
  norwayLocations <- readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "sykdomspuls"))
  norwayLocations <- norwayLocations[is.na(norwayLocations$yearEnd), c("municip", "municipName", "county", "countyName")]
  return(norwayLocations)
}

#' GenNorwayMunicipMerging
#' Last updated 2017-07-29
#' @importFrom lubridate today
#' @importFrom readxl read_excel
#' @import data.table
#' @importFrom zoo na.locf
#' @export GenNorwayMunicipMerging
GenNorwayMunicipMerging <- function() {
  # variables used in data.table functions in this function
  yearStart <- NULL
  municip <- NULL
  yearEnd <- NULL
  municipName <- NULL
  municipEnd <- NULL
  county <- NULL
  countyName <- NULL
  region <- NULL
  regionName <- NULL
  realEnd <- NULL
  # end

  masterData <- data.table(readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "sykdomspuls")))
  maxYear <- max(data.table::year(lubridate::today()), max(masterData$yearStart, na.rm = T))

  masterData[yearStart <= 2006, yearStart := 2006]
  setnames(masterData, "yearStart", "year")
  skeleton <- expand.grid(year = as.numeric(2006:maxYear), municip = unique(masterData$municip), stringsAsFactors = FALSE)
  skeleton <- data.table(merge(skeleton, masterData, by = c("municip", "year"), all.x = T))
  setorder(skeleton, municip, year)
  skeleton[is.na(yearEnd), yearEnd := maxYear]
  skeleton[, yearEnd := min(yearEnd, na.rm = T), by = municip]
  skeleton <- skeleton[year <= yearEnd]
  skeleton[, yearEnd := NULL]
  skeleton[, yearStart := 9999]
  skeleton[!is.na(municipName), yearStart := year]
  skeleton[, yearStart := min(yearStart, na.rm = T), by = municip]
  skeleton <- skeleton[year >= yearStart]
  skeleton[, municipEnd := zoo::na.locf(municipEnd), by = municip]
  skeleton[, municipName := zoo::na.locf(municipName), by = municip]
  skeleton[, county := zoo::na.locf(county), by = municip]
  skeleton[, countyName := zoo::na.locf(countyName), by = municip]
  skeleton[, region := zoo::na.locf(region), by = municip]
  skeleton[, regionName := zoo::na.locf(regionName), by = municip]

  skeletonFinal <- skeleton[year == maxYear]
  skeletonFinal[, year := NULL]
  skeletonFinal[, municipEnd := NULL]
  skeletonOther <- skeleton[, c("municip", "year", "municipEnd")]

  mappings <- unique(skeleton[!is.na(municipEnd), c("municip", "municipEnd")])
  setnames(mappings, c("municipEnd", "realEnd"))

  continueWithMerging <- TRUE
  while (continueWithMerging) {
    skeletonOther <- merge(skeletonOther, mappings, all.x = T, by = "municipEnd")
    skeletonOther[!is.na(realEnd), municipEnd := realEnd]
    # print(skeletonOther[municip %in% c("municip1723","municip1756","municip5053")])
    if (sum(!is.na(skeletonOther$realEnd)) == 0) {
      continueWithMerging <- FALSE
    }
    skeletonOther[, realEnd := NULL]
  }
  skeletonOther[, realEnd := municip]
  skeletonOther[!is.na(municipEnd), realEnd := municipEnd]
  setnames(skeletonFinal, "municip", "realEnd")

  skeletonFinal <- merge(skeletonOther, skeletonFinal, by = c("realEnd"))
  skeletonFinal[is.na(municipEnd), municipEnd := municip]
  setorder(skeletonFinal, realEnd, year)
  skeletonFinal[, realEnd := NULL]
  # skeletonFinal[municip %in% c("municip1723","municip1756","municip5053")]
  # skeletonFinal[municip %in% c("municip0301")]

  return(skeletonFinal)
}
