#' CheckOutOfDate
#' @export
CheckOutOfDate <- function() {
  if (CONFIG$verbose) {
    fhi::DashboardMsg("\u2620\u2620Data files accessed (this is slow)!\u2620\u2620", syscallsDepth = 0)
  }
  if (CONFIG$checkedOutOfDate) {
    return(TRUE)
  }

  # Create files if they dont exist
  if (!file.exists(system.file("createddata", "norwayPopulation.RDS", package = "sykdomspuls"))) {
    GenNorwayPopulation()
  }
  if (!file.exists(system.file("createddata", "norwayMunicipMerging.RDS", package = "sykdomspuls"))) {
    GenNorwayMunicipMerging()
    GenNorwayLocations()
  }
  if (!file.exists(system.file("createddata", "norwayLocations.RDS", package = "sykdomspuls"))) {
    GenNorwayLocations()
  }

  norwayPopulationCreated <- readRDS(system.file("createddata", "norwayPopulation.RDS", package = "sykdomspuls"))
  if (max(norwayPopulationCreated[imputed == FALSE]$year) == RAWmisc::YearN(lubridate::today())) {
    CONFIG$outOfDate[["norwayPopulation"]] <- FALSE
  } else {
    norwayPopulationCreated <- GenNorwayPopulation()
  }

  # Population
  VARS$norwayPopulation <- norwayPopulationCreated

  norwayMunicipMergingCreated <- readRDS(system.file("createddata", "norwayMunicipMerging.RDS", package = "sykdomspuls"))
  norwayLocationsCreated <- readRDS(system.file("createddata", "norwayLocations.RDS", package = "sykdomspuls"))
  if (max(norwayMunicipMergingCreated$year) == RAWmisc::YearN(lubridate::today())) {
    CONFIG$outOfDate[["norwayMunicipMerging"]] <- FALSE
    CONFIG$outOfDate[["norwayLocations"]] <- FALSE
  } else {
    norwayMunicipMergingCreated <- GenNorwayMunicipMerging()
    norwayLocationsCreated <- GenNorwayLocations()
  }

  # List of municipality merging over time
  VARS$norwayMunicipMerging <- norwayMunicipMergingCreated

  # List of municipalities and counties
  VARS$norwayLocations <- norwayLocationsCreated

  a <- data.table(location = "Norge", locationName = "Norge")
  b <- VARS$norwayLocations[, c("municip", "municipName")]
  c <- VARS$norwayLocations[, c("county", "countyName")]
  setnames(b, c("location", "locationName"))
  setnames(c, c("location", "locationName"))

  VARS$norwayLocationsLong <- unique(rbind(a, b, c))

  fhi::DashboardMsg("\u2620\u2620Data files have been read!\u2620\u2620", syscallsDepth = 0)
  fhi::DashboardMsg("\u2620\u2620This is slow!\u2620\u2620", syscallsDepth = 0)
  CONFIG$checkedOutOfDate <- TRUE
}

#' NorwayPopulation
#' @export NorwayPopulation
NorwayPopulation <- function() {
  CheckOutOfDate()
  VARS$norwayPopulation
}

#' NorwayMunicipMerging
#' @export NorwayMunicipMerging
NorwayMunicipMerging <- function() {
  CheckOutOfDate()
  VARS$norwayMunicipMerging
}

#' NorwayLocations
#' @export NorwayLocations
NorwayLocations <- function() {
  CheckOutOfDate()
  VARS$norwayLocations
}

#' NorwayLocationsLong
#' @export NorwayLocationsLong
NorwayLocationsLong <- function() {
  CheckOutOfDate()
  VARS$norwayLocationsLong
}
