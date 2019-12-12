fd::initialize("sykdomspuls")

fd::msg("sykdomspuls - downloading new data", slack = T)

sykdomspuls_aggregate(
  date_from = "2006-01-01",
  date_to = format(Sys.time(), "%Y-%m-%d"),
  folder = fd::path("data_raw"),
  ages = c(
    "0-4" = "0-4",
    "5-14" = "5-9",
    "5-14" = "10-14",
    "15-19" = "15-19",
    "20-29" = "20-29",
    "30-64" = "30-39",
    "30-64" = "40-49",
    "30-64" = "50-59",
    "30-64" = "60-64",
    "65-69" = "65+",
    "70-79" = "65+",
    "80+" = "65+"
  ))
get_n_doctors(
  folder = fd::path("data_raw")
)

fd::msg("sykdomspuls - finished downloading new data", slack = T)
