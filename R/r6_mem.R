#' Mem implementation
#'
#' @import data.table
#' @import ggplot2
#' @export
MeM <- R6::R6Class(
  "MeM",
  portable = FALSE,
  list(
    conf = NULL,
    db_config = NULL,
    initialize = function(conf = NULL, db_config = NULL) {
      conf <<- conf
      db_config <<- db_config
      mem_schema <<- get_mem_schema()
      mem_limits_schema <<- get_mem_limits_schema()
    },
    run_all = function() {
      mem_schema$db_connect(db_config)
      mem_schema$db_drop_all_rows()
      mem_limits_schema$db_connect(db_config)
      mem_limits_schema$db_drop_all_rows()

      for (i in 1:nrow(conf)) {
        current_conf <- conf[i]
        run_all_mem(current_conf, mem_schema, mem_limits_schema)
      }
      try(DBI::dbExecute(
        mem_schema$results_x$conn,
        glue::glue(
          "ALTER TABLE `{tb}` ADD INDEX `ind1` (`tag`(10),`location`(10), `season`(10))",
          tb = mem_schema$results_x$db_table
        )
      ), TRUE)
    }
  )
)


#'
#' run_all_mem
#'
#' Runs mem analysis for all available years for the whole
#' country and for each county
#'
#' @param conf A mem model configuration object
#' @param mem_schema Mem schema
#' @param mem_limits_schema Mem limits schema
#'
#' @export
run_all_mem <- function(conf, mem_schema, mem_limits_schema) {
  ages <- jsonlite::fromJSON(conf$age)

  for (age_group in names(ages)) {
    data_age <- ages[[age_group]]

    data <- data.table()
    for (part_age in data_age) {
      f <- fd::path(
        "data_clean",
        sprintf("%s_%s_%s_cleaned.RDS", LatestRawID(), conf$syndrome, part_age)
      )
      new_data <- readRDS(file = f)[granularity_geo != "municip"]


      data <- rbind(data, new_data)
    }



    data <- data[, .(
      n = sum(n),
      consult_without_influenza = sum(consult_without_influenza),
      consult_with_influenza = sum(consult_with_influenza),
      pop = sum(pop)
    ),
    by = c("granularity_geo", "county_code", "location_code", "date", "holiday")
    ]




    data[, week := fhi::isoweek_n(date)]
    data[, year := fhi::isoyear_n(date)]
    data[, yrwk := fhi::isoyearweek(date)]
    data[, season := fhi::season(yrwk)]

    # National
    national <- data[location_code == "Norge",
      .(
        n = sum(n),
        denominator = get(conf$weeklyDenominatorFunction)(get(conf$denominator))
      ),
      by = .(year, week, yrwk, season)
    ]

    mem_national_df <- prepare_data_frame(national, mult_factor = conf$multiplicative_factor)
    mem_results <- run_mem_model(mem_national_df, conf)
    mem_results_db <- mem_results
    mem_results_db[, location_code := "norge"]
    mem_results_db[, age := age_group]
    mem_results_db[, tag := conf$tag]
    mem_limits_schema$db_load_data_infile(mem_results_db)

    national[mem_results, on = "season", low := low]
    national[mem_results, on = "season", medium := medium]
    national[mem_results, on = "season", high := high]
    national[mem_results, on = "season", very_high := very_high]

    national[, location_code := "norge"]
    # County

    counties <- data[granularity_geo == "county",
      .(
        n = sum(n),
        denominator = get(conf$weeklyDenominatorFunction)(get(conf$denominator))
      ),
      by = .(year, week, yrwk, season, location_code)
    ]


    nam <- unique(counties[, location_code])
    res <- vector("list", length = length(nam))
    for (i in seq_along(nam)) {
      county <- nam[i]
      mem_df <- prepare_data_frame(counties[location_code == county],
        mult_factor = conf$multiplicative_factor
      )
      mem_results <- run_mem_model(mem_df, conf)
      mem_results[, location_code := county]
      res[[i]] <- mem_results
      mem_results_db <- mem_results
      mem_results_db[, location_code := county]
      mem_results_db[, age := age_group]
      mem_results_db[, tag := conf$tag]
      mem_limits_schema$db_load_data_infile(mem_results_db)
    }
    res <- rbindlist(res)

    counties[res, on = c("season", "location_code"), low := low]
    counties[res, on = c("season", "location_code"), medium := medium]
    counties[res, on = c("season", "location_code"), high := high]
    counties[res, on = c("season", "location_code"), very_high := very_high]

    out <- rbind(national, counties)
    out[, age := age_group]
    out[, tag := conf$tag]
    out[, rate := n / denominator * conf$multiplicative_factor]
    out[fhidata::days, on = "yrwk", date := mon]

    out[, status := as.character(NA)]
    out[rate < low, status := "verylow"]
    out[rate >= low & rate < medium, status := "low"]
    out[rate >= medium & rate < high, status := "medium"]
    out[rate >= high & rate < very_high, status := "high"]
    out[rate >= very_high, status := "veryhigh"]


    out[, x := fhi::x(week)]

    mem_schema$db_load_data_infile(out)
  }
}



prepare_data_frame <- function(data, mult_factor = 100) {
  useful_data <- data[week %in% c(1:20, 40:52)]
  useful_data[, x := fhi::x(week)]
  useful_data[, rate := n / denominator * mult_factor]
  out <- dcast.data.table(useful_data, x ~ season, value.var = "rate")
  out[, x := NULL]
  out <- data.frame(out)
  names(out) <- stringr::str_replace_all(names(out), "\\.", "/")
  names(out) <- stringr::str_remove(names(out), "X")
  if (is.na(out[1, 1])) out <- out[, -1]

  rownames(out) <- c(40:52, 1:20)
  return(out)
}

next_season <- function(season) {
  last_year <- as.integer(stringr::str_split(season, "/")[[1]][2])
  return(paste(last_year, last_year + 1, sep = "/"))
}

run_mem_model <- function(data, conf) {
  out <- list()

  # We need 5 season to calculate the thresholds so start at 6
  for (i in 6:ncol(data)) {
    col <- next_season(names(data)[i])
    model_data <- data[, names(data)[1:i]]

    model_data <- data[, names(model_data)[!(names(model_data) %in% conf$excludeSeason)]]
    epi <- mem::memmodel(model_data)
    out[[col]] <- c(
      epi$epidemic.thresholds[1],
      epi$epi.intervals[1, 4],
      epi$epi.intervals[2, 4],
      epi$epi.intervals[3, 4]
    )
  }

  mem_results <- data.frame(out)
  mem_results$val <- c("low", "medium", "high", "very_high")
  mem_results <- reshape2::melt(mem_results, id = "val")
  setDT(mem_results)
  mem_results[, season := stringr::str_replace(variable, "\\.", "/")]
  mem_results[, season := stringr::str_remove(season, "X")]
  mem_results <- dcast.data.table(mem_results, season ~ val, value.var = "value")

  return(mem_results)
}



mem_results_field_types <- c(
  "tag" = "TEXT",
  "location_code" = "TEXT",
  "season" = "TEXT",
  "yrwk" = "TEXT",
  "year" = "INTEGER",
  "week" = "INTEGER",
  "age" = "TEXT",
  "n" = "INTEGER",
  "denominator" = "INTEGER",
  "x" = "INTEGER",
  "date" = "DATE",
  "rate" = "DOUBLE",
  "low" = "DOUBLE",
  "medium" = "DOUBLE",
  "high" = "DOUBLE",
  "very_high" = "DOUBLE",
  "status" = "TEXT"
)

mem_results_keys <- c(
  "tag",
  "location_code",
  "year",
  "week",
  "age"
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
  ))

#' get_mem_schema
#'
#' DB schema for mem_results
#'
#' @export
get_mem_limits_schema <- function()
  return(fd::schema$new(
    db_table = "spuls_mem_limits",
    db_field_types = list(
      "season" = "TEXT",
      "tag" = "TEXT",
      "age" = "TEXT",
      "location_code" = "TEXT",
      "low" = "DOUBLE",
      "medium" = "DOUBLE",
      "high" = "DOUBLE",
      "very_high" = "DOUBLE"
    ),
    db_load_folder = "/xtmp/",
    keys = c("season", "tag", "age", "location_code")
  ))
