#' split_into_equal
#' @param var a
#' @param length a
#' @export
split_into_equal <- function(var, length = 20) {
  split(var, ceiling(seq_along(var) / length))
}


#' connect_db
#'
#' @export
connect_db <- function(table){
  conn <- DBI::dbConnect(odbc::odbc(),
                         driver = CONFIG$DB_CONFIG$driver,
                         server = CONFIG$DB_CONFIG$server,
                         port = CONFIG$DB_CONFIG$port,
                         user = CONFIG$DB_CONFIG$user,
                         password = CONFIG$DB_CONFIG$password
                         )
  fd::use_db(conn, CONFIG$DB_CONFIG$db)
  return(dplyr::tbl(conn, table))

}
