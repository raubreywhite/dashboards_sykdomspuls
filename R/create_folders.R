#' create_folders
#' @export
create_folders <- function() {
  # standard
  fs::dir_create(fd::path("results", "externalapi"))
  fs::dir_create(fd::path("results", latest_date()))
  fs::dir_create(fd::path("results", latest_date(), "standard"))
  fs::dir_create(fd::path("results", latest_date(), "standard", "alert_pdfs"))
  fs::dir_create(fd::path("results", latest_date(), "emerg"))
  fs::dir_create(fd::path("results", latest_date(), "stats"))
  fs::dir_create(fd::path("results", latest_date(), "skabb"))

  # mem?
}
