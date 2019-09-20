#' split_into_equal
#' @param var a
#' @param length a
#' @export
split_into_equal <- function(var, length = 20) {
  split(var, ceiling(seq_along(var) / length))
}
