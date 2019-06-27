split_into_equal <- function(var, length=20){
  split(var, ceiling(seq_along(var)/length))
}
