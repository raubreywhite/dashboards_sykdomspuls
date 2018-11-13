#' Convert the new config for use in the API
#'
#' @export ConvertConfigForAPI
ConvertConfigForAPI <- function(){
  CONFIG_NEW <- new.env(parent = emptyenv())
  for(i in names(CONFIG)){
    CONFIG_NEW[[i]] <- CONFIG[[i]]
  }
  CONFIG_NEW$AGES <- names(CONFIG_NEW$AGES)

  return(CONFIG_NEW)
}
