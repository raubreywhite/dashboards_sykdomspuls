#' Convert the new \code{CONFIG} for use in the old external API
#' @export ConvertConfigForAPI
ConvertConfigForAPI <- function(){
  alertInternal <- NULL
  alertExternal <- NULL

  CONFIG_NEW <- new.env(parent = emptyenv())
  for(i in names(CONFIG)){
    CONFIG_NEW[[i]] <- CONFIG[[i]]
  }
  CONFIG_NEW$AGES <- names(CONFIG_NEW$AGES)

  CONFIG_NEW$SYNDROMES <- CONFIG$SYNDROMES$tag
  names(CONFIG_NEW$SYNDROMES) <- CONFIG$SYNDROMES$namesLong

  CONFIG_NEW$SYNDROMES_ALERT_INTERNAL <- CONFIG$SYNDROMES[alertInternal==TRUE]$tag
  names(CONFIG_NEW$SYNDROMES_ALERT_INTERNAL) <- CONFIG$SYNDROMES[alertInternal==TRUE]$namesLong

  CONFIG_NEW$SYNDROMES_ALERT_EXTERNAL <- CONFIG$SYNDROMES[alertExternal==TRUE]$tag
  names(CONFIG_NEW$SYNDROMES_ALERT_EXTERNAL) <- CONFIG$SYNDROMES[alertExternal==TRUE]$namesLong

  CONFIG_NEW$SYNDROMES_SHORT <- CONFIG$SYNDROMES[alertInternal==TRUE]$tag
  names(CONFIG_NEW$SYNDROMES_SHORT) <- CONFIG$SYNDROMES[alertInternal==TRUE]$namesShort

  return(CONFIG_NEW)
}
