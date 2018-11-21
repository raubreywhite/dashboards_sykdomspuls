#' ResultsAggregateStack
#' @export ResultsAggregateStack
ResultsAggregateStack <- function(){
  fileNameInput <- NULL
  fileType <- NULL
  tag <- NULL
  fileNameOutput1 <- NULL
  fileNameOutput2 <- NULL
  variable <- NULL

  files <- data.table(expand.grid(
    "fileType"=c("resComparisons", "resRecentLine", "resYearLine", "resYearLineMunicip"),
    "tag"=sykdomspuls::CONFIG$SYNDROMES$tag,
    stringsAsFactors = F
  ))
  files[,fileNameInput:=fhi::DashboardFolder("results",
                                             sprintf("%s_%s.RDS",
                                                     fileType,
                                                     tag
                                             ))]
  files[,fileNameOutput1:=fhi::DashboardFolder("results",
                                               sprintf("%s.RDS",
                                                       fileType
                                               ))]
  files[,fileNameOutput2:=fhi::DashboardFolder("data_app",
                                               sprintf("%s.RDS",
                                                       fileType
                                               ))]
  files <- melt.data.table(files,id.vars=c(
    "fileType",
    "tag",
    "fileNameInput"
  ),
  value.name="fileNameOutput")
  files[,variable:=NULL]

  return(files)
}

#' ResultsAggregateApply
#' @param stackFiles The file stack that chooses which files to aggregate
#' @export ResultsAggregateApply
ResultsAggregateApply <- function(stackFiles = ResultsAggregateStack()){
  fileNameOutput <- NULL

  for(fo in unique(stackFiles$fileNameOutput)){
    fis <- stackFiles[fileNameOutput==fo]$fileNameInput
    res <- vector("list",length=length(fis))
    for(i in seq_along(fis)){
      res[[i]] <- readRDS(fis[i])
    }
    res <- rbindlist(res)
    saveRDS(res,fo)
  }
}
