#' Model classes
#'
#' @import R6 

QuasiPoission <-  R6Class(
  "QuasiPoission",
  list(
    conf = NULL,
    run = function(conf) {
      print("run QP")
      fhi::Log("analyse1Before")
      fhi::DashboardMsg(conf$tag)
      stackAndData <- StackAndEfficientDataForAnalysisInList(conf = conf)
      
      res <- pbmclapply(stackAndData,
                        function(x) RunOneAnalysis(analysesStack = x$stack, analysisData = x$data),
                        mc.cores = parallel::detectCores()
                        )
      
      res <- rbindlist(res)
      
                                        # adding in extra information
      AddLocationName(res)
      AddCounty(res)
      
                                        # cleaning on small municipalities
      res[location %in% CONFIG$smallMunicips & age != "Totalt", n := 0 ]
      res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold2 := 5 ]
      res[location %in% CONFIG$smallMunicips & age != "Totalt", threshold4 := 10 ]
      
      fhi::DashboardMsg("Saving files", newLine = T)
      for (f in unique(res$file)) {
        fhi::DashboardMsg(sprintf("Saving file %s", f))
        saveRDS(res[file == f], file = fhi::DashboardFolder("results", sprintf("%s/%s", LatestRawID(), f)))
      }
      
      rm("res", "stackAndData")
      
                                        # displaying timing information
      timeElapsed <- as.numeric(difftime(Sys.time(), fhi::LogGet()$analyse1Before, units = "min"))
      timeTotal <- (timeElapsed / i) * nrow(sykdomspuls::CONFIG$QUASIPOISSON)
      timeRemaining <- timeTotal * (nrow(sykdomspuls::CONFIG$QUASIPOISSON) - i) / nrow(sykdomspuls::CONFIG$QUASIPOISSON)
      fhi::DashboardMsg(sprintf(
             "%s min total, %s min elapsed, %s min remaining",
             round(timeTotal),
             round(timeElapsed),
             round(timeRemaining)
           ))
  },
  save = function(){
    print("save QP")
    fhi::Log("save1Before")
    ResultsAggregateApply()
    fhi::Log("save1After")
  },
  setup_db = function(){
    return(NULL)
  }
)
)

MeM <-  R6Class(
  "MeM",
  list(
    conf = NULL,
    run = function(conf) {
      print("run MeM")
      sykdomspuls::run_all_mem(conf)
    },
    setup_db = function(){
      mem_schema$db_connect(sykdomspuls::CONFIG$DB_CONFIG)
      mem_schema$db_create_table()
     # mem_schema$add_index_db()
    },
    save = function(){
      print("save MeM")
    } )
)



#'
#' Model List
#'
#' This variable includes a list of all the model classes
#' @export models
models = list("quasipoisson" = QuasiPoission,
              "mem" = MeM)
