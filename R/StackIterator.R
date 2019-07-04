#' Stack Iterator
#' @param stack a
#' @param data a
#' @export
stack_iterator <- function(stack, data) {
  . <- NULL

  it <- iterators::icount(nrow(stack))
  pb <- fhi::txt_progress_bar(min = 0, max = nrow(stack))

  nextEl <- function() {
    i <- iterators::nextElem(it)

    if(i %%10 == 0) setTxtProgressBar(pb, i)

    retval_stack <- stack[i]
    retval_data <- data[.(retval_stack$location, retval_stack$age)]
    retval <- list(
      stack = retval_stack,
      data = retval_data,
      i = i
    )

    retval
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c("abstractiter", "iter")
  obj
}

#' Stack Iterator
#' @param stack a
#' @param data a
#' @export
stack_iterator_batch <- function(stack, data) {
  . <- NULL

  locations <- unique(stack$location)
  locations <- split_into_equal(locations,6)

  it <- iterators::icount(length(locations))
  #pb <- fhi::txt_progress_bar(min = 0, max = nrow(stack))


  nextEl <- function() {
    i <- iterators::nextElem(it)

    #setTxtProgressBar(pb, i)

    retval_stack <- stack[location %in% locations[[i]]]
    retval_data <- data[.(locations[[i]])]
    retval <- list(
      stack = retval_stack,
      data = retval_data,
      i = i
    )

    retval
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c("abstractiter", "iter")
  obj
}


run_stack_not_parallel <- function(stack, data){
  retval <- vector("list", length = nrow(stack))
  for (i in seq_along(retval)) {
    retval[[i]] <- list("stack" = stack[i], "data" = data[.(stack$location[i], stack$age[i])])
  }

  lapply(retval, function(x){
    uuid <- x$stack$uuid

    retval <- sykdomspuls::QuasipoissonTrainPredictData(
      datasetTrain = x$data[date >= x$stack$date_train_min & date <= x$stack$date_train_max],
      datasetPredict = x$data[date >= x$stack$date_predict_min & date <= x$stack$date_predict_max],
      isDaily = x$stack$granularity_time == "daily",
      v = v,
      weeklyDenominatorFunction = ifelse(x$stack$weeklyDenominatorFunction=="sum",sum,mean),
      denominator_string = x$stack$denominator
    )
    retval[,uuid:=uuid]
  })
}
