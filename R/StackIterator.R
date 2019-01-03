#' Stack Iterator
#' @param stackStrata a
#' @param stack a
#' @param data a
#' @param progressFunction a
#' @export
StackIterator <- function(stackStrata, stack, data, progressFunction) {
  . <- NULL

  it <- iterators::icount(length(stackStrata))

  nextEl <- function() {
    i <- iterators::nextElem(it)
    progressFunction()
    run <- stackStrata[[i]]
    retval <- vector("list", length = length(run))
    for (j in seq_along(run)) {
      retval[[j]] <- list("stack" = stack[run[j]], "data" = data[.(stack$location[run[j]], stack$age[run[j]])])
    }
    retval
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c("abstractiter", "iter")
  obj
}
