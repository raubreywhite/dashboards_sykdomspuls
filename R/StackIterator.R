#' Stack Iterator
#' @param stack a
#' @param data a
#' @param progressFunction a
#' @export StackIterator
StackIterator <- function(stack, data, progressFunction) {
  . <- NULL

  it <- iterators::icount(nrow(stack))

  nextEl <- function() {
    i <- iterators::nextElem(it)
    progressFunction()
    list("stack" = stack[i], "data" = data[.(stack$location[i], stack$age[i])])
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c("abstractiter", "iter")
  obj
}
