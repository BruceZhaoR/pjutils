
#' Get the position of x in vector y
#'
#' @author ZhaoWei
#' @param x A numeric value
#' @param y A numeric vector
#' @param tol double equal tolerance
#'
#' @return A integer of the \code{x} position in \code{y}.
#' @export
#' @rdname binarySearch
#'
#' @examples
#' binary_search_r(2.5, 1:10)
#' binary_search_r(1.2, 1:10)
#' binary_search_r(0.5, 1:10)
#' binary_search_r(9.5, 1:10)
#' binary_search_r(11, 1:10)
#'
binary_search_r <- function(x, y, tol = sqrt(.Machine$double.eps)) {

  startIndex <- 1L
  endIndex <- length(y)
  stopifnot(is.numeric(x) && is.vector(y, mode = "numeric"))

  while (startIndex <= endIndex) {
    midIndex <- as.integer(ceiling((startIndex + endIndex) / 2))
    midValue <- y[midIndex]

    if (midValue < x - tol) {
      startIndex = midIndex + 1L
    } else if (midValue > x + tol) {
      endIndex = midIndex - 1L
    } else {
      return(midIndex)
    }
  }
  return(startIndex)
}
