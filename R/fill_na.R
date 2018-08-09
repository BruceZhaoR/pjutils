#' Fill NA with the last no-NA value generic.
#'
#' This method can apply to numeric vector, data.frame/tibble. grouped_df.
#' If you want to apply to data.frame/tibble, please \code{library(dplyr)}.
#'
#' @param x a data.frame or a vector
#' @param ... col names character vector or just bare name.
#'
#' @return data.frame or a vector
#'
#' @examples
#' x <- c(1, NA, NA, 2, 3)
#' y <- c(NA, NA, 1, NA, 3)
#' xy <- data.frame(x,y)
#' fill_na(x)
#' fill_na(y)
#' \dontrun{
#' fill_na(xy, x, y)
#' fill_na(xy, c("x","y"))
#' fill_na(xy, starts_with("x"))
#' }
#'
#'@export
fill_na <- function(x, ...) {
  UseMethod("fill_na")
}


#' @inheritParams fill_na
#' @export
#' @rdname fill_na
fill_na.default <- function(x, ...) {
  stopifnot(is.vector(x))
  a <- !is.na(x)
  x[which(a)[c(1, seq_along(which(a)))][cumsum(a) + 1]]
}

#' @inheritParams fill_na
#' @export
#' @rdname fill_na
fill_na.tbl_df<- function(x, ...) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    fill_cols <- unname(tidyselect::vars_select(names(x), ...))
    for (col in fill_cols) {
      x[[col]] <- fill_na(x[[col]])
    }
    x
  } else {
    print("please library(dplyr) ")
  }
}

#' @inheritParams fill_na
#' @export
#' @rdname fill_na
fill_na.data.frame <- function(x, ...) {
  if (requireNamespace("tidyselect", quietly = TRUE)) {
    fill_cols <- unname(tidyselect::vars_select(names(x), ...))
    for (col in fill_cols) {
      x[[col]] <- fill_na(x[[col]])
    }
    x
  } else {
    print("please library(tidyselect) ")
  }
}

#' @inheritParams fill_na
#' @export
#' @rdname fill_na
fill_na.grouped_df <- function(x, ...) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::do(x, fill_na(...))
  } else {
    print("please library(dplyr) ")
  }

}
