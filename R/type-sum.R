#' @importFrom tibble type_sum
#' @export
type_sum.tbl_pb <- function(x) "pibble"

#' @export
#' @importFrom tibble tbl_sum
#' @importFrom rlang is_empty
tbl_sum.tbl_pb <- function(x) {
  res <- c("A pibble" = dim_desc(x))

  i <- x %@% ".i"
  t <- x %@% ".t"
  d <- x %@% ".d"

  if (!is_empty(i)) {
    # there may be more than one index var
    i <- paste(i, collapse = " ")
    res <- c(res, "Individual-level identifier (.i)" = i)
  }

  if (!is_empty(t)) {
    # for some reason .t is a named vector - strip names to ensure
    # we don't print redundant information
    res <- c(res, "Time variable (.t)" = unname(t))
  }

  if (!is_empty(d)) {
    res <- c(res, "Gap (.d)" = d)
  }

  res
}
