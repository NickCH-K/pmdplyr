#' @importFrom tibble type_sum
#' @export
type_sum.tbl_pb <- function(x) "pibble"

brackets <- function(x) {
  paste0("[", x, "]")
}

unique_brackets <- function(x, col) {
  brackets(length(unique(x[[col]])))
}

#' @export
#' @importFrom tibble tbl_sum
#' @importFrom rlang is_empty syms
#' @importFrom dplyr group_vars
#' @importFrom pillar dim_desc
tbl_sum.tbl_pb <- function(x) {
  res <- c("A pibble" = dim_desc(x))

  # .t and .i are named vectors and this causes problems
  # when the vector is extended with c(). strip names to ensure
  # we don't print redundant information
  i <- x %@% ".i" %>% unname()
  t <- x %@% ".t" %>% unname()
  d <- x %@% ".d"
  groups <- x %@% "groups"

  if (!rlang::is_empty(i)) {
    if (!identical(i, NA)) {
      n_distinct_i <- x %>%
        dplyr::distinct(!!!syms(i)) %>%
        nrow()
      if (length(i) > 1) {
        i <- paste(i, collapse = ", ")
        res <- c(res, "Individual-level identifiers (.i)" = paste(i, brackets(n_distinct_i)))
      } else {
        res <- c(res, "Individual-level identifier (.i)" = paste(i, brackets(n_distinct_i)))
      }
    }
  }

  if (!is_empty(t)) {
    if (!is.na(t)) {
      res <- c(res, "Time variable (.t)" = paste(t, unique_brackets(x, t)))
    }
  }

  if (!is_empty(d)) {
    res <- c(res, "Gap (.d)" = d)
  }

  if (!is_empty(groups)) {
    n_grps <- NROW(groups)
    if (n_grps == 0) n_grps <- "?"
    res <- c(res, "Groups" = paste(group_vars(x), brackets(n_grps)))
  }

  res
}
