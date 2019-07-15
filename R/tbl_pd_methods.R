#' pdeclare methods
#'
#' These are variants of existing functions that are designed to retain the \code{pdeclare} status of the object, as well as its \code{.i}, \code{.t}, and \code{.d} attributes.
#'
#' If a function is not on this list, then you may need to re-\code{as_pdeclare} your object after using the function.
#' @name pdeclare_methods
NULL

#' @rdname pdeclare_methods
#' @method mutate tbl_pd
#' @export
#' @export mutate.tbl_pd
mutate.tbl_pd <- function(.data,...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::mutate.tbl_df(.data,...), .i = .i, .t = .t, .d = .d))
}

#' @rdname pdeclare_methods
#' @method select tbl_pd
#' @export
#' @export select.tbl_pd
select.tbl_pd <- function(.data,...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  #perform select
  .data <- dplyr:::select.data.frame(.data,...)

  #see if we just dropped .i or .t
  if (min(c(.i,.t) %in% names(.data)) == 0) {
    warning('select() function used to drop variables in .i or .t. Removing pdeclare status.')

    class(.data) <- class(.data)[!(class(.data) %in% 'tbl_pd')]

    # %@% not working here for some reason?
    attr(.data,".i") <- NULL
    attr(.data,".t") <- NULL
    attr(.data,".d") <- NULL

    return(.data)
  } else {
    return(build_pdeclare(.data, .i = .i, .t = .t, .d = .d))
  }
}
