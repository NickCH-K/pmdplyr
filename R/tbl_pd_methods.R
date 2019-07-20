#' pdeclare methods
#'
#' These are variants of existing functions that are designed to retain the \code{pdeclare} status of the object, as well as its \code{.i}, \code{.t}, and \code{.d} attributes.
#'
#' Some functions that already preserve \code{pdeclare} status and so don't need special methods include:
#'
#' \code{dplyr::filter, dplyr::arrange, dplyr::sample_frac, dplyr::slice, dplyr::sample_n, dplyr::top_n, dplyr::add_row, tibble:add_column}
#'
#' Also note that any function that takes two data frames/tibbles as inputs will retain the panel structure of the \emph{first} argument.
#'
#' If a function is not on the above list or elsewhere in this help file, then you may need to re-\code{as_pdeclare} your object after using the function.
#' @param .data These functions take a \code{tbl_pd} (i.e. \code{pdeclare}) object as input
#' @param ... and pass other arguments onto the relevant functions.
#' @name pdeclare_methods
NULL

########## PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE PDECLARE STATUS AFTER RUNNING
#' @rdname pdeclare_methods
#' @method mutate tbl_pd
#' @export
#' @export mutate.tbl_pd
mutate.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::mutate.tbl_df(.data, ...), .i = .i, .t = .t, .d = .d))
}

#' @rdname pdeclare_methods
#' @method distinct tbl_pd
#' @export
#' @export distinct.tbl_pd
distinct.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::distinct.tbl_df(.data, ...), .i = .i, .t = .t, .d = .d))
}

#' @rdname pdeclare_methods
#' @method group_by tbl_pd
#' @export
#' @export group_by.tbl_pd
group_by.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::group_by.data.frame(.data, ...), .i = .i, .t = .t, .d = .d))
}

#' @rdname pdeclare_methods
#' @method ungroup tbl_pd
#' @export
#' @export ungroup.tbl_pd
ungroup.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::ungroup.grouped_df(.data, ...), .i = .i, .t = .t, .d = .d))
}

#' @rdname pdeclare_methods
#' @method bind_cols tbl_pd
#' @export
#' @export bind_cols.tbl_pd
bind_cols.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(build_pdeclare(dplyr:::bind_cols(.data, ...), .i = .i, .t = .t, .d = .d))
}

### BIND_ROWS GOES HERE WHY DOES IT NOT WORK

#' @rdname pdeclare_methods
#' @method left_join tbl_pd
#' @export
#' @export left_join.tbl_pd
left_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::left_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method inner_join tbl_pd
#' @export
#' @export inner_join.tbl_pd
inner_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::inner_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method right_join tbl_pd
#' @export
#' @export right_join.tbl_pd
right_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::right_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method full_join tbl_pd
#' @export
#' @export full_join.tbl_pd
full_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::full_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method semi_join tbl_pd
#' @export
#' @export semi_join.tbl_pd
semi_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::semi_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method nest_join tbl_pd
#' @export
#' @export nest_join.tbl_pd
nest_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::nest_join(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @method anti_join tbl_pd
#' @export
#' @export anti_join.tbl_pd
anti_join.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr:::anti_join(.data, ...), .i, .t, .d))
}

########### PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE STATUS & CHECK IF VARS DROPPED
#' @rdname pdeclare_methods
#' @method select tbl_pd
#' @export
#' @export select.tbl_pd
select.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(dropchecker(dplyr:::select.data.frame(.data, ...), .i, .t, .d, "summarize"))
}

#' @rdname pdeclare_methods
#' @method rename tbl_pd
#' @export
#' @export rename.tbl_pd
rename.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  return(dropchecker(dplyr:::rename.data.frame(.data, ...), .i, .t, .d, "rename"))
}

#' @rdname pdeclare_methods
#' @method summarize tbl_pd
#' @export
#' @export summarize.tbl_pd
summarize.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(dropchecker(dplyr:::summarize(.data, ...), .i, .t, .d, "summarize"))
}

#' @rdname pdeclare_methods
#' @method summarise tbl_pd
#' @export
#' @export summarise.tbl_pd
summarise.tbl_pd <- summarize.tbl_pd


#' @rdname pdeclare_methods
#' @method transmute tbl_pd
#' @export
#' @export transmute.tbl_pd
transmute.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  if (is_grouped_df(.data)) {
    return(dropchecker(dplyr:::transmute.grouped_df(.data, ...), .i, .t, .d, "transmute"))
  } else {
    return(dropchecker(dplyr:::transmute.default(.data, ...), .i, .t, .d, "transmute"))
  }
}

dropchecker <- function(.data, .i, .t, .d, method) {
  # see if we just renamed .i or .t
  if (min(c(.i, .t) %in% names(.data)) == 0) {
    warning(paste(method, "() function leaves data without variables listed in .i or .t. Removing pdeclare status.", sep = ""))

    class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

    # %@% not working here for some reason?
    attr(.data, ".i") <- NULL
    attr(.data, ".t") <- NULL
    attr(.data, ".d") <- NULL

    return(.data)
  } else {
    return(build_pdeclare(.data, .i = .i, .t = .t, .d = .d))
  }
}
