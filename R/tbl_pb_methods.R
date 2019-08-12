#' pibble methods
#'
#' These are variants of existing functions that are designed to retain the \code{pibble} status of the object, as well as its \code{.i}, \code{.t}, and \code{.d} attributes.
#'
#' Some functions that already preserve \code{pibble} status and so don't need special methods include:
#'
#' \code{dplyr::filter, dplyr::filter_all, dplyr::filter_at, dplyr::filter_if, dplyr::arrange, dplyr::arrange_all, dplyr::arrange_at, dplyr::arrange_if, dplyr::sample_frac, dplyr::slice, dplyr::sample_n, dplyr::top_n, dplyr::add_row, tibble:add_column}
#'
#' \code{dplyr::bind_rows} is currently not supported.
#'
#' Any function that takes two data frames/tibbles as inputs will retain the panel structure of the \emph{first} argument.
#'
#' If a function is not on the above list or elsewhere in this help file, then you may need to re-\code{as_pibble} your object after using the function.
#' @param .data,x,.tbl These functions take a \code{tbl_pb} (i.e. \code{pibble}) object as input
#' @param .add,by,.cols,copy,.drop,.funs,keep,.keep_all,name,.predicate,suffix,.vars,y,... Other parameters to be passed to the relevant functions
#' @name pibble_methods
NULL


########## PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE PIBBLE STATUS AFTER RUNNING
#' @rdname pibble_methods
#' @importFrom dplyr mutate
#' @method mutate tbl_pb
#' @export
#' @export mutate.tbl_pb
mutate.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(build_pibble(dplyr::mutate(.data, ...), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr mutate_all
#' @method mutate_all tbl_pb
#' @export
#' @export mutate_all.tbl_pb
mutate_all.tbl_pb <- function(.tbl, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::mutate_all(.tbl, .funs, ...), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr mutate_at
#' @method mutate_at tbl_pb
#' @export
#' @export mutate_at.tbl_pb
mutate_at.tbl_pb <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::mutate_at(.tbl, .vars, .funs, ..., .cols), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr mutate_if
#' @method mutate_if tbl_pb
#' @export
#' @export mutate_if.tbl_pb
mutate_if.tbl_pb <- function(.tbl, .predicate, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::mutate_if(.tbl, .predicate, .funs, ...), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr distinct
#' @method distinct tbl_pb
#' @export
#' @export distinct.tbl_pb
distinct.tbl_pb <- function(.data, ..., .keep_all = FALSE) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(build_pibble(dplyr::distinct(.data, ..., .keep_all = .keep_all), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr group_by
#' @method group_by tbl_pb
#' @export
#' @export group_by.tbl_pb
group_by.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(build_pibble(dplyr::group_by(.data, ...), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr group_by_all
#' @method group_by_all tbl_pb
#' @export
#' @export group_by_all.tbl_pb
group_by_all.tbl_pb <- function(.tbl, .funs = list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::group_by(.tbl, .funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr group_by_all
#' @method group_by_all tbl_pb
#' @export
#' @export group_by_all.tbl_pb
group_by_all.tbl_pb <- function(.tbl, .funs = list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::group_by_all(.tbl, .funs, ..., .add, .drop), .i, .t, .d))
}


#' @rdname pibble_methods
#' @importFrom dplyr group_by_at
#' @method group_by_at tbl_pb
#' @export
#' @export group_by_at.tbl_pb
group_by_at.tbl_pb <- function(.tbl, .vars, .funs = list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::group_by_at(.tbl, .vars, .funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr group_by_if
#' @method group_by_if tbl_pb
#' @export
#' @export group_by_if.tbl_pb
group_by_if.tbl_pb <- function(.tbl, .predicate, .funs = list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(build_pibble(dplyr::group_by_if(.tbl, .predicate, .funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr ungroup
#' @method ungroup tbl_pb
#' @export
#' @export ungroup.tbl_pb
ungroup.tbl_pb <- function(x, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::ungroup(x, ...), .i, .t, .d))
}

#' @rdname pibble_methods
#' @importFrom dplyr bind_cols
#' @method bind_cols tbl_pb
#' @export
#' @export bind_cols.tbl_pb
bind_cols.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(build_pibble(dplyr::bind_cols(.data, ...), .i, .t, .d))
}

##### BIND_ROWS WHY WON'T YOU CALL BIND_ROWS.tbl_pb???
#' Set operations
#'
#' These functions overwrite the set functions provided in base to make them generic to be used to
#' join pibbles. See  \link[dplyr]{setops} for details.
#'
#' @rdname setops
#' @inheritParams dplyr::setops
#' @name setops
NULL

#' @rdname setops
#' @importFrom dplyr intersect
#' @method intersect tbl_pb
#' @export
#' @export intersect.tbl_pb
intersect.tbl_pb <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::intersect(x, y, ...), .i, .t, .d))
}

# easter egg
greatest_hits <- function() {
  lyrics <- c(
    "If you're Steve", "You're Steve now", "You'll be Steve", "another day", "Promise me,", "that you won't fight", "",
    "or attrit until you're old and gray...", "", "We've measured you", "so many times", "Both N and T are vast", "", "",
    "Every second,", "every moment", "we'll melt and then recast!", "", "I mutate once", "I mutate twice", "I'll tidy you",
    "at any price.", "I'll lag you now", "turn now to then", "You always said", "adjust for trends...", "", "someday."
  )

  for (l in lyrics) {
    print(l)
    Sys.sleep(1.5)
  }
}

#' @rdname setops
#' @importFrom dplyr union
#' @method union tbl_pb
#' @export
#' @export union.tbl_pb
union.tbl_pb <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::union(x, y, ...), .i, .t, .d))
}

#' @rdname setops
#' @importFrom dplyr union_all
#' @method union_all tbl_pb
#' @export
#' @export union_all.tbl_pb
union_all.tbl_pb <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::union_all(x, y, ...), .i, .t, .d))
}

#' @rdname setops
#' @importFrom dplyr setdiff
#' @method setdiff tbl_pb
#' @export
#' @export setdiff.tbl_pb
setdiff.tbl_pb <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::setdiff(x, y, ...), .i, .t, .d))
}


#' Join two pibbles together
#'
#' These are generic functions that dispatch to individual pibble methods. See  \link[dplyr]{join} for
#' complete documentation.
#'
#' @rdname join
#' @inheritParams dplyr::join
#' @name join.tbl_pb
NULL

#' @importFrom dplyr left_join
#' @method left_join tbl_pb
#' @export
#' @export left_join.tbl_pb
left_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::left_join(x, y, by, copy, suffix, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr inner_join
#' @method inner_join tbl_pb
#' @export
#' @export inner_join.tbl_pb
inner_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::inner_join(x, y, by, copy, suffix, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr right_join
#' @method right_join tbl_pb
#' @export
#' @export right_join.tbl_pb
right_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::right_join(x, y, by, copy, suffix, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr full_join
#' @method full_join tbl_pb
#' @export
#' @export full_join.tbl_pb
full_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::full_join(x, y, by, copy, suffix, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr semi_join
#' @method semi_join tbl_pb
#' @export
#' @export semi_join.tbl_pb
semi_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::semi_join(x, y, by, copy, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr nest_join
#' @method nest_join tbl_pb
#' @export
#' @export nest_join.tbl_pb
nest_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::nest_join(x, y, by, copy, keep, name, ...), .i, .t, .d))
}

#' @rdname join
#' @importFrom dplyr anti_join
#' @method anti_join tbl_pb
#' @export
#' @export anti_join.tbl_pb
anti_join.tbl_pb <- function(x, y, by = NULL, copy = FALSE, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pb status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pb")]

  return(build_pibble(dplyr::anti_join(x, y, by, copy, ...), .i, .t, .d))
}

########### PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE STATUS & CHECK IF VARS DROPPED
#' @rdname pibble_methods
#' @importFrom dplyr select
#' @method select tbl_pb
#' @export
#' @export select.tbl_pb
select.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(dropchecker(dplyr::select(.data, ...), .i, .t, .d, "select"))
}

#' @rdname pibble_methods
#' @importFrom dplyr select_all
#' @method select_all tbl_pb
#' @export
#' @export select_all.tbl_pb
select_all.tbl_pb <- function(.tbl, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::select_all(.tbl, .funs, ...), .i, .t, .d, "select_all"))
}

#' @rdname pibble_methods
#' @importFrom dplyr select_at
#' @method select_at tbl_pb
#' @export
#' @export select_at.tbl_pb
select_at.tbl_pb <- function(.tbl, .vars, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::select_at(.tbl, .vars, .funs, ...), .i, .t, .d, "select_at"))
}

#' @rdname pibble_methods
#' @importFrom dplyr select_if
#' @method select_if tbl_pb
#' @export
#' @export select_if.tbl_pb
select_if.tbl_pb <- function(.tbl, .predicate, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::select_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "select_if"))
}

#' @rdname pibble_methods
#' @importFrom dplyr rename
#' @method rename tbl_pb
#' @export
#' @export rename.tbl_pb
rename.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(dropchecker(dplyr::rename(.data, ...), .i, .t, .d, "rename"))
}

#' @rdname pibble_methods
#' @importFrom dplyr rename_all
#' @method rename_all tbl_pb
#' @export
#' @export rename_all.tbl_pb
rename_all.tbl_pb <- function(.tbl, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::rename_all(.tbl, .funs, ...), .i, .t, .d, "rename_all"))
}

#' @rdname pibble_methods
#' @importFrom dplyr rename_at
#' @method rename_at tbl_pb
#' @export
#' @export rename_at.tbl_pb
rename_at.tbl_pb <- function(.tbl, .vars, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::rename_at(.tbl, .vars, .funs, ...), .i, .t, .d, "rename_at"))
}

#' @rdname pibble_methods
#' @importFrom dplyr rename_if
#' @method rename_if tbl_pb
#' @export
#' @export rename_if.tbl_pb
rename_if.tbl_pb <- function(.tbl, .predicate, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::rename_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "rename_if"))
}

#' @rdname pibble_methods
#' @importFrom dplyr summarize
#' @method summarize tbl_pb
#' @export
#' @export summarize.tbl_pb
summarize.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(dropchecker(dplyr::summarize(.data, ...), .i, .t, .d, "summarize"))
}

#' @rdname pibble_methods
#' @importFrom dplyr summarize_all
#' @method summarize_all tbl_pb
#' @export
#' @export summarize_all.tbl_pb
summarize_all.tbl_pb <- function(.tbl, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::summarize(.tbl, .funs, ...), .i, .t, .d, "summarize_all"))
}

#' @rdname pibble_methods
#' @importFrom dplyr summarize_at
#' @method summarize_at tbl_pb
#' @export
#' @export summarize_at.tbl_pb
summarize_at.tbl_pb <- function(.tbl, .vars, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::summarize_at(.tbl, .vars, .funs, ...), .i, .t, .d, "summarize_at"))
}

#' @rdname pibble_methods
#' @importFrom dplyr summarize_if
#' @method summarize_if tbl_pb
#' @export
#' @export summarize_if.tbl_pb
summarize_if.tbl_pb <- function(.tbl, .predicate, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::summarize_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "summarize_if"))
}

#' @rdname pibble_methods
#' @importFrom dplyr summarise
#' @method summarise tbl_pb
#' @export
#' @export summarise.tbl_pb
summarise.tbl_pb <- summarize.tbl_pb

#' @rdname pibble_methods
#' @importFrom dplyr summarise_all
#' @method summarise_all tbl_pb
#' @export
#' @export summarise_all.tbl_pb
summarise_all.tbl_pb <- summarize_all.tbl_pb

#' @rdname pibble_methods
#' @importFrom dplyr summarise_at
#' @method summarise_at tbl_pb
#' @export
#' @export summarise_at.tbl_pb
summarise_at.tbl_pb <- summarize_at.tbl_pb

#' @rdname pibble_methods
#' @importFrom dplyr summarise_if
#' @method summarise_if tbl_pb
#' @export
#' @export summarise_if.tbl_pb
summarise_if.tbl_pb <- summarize_if.tbl_pb


#' @rdname pibble_methods
#' @importFrom dplyr transmute
#' @method transmute tbl_pb
#' @export
#' @export transmute.tbl_pb
transmute.tbl_pb <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

  return(dropchecker(dplyr::transmute(.data, ...), .i, .t, .d, "transmute"))
}

#' @rdname pibble_methods
#' @importFrom dplyr transmute_all
#' @method transmute_all tbl_pb
#' @export
#' @export transmute_all.tbl_pb
transmute_all.tbl_pb <- function(.tbl, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::transmute_all(.tbl, .funs, ...), .i, .t, .d, "transmute_all"))
}

#' @rdname pibble_methods
#' @importFrom dplyr transmute_at
#' @method transmute_at tbl_pb
#' @export
#' @export transmute_at.tbl_pb
transmute_at.tbl_pb <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::transmute_at(.tbl, .vars, .funs, ..., .cols), .i, .t, .d, "transmute_at"))
}

#' @rdname pibble_methods
#' @importFrom dplyr transmute_if
#' @method transmute_if tbl_pb
#' @export
#' @export transmute_if.tbl_pb
transmute_if.tbl_pb <- function(.tbl, .predicate, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pb status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pb")]

  return(dropchecker(dplyr::transmute_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "transmute_if"))
}


dropchecker <- function(.data, .i, .t, .d, method) {
  # see if we just renamed .i or .t
  checknames <- c(.i, .t)
  checknames <- checknames[!is.na(checknames)]

  if (min(checknames %in% names(.data)) == 0) {
    warning(paste(method, "() function leaves data without variables listed in .i or .t. Removing pibble status.", sep = ""))

    class(.data) <- class(.data)[!(class(.data) %in% "tbl_pb")]

    # %@% not working here for some reason?
    attr(.data, ".i") <- NULL
    attr(.data, ".t") <- NULL
    attr(.data, ".d") <- NULL

    return(.data)
  } else {
    return(build_pibble(.data, .i, .t, .d))
  }
}
