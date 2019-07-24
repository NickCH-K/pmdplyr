#' pdeclare methods
#'
#' These are variants of existing functions that are designed to retain the \code{pdeclare} status of the object, as well as its \code{.i}, \code{.t}, and \code{.d} attributes.
#'
#' Some functions that already preserve \code{pdeclare} status and so don't need special methods include:
#'
#' \code{dplyr::filter, dplyr::filter_all, dplyr::filter_at, dplyr::filter_if, dplyr::arrange, dplyr::arrange_all, dplyr::arrange_at, dplyr::arrange_if, dplyr::sample_frac, dplyr::slice, dplyr::sample_n, dplyr::top_n, dplyr::add_row, tibble:add_column}
#'
#' \code{dplyr::bind_rows} is currently not supported.
#'
#' Any function that takes two data frames/tibbles as inputs will retain the panel structure of the \emph{first} argument.
#'
#' If a function is not on the above list or elsewhere in this help file, then you may need to re-\code{as_pdeclare} your object after using the function.
#' @param .data,x,.tbl These functions take a \code{tbl_pd} (i.e. \code{pdeclare}) object as input
#' @param .add,by,.cols,copy,.drop,.funs,keep,.keep_all,name,.predicate,suffix,.vars,y,... Other parameters to be passed to the relevant functions
#' @name pdeclare_methods
NULL


########## PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE PDECLARE STATUS AFTER RUNNING
#' @rdname pdeclare_methods
#' @importFrom dplyr mutate
#' @method mutate tbl_pd
#' @export
#' @export mutate.tbl_pd
mutate.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::mutate(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr mutate_all
#' @method mutate_all tbl_pd
#' @export
#' @export mutate_all.tbl_pd
mutate_all.tbl_pd <- function(.tbl,.funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::mutate_all(.tbl,.funs, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr mutate_at
#' @method mutate_at tbl_pd
#' @export
#' @export mutate_at.tbl_pd
mutate_at.tbl_pd <- function(.tbl,.vars,.funs, ...,.cols=NULL) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::mutate_at(.tbl,.vars,.funs, ...,.cols), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr mutate_if
#' @method mutate_if tbl_pd
#' @export
#' @export mutate_if.tbl_pd
mutate_if.tbl_pd <- function(.tbl,.predicate,.funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::mutate_if(.tbl,.predicate,.funs, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr distinct
#' @method distinct tbl_pd
#' @export
#' @export distinct.tbl_pd
distinct.tbl_pd <- function(.data, ...,.keep_all=FALSE) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::distinct(.data, ...,.keep_all), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr group_by
#' @method group_by tbl_pd
#' @export
#' @export group_by.tbl_pd
group_by.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::group_by(.data, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr group_by_all
#' @method group_by_all tbl_pd
#' @export
#' @export group_by_all.tbl_pd
group_by_all.tbl_pd <- function(.tbl, .funs=list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::group_by(.tbl,.funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr group_by_all
#' @method group_by_all tbl_pd
#' @export
#' @export group_by_all.tbl_pd
group_by_all.tbl_pd <- function(.tbl, .funs=list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::group_by_all(.tbl,.funs, ..., .add, .drop), .i, .t, .d))
}


#' @rdname pdeclare_methods
#' @importFrom dplyr group_by_at
#' @method group_by_at tbl_pd
#' @export
#' @export group_by_at.tbl_pd
group_by_at.tbl_pd <- function(.tbl, .vars, .funs=list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::group_by_at(.tbl,.vars,.funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr group_by_if
#' @method group_by_if tbl_pd
#' @export
#' @export group_by_if.tbl_pd
group_by_if.tbl_pd <- function(.tbl, .predicate, .funs=list(), ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.tbl)) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::group_by_if(.tbl,.predicate,.funs, ..., .add, .drop), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr ungroup
#' @method ungroup tbl_pd
#' @export
#' @export ungroup.tbl_pd
ungroup.tbl_pd <- function(x, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::ungroup(x, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr bind_cols
#' @method bind_cols tbl_pd
#' @export
#' @export bind_cols.tbl_pd
bind_cols.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::bind_cols(.data, ...), .i, .t, .d))
}

#####BIND_ROWS WHY WON'T YOU CALL BIND_ROWS.TBL_PD???

#' @rdname pdeclare_methods
#' @importFrom dplyr intersect
#' @method intersect tbl_pd
#' @export
#' @export intersect.tbl_pd
intersect.tbl_pd <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::intersect(x, y, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr union
#' @method union tbl_pd
#' @export
#' @export union.tbl_pd
union.tbl_pd <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::union(x, y, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr union_all
#' @method union_all tbl_pd
#' @export
#' @export union_all.tbl_pd
union_all.tbl_pd <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::union_all(x, y, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr setdiff
#' @method setdiff tbl_pd
#' @export
#' @export setdiff.tbl_pd
setdiff.tbl_pd <- function(x, y, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::setdiff(x, y, ...), .i, .t, .d))
}


#' @rdname pdeclare_methods
#' @importFrom dplyr left_join
#' @method left_join tbl_pd
#' @export
#' @export left_join.tbl_pd
left_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::left_join(x,y,by,copy,suffix, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr inner_join
#' @method inner_join tbl_pd
#' @export
#' @export inner_join.tbl_pd
inner_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::inner_join(x,y,by,copy,suffix, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr right_join
#' @method right_join tbl_pd
#' @export
#' @export right_join.tbl_pd
right_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::right_join(x,y,by,copy,suffix, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr full_join
#' @method full_join tbl_pd
#' @export
#' @export full_join.tbl_pd
full_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"), ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::full_join(x,y,by,copy,suffix, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr semi_join
#' @method semi_join tbl_pd
#' @export
#' @export semi_join.tbl_pd
semi_join.tbl_pd <- function(x, y, by=NULL, copy=FALSE, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::semi_join(x,y,by,copy, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr nest_join
#' @method nest_join tbl_pd
#' @export
#' @export nest_join.tbl_pd
nest_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE,keep=FALSE,name=NULL, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::nest_join(x,y,by,copy,keep,name, ...), .i, .t, .d))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr anti_join
#' @method anti_join tbl_pd
#' @export
#' @export anti_join.tbl_pd
anti_join.tbl_pd <- function(x,y,by=NULL,copy=FALSE, ...) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  # remove tbl_pd status so regular version is run
  class(x) <- class(x)[!(class(x) %in% "tbl_pd")]

  return(build_pdeclare(dplyr::anti_join(x,y,by,copy, ...), .i, .t, .d))
}

########### PUT HERE ALL METHODS THAT SIMPLY NEED TO RESTORE STATUS & CHECK IF VARS DROPPED
#' @rdname pdeclare_methods
#' @importFrom dplyr select
#' @method select tbl_pd
#' @export
#' @export select.tbl_pd
select.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(dropchecker(dplyr::select(.data, ...), .i, .t, .d, "select"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr select_all
#' @method select_all tbl_pd
#' @export
#' @export select_all.tbl_pd
select_all.tbl_pd <- function(.tbl, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::select_all(.tbl, .funs, ...), .i, .t, .d, "select_all"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr select_at
#' @method select_at tbl_pd
#' @export
#' @export select_at.tbl_pd
select_at.tbl_pd <- function(.tbl, .vars, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::select_at(.tbl, .vars, .funs, ...), .i, .t, .d, "select_at"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr select_if
#' @method select_if tbl_pd
#' @export
#' @export select_if.tbl_pd
select_if.tbl_pd <- function(.tbl, .predicate, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::select_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "select_if"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr rename
#' @method rename tbl_pd
#' @export
#' @export rename.tbl_pd
rename.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(dropchecker(dplyr::rename(.data, ...), .i, .t, .d, "rename"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr rename_all
#' @method rename_all tbl_pd
#' @export
#' @export rename_all.tbl_pd
rename_all.tbl_pd <- function(.tbl, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::rename_all(.tbl, .funs, ...), .i, .t, .d, "rename_all"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr rename_at
#' @method rename_at tbl_pd
#' @export
#' @export rename_at.tbl_pd
rename_at.tbl_pd <- function(.tbl, .vars, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::rename_at(.tbl, .vars, .funs, ...), .i, .t, .d, "rename_at"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr rename_if
#' @method rename_if tbl_pd
#' @export
#' @export rename_if.tbl_pd
rename_if.tbl_pd <- function(.tbl, .predicate, .funs = list(), ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::rename_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "rename_if"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr summarize
#' @method summarize tbl_pd
#' @export
#' @export summarize.tbl_pd
summarize.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(dropchecker(dplyr::summarize(.data, ...), .i, .t, .d, "summarize"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr summarize_all
#' @method summarize_all tbl_pd
#' @export
#' @export summarize_all.tbl_pd
summarize_all.tbl_pd <- function(.tbl, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::summarize(.tbl, .funs, ...), .i, .t, .d, "summarize_all"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr summarize_at
#' @method summarize_at tbl_pd
#' @export
#' @export summarize_at.tbl_pd
summarize_at.tbl_pd <- function(.tbl, .vars, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::summarize_at(.tbl, .vars, .funs, ...), .i, .t, .d, "summarize_at"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr summarize_if
#' @method summarize_if tbl_pd
#' @export
#' @export summarize_if.tbl_pd
summarize_if.tbl_pd <- function(.tbl, .predicate, .funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::summarize_if(.tbl, .predicate, .funs, ...), .i, .t, .d, "summarize_if"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr summarise
#' @method summarise tbl_pd
#' @export
#' @export summarise.tbl_pd
summarise.tbl_pd <- summarize.tbl_pd

#' @rdname pdeclare_methods
#' @importFrom dplyr summarise_all
#' @method summarise_all tbl_pd
#' @export
#' @export summarise_all.tbl_pd
summarise_all.tbl_pd <- summarize_all.tbl_pd

#' @rdname pdeclare_methods
#' @importFrom dplyr summarise_at
#' @method summarise_at tbl_pd
#' @export
#' @export summarise_at.tbl_pd
summarise_at.tbl_pd <- summarize_at.tbl_pd

#' @rdname pdeclare_methods
#' @importFrom dplyr summarise_if
#' @method summarise_if tbl_pd
#' @export
#' @export summarise_if.tbl_pd
summarise_if.tbl_pd <- summarize_if.tbl_pd


#' @rdname pdeclare_methods
#' @importFrom dplyr transmute
#' @method transmute tbl_pd
#' @export
#' @export transmute.tbl_pd
transmute.tbl_pd <- function(.data, ...) {
  .i <- .data %@% ".i"
  .t <- .data %@% ".t"
  .d <- .data %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

  return(dropchecker(dplyr::transmute(.data, ...), .i, .t, .d, "transmute"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr transmute_all
#' @method transmute_all tbl_pd
#' @export
#' @export transmute_all.tbl_pd
transmute_all.tbl_pd <- function(.tbl,.funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::transmute_all(.tbl,.funs, ...), .i, .t, .d,"transmute_all"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr transmute_at
#' @method transmute_at tbl_pd
#' @export
#' @export transmute_at.tbl_pd
transmute_at.tbl_pd <- function(.tbl, .vars, .funs, ..., .cols=NULL) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::transmute_at(.tbl,.vars,.funs, ...,.cols), .i, .t, .d, "transmute_at"))
}

#' @rdname pdeclare_methods
#' @importFrom dplyr transmute_if
#' @method transmute_if tbl_pd
#' @export
#' @export transmute_if.tbl_pd
transmute_if.tbl_pd <- function(.tbl,.predicate,.funs, ...) {
  .i <- .tbl %@% ".i"
  .t <- .tbl %@% ".t"
  .d <- .tbl %@% ".d"

  # remove tbl_pd status so regular version is run
  class(.tbl) <- class(.tbl)[!(class(.tbl) %in% "tbl_pd")]

  return(dropchecker(dplyr::transmute_if(.tbl,.predicate,.funs, ...), .i, .t, .d, "transmute_if"))
}


dropchecker <- function(.data, .i, .t, .d, method) {
  # see if we just renamed .i or .t
  checknames <- c(.i, .t)
  checknames <- checknames[!is.na(checknames)]

  if (min(checknames %in% names(.data)) == 0) {
    warning(paste(method, "() function leaves data without variables listed in .i or .t. Removing pdeclare status.", sep = ""))

    class(.data) <- class(.data)[!(class(.data) %in% "tbl_pd")]

    # %@% not working here for some reason?
    attr(.data, ".i") <- NULL
    attr(.data, ".t") <- NULL
    attr(.data, ".d") <- NULL

    return(.data)
  } else {
    return(build_pdeclare(.data, .i, .t, .d))
  }
}
