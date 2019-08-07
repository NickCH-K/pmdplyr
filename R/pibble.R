#' Create a pibble panel data set object
#'
#' This function declares a pibble tibble with the attributes \code{.i}, \code{.t}, and \code{.d}.
#'
#' \itemize{
#'   \item \code{.i}, Quoted or unquoted variable(s) indicating the individual-level panel identifier
#'   \item \code{.t}, Quoted or unquoted variable indicating the time variable
#'   \item \code{.d}, a number indicating the gap
#' }
#'
#' The \code{pibble()} function is for the purpose of creating \code{pibble} objects from scratch. You probably want \code{as_pibble}.
#'
#' Note that \code{pibble} does not require that \code{.i} and \code{.t} uniquely identify the observations in your data, but it will give a warning message (a maximum of once per session, unless \code{.uniqcheck=TRUE}) if they do not.
#'
#' @param .df Data frame or tibble to declare as a panel.
#' @param .i Quoted or unquoted variable(s) that identify the individual cases. If this is omitted, \code{pibble} will assume the data set is a single time series.
#' @param .t Quoted or unquoted variable indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to perform a check of whether \code{.i} and \code{.t} uniquely identify observations, and present a message if not. By default this is set to FALSE and the warning message occurs only once per session.
#' @name pibble
#'
#' @examples
#' # Creating a pibble from scratch
#' pd <- pibble(
#'   i = c(1, 1, 1, 2, 2, 2),
#'   t = c(1, 2, 3, 1, 2, 2),
#'   x = rnorm(6),
#'   .i = i,
#'   .t = t
#' )
#' is_pibble(pd)
#' # I set .d=0 here to indicate that I don't care how large the gap between one period and the next is
#' # If I want to use 'seconds' for t.
#' # See time_variable() to turn unruly variables into well-behaved integers, as well
#' pd2 <- pibble(
#'   i = c(1, 1, 1, 2, 2, 2),
#'   seconds = c(123, 456, 789, 103, 234, 238),
#'   .i = i,
#'   .t = seconds,
#'   .d = 0
#' )
#' is_pibble(pd2)
NULL
#' @export

# varname <- function(data,var) {
# mean(data %>% pull({{ var }}),na.rm=TRUE)
# }
# varname(SPrail,!!parse_expr('price'))

# varname <- function(data,var) {
#   enexprs(var)[[1]]
# }
# varname(SPrail,price)
#

# quo_is_missing(enquo(var))


pibble <- function(..., .i = NULL, .t = NULL, .d = 1, .uniqcheck = FALSE) {

  # Create tibble
  tbl <- tibble::tibble(...)

  # Pull out variable names; build_pibble takes strings
  .i <- tidyselect::vars_select(names(tbl), {{ .i }})
  if (length(.i) == 0) {
    .i <- NA_character_
  }
  .t <- tidyselect::vars_select(names(tbl), {{ .t }})
  if (length(.t) == 0) {
    .t <- NA_character_
  }

  # check inputs
  check_panel_inputs(tbl,
    .i = .i,
    .t = .t,
    .d = .d,
    .uniqcheck = .uniqcheck
  )

  # make a pibble
  tbl <- build_pibble(tbl, .i = .i, .t = .t, .d = .d, .uniqcheck = .uniqcheck)

  return(tbl)
}

###### .i and .t are strings for new_pibble
new_pibble <- function(x, ..., class = NULL) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  x <- tibble::new_tibble(x, ..., nrow = nrow(x), class = c("tbl_pb", class))

  return(x)
}

#' @importFrom rlang .data
#' @importFrom vctrs vec_restore
#' @method vec_restore tbl_pb
vec_restore.tbl_pb <- function(x, to) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  return(build_pibble(to, .i = .i, .t = .t, .d = .d))
}


#' Coerce to a pibble panel data set object
#'
#' This function coerces a tibble, data.frame, or list to a pibble tibble by adding the \code{.i}, \code{.t}, and \code{.d} attributes to it.
#'
#' \itemize{
#'   \item \code{.i}, Quoted or unquoted variable(s) indicating the individual-level panel identifier
#'   \item \code{.t}, Quoted or unquoted variable indicating the time variable
#'   \item \code{.d}, a number indicating the gap
#' }
#'
#' Note that pibble does not require that \code{.i} and \code{.t} uniquely identify the observations in your data, but it will give a warning message (a maximum of once per session, unless \code{.uniqcheck=TRUE}) if they do not.
#'
#' @param x A data frame, tibble or list
#' @inheritParams pibble
#' @param ... Other arguments passed on to individual methods.
#' @examples
#' data(SPrail)
#' # I set .d=0 here to indicate that I don't care how large the gap
#' # between one period and the next is.
#' # If I want to use 'insert_date' for .t with a fixed gap between periods,
#' # I need to transform it into an integer first; see time_variable()
#' SP <- as_pibble(SPrail,
#'   .i = c(origin, destination),
#'   .t = insert_date,
#'   .d = 0
#' )
#' is_pibble(SP)
#' attr(SP, ".i")
#' attr(SP, ".t")
#' attr(SP, ".d")
#'
#' data(Scorecard)
#' # Here, year is an integer, so I can use it with .d = 1 to
#' # indicate that one period is a change of one unit in year
#' # Conveniently, .d = 1 is the default
#' Scorecard <- as_pibble(Scorecard, .i = unitid, .t = year)
#' is_pibble(Scorecard)
#' @rdname as_pibble
#' @export
as_pibble <- function(x,
                      .i = NULL,
                      .t = NULL,
                      .d = 1,
                      .uniqcheck = FALSE,
                      ...) {
  UseMethod("as_pibble")
}

#' @rdname as_pibble
#' @export
as_pibble.tbl_df <- function(x,
                             .i = NULL,
                             .t = NULL,
                             .d = 1,
                             .uniqcheck = FALSE,
                             ...) {

  # Pull out variable names; build_pibble takes strings
  .i <- tidyselect::vars_select(names(x), {{ .i }})
  if (length(.i) == 0) {
    .i <- NA_character_
  }
  .t <- tidyselect::vars_select(names(x), {{ .t }})
  if (length(.t) == 0) {
    .t <- NA_character_
  }

  # check inputs
  check_panel_inputs(x,
    .i = .i,
    .t = .t,
    .d = .d,
    .uniqcheck = .uniqcheck
  )

  return(build_pibble(x, .i, .t, .d, .uniqcheck = .uniqcheck, ...))
}


#' @rdname as_pibble
#' @export
as_pibble.grouped_df <- as_pibble.tbl_df

#' @rdname as_pibble
#' @export
as_pibble.data.frame <- as_pibble.tbl_df

#' @rdname as_pibble
#' @export
as_pibble.list <- as_pibble.tbl_df

#' @keywords internal
#' @export
as_pibble.NULL <- function(x, ...) {
  stop("A pibble must not be NULL.")
}

#' Low-level constructor for a pibble object
#'
#' \code{build_pibble()} creates a \code{tbl_pb} object with more controls. It is useful for creating a \code{tbl_pb} internally inside a function.
#'
#' Be aware that \code{pibble} objects store \code{.i} and \code{.t} as strings. As a low-level constructor, \code{build_pibble()} takes only character arguments for \code{.i} and \code{.t}, not unquoted variables.
#'
#' For speed, \code{build_pibble()} does not check the adequacy of the inputs.
#'
#' @export
#' @param .df Data frame or tibble to declare as a panel.
#' @param .i Quoted variable name(s) that identify the individual cases. If this is omitted, \code{pibble} will assume the data set is a single time series.
#' @param .t Quoted variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to perform a check of whether \code{.i} and \code{.t} uniquely identify observations, and present a message if not. By default this is set to FALSE and the warning message occurs only once per session.
#' @keywords internal
#' @importFrom rlang %@%
build_pibble <- function(tbl,
                         .i = NA,
                         .t = NA,
                         .d = 1,
                         .uniqcheck = FALSE) {
  ###### .i and .t are strings by the time we get to build_pibble

  grp_data <- tbl %@% "groups"

  if (dplyr::is_grouped_df(tbl)) {
    cls <- "grouped_df"
    tbl <- new_pibble(tbl,
      groups = grp_data,
      .i = .i,
      .t = .t,
      .d = .d,
      class = cls
    )
  } else {
    tbl <- new_pibble(tbl,
      .i = .i,
      .d = .d,
      .t = .t,
      groups = NULL
    )
  }

  return(tbl)
}

check_panel_inputs <- function(.df, .i, .t, .d, .uniqcheck) {
  #### CHECK INPUTS
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df", "list")) == 0) {
    stop("Requires data to be a data frame, tibble, pibble, or list.")
  }
  if (sum(class(.df) %in% c("data.table", "list")) > 0) {
    warning("data.tables and lists will be coerced to pibble.")
    .df <- as.data.frame(.df)
  }
  if (!(max(is.character(.i))) & min(is.na(.i)) == 0) {
    stop("Internal issue: .i should have been converted to a character variable with variable names by this point. Please report errors on https://github.com/NickCH-K/pmdplyr")
  }
  if (!(is.character(.t)) & !is.na(.t)) {
    stop("Internal issue: .t should have been converted to character variable with variable names by this point. Please report errors on https://github.com/NickCH-K/pmdplyr")
  }
  if (length(.t) > 1) {
    stop("Only one time variable allowed.")
  }
  if (!(is.numeric(.d)) & !(is.na(.d))) {
    stop(".d must be numeric.")
  }
  if (min(is.na(.i)) == 0 & min(.i %in% names(.df)) == 0) {
    stop("Elements of .i must be variables present in the data.")
  }
  if (!is.na(.t) & min(.t %in% names(.df)) == 0) {
    stop(".t must be a variable present in the data.")
  }
  if (!is.na(.uniqcheck) & !is.logical(.uniqcheck)) {
    stop(".uniqcheck must be TRUE or FALSE.")
  }
  if (!is.na(.d) & !is.na(.t)) {
    if (.d > 0 & !is.numeric(.df[[.t]])) {
      stop("Unless .d = 0, indicating an ordinal time variable, .t must be numeric.")
    }
  }

  #### Warn about multiple obs per id/t, but only once per session
  if (getOption("pibble.warning4.0", TRUE) | .uniqcheck == TRUE) {
    # Check for uniqueness
    groupvec <- c(.i, .t)
    groupvec <- groupvec[!is.na(groupvec)]
    if (anyDuplicated(.df[, groupvec]) > 0) {
      message("Note that the selected .i and .t do not uniquely identify observations in the data.
This message will be displayed only once per session unless the .uniqcheck option is set to TRUE.")
      options("pibble.warning4.0" = FALSE)
    }
  }
}

#' Check whether an object has been declared as panel data
#'
#' Checks whether a data set (\code{data.frame} or \code{tibble}) has been assigned panel identifiers in the \code{pmdplyr} format. If so, returns those identifiers.
#'
#' @param .df Data frame or tibble
#' @param .silent Set to TRUE to suppress output reporting what the panel identifiers are. Defaults to FALSE
#' @examples
#'
#' data(Scorecard)
#' Scorecard <- as_pibble(Scorecard, .i = "unitid", .t = "year")
#' is_pibble(Scorecard)
#' @export

is_pibble <- function(.df, .silent = FALSE) {
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df")) == 0) {
    stop("Requires data to be a data frame or tibble.")
  }
  if (!is.logical(.silent)) {
    stop("silent must be TRUE or FALSE.")
  }

  i <- ifelse(is.null(.df %@% ".i"), NA, paste0(.df %@% ".i", collapse = ", "))
  t <- ifelse(is.null(.df %@% ".t"), NA, .df %@% ".t")
  d <- ifelse(is.null(.df %@% ".d"), NA, .df %@% ".d")

  if ((!is.na(i) | !is.na(t)) & "tbl_pb" %in% class(.df)) {
    if (.silent == FALSE) {
      message(paste(".i = ", i, "; .t = ", t, "; .d = ", d, ".", sep = ""))
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}

##### declare_in_fcn_check takes .i and .t and strings
declare_in_fcn_check <- function(.df, .i, .t, .d, .uniqcheck, .setpanel, .noneed = FALSE) {
  # Check inputs
  if (!is.na(.uniqcheck) & !is.logical(.uniqcheck)) {
    stop("uniqcheck must be TRUE or FALSE.")
  }
  if (!is.na(.setpanel) & !is.logical(.setpanel)) {
    stop("setpanel must be TRUE or FALSE.")
  }

  # Collect original panel settings, if any.
  # To be consistent with other input checking, make them NA not NULL if appropriate
  orig_i <- ifelse(is.null(.df %@% ".i"), NA, .df %@% ".i")
  orig_t <- ifelse(is.null(.df %@% ".t"), NA, .df %@% ".t")
  orig_d <- ifelse(is.null(.df %@% ".d"), NA, .df %@% ".d")
  is_tbl_pb <- is_pibble(.df, .silent = TRUE)

  # If uniqcheck is TRUE but panel is not being reset, run through check_panel_inputs
  # just to check, using already-set panel info
  if (min(is.na(.i)) > 0 & is.na(.t) & .uniqcheck == TRUE) {
    check_panel_inputs(.df, .i = orig_i, .t = orig_t, .d = orig_d, .uniqcheck = TRUE)
  }

  # If nothing was declared, use the original values
  if (min(is.na(.i)) > 0 & is.na(.t)) {
    .i <- orig_i
    .t <- orig_t
    .d <- orig_d
  }

  # If everything is still missing and you need something, error
  if (min(is.na(.i)) > 0 & is.na(.t) & .noneed == FALSE) {
    stop("Attempt to use panel indicators i and/or t, but no i or t are declared in command or stored in data.")
  }


  return(list(
    orig_i = orig_i,
    orig_t = orig_t,
    orig_d = orig_d,
    i = .i,
    t = .t,
    d = .d,
    is_tbl_pb = is_tbl_pb
  ))
}
