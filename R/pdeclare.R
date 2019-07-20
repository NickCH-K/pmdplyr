#' pdeclare
#'
#' WARNING: FOR NOW, BE AWARE THAT AFTER USING PDECLARE, MOST FUNCTIONS WILL NOT PRESERVE THE ATTRIBUTES.
#'
#' This function declares pdeclare tibble with three attributes:
#'
#' \itemize{
#'   \item \code{.i}, a character or character vector indicating the variables that constitute the individual-level panel identifier
#'   \item \code{.t}, a character vector indicating the time variable
#'   \item \code{.d}, a number indicating the gap
#' }
#'
#' The \code{pdeclare()} function is for the purpose of creating \code{pdeclare} objects from scratch. You probably want \code{as_pdeclare}.
#'
#' Note that pdeclare does not require that \code{.i} and \code{.t} uniquely identify the observations in your data, but it will give a warning message (a maximum of once per session, unless \code{.uniqcheck=TRUE}) if they do not.
#'
#' @param .df Data frame or tibble to declare as a panel.
#' @param .i Character or character vector with the variable names that identify the individual cases. If this is omitted, \code{pdeclare} will assume the data set is a single time series.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to perform a check of whether \code{.i} and \code{.t} uniquely identify observations, and present a message if not. By default this is set to FALSE and the warning message occurs only once per session.
#' @name pdeclare
#'
#' @examples
#' #Creating a pdeclare from scratch
#' pd <- pdeclare(i = c(1,1,1,2,2,2),
#'                t = c(1,2,3,1,2,2),
#'                x = rnorm(6),
#'                .i = 'i',
#'                .t = 't')
#' is_pdeclare(pd)
#' #I set .d=0 here to indicate that I don't care how large the gap between one period and the next is
#' #If I want to use 'seconds' for t.
#' #See time_variable() to turn unruly variables into well-behaved integers, as well
#' pd2 <- pdeclare(i = c(1,1,1,2,2,2),
#'                seconds = c(123,456,789,103,234,238),
#'                .i = '.i',
#'                .t = 'seconds',
#'                .d = 0)
#' is_pdeclare(pd2)
#'
#'
NULL
#' @export


pdeclare <- function(..., .i = NA, .t = NA, .d = 1, .uniqcheck = FALSE) {

  #Create tibble
  tbl <- tibble::tibble(...)

  #check inputs
  check_panel_inputs(tbl,
                     .i = .i,
                     .t = .t,
                     .d = .d,
                     .uniqcheck = .uniqcheck)

  #make a pdeclare
  tbl <- build_pdeclare(tbl,.i=.i,.t=.t,.d=.d,.uniqcheck=.uniqcheck)

  return(tbl)
}

new_pdeclare <- function(x, ...,class=NULL) {

  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  x <- tibble::new_tibble(x, ..., nrow = nrow(x), class = c('tbl_pd',class))

  return(x)
}

#' @importFrom vctrs vec_restore
#' @method vec_restore tbl_pd
vec_restore.tbl_pd <- function(x, to) {
  .i <- x %@% ".i"
  .t <- x %@% ".t"
  .d <- x %@% ".d"

  return(build_pdeclare(to, .i = .i, .t = .t, .d = .d))
}


#' Coerce to a pdeclare tibble
#'
#' WARNING: FOR NOW, BE AWARE THAT AFTER USING AS_PDECLARE, MOST FUNCTIONS WILL NOT PRESERVE THE ATTRIBUTES.
#'
#' This function coerces a tibble, data.frame, or list to a pdeclare tibble by adding three attributes to it:
#'
#' \itemize{
#'   \item \code{.i}, a character or character vector indicating the variables that constitute the individual-level panel identifier
#'   \item \code{.t}, a character vector indicating the time variable
#'   \item \code{.d}, a number indicating the gap
#' }
#'
#' Note that pdeclare does not require that \code{.i} and \code{.t} uniquely identify the observations in your data, but it will give a warning message (a maximum of once per session, unless \code{.uniqcheck=TRUE}) if they do not.
#'
#' @param x A data frame, tibble or list
#' @inheritParams pdeclare
#' @param ... Other arguments passed on to individual methods.
#' @examples
#' data(SPrail)
#' #I set .d=0 here to indicate that I don't care how large the gap
#' #between one period and the next is.
#' #If I want to use 'insert_date' for .t with a fixed gap between periods,
#' #I need to transform it into an integer first; see time_variable()
#' SP <- as_pdeclare(SPrail,
#'                .i = c('origin', 'destination'),
#'                .t = 'insert_date',
#'                .d = 0)
#' is_pdeclare(SP)
#' attr(SP,'.i')
#' attr(SP,'.t')
#' attr(SP,'.d')
#'
#' data(Scorecard)
#' #Here, year is an integer, so I can use it with .d = 1 to
#' #indicate that one period is a change of one unit in year
#' #Conveniently, .d = 1 is the default
#' Scorecard <- as_pdeclare(Scorecard,.i='unitid',.t='year')
#' is_pdeclare(Scorecard)
#'
#' @rdname as_pdeclare
#' @export
as_pdeclare <- function(x,
                        .i = NA,
                        .t = NA,
                        .d = 1,
                        .uniqcheck = FALSE,
                        ...) {
  UseMethod("as_pdeclare")
}

#' @rdname as_pdeclare
#' @export
as_pdeclare.tbl_df <- function(x,
                               .i = NA,
                               .t = NA,
                               .d = 1,
                               .uniqcheck = FALSE,
                               ...) {

  #check inputs
  check_panel_inputs(x,
                     .i = .i,
                     .t = .t,
                     .d = .d,
                     .uniqcheck = .uniqcheck)

  return(build_pdeclare(x, .i = .i, .d = .d, .t = .t, .uniqcheck = .uniqcheck, ...))
}

#' @rdname as_pdeclare
#' @export
as_pdeclare.grouped_df <- as_pdeclare.tbl_df

#' @rdname as_pdeclare
#' @export
as_pdeclare.data.frame <- as_pdeclare.tbl_df

#' @rdname as_pdeclare
#' @export
as_pdeclare.list <- as_pdeclare.tbl_df

#' @keywords internal
#' @export
as_pdeclare.NULL <- function(x, ...) {
  abort("A pdeclare must not be NULL.")
}

#' Low-level constructor for a pdeclare object
#'
#' `build_pdeclare()` creates a `tbl_pd` object with more controls. It is useful
#' for creating a `tbl_pd` internally inside a function.
#'
#' Note that, for speed, `build_pdeclare()` does not check the adequacy of the inputs.
#'
#' @export
#' @inheritParams pdeclare
#' @importFrom rlang %@%
build_pdeclare <- function(tbl,
                           .i = NA,
                           .t = NA,
                           .d = 1,
                           .uniqcheck = FALSE){

  grp_data <- tbl %@% "groups"

  if (dplyr::is_grouped_df(tbl)) {
    cls <- 'grouped_df'
    tbl <- new_pdeclare(tbl,
                        groups = grp_data,
                        .i = .i,
                        .t = .t,
                        .d = .d,
                        class = cls)
  } else {
    tbl <- new_pdeclare(tbl,
                        .i = .i,
                        .d = .d,
                        .t = .t,
                        groups = NULL)
  }

  return(tbl)
}

check_panel_inputs <- function(.df,.i,.t,.d,.uniqcheck) {
  ####CHECK INPUTS
  if (sum(class(.df) %in% c('data.frame','tbl','tbl_df','list')) == 0) {
    stop('Requires data to be a data frame, tibble, or list.')
  }
  if (sum(class(.df) %in% c('data.table','list')) > 0) {
    warning('data.tables and lists will be coerced to pdeclare tibbles.')
    .df <- as.data.frame(.df)
  }
  if (!(max(is.character(.i))) & min(is.na(.i)) == 0) {
    stop('.i must be a character variable or a character vector.')
  }
  if (!(is.character(.t)) & !is.na(.t)) {
    stop('.t must be a character variable.')
  }
  if (length(.t)>1) {
    stop('Only one time variable allowed.')
  }
  if (!(is.numeric(.d)) & !(is.na(.d))) {
    stop('.d must be numeric.')
  }
  if (min(is.na(.i)) == 0 & min(.i %in% names(.df)) == 0) {
    stop('Elements of .i must be variables present in the data.')
  }
  if (!is.na(.t) & min(.t %in% names(.df)) == 0) {
    stop('.t must be a variable present in the data.')
  }
  if (!is.na(.uniqcheck) & !is.logical(.uniqcheck)) {
    stop('.uniqcheck must be TRUE or FALSE.')
  }
  if (!is.na(.d) & !is.na(.t)) {
    if (.d > 0 & !is.numeric(.df[[.t]])) {
      stop('Unless .d = 0, indicating an ordinal time variable, .t must be numeric.')
    }
  }

  #### Warn about multiple obs per id/t, but only once per session
  if (getOption("pdeclare.warning4.0",TRUE) | .uniqcheck == TRUE) {
    # Check for uniqueness
    groupvec <- c(.i,.t)
    groupvec <- groupvec[!is.na(groupvec)]
    if (anyDuplicated(.df[,groupvec]) > 0) {
      message('Note that the selected .i and .t do not uniquely identify observations in the data.\nThis message will be displayed only once per session unless the .uniqcheck option is set to TRUE.')
      options("pdeclare.warning4.0"=FALSE)
    }
  }
}

#' Function to check whether an object has been declared as panel data
#'
#' Checks whether a data set (\code{data.frame} or \code{tibble}) has been assigned panel identifiers in the \code{pmdplyr} format. If so, returns those identifiers.
#'
#' @param .df Data frame or tibble
#' @param .silent Set to TRUE to suppress output reporting what the panel identifiers are. Defaults to FALSE
#' @examples
#'
#' data(Scorecard)
#' Scorecard <- as_pdeclare(Scorecard,.i='unitid',.t='year')
#' is_pdeclare(Scorecard)
#'
#' @export

is_pdeclare <- function(.df,.silent=FALSE) {
  if (sum(class(.df) %in% c('data.frame','tbl','tbl_df')) == 0) {
    stop('Requires data to be a data frame or tibble.')
  }
  if (!is.logical(.silent)) {
    stop('silent must be TRUE or FALSE.')
  }

  i <- ifelse(is.null(.df %@% '.i'),NA,paste0(.df %@% '.i',collapse=', '))
  t <- ifelse(is.null(.df %@% '.t'),NA,.df %@% '.t')
  d <- ifelse(is.null(.df %@% '.d'),NA,.df %@% '.d')

  if ((!is.na(i) | !is.na(t)) & 'tbl_pd' %in% class(.df)) {
    if (.silent == FALSE) {
      message(paste('.i = ',i,'; .t = ',t,'; .d = ',d,'.',sep=''))
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}


declare_in_fcn_check <- function(.df,.i,.t,.d,.uniqcheck,.setpanel,.noneed=FALSE) {
  #Check inputs
  if (!is.na(.uniqcheck) & !is.logical(.uniqcheck)) {
    stop('uniqcheck must be TRUE or FALSE.')
  }
  if (!is.na(.setpanel) & !is.logical(.setpanel)) {
    stop('setpanel must be TRUE or FALSE.')
  }

  #Collect original panel settings, if any.
  #To be consistent with other input checking, make them NA not NULL if appropriate
  orig_i <- ifelse(is.null(.df %@% '.i'),NA,.df %@% '.i')
  orig_t <- ifelse(is.null(.df %@% '.t'),NA,.df %@% '.t')
  orig_d <- ifelse(is.null(.df %@% '.d'),NA,.df %@% '.d')

  #If uniqcheck is TRUE but panel is not being reset, run through check_panel_inputs
  #just to check, using already-set panel info
  if (min(is.na(.i)) > 0 & is.na(.t) & .uniqcheck == TRUE) {
    check_panel_inputs(.df,.i=orig_i,.t=orig_t,.d=orig_d,.uniqcheck=TRUE)
  }

  #If nothing was declared, use the original values
  if (min(is.na(.i)) > 0 & is.na(.t)) {
    .i <- orig_i
    .t <- orig_t
    .d <- orig_d
  }

  #If everything is still missing and you need something, error
  if (min(is.na(.i)) > 0 & is.na(.t) & .noneed == FALSE) {
    stop('Attempt to use panel indicators i and/or t, but no i or t are declared in command or stored in data.')
  }


  return(list(
    orig_i=orig_i,
    orig_t=orig_t,
    orig_d=orig_d,
    i=.i,
    t=.t,
    d=.d
  ))
}
