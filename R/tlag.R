#' Function to retrieve time-lagged data
#'
#' This function retrieves the time-lagged values of a variable, using the time variable defined in \code{.t} in the function or by \code{as_pibble()}. \code{tlag()} is highly unusual among time-lag functions in that it is usable even if observations are not uniquely identified by \code{.t} (and \code{.i}, if defined).
#'
#' @param .var Unquoted variable from \code{.df} to be lagged.
#' @param .df Ungrouped data frame or tibble (usually the data frame or tibble that contains \code{.var}) which contains the panel structure variables either listed in \code{.i} and \code{.t}, or earlier declared with \code{as_pibble()}. If \code{tlag} is called inside of a \code{dplyr} verb, this can be omitted and the data will be picked up automatically.
#' @param .n Number of periods to lag by. 1 by default. Note that this is automatically scaled by \code{.d}. If \code{.d = 2} and \code{.n = 1}, then the lag of \code{.t = 3} will be \code{.t = 1}. Allows negative values, equivalent to \code{tlead()} with the same value but positive. Note that \code{.n} is ignored if \code{.d=0}.
#' @param .default Fill-in value used when lagged observation is not present. Defaults to NA.
#' @param .quick If \code{.i} and \code{.t} uniquely identify observations in your data, **and** there either \code{.d = 0} or there are no time gaps for any individuals (perhaps use \code{panel_fill()} first), set \code{.quick = TRUE} to improve speed. \code{tlag()} will not check if either of these things are true (except unique identification, which will be checked if \code{.uniqcheck = 1} or if \code{.i} or \code{.t} are specified in-function), so make sure they are or you will get strange results.
#' @param .resolve If there is more than one observation per individal/period, and the value of \code{.var} is identical for all of them, that's no problem. But what should \code{tlag()} do if they're not identical? Set \code{.resolve = 'error'} (or, really, any string) to throw an error in this circumstance. Or, set \code{.resolve} to a function (ideally, a vectorized one) that can be used within \code{dplyr::summarize()} to select a single value per individual/period. For example, \code{.resolve = mean} to get the mean value of all observations present for that individual/period.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{tlag()} will group the data by \code{.i}, ignoring any grouping already implemented. Set \code{.group_i = FALSE} to avoid this.
#' @param .i Quoted or unquotes variable(s) that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pibble()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Quoted or unquoted vairable indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @examples
#'
#' data(Scorecard)
#'
#' # The Scorecard data is uniquely identified by unitid and year.
#' # However, there are sometimes gaps between years.
#' # In cases like this, using dplyr::lag() will still use the row before,
#' # whereas tlag() will respect the gap and give a NA, much like plm::lag()
#' # (although tlag is slower than either, sorry)
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(pmdplyr_tlag = tlag(earnings_med,
#'                                     .i = unitid,
#'                                     .t = year))
#' Scorecard <- Scorecard %>%
#'   dplyr::arrange(year) %>%
#'   dplyr::group_by(unitid) %>%
#'   dplyr::mutate(dplyr_lag = dplyr::lag(earnings_med)) %>%
#'   dplyr::ungroup()
#'
#' # more NAs in the pmdplyr version - observations with a gap and thus no real lag present in data
#' sum(is.na(Scorecard$pmdplyr_tlag))
#' sum(is.na(Scorecard$dplyr_lag))
#'
#' # If we want to ignore gaps, or have .d = 0, and .i and .t uniquely identify observations,
#' # we can use the .quick option to match dplyr::lag()
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(pmdplyr_quick_tlag = tlag(earnings_med,
#'                                          .i = unitid,
#'                                          .t = year,
#'                                          .d = 0,
#'                                          .quick = TRUE))
#' sum(Scorecard$dplyr_lag != Scorecard$pmdplyr_quick_tlag, na.rm = TRUE)
#'
#' # Where tlag shines is when you have multiple observations per .i/.t
#' # If the value of .var is constant within .i/.t, it will work just as you expect.
#' # If it's not, it will throw an error, or you can set
#' # .resolve to tell tlag how to select a single value from the many
#' # Maybe we want to get the lagged average earnings within degree award type
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(
#'     last_year_earnings_by_category =
#'       tlag(earnings_med,
#'         .i = pred_degree_awarded_ipeds, .t = year,
#'         .resolve = function(x) mean(x, na.rm = TRUE)
#'       )
#'   )
#' # Or maybe I want the lagged earnings across all types - .i isn't necessary!
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(last_year_earnings_all = tlag(earnings_med,
#'     .t = "year",
#'     .resolve = function(x) mean(x, na.rm = TRUE)
#'   ))
#' # Curious why the first nonmissing obs show up in 2012?
#' # It's because there's no 2008 or 2010 in the data, so when 2009 or 2011 look back
#' # a year, they find nothing!
#' # We could get around this by setting .d = 0 to ignore gap length
#' # Note this can be a little slow.
#' Scorecard <- Scorecard %>%
#'    dplyr::mutate(last_year_earnings_all = tlag(earnings_med,
#'     .t = year, .d = 0,
#'     .resolve = function(x) mean(x, na.rm = TRUE)
#'    ))
#' @export

# WHEN CHANGING THIS ONE BE SURE TO CHANGE THE MUTATE_CASCADE EXAMPLE WHICH
#INCLUDES IT

tlag <- function(.var, .df = get(".", envir = parent.frame()), .n = 1, .default = NA, .quick = FALSE, .resolve = "error", .group_i = TRUE, .i = NULL, .t = NULL, .d = NA, .uniqcheck = FALSE) {
  if (!is.numeric(.n) | length(.n) > 1) { stop(".n must be a single integer.") }
  if (!(as.integer(.n) == .n)) { stop(".n must be an integer.") }
  if (!is.character(.resolve) & !is.function(.resolve)) { stop(".resolve must be a function.") }
  if (!is.logical(.group_i)) { stop(".group_i must be TRUE or FALSE") }
  if (!is.logical(.quick)) { stop(".quick must be TRUE or FALSE") }
  if (length(.default) > 1) { stop(".default must be a single value.") }

  # ugh
  .df <- .df

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- origgroups[1:(length(origgroups) - 1)]
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  # Pull out variable names
  .icall <- tidyselect::vars_select(names(.df),{{.i}})
  if (length(.icall) == 0) {
    .icall <- NA_character_
  }
  .tcall <- tidyselect::vars_select(names(.df),{{.t}})
  if (length(.tcall) == 0) {
    .tcall <- NA_character_
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .icall, .tcall, .d, .uniqcheck, .setpanel = FALSE)
  if (is.na(inp$t)) { stop("tlag() requires that .t be declared either in the function or by as_pibble().") }

  # If changes have been made, fill in default .d
  if ((min(is.na(.icall)) == 0 | !is.na(.tcall)) & is.na(.d)) {
    inp$d <- 1
  }

  arrnames <- c(inp$i, inp$t)
  if (.group_i == FALSE) {
    arrnames <- inp$t
  }
  arrnames <- arrnames[!is.na(arrnames)]

  #Figure out if we're working with grouped data and so length(var) < nrow(.df)
  #If we are, switch everything over to .data
  if (length(.var) < nrow(.df)) {
    #Pull out the .data
    datapro <- get('.data',envir=parent.frame())

    .df <- data.frame(lapply(arrnames, function(x) datapro[[x]]))
    names(.df) <- arrnames

    if (length(.var) != nrow(.df)) { stop('Length of variable does not match length of full data set or group-subsample.') }
  } else {
    # If we're not working with grouped data, we're good.
    # We only need these
    .df <- .df %>% dplyr::select_at(arrnames)
  }

  if (.quick == TRUE) {
    dat <- dplyr::bind_cols(
      .df,
      data.frame(.var)
    )
    varname <- names(dat)[ncol(dat)]
    dat[, ncol(dat) + 1] <- 1:nrow(dat)
    origorder <- names(dat)[ncol(dat)]

    return((dat %>%
      dplyr::arrange_at(arrnames) %>%
      dplyr::mutate_at(varname, .funs = function(x) dplyr::lag(x, n = .n, default = .default)) %>%
      dplyr::arrange_at(origorder))[[varname]])
  }

  # Create lookup table.
  lookup <- .df
  lookup <- dplyr::bind_cols(
    lookup,
    data.frame(.var)
  )
  varname <- names(lookup)[ncol(lookup)]
  # Do we need to group it?
  if (!setequal(origgroups, arrnames)) {
    lookup <- lookup %>%
      dplyr::group_by_at(arrnames)
  }
  # Check if there's uniformity, if .resolve = 'error'
  if (is.character(.resolve)) {
    if ((lookup %>%
             #only need to check for uniformity if there's more than one obs
             dplyr::filter_at(varname, dplyr::any_vars(dplyr::n() > 1)) %>%
             #check for uniformity
             #drop duplicates
             dplyr::distinct() %>%
             #go to just arrnames
             dplyr::select(arrnames) %>%
             #if there are any duplicated arrnames, that means there were multiple vals within 'em
             anyDuplicated()) > 0) {
      stop("Values are not consistent within (.i, if specified, and) .t. See .resolve option.")
    }
    .resolve <- dplyr::first
  }

  # And turn into the lookup table, with adjusted time variable for linking
  # Since we're matching time periods, do this differently by whether .d is 0 or not
  if (inp$d > 0) {
    lookup <- lookup %>%
      dplyr::summarize_at(varname, .resolve) %>%
      dplyr::mutate_at(inp$t, .funs = function(x) x + .n * inp$d)
  } else {
    #Flatten with resolve
    lookup <- lookup %>%
      dplyr::summarize_at(varname, .resolve)

    #Put grouping to i level for lag
    if (.group_i & !is.na(inp$i)) {
      lookup <- lookup %>% dplyr::group_by_at(inp$i)
    } else {
      lookup <- lookup %>% dplyr::ungroup()
    }

    lookup <- lookup %>%
      dplyr::arrange_at(arrnames) %>%
      dplyr::mutate_at(inp$t, .funs = dplyr::lead)
  }

  lookup[, ncol(lookup) + 1] <- 1
  foundmatch <- names(lookup)[ncol(lookup)]



  # Do the linkup and return the lagged value
  .df <- .df %>%
    dplyr::left_join(lookup, by = arrnames)

  if (!is.na(.default)) {
    .df <- .df %>% dplyr::mutate_at(varname, .funs = function(x)
      ifelse(is.na(.df[[foundmatch]]), .default, .df[[varname]]))
  }

  #Failure to look up is NA, not NaN
  .df <- .df %>%
    dplyr::mutate_at(varname, .funs = function(x)
      ifelse(is.nan(x),NA,x))

  return(.df[[varname]])
}
