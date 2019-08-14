#' Perform standard panel-data calculations
#'
#' These functions perform the standard between and within transformations on panel data.
#'
#' These functions do not take a \code{.d} argument because it is irrelevant here.
#'
#' @param .var Vector to be transformed
#' @param .df Data frame, pibble, or tibble (usually the data frame or tibble that contains \code{.var}) which contains the panel structure variables either listed in \code{.i} and \code{.t}, or earlier declared with \code{as_pibble()}. If \code{tlag} is called inside of a \code{dplyr} verb, this can be omitted and the data will be picked up automatically.
#' @param .fcn The function to be passed to \code{dplyr::summarize()}. \code{x - .fcn(x)} within \code{.i} is the within tranformation. \code{.fcn(x)} within \code{.i} minus \code{.fcn} overall is the between transformation. This will almost always be the default \code{.fcn = function(x) mean(x,na.rm=TRUE)}.
#' @param .i Quoted or unquoted variable(s) that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pibble()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Quoted or unquoted variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @examples
#'
#' data(SPrail)
#' # Calculate within- and between-route variation in price and add it to the data
#' SPrail <- SPrail %>%
#'   dplyr::mutate(
#'     within_route = within_i(price, .i = c(origin, destination)),
#'     between_route = between_i(price, .i = c(origin, destination))
#'   )
#' @name panel_calculations
NULL

#' @rdname panel_calculations
#' @export
within_i <- function(.var, .df = get(".", envir = parent.frame()), .fcn = function(x) mean(x, na.rm = TRUE), .i = NULL, .t = NULL, .uniqcheck = FALSE) {
  if (!is.vector(.var)) {
    stop(".var must be a vector.")
  }
  if (!is.function(.fcn)) {
    stop(".fcn must be a function.")
  }

  # It's so weird this is necessary
  .df <- .df


  # Pull out variable names
  .icall <- tidyselect::vars_select(names(.df), {{ .i }})
  if (length(.icall) == 0) {
    .icall <- NA_character_
  }
  .tcall <- tidyselect::vars_select(names(.df), {{ .t }})
  if (length(.tcall) == 0) {
    .tcall <- NA_character_
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .i = .icall, .t = .tcall, .d = NA, .uniqcheck, .setpanel = FALSE)
  if (max(is.na(inp$i)) == 1) {
    stop("within_i() requires that .i be declared either in the function or by as_pibble().")
  }

  # Convert to tibble so as to avoid dropping pibble status
  .df <- dplyr::as_tibble(.df)

  # Figure out if we're working with grouped data and so length(var) < nrow(.df)
  # If we are, switch everything over to .data
  if (length(.var) < nrow(.df)) {
    # Pull out the .data
    datapro <- get(".data", envir = parent.frame())

    .df <- dplyr::as_tibble(data.frame((lapply(inp$i, function(x) datapro[[x]]))))
    names(.df) <- inp$i

    if (length(.var) != nrow(.df)) {
      stop("Length of variable does not match length of full data set or group-subsample.")
    }
  } else {
    # If we're not working with grouped data, we're good.
    # We only need these
    .df <- .df %>% dplyr::select_at(inp$i)
  }

  # Figure out longest variable name and expand it so we don't overwrite names
  varname <- uniqname(.df)

  # Calculate within transformation
  return(.df %>%
    dplyr::mutate(!!varname := .var) %>%
    dplyr::group_by_at(inp$i) %>%
    dplyr::mutate(!!varname := .data[[varname]] - .fcn(.data[[varname]])) %>%
    dplyr::ungroup() %>%
    dplyr::pull(!!varname))
}

#' @rdname panel_calculations
#' @export
between_i <- function(.var, .df = get(".", envir = parent.frame()), .fcn = function(x) mean(x, na.rm = TRUE), .i = NULL, .t = NULL, .uniqcheck = FALSE) {
  if (!is.vector(.var)) {
    stop(".var must be a vector.")
  }
  if (!is.function(.fcn)) {
    stop(".fcn must be a function.")
  }

  # ugh
  .df <- .df

  # Pull out variable names
  .icall <- tidyselect::vars_select(names(.df), {{ .i }})
  if (length(.icall) == 0) {
    .icall <- NA_character_
  }
  .tcall <- tidyselect::vars_select(names(.df), {{ .t }})
  if (length(.tcall) == 0) {
    .tcall <- NA_character_
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .i = .icall, .t = .tcall, .d = NA, .uniqcheck, .setpanel = FALSE)
  if (max(is.na(inp$i)) == 1) {
    stop("between_i() requires that .i be declared either in the function or by as_pibble().")
  }

  # Convert to tibble so as to avoid dropping pibble status
  .df <- dplyr::as_tibble(.df)

  # Figure out if we're working with grouped data and so length(var) < nrow(.df)
  # If we are, switch everything over to .data
  if (length(.var) < nrow(.df)) {
    # Pull out the .data
    datapro <- get(".data", envir = parent.frame())

    .df <- dplyr::as_tibble(data.frame(lapply(inp$i, function(x) datapro[[x]])))
    names(.df) <- inp$i

    if (length(.var) != nrow(.df)) {
      stop("Length of variable does not match length of full data set or group-subsample.")
    }
  } else {
    # If we're not working with grouped data, we're good.
    # We only need these
    .df <- .df %>% dplyr::select_at(inp$i)
  }


  # Figure out longest variable name and expand it so we don't overwrite names
  varname <- uniqname(.df)
  gm <- paste(varname, ".1", sep = "")

  # Calculate between transformation
  return(.df %>%
    dplyr::mutate(!!varname := .var) %>%
    dplyr::mutate(!!gm := .fcn(.[[varname]])) %>%
    dplyr::group_by_at(inp$i) %>%
    dplyr::mutate(!!varname := .fcn(.data[[varname]] - .data[[gm]])) %>%
    dplyr::ungroup() %>%
    dplyr::pull(!!varname))
}
