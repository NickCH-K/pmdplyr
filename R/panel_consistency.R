#' Function to fill in gaps in panel data
#'
#' WARNING: FOR NOW, ALWAYS SPECIFY THE PANEL STRUCTURE EVERY TIME YOU CALL THIS FUNCTION. PDECLARE STATUS IS DROPPED BY MOST FUNCTIONS.
#'
#' This function creates new observations to fill in any gaps in panel data. For example, if individual 1 has an observation in periods t = 1 and t = 3 but no others, this function will create an observation for t = 2. By default, the t = 2 observation will be identical to the t = 1 observation except for the time variable, but this can be adjusted. This function returns data sorted by \code{.i} and \code{.t}.
#'
#' Note that, in the case where there is more than one observation for a given individual/time period (or just time period if \code{.group_i = FALSE}), \code{panel_fill()} will create copies of *every observation* in the appropriate individual/time period for filling-in purposes. So if there are four t = 1 observations and nothing in t = 2, \code{panel_fill()} will create four new observations with t = 2, copying the original four in t = 1.
#'
#' By default, the \code{panel_fill()} operation is grouped by \code{.i}, although it will retun the data in the original grouping structure. Leave \code{.i} blank, or, if \code{.i} is already in the data from \code{as_pdeclare}, set \code{.group_i=FALSE} to run the function ungrouped, or with the existing group structure.
#'
#' This function requires \code{.t} and \code{.d} to be declared in the function or already established in the data by \code{as_pdeclare()}. Also, this requires a cardinal \code{.t}. It must not be the case that \code{.d=0}.
#'
#' @param .df Tibble or data frame which either has the \code{.t} and \code{.d} (and perhaps \code{.i}) attributes included by \code{as_pdeclare()}, or the appropriate panel structure is declared in the function.
#' @param .set_NA Should values in newly-created observations be set to adjacent values or to NA? Set to \code{TRUE} to set all new values to NA except for .i and .t. To make only specific variables NA, list them as a character vector. Defaults to FALSE; all values are filled in using the most recently available data.
#' @param .min Sets the first time period in the data for each individual to be \code{.min}, and fills in gaps between period \code{.min} and the actual start of the data. Copies data from the first period present in the data for each individual (if grouped). Handy for creating balanced panels.
#' @param .max Sets the last time period in the data for each individual to be \code{.max}, and fills in gaps between period \code{.max} and the actual start of the data. Copies data from the flast period present in the data for each individual (if grouped). Handy for creating balanced panels.
#' @param .backwards By default, values of newly-created observations are copied from the most recently available period. Set \code{.backwards = TRUE} to instead copy values from the closest *following* period.
#' @param .group_i By default, \code{panel_fill()} will fill in gaps within values of \code{.i}. If \code{.i} is missing, it won't do that. If \code{.i} is in the data and you still don't want \code{panel_fill()} to run within \code{.i}, set \code{.group_i = FALSE}.
#' @param .flag The name of a new variable indicating which observations are newly created by \code{panel_fill()}.
#' @param .i Character or character vector with the variable names that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pdeclare()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @param .setpanel Logical parameter. Set to FALSE to return data with the same \code{.i}, \code{.t}, \code{.d} attributes it came in with, even if those are null. TRUE by default, but ignored if \code{.i}, \code{.t}, and \code{.d} are all NA.
#' @examples
#'
#' # Examples too slow to run
#' if (interactive()) {
#'   data(Scorecard)
#'   # Notice that, in the Scorecard data, the gap between one year and the next is not always constant
#'   table((Scorecard %>% dplyr::arrange(year) %>%
#'     dplyr::group_by(unitid) %>%
#'     dplyr::mutate(diff = year - dplyr::lag(year)))$diff)
#'   # And also that not all universities show up for the first or last times in the same year
#'   year_range <- Scorecard %>%
#'     dplyr::group_by(unitid) %>%
#'     dplyr::summarize(first_year = min(year), last_year = max(year))
#'   table(year_range$first_year)
#'   table(year_range$last_year)
#'   rm(year_range)
#'
#'   # We can deal with the inconsistent-gaps problem by creating new obs to fill in
#'   # this version will fill in the new obs with the most recently observed data, and flag them
#'   Scorecard_filled <- panel_fill(Scorecard, .i = "unitid", .t = "year", .flag = "new")
#'
#'   # Or maybe we want those observations in there but don't want to treat them as real data
#'   # so instead of filling them in, just leave all the data in the new obs blank
#'   # (note this sets EVERYTHING not in .i or .t to NA - if you only want some variables NA,
#'   # make .set_NA a character vector of those variable names)
#'   Scorecard_filled <- panel_fill(Scorecard, .i = "unitid", .t = "year", .flag = "new", .set_NA = TRUE)
#'
#'   # Perhaps we want a perfectly balanced panel. So let's set .max and .min to the start and end
#'   # of the data, and it will fill in everything.
#'   Scorecard_filled <- panel_fill(Scorecard,
#'     .i = "unitid", .t = "year", .flag = "new",
#'     .min = min(Scorecard$year), .max = max(Scorecard$year)
#'   )
#'   # how many obs of each college? Should be identical, and equal to the number of years there are
#'   table(table(Scorecard_filled$unitid))
#'   length(unique(Scorecard_filled$year))
#' }
#' @export

panel_fill <- function(.df, .set_NA = FALSE, .min = NA, .max = NA, .backwards = FALSE, .group_i = TRUE, .flag = NA, .i = NA, .t = NA, .d = 1, .uniqcheck = FALSE, .setpanel = TRUE) {
  if (!is.logical(.backwards)) {
    stop(".backwards must be TRUE or FALSE")
  }
  if (!(is.logical(.set_NA)) & !(is.character(.set_NA))) {
    stop(".set_NA must be a character vector, TRUE, or FALSE.")
  }
  if (!is.logical(.group_i)) {
    stop(".group_i must be TRUE or FALSE")
  }
  if (!is.na(.min) & !is.numeric(.min)) {
    stop(".min must either be NA or an integer.")
  }
  if (!is.na(.max) & !is.numeric(.max)) {
    stop(".max must either be NA or an integer.")
  }
  if (!is.na(.flag) & !is.character(.flag)) {
    stop(".flag must be a character variable.")
  }

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- origgroups[1:(length(origgroups) - 1)]
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .i, .t, .d, .uniqcheck, .setpanel)
  if (is.na(inp$t)) {
    stop("panel_fill() requires that .t be declared either in the function or by as_pdeclare().")
  }

  # Panel-declare data if any changes have been made.
  if (min(is.na(.i)) == 0 | !is.na(.t)) {
    .df <- as_pdeclare(.df, .i = .i, .t = .t, .d = .d, .uniqcheck = .uniqcheck)

    # .d might be unspecified and so inp$d is NA, but now .d is 1 from as_pdeclare default
    inp$d <- .df %@% ".d"
  }

  # we need a positive numeric .d, and a .t
  if (is.na(inp$d) | inp$d == 0) {
    stop("panel_fill() requires that .d be declared either in the function or by as_pdeclare(). Ordinal time variables (.d = 0) are not acceptable for this function.")
  }
  # Can't set the .i and .t variables to 0
  if (max(c(inp$i, inp$t) %in% .set_NA) == 1) {
    stop("Variables in .i or .t cannot be in .set_NA.")
  }

  # FOr use later
  arrnames <- c(inp$i, inp$t)
  arrnames <- arrnames[!is.na(arrnames)]

  # see if we can skip this because we're already grouped by inp$i
  if (.group_i == TRUE & (min(is.na(inp$i)) == 0) & !setequal(origgroups, inp$i)) {
    .df <- .df %>%
      dplyr::group_by_at(inp$i)
  }

  # If max and/or min are specified, create anchor observations so they will be filled in
  if (!is.na(.min)) {
    # vector identifying the early obs that AREN'T already at the min
    earlyobs <- (.df %>% dplyr::mutate_at(inp$t, .funs = function(x) x == min(x, na.rm = TRUE) & x != .min))[[inp$t]]

    # Pull out that data and set it to the early period
    earlydat <- .df %>%
      dplyr::ungroup() %>%
      dplyr::filter(earlyobs) %>%
      dplyr::mutate_at(inp$t, .funs = function(x) .min)
    # Whatever is being set to missing, drop it
    if (.set_NA == TRUE) {
      earlydat <- earlydat %>% dplyr::select_at(arrnames)
    } else if (is.character(.set_NA)) {
      earlydat <- earlydat %>%
        dplyr::select(which(!(names(earlydat) %in% .set_NA)))
    }

    .df <- dplyr::bind_rows(
      .df,
      # Make you a new obs if your earliest obs isn't the minimum
      earlydat
    )

    rm(earlyobs, earlydat)
  }
  if (!is.na(.max)) {
    # vector identifying the early obs that AREN'T already at the max
    lateobs <- (.df %>% dplyr::mutate_at(inp$t, .funs = function(x) x == max(x, na.rm = TRUE) & x != .max))[[inp$t]]

    # Pull out that data and set it to the early period
    latedat <- .df %>%
      dplyr::ungroup() %>%
      dplyr::filter(lateobs) %>%
      dplyr::mutate_at(inp$t, .funs = function(x) .max)
    # Whatever is being set to missing, drop it
    if (.set_NA == TRUE) {
      datedat <- datedat %>% dplyr::select_at(arrnames)
    }
    else if (is.character(.set_NA)) {
      datedat <- datedat %>%
        dplyr::select(which(!(names(datedat) %in% .set_NA)))
    }

    .df <- dplyr::bind_rows(
      .df,
      # Make you a new obs if your latest obs isn't the maximum
      latedat
    )

    rm(latedat, lateobs)
  }

  # Put in order
  .df <- .df %>%
    dplyr::arrange_at(inp$t)
  if (.backwards == TRUE) {
    .df <- .df[nrow(.df):1, ]
  }

  # get number of gaps FOLLOWING (for regular, or leading, if .fill is different)
  # since this is how many copies of each obs will be in the final data
  #-1 for the original copy. Make it IN the data b/c I'll need group structure in a sec
  .df[, ncol(.df) + 1] <- .df[[.t]]
  copyname <- names(.df)[ncol(.df)]
  .df <- .df %>% dplyr::mutate_at(copyname, .funs = function(x) abs(x - dplyr::lead(x)) / inp$d - 1)

  # And propogate that result to others in the same .t, since only the last-listed obs
  # in that period will get the right number
  if (.group_i == TRUE & (min(is.na(inp$i)) == 0)) {
    .df <- .df %>%
      dplyr::group_by_at(c(inp$i, inp$t))
  } else {
    .df <- .df %>%
      dplyr::group_by_at(inp$t)
  }
  .df <- .df %>%
    dplyr::mutate_at(copyname, .funs = function(x)
      max(dplyr::case_when(!is.na(x) & !is.infinite(x) ~ x, TRUE ~ 0), na.rm = TRUE))

  # If there are any fractional gaps, something is wrong, probably with .d
  if (min(.df[[copyname]] == as.integer(.df[[copyname]])) == 0) {
    stop("There are fractional gaps between some periods. Are you sure .d is defined correctly?")
  }

  # Which vars will actually be copied
  if (min(.set_NA == TRUE) == 1) {
    selnames <- c(inp$i, inp$t, copyname)
    selnames <- selnames[!is.na(selnames)]
  } else if (min(.set_NA == FALSE) == 1) {
    selnames <- names(.df)
  } else {
    selnames <- names(.df)[!(names(.df) %in% .set_NA)]
  }

  tocopy <- .df %>%
    dplyr::filter_at(copyname, dplyr::any_vars(. > 0)) %>%
    dplyr::select_at(selnames)
  # now's the time to flag new obs
  if (!is.na(.flag)) {
    .df[[.flag]] <- FALSE
    tocopy[[.flag]] <- TRUE
  }
  # Find an id name that isn't already in the data
  newidname <- "newidname"
  while (newidname %in% names(tocopy)) {
    newidname <- paste(newidname, ".", sep = "")
  }
  # Perform the copying. Extract the copynum variable to avoid spending time copying it
  # and becuase tidyr doesn't like it in there if I'm referring by string
  copynum <- tocopy[[copyname]]
  tocopy <- tocopy %>%
    dplyr::ungroup() %>%
    tidyr::uncount(copynum, .remove = FALSE, .id = newidname)
  # Increment the time to fill in, using .id
  # Which goes up or down depending on whether it's backwards or forwards
  tocopy <- tocopy %>%
    dplyr::mutate_at(.t, .funs = function(x)
      if (.backwards == FALSE) {
        tocopy[[.t]] + tocopy[[newidname]] * inp$d
      }
      else {
        tocopy[[.t]] - tocopy[[newidname]] * inp$d
      })
  tocopy[, newidname] <- NULL

  # plop everything back together, arrange, restore the original grouping, and return
  .df <- dplyr::bind_rows(.df, tocopy) %>%
    dplyr::arrange_at(arrnames)
  .df[[copyname]] <- NULL
  # Check if grouping has changed and there WAS an original grouping
  if (!setequal(c(origgroups, ".rows"), .df %@% "groups")) {
    if (max(is.na(origgroups)) == 1) {
      .df <- .df %>% dplyr::ungroup()
    }
    else {
      .df <- .df %>% dplyr::group_by_at(origgroups)
    }
  }

  # If it wants the original panel setting back, do that
  if (.setpanel == FALSE) {
    .df %@% ".i" <- inp$orig_i
    .df %@% ".t" <- inp$orig_t
    .df %@% ".d" <- inp$orig_d
  }

  return(.df)
}

#' Function to fill in missing (or other) values using known data
#'
#' This function looks for a list of values (usually, just \code{NA}) in a variable \code{.var} and overwrites those values with the most recent (or next-coming) values that are not from that list.
#'
#' \code{panel_locf()} is unusual among last-observation-carried-forward functions (like \code{zoo}'s \code{na.locf}) in that it is usable even if observations are not uniquely identified by \code{.t} (and \code{.i}, if defined).
#'
#' @param .var Vector to be modified.
#' @param .df Data frame or tibble (usually the data frame or tibble that contains \code{.var}) which contains the panel structure variables either listed in \code{.i} and \code{.t}, or earlier declared with \code{as_pdeclare()}. If \code{tlag} is called inside of a \code{dplyr} verb, this can be omitted and the data will be picked up automatically.
#' @param .fill Vector of values to be overwritten. Just \code{NA} by default.
#' @param .backwards By default, values of newly-created observations are copied from the most recently available period. Set \code{.backwards = TRUE} to instead copy values from the closest *following* period.
#' @param .resolve If there is more than one observation per individal/period, and the value of \code{.var} is identical for all of them, that's no problem. But what should \code{panel_locf()} do if they're not identical? Set \code{.resolve = 'error'} (or, really, any string) to throw an error in this circumstance. Or, set \code{.resolve} to a function that can be used within \code{dplyr::summarize()} to select a single value per individual/period. For example, \code{.resolve = function(x) mean(x)} to get the mean value of all observations present for that individual/period. \code{.resolve} will also be used to fill in values if some values in a given individual/period are to be overwritten and others aren't.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{panel_locf()} will group the data by \code{.i}, ignoring any grouping already implemented. Set \code{.group_i = FALSE} to avoid this.
#' @param .i Character or character vector with the variable names that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pdeclare()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @examples
#'
#' # Examples are too slow to run
#' if (interactive()) {
#'
#'   # The SPrail data has some missing price values.
#'   # Let's fill them in!
#'   # Note .d=0 tells it to ignore how big the gaps are
#'   # between one period and the next, just look for the most recent insert_date
#'   # .resolve tells it what value to pick if there are multiple
#'   # observed prices for that route/insert_date
#'   # (.resolve is not necessary if .i and .t uniquely identify obs,
#'   # or if .var is either NA or constant within them)
#'   # Also note - this will fill in using CURRENT-period
#'   # data first (if available) before looking for lagged data.
#'   data(SPrail)
#'   sum(is.na(SPrail$price))
#'   SPrail <- SPrail %>%
#'     dplyr::mutate(price = panel_locf(price,
#'       .i = c("origin", "destination"), .t = "insert_date", .d = 0,
#'       .resolve = function(x) mean(x, na.rm = TRUE)
#'     ))
#'
#'   # The spec is a little easier with data like Scorecard where
#'   # .i and .t uniquely identify observations.
#'   # Note that when the raw Scorecard data came in, it had -3 in place of NA. Let's restore that
#'   data(Scorecard)
#'   sum(is.na(Scorecard$earnings_med))
#'   Scorecard <- Scorecard %>%
#'     dplyr::mutate(earnings_med = dplyr::case_when(
#'       is.na(earnings_med) ~ as.double(-3),
#'       TRUE ~ as.double(earnings_med)
#'     )) %>%
#'     # Now let's fill in NAs and also -3s
#'     dplyr::mutate(earnings_med = panel_locf(earnings_med,
#'       .fill = c(NA, -3),
#'       .i = "unitid", .t = "year"
#'     ))
#'   # Note that there are still some missings - these are missings that come before the first
#'   # non-missing value in that unitid, so there's nothing to pull from.
#'   sum(is.na(Scorecard$earnings_med))
#' }
#' @export

panel_locf <- function(.var, .df = get(".", envir = parent.frame()), .fill = NA, .backwards = FALSE, .resolve = "error", .group_i = TRUE, .i = NA, .t = NA, .d = 1, .uniqcheck = FALSE) {
  if (!is.vector(.var)) {
    stop(".var must be a vector.")
  }
  if (!is.character(.resolve) & !is.function(.resolve)) {
    stop(".resolve must be a function or 'error'.")
  }
  if (!is.logical(.group_i)) {
    stop(".group_i must be TRUE or FALSE")
  }
  if (!is.logical(.backwards)) {
    stop(".backwards must be TRUE or FALSE")
  }

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- origgroups[1:(length(origgroups) - 1)]
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .i, .t, .d, .uniqcheck, .setpanel = FALSE)
  if (is.na(inp$t)) {
    stop("panel_locf() requires that .t be declared either in the function or by as_pdeclare().")
  }

  arrnames <- c(inp$i, inp$t)
  if (.group_i == FALSE) {
    arrnames <- inp$t
  }
  arrnames <- arrnames[!is.na(arrnames)]

  # We only need these
  .df <- .df %>%
    dplyr::ungroup() %>%
    dplyr::select_at(arrnames)

  # Get rid of our undesirable values
  .var <- ifelse(.var %in% .fill, NA, .var)

  # then add that cleaned-up vector in
  .df[, ncol(.df) + 1] <- .var
  # and the original order
  .df[, ncol(.df) + 1] <- 1:nrow(.df)
  # and indicators of needing to be filled in
  .df[, ncol(.df) + 1] <- is.na(.var)

  # get the names of our variables
  worknames <- names(.df)[(ncol(.df) - 2):ncol(.df)]

  # and group as appropriate
  .df <- .df %>% dplyr::group_by_at(arrnames)

  # Check if there's uniformity, if .resolve = 'error'
  if (is.character(.resolve)) {
    if (max((.df %>%
      dplyr::mutate_at(worknames[1],
        .funs = function(x) dplyr::n_distinct(x)
      ))[[worknames[1]]]) > 1) {
      stop("Values are not consistent within (.i, if specified, and) .t. See .resolve option.")
    }
    .resolve <- function(x) dplyr::first(x)
  }

  # If we're backwards, go backwards!
  if (.backwards == TRUE) {
    if (is.character(.df[[inp$t]])) {
      .df[[inp$t]] <- as.numeric(as.factor(.df[[inp$t]]))
    }
    .df[[inp$t]] <- -.df[[inp$t]]
  }

  .df <- .df %>%
    # implement the .resolve function
    dplyr::mutate_at(worknames[1], .funs = .resolve)

  # If we're grouping by i, do that
  if (length(arrnames) > 1) {
    .df <- .df %>%
      dplyr::group_by_at(arrnames[-length(arrnames)])
  }
  else {
    .df <- .df %>%
      dplyr::ungroup()
  }

  # Sort for easy fillin
  .df <- .df %>% dplyr::arrange_at(arrnames)


  # Now it's time to fill in
  # first, find the highest row number that is nonmissing up to that point
  .df <- .df %>%
    dplyr::do(
      dplyr::mutate_at(., worknames[3], function(x) dplyr::row_number() -
          cummax(dplyr::row_number() *
            (!is.na(.[[worknames[1]]])))) %>%
        # If it's the first row, set to 0 so it doesn't try to reach
        dplyr::mutate_at(worknames[3], function(x) ifelse(dplyr::row_number() == .[[worknames[3]]], 0, .[[worknames[3]]])) %>%
        # Then, count how many back we need to go for a nonmissing, find it, and fill in
        dplyr::mutate_at(worknames[1], function(x) .[[worknames[1]]][1:nrow(.) - .[[worknames[3]]]])
    ) %>%
    # reorder back how it was
    dplyr::arrange_at(worknames[2])

  # Fill in anything that was missing
  .var <- ifelse(is.na(.var), .df[[worknames[1]]], .var)

  return(.var)
}


#' Function to check for inconsistency in variables that should be fixed
#'
#' This function checks whether one set of variables is consistent within values of another set of variables. If they are, returns \code{TRUE}. If they aren't, it will return a list of data frames, one for each element of \code{.var}, consisting only of the observations and variables in which there are inconsistencies.
#'
#' @param .df Data frame or tibble.
#' @param .var Character vector of variable names in \code{.df} that are to be checked for consistency. If not specified, uses all variables in \code{.df} that are not in \code{.within}.
#' @param .within Character vector of variable names that the \code{.var} variables should be consistent within.
#' @examples
#'
#' # In the Scorecard data, it should be the case that
#' # state_abbr and inst_name never change within university.
#' # Let's see if that's true
#' data(Scorecard)
#' fixed_check(Scorecard, .var = c("state_abbr", "inst_name"), .within = "unitid")
#' # it returns TRUE! We're good to go
#'
#' # count_not_working has no reason to be constant within unitid,
#' # but let's see what happens if we run it through
#' fixed_check(Scorecard, .var = c("count_not_working"), .within = "unitid")
#' # It gives back a tibble with inconsistent obs!
#' @export

fixed_check <- function(.df, .var = NA, .within) {
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df")) == 0) {
    stop("Requires data to be a data frame or tibble.")
  }
  if (sum(class(.df) == "data.table") > 0) {
    warning("pmdplyr functions have not been tested with data.tables.")
  }

  # if .var is unspecified
  if (max(is.na(.var) == 1)) {
    .var <- names(.df)[!(names(.df) %in% .within)]
  }

  if (!is.character(.var)) {
    stop(".var must be a character vector.")
  }
  if (!is.character(.within)) {
    stop(".within must be a character vector.")
  }
  if (min(.var %in% names(.df)) == 0 | min(.within %in% names(.df)) == 0) {
    stop(".var and .within must be names of variables in .df")
  }

  # apply grouping for within
  .df <- .df %>%
    dplyr::group_by_at(.within)

  # for each element of .var, drop consistent obs
  result <- lapply(.var, function(x)
    .df %>%
      dplyr::arrange_at(x) %>%
      dplyr::filter_at(x, function(y) dplyr::first(y) != dplyr::last(y)))

  # check if there are any inconsistent obs
  # if so, return that. Otherwise return TRUE
  if (max(sapply(result, function(x) nrow(x))) == 0) {
    return(TRUE)
  }
  else {
    return(result)
  }
}

#' Function to enforce consistency in variables
#'
#' This function forces values the variables in \code{.var} to take constant values within combinations of the variables in \code{.within}. \code{fixed_force()} will return a data frame with consistency enforced.
#'
#' Inconsistencies will be resolved by the function \code{.resolve}. Or, set \code{.resolve} to \code{'drop'} (or any string, really) to drop all cases with inconsistency.
#'
#' @param .df Data frame or tibble.
#' @param .var Character vector of variable names in \code{.df} that should be consistent. If not specified, uses all variables in \code{.df} that are not in \code{.within}.
#' @param .within Character vector of variable names that the \code{.var} variables should be consistent within.
#' @param .resolve Function capable of being passed to \code{dplyr::summarize()} that will be used to resolve inconsistencies. Or, set to \code{'drop'} or any string to drop all inconsistent observations. By default, this will return the mode (ties use the first observed value).
#' @param .flag String indicating the name of a new variable that flags any observations altered by \code{fixed_force()}.
#' @examples
#'
#' data(Scorecard)
#' # The variables pred_degree_awarded_ipeds and state_abbr should be constant within unitid
#' # However, sometimes colleges change what they offer.
#' # For the purpose of my analysis, though,
#' # I want to treat any changers as whatever they are most often (the mode).
#' # So let's enforce that with fixed_force
#' Scorecard <- fixed_force(Scorecard,
#'   .var = c("pred_degree_awarded_ipeds", "state_abbr"),
#'   .within = "unitid", .flag = "changed"
#' )
#' # Did we catch any changers?
#' table(Scorecard$changed)
#' # We did!
#' @export

fixed_force <- function(.df, .var = NA, .within, .resolve = function(x) unique(x)[which.max(tabulate(match(x, unique(x))))], .flag = NA) {
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df")) == 0) {
    stop("Requires data to be a data frame or tibble.")
  }
  if (sum(class(.df) == "data.table") > 0) {
    warning("pmdplyr functions have not been tested with data.tables.")
  }

  # if .var is unspecified
  if (max(is.na(.var)) == 1) {
    .var <- names(.df)[!(names(.df) %in% .within)]
  }

  if (!is.character(.var)) {
    stop(".var must be a character vector.")
  }
  if (!is.character(.within)) {
    stop(".within must be a character vector.")
  }
  if (min(.var %in% names(.df)) == 0 | min(.within %in% names(.df)) == 0) {
    stop(".var and .within must be names of variables in .df")
  }
  if (!is.character(.resolve) & !is.function(.resolve)) {
    stop(".resolve must be a function or 'drop'.")
  }
  if (!is.na(.flag) & !is.character(.flag)) {
    stop(".flag must be a character variable.")
  }

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- origgroups[1:(length(origgroups) - 1)]
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  .df <- .df %>%
    dplyr::group_by_at(.within)

  # If .resolve is a string, drop all inconsistencies
  if (is.character(.resolve)) {
    .df[, ncol(.df) + 1] <- 1:nrow(.df)
    origorder <- names(.df)[ncol(.df)]

    # for each element of .var, drop inconsistent obs
    for (v in .var) {
      .df <- .df %>%
        dplyr::arrange_at(v) %>%
        dplyr::filter_at(v, function(y) dplyr::first(y) == dplyr::last(y))
    }

    # Rearrange in original order and drop origorder
    .df <- .df %>% dplyr::arrange_at(origorder)
    data[[origorder]] <- NULL
  }
  else {
    # otherwise, use .resolve to, uh, resolve inconsistencies
    if (is.character(.flag)) {
      databkup <- .df
    }

    .df <- .df %>%
      dplyr::mutate_at(.var, .funs = .resolve)

    if (is.character(.flag)) {
      # check if every column matches. If not, flag 'em
      .df[, .flag] <- rowSums(databkup == .df) < ncol(.df)
      rm(databkup)
    }
  }

  # Restore grouping, if present
  if (!is.na(origgroups)) {
    .df <- .df %>% dplyr::group_by_at(origgroups)
  }
  else {
    .df <- .df %>% dplyr::ungroup()
  }

  return(.df)
}
