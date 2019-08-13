#' Fill in gaps in panel data
#'
#' This function creates new observations to fill in any gaps in panel data. For example, if individual 1 has an observation in periods t = 1 and t = 3 but no others, this function will create an observation for t = 2. By default, the t = 2 observation will be identical to the t = 1 observation except for the time variable, but this can be adjusted. This function returns data sorted by \code{.i} and \code{.t}.
#'
#' Note that, in the case where there is more than one observation for a given individual/time period (or just time period if \code{.group_i = FALSE}), \code{panel_fill()} will create copies of *every observation* in the appropriate individual/time period for filling-in purposes. So if there are four t = 1 observations and nothing in t = 2, \code{panel_fill()} will create four new observations with t = 2, copying the original four in t = 1.
#'
#' By default, the \code{panel_fill()} operation is grouped by \code{.i}, although it will retun the data in the original grouping structure. Leave \code{.i} blank, or, if \code{.i} is already in the data from \code{as_pibble}, set \code{.group_i=FALSE} to run the function ungrouped, or with the existing group structure.
#'
#' This function requires \code{.t} and \code{.d} to be declared in the function or already established in the data by \code{as_pibble()}. Also, this requires a cardinal \code{.t}. It must not be the case that \code{.d=0}.
#'
#' @param .df Tibble or data frame which either has the \code{.t} and \code{.d} (and perhaps \code{.i}) attributes included by \code{as_pibble()}, or the appropriate panel structure is declared in the function.
#' @param .set_NA Should values in newly-created observations be set to adjacent values or to NA? Set to \code{TRUE} to set all new values to NA except for .i and .t. To make only specific variables NA, list them as a character vector. Defaults to FALSE; all values are filled in using the most recently available data.
#' @param .min Sets the first time period in the data for each individual to be \code{.min}, and fills in gaps between period \code{.min} and the actual start of the data. Copies data from the first period present in the data for each individual (if grouped). Handy for creating balanced panels.
#' @param .max Sets the last time period in the data for each individual to be \code{.max}, and fills in gaps between period \code{.max} and the actual start of the data. Copies data from the flast period present in the data for each individual (if grouped). Handy for creating balanced panels.
#' @param .backwards By default, values of newly-created observations are copied from the most recently available period. Set \code{.backwards = TRUE} to instead copy values from the closest *following* period.
#' @param .group_i By default, \code{panel_fill()} will fill in gaps within values of \code{.i}. If \code{.i} is missing, it won't do that. If \code{.i} is in the data and you still don't want \code{panel_fill()} to run within \code{.i}, set \code{.group_i = FALSE}.
#' @param .flag The name of a new variable indicating which observations are newly created by \code{panel_fill()}.
#' @param .i Quoted or unquoted variables that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pibble()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Quoted or unquoted variable indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to \code{TRUE} to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @param .setpanel Logical parameter. \code{TRUE} by default, and so if \code{.i}, \code{.t}, and/or \code{.d} are declared, will return a \code{pibble} set in that way.
#' @examples
#'
#' # Examples are too slow to run - this function is slow!
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
#'   Scorecard_filled <- panel_fill(Scorecard,
#'     .i = unitid,
#'     .t = year,
#'     .flag = "new"
#'   )
#'
#'   # Or maybe we want those observations in there but don't want to treat them as real data
#'   # so instead of filling them in, just leave all the data in the new obs blank
#'   # (note this sets EVERYTHING not in .i or .t to NA - if you only want some variables NA,
#'   # make .set_NA a character vector of those variable names)
#'   Scorecard_filled <- panel_fill(Scorecard,
#'     .i = unitid,
#'     .t = year,
#'     .flag = "new",
#'     .set_NA = TRUE
#'   )
#'
#'   # Perhaps we want a perfectly balanced panel. So let's set .max and .min to the start and end
#'   # of the data, and it will fill in everything.
#'   Scorecard_filled <- panel_fill(Scorecard,
#'     .i = unitid, .t = year, .flag = "new",
#'     .min = min(Scorecard$year), .max = max(Scorecard$year)
#'   )
#'   # how many obs of each college? Should be identical, and equal to the number of years there are
#'   table(table(Scorecard_filled$unitid))
#'   length(unique(Scorecard_filled$year))
#' }
#' @export

panel_fill <- function(.df, .set_NA = FALSE, .min = NA, .max = NA, .backwards = FALSE, .group_i = TRUE, .flag = NA, .i = NULL, .t = NULL, .d = 1, .uniqcheck = FALSE, .setpanel = TRUE) {
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

  # Pull out variable names
  .icall <- tidyselect::vars_select(names(.df), {{ .i }})
  if (length(.icall) == 0) {
    .icall <- NA_character_
  }
  .tcall <- tidyselect::vars_select(names(.df), {{ .t }})
  if (length(.tcall) == 0) {
    .tcall <- NA_character_
  }

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- utils::head(origgroups, -1)
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .icall, .tcall, .d, .uniqcheck, .setpanel)
  if (is.na(inp$t)) {
    stop("panel_fill() requires that .t be declared either in the function or by as_pibble().")
  }

  # Panel-declare data if any changes have been made.
  if (min(is.na(.icall)) == 0 | !is.na(.tcall)) {
    .df <- as_pibble(.df, {{ .i }}, {{ .t }}, .d, .uniqcheck = .uniqcheck)

    # .d might be unspecified and so inp$d is NA, but now .d is 1 from as_pibble default
    inp$d <- .df %@% ".d"
  }

  # we need a positive numeric .d, and a .t
  if (is.na(inp$d) | inp$d == 0) {
    stop("panel_fill() requires that .d be declared either in the function or by as_pibble(). Ordinal time variables (.d = 0) are not acceptable for this function.")
  }
  # Can't set the .i and .t variables to 0
  if (max(c(inp$i, inp$t) %in% .set_NA) == 1) {
    stop("Variables in .i or .t cannot be in .set_NA.")
  }

  # FOr use later
  arrnames <- c(inp$i, inp$t)
  arrnames <- arrnames[!is.na(arrnames)]

  # see if we can skip this because we're already grouped by inp$i
  if (.group_i == TRUE & !anyNA(inp$i) & !setequal(origgroups, inp$i)) {
    .df <- .df %>%
      dplyr::group_by_at(inp$i)
  }

  # If max and/or min are specified, create anchor observations so they will be filled in
  if (!is.na(.min)) {
    # vector identifying the early obs that AREN'T already at the min
    earlyobs <- .df %>%
      dplyr::mutate_at(inp$t,
        .funs = function(x) {
          x == min(x, na.rm = TRUE) &
            x != .min
        }
      ) %>%
      dplyr::pull(!!inp$t)

    # Pull out that data and set it to the early period
    earlydat <- .df %>%
      dplyr::ungroup() %>%
      dplyr::filter(earlyobs) %>%
      dplyr::mutate_at(inp$t, .funs = function(x) .min)
    # Whatever is being set to missing, drop it
    if (identical(.set_NA, TRUE)) {
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
    lateobs <- .df %>%
      dplyr::mutate_at(inp$t,
        .funs = function(x) {
          x == max(x, na.rm = TRUE) & x != .max
        }
      ) %>%
      dplyr::pull(!!inp$t)

    # Pull out that data and set it to the early period
    latedat <- .df %>%
      dplyr::ungroup() %>%
      dplyr::filter(lateobs) %>%
      dplyr::mutate_at(inp$t, .funs = function(x) .max)
    # Whatever is being set to missing, drop it
    if (.set_NA == TRUE) {
      latedat <- latedat %>% dplyr::select_at(arrnames)
    }
    else if (is.character(.set_NA)) {
      latedat <- latedat %>%
        dplyr::select(which(!(names(latedat) %in% .set_NA)))
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
    .df <- .df %>%
      dplyr::arrange(nrow(.):1)
  }

  # get number of gaps FOLLOWING (for regular, or leading, if .fill is different)
  # since this is how many copies of each obs will be in the final data
  #-1 for the original copy. Make it IN the data b/c I'll need group structure in a sec
  copyname <- uniqname(.df)
  .df <- .df %>%
    dplyr::mutate(!!copyname := .data[[inp$t]]) %>%
    dplyr::mutate_at(copyname,
      .funs =
        function(x) abs(x - dplyr::lead(x)) / inp$d - 1
    ) %>%
    dplyr::mutate_at(copyname, .funs = function(x) {
      dplyr::case_when(
        is.na(x) | is.infinite(x) ~ 0,
        TRUE ~ x
      )
    })

  # And propogate that result to others in the same .t, since only the last-listed obs
  # in that period will get the right number
  if (.group_i == TRUE & (min(is.na(inp$i)) == 0)) {
    .df <- .df %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(c(inp$i, inp$t))
  } else {
    .df <- .df %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(inp$t)
  }
  .df <- .df %>%
    dplyr::mutate_at(copyname, .funs = max, na.rm = TRUE)

  # Which vars will actually be copied
  if (identical(.set_NA, TRUE)) {
    selnames <- c(inp$i, inp$t, copyname)
    selnames <- selnames[!is.na(selnames)]
  } else if (identical(.set_NA, FALSE)) {
    selnames <- names(.df)
  } else {
    selnames <- names(.df)[!(names(.df) %in% .set_NA)]
  }

  suppressWarnings(tocopy <- .df %>%
    dplyr::filter_at(copyname, dplyr::any_vars(. > 0)) %>%
    dplyr::select_at(selnames))
  # now's the time to flag new obs
  if (!is.na(.flag)) {
    .df <- .df %>%
      dplyr::mutate(!!.flag := FALSE)
    tocopy <- tocopy %>%
      dplyr::mutate(!!.flag := TRUE)
  }
  # Find an id name that isn't already in the data
  newidname <- uniqname(tocopy)
  # Perform the copying. Extract the copynum variable to avoid spending time copying it
  # and becuase tidyr doesn't like it in there if I'm referring by string
  # Make it a data frame in case the variable 'copynum' is in the data
  copynum <- data.frame(c = tocopy[[copyname]])
  tocopy <- tocopy %>%
    dplyr::ungroup() %>%
    tidyr::uncount(copynum$c, .remove = FALSE, .id = newidname) %>%
    dplyr::select(-!!copyname)
  # Increment the time to fill in, using .id
  # Which goes up or down depending on whether it's backwards or forwards
  tocopy <- tocopy %>%
    dplyr::mutate_at(inp$t, .funs = function(x) {
      if (.backwards == FALSE) {
        tocopy[[inp$t]] + tocopy[[newidname]] * inp$d
      }
      else {
        tocopy[[inp$t]] - tocopy[[newidname]] * inp$d
      }
    }) %>%
    dplyr::select(-!!newidname)

  # plop everything back together, arrange, restore the original grouping, and return
  .df <- dplyr::bind_rows(
    .df %>%
      dplyr::select(-!!copyname),
    tocopy
  ) %>%
    dplyr::arrange_at(arrnames)
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
    if (inp$is_tbl_pb) {
      if (is.na(inp$orig_i)) {
        inp$orig_i <- NULL
      }
      if (is.na(inp$orig_t)) {
        inp$orig_t <- NULL
      }

      .df <- as_pibble(.df, inp$orig_i, inp$orig_t, inp$orig_d, .uniqcheck = FALSE)
    } else {
      attr(.df, ".i") <- NULL
      attr(.df, ".t") <- NULL
      attr(.df, ".d") <- NULL
      class(.df) <- class(.df)[!(class(.df) %in% "tbl_pb")]
    }
  }

  return(.df)
}

#' Fill in missing (or other) values of a panel data set using known data
#'
#' This function looks for a list of values (usually, just \code{NA}) in a variable \code{.var} and overwrites those values with the most recent (or next-coming) values that are not from that list ("last observation carried forward").
#'
#' \code{panel_locf()} is unusual among last-observation-carried-forward functions (like \code{zoo::na.locf()}) in that it is usable even if observations are not uniquely identified by \code{.t} (and \code{.i}, if defined).
#'
#' @param .var Vector to be modified.
#' @param .df Data frame, pibble, or tibble (usually the one containing \code{.var}) that contains the panel structure variables either listed in \code{.i} and \code{.t}, or earlier declared with \code{as_pibble()}. If \code{tlag} is called inside of a \code{dplyr} verb, this can be omitted and the data will be picked up automatically.
#' @param .fill Vector of values to be overwritten. Just \code{NA} by default.
#' @param .backwards By default, values of newly-created observations are copied from the most recently available period. Set \code{.backwards = TRUE} to instead copy values from the closest *following* period.
#' @param .resolve If there is more than one observation per individal/period, and the value of \code{.var} is identical for all of them, that's no problem. But what should \code{panel_locf()} do if they're not identical? Set \code{.resolve = 'error'} (or, really, any string) to throw an error in this circumstance. Or, set \code{.resolve} to a function that can be used within \code{dplyr::summarize()} to select a single value per individual/period. For example, \code{.resolve = function(x) mean(x)} to get the mean value of all observations present for that individual/period. \code{.resolve} will also be used to fill in values if some values in a given individual/period are to be overwritten and others aren't. Using a function will be quicker than \code{.resolve = 'error'}, so if you're certain there's no issue, you can speed up execution by setting, say, \code{.resolve = dplyr::first}.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{panel_locf()} will group the data by \code{.i}, ignoring any grouping already implemented. Set \code{.group_i = FALSE} to avoid this.
#' @param .i Quoted or unquoted variables that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pibble()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Quoted or unquoted variable indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @examples
#'
#'
#' # The SPrail data has some missing price values.
#' # Let's fill them in!
#' # Note .d=0 tells it to ignore how big the gaps are
#' # between one period and the next, just look for the most recent insert_date
#' # .resolve tells it what value to pick if there are multiple
#' # observed prices for that route/insert_date
#' # (.resolve is not necessary if .i and .t uniquely identify obs,
#' # or if .var is either NA or constant within them)
#' # Also note - this will fill in using CURRENT-period
#' # data first (if available) before looking for lagged data.
#' data(SPrail)
#' sum(is.na(SPrail$price))
#' SPrail <- SPrail %>%
#'   dplyr::mutate(price = panel_locf(price,
#'     .i = c(origin, destination), .t = insert_date, .d = 0,
#'     .resolve = function(x) mean(x, na.rm = TRUE)
#'   ))
#'
#' # The spec is a little easier with data like Scorecard where
#' # .i and .t uniquely identify observations
#' # so .resolve isn't needed.
#' data(Scorecard)
#' sum(is.na(Scorecard$earnings_med))
#' Scorecard <- Scorecard %>%
#'   # Let's speed this up by just doing four-year colleges
#'   dplyr::filter(pred_degree_awarded_ipeds == 3) %>%
#'   # Now let's fill in NAs and also in case there are any erroneous 0s
#'   dplyr::mutate(earnings_med = panel_locf(earnings_med,
#'     .fill = c(NA, 0),
#'     .i = unitid, .t = year
#'   ))
#' # Note that there are still some missings - these are missings that come before the first
#' # non-missing value in that unitid, so there's nothing to pull from.
#' sum(is.na(Scorecard$earnings_med))
#' @export

panel_locf <- function(.var, .df = get(".", envir = parent.frame()), .fill = NA, .backwards = FALSE, .resolve = "error", .group_i = TRUE, .i = NULL, .t = NULL, .d = 1, .uniqcheck = FALSE) {
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

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- utils::head(origgroups, -1)
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  # Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df, .icall, .tcall, .d, .uniqcheck, .setpanel = FALSE)
  if (is.na(inp$t)) {
    stop("panel_locf() requires that .t be declared either in the function or by as_pibble().")
  }

  arrnames <- c(inp$i, inp$t)
  if (.group_i == FALSE) {
    arrnames <- inp$t
  }
  arrnames <- arrnames[!is.na(arrnames)]

  # We only need these
  .df <- suppressWarnings(.df %>%
    dplyr::ungroup() %>%
    dplyr::select_at(arrnames))

  # Get rid of our undesirable values
  .var <- ifelse(.var %in% .fill, NA, .var)
  # Store our .var at this point so we know what to overwrite later
  # Since we don't want any nonmissing values manipulated by .resolve to get passed back
  orig_var <- .var

  # then add that cleaned-up vector in
  worknames <- uniqname(.df)
  worknames[2] <- paste(worknames[1], ".1", sep = "")

  .df <- .df %>%
    dplyr::mutate(
      !!worknames[1] := !!.var,
      # And the original order
      !!worknames[2] := 1:nrow(.)
    ) %>%
    dplyr::ungroup()

  # If we're backwards, go backwards!
  if (.backwards == TRUE) {
    if (is.character(.df[[inp$t]])) {
      .df <- .df %>%
        dplyr::mutate(!!inp$t := as.numeric(as.factor(.data[[inp$t]])))
    }
    .df <- .df %>%
      dplyr::mutate(!!inp$t := -.data[[inp$t]])
  }

  # and group as appropriate
  .df <- .df %>% dplyr::group_by_at(arrnames)

  # Check if there's ever more than one obs per group. If not no need to
  # check uniformity or use the resolve function
  dfmult <- nrow(.df %>%
    dplyr::filter(dplyr::n() > 1))

  # Check if there's uniformity, if .resolve = 'error'
  if (is.character(.resolve) & dfmult > 0) {
    if (max(.df %>%
      dplyr::mutate_at(
        worknames[1],
        function(x) !(x %in% dplyr::first(x))
      ) %>%
      dplyr::pull(worknames[1])) == 1) {
      stop("Values are not consistent within (.i, if specified, and) .t. See .resolve option.")
    }
    .resolve <- dplyr::first
  }

  if (dfmult > 0) {
    .df <- .df %>%
      # implement the .resolve function
      dplyr::mutate_at(worknames[1], .funs = .resolve) %>%
      # If there aren't comparison values, can often resolve to NAN causing problems
      dplyr::mutate(!!worknames[1] := ifelse(
        is.nan(.data[[worknames[1]]]), NA, .data[[worknames[1]]]
      ))
  }

  # If we're grouping by i, do that
  if (length(arrnames) > 1) {
    .df <- .df %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(arrnames[-length(arrnames)])
  } else {
    .df <- .df %>%
      dplyr::ungroup()
  }

  # Sort for easy fillin
  .df <- .df %>% dplyr::arrange_at(arrnames)


  # Now it's time to fill in

  # Within groups, get number of each value in a row, fill in NAs with a dplyr::lag,
  # and then uncount() to recover our .var
  # So check() doesn't complain
  lengthname <- uniqname(.df)
  valuename <- paste(lengthname, ".1", sep = "")

  # So check doesn't complain
  values <- NULL
  lengths <- NULL

  getvalues <- .df %>%
    dplyr::do(
      data.frame(
        lengths = rle_na(.data[[worknames[1]]])$lengths,
        values = rle_na(.data[[worknames[1]]])$values
      ) %>%
        dplyr::rename(
          !!lengthname := lengths,
          !!valuename := values
        )
    ) %>%
    tidyr::uncount(.data[[lengthname]],
      .remove = FALSE
    ) %>%
    dplyr::pull(!!valuename)


  # Put in the result, reorder back how it was and pull the variable out
  .var <- .df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!worknames[1] := !!getvalues) %>%
    dplyr::arrange_at(worknames[2]) %>%
    dplyr::pull(!!worknames[1])

  # Failure to lookup (NaN) is really a NA for us
  .var <- ifelse(is.nan(.var), NA, .var)

  return(ifelse(is.na(orig_var), .var, orig_var))
}


#' Check for inconsistency in variables that should be fixed
#'
#' This function checks whether one set of variables is consistent within values of another set of variables. If they are, returns \code{TRUE}. If they aren't, it will return a list of data frames, one for each element of \code{.var}, consisting only of the observations and variables in which there are inconsistencies.
#'
#' @param .df Data frame, pibble, or tibble.
#' @param .var Quoted or unquoted variable(s) in \code{.df} that are to be checked for consistency. If not specified, uses all variables in \code{.df} that are not in \code{.within}.
#' @param .within Quotes or unquoted variable(s) that the \code{.var} variables should be consistent within.
#' @examples
#'
#' # In the Scorecard data, it should be the case that
#' # state_abbr and inst_name never change within university.
#' # Let's see if that's true
#' data(Scorecard)
#' fixed_check(Scorecard, .var = c(state_abbr, inst_name), .within = unitid)
#' # it returns TRUE! We're good to go
#'
#' # count_not_working has no reason to be constant within unitid,
#' # but let's see what happens if we run it through
#' fixed_check(Scorecard, .var = count_not_working, .within = unitid)
#' # It gives back a tibble with inconsistent obs!
#' @export

fixed_check <- function(.df, .var = NULL, .within = NULL) {
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df")) == 0) {
    stop("Requires data to be a data frame or tibble.")
  }
  if (sum(class(.df) == "data.table") > 0) {
    warning("pmdplyr functions have not been tested with data.tables.")
  }

  # Pull out variable names
  .withincall <- tidyselect::vars_select(names(.df), {{ .within }})
  if (length(.withincall) == 0) {
    stop(".within must be specified as variable(s) in df.")
  }
  .varcall <- tidyselect::vars_select(names(.df), {{ .var }})
  # If .var is unspecified
  if (length(.varcall) == 0) {
    .varcall <- names(.df)[!(names(.df) %in% .withincall)]
  }

  # apply grouping for within
  .df <- .df %>%
    dplyr::group_by_at(.withincall)

  # for each element of .var, drop consistent obs
  result <- lapply(.varcall, function(x) {
    .df %>%
      dplyr::arrange_at(x) %>%
      dplyr::filter_at(x, dplyr::any_vars(dplyr::first(.) != dplyr::last(.) |
        is.na(dplyr::first(.)) != is.na(dplyr::last(.)))) %>%
      dplyr::ungroup()
  })

  # check if there are any inconsistent obs
  # if so, return that. Otherwise return TRUE
  if (max(sapply(result, function(x) nrow(x))) == 0) {
    return(TRUE)
  }
  else {
    return(result)
  }
}

#' Enforce consistency in variables
#'
#' This function forces values the variables in \code{.var} to take constant values within combinations of the variables in \code{.within}. \code{fixed_force()} will return a data frame with consistency enforced.
#'
#' Inconsistencies will be resolved by the function \code{.resolve}. Or, set \code{.resolve} to \code{'drop'} (or any string, really) to drop all cases with inconsistency.
#'
#' @param .df Data frame, pibble, or tibble.
#' @param .var Quoted or unquoted variable(s) in \code{.df} that should be consistent. If not specified, uses all variables in \code{.df} that are not in \code{.within}.
#' @param .within Quotes or unquoted variable(s) that the \code{.var} variables should be consistent within.
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
#'   .var = c(pred_degree_awarded_ipeds, state_abbr),
#'   .within = unitid, .flag = "changed"
#' )
#' # Did we catch any changers?
#' table(Scorecard$changed)
#' # We did!
#' @export

fixed_force <- function(.df, .var = NULL, .within = NULL, .resolve = mode_order, .flag = NA) {
  if (sum(class(.df) %in% c("data.frame", "tbl", "tbl_df")) == 0) {
    stop("Requires data to be a data frame, pibble, or tibble.")
  }
  if (sum(class(.df) == "data.table") > 0) {
    warning("pmdplyr functions have not been tested with data.tables.")
  }

  # Pull out variable names
  .withincall <- tidyselect::vars_select(names(.df), {{ .within }})
  if (length(.withincall) == 0) {
    stop(".within must be specified as variable(s) in .df.")
  }
  .varcall <- tidyselect::vars_select(names(.df), {{ .var }})
  # if .var is unspecified
  if (length(.varcall) == 0) {
    .varcall <- names(.df)[!(names(.df) %in% .withincall)]
  }

  if (!is.character(.resolve) & !is.function(.resolve)) {
    stop(".resolve must be a function or 'drop'.")
  }
  if (!is.na(.flag) & !is.character(.flag)) {
    stop(".flag must be a character variable.")
  }

  # original grouping structure
  origgroups <- names(.df %@% "groups")
  origgroups <- utils::head(origgroups, -1)
  if (is.null(origgroups)) {
    origgroups <- NA
  }

  origorder <- uniqname(.df)
  .df <- .df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!origorder := 1:nrow(.)) %>%
    dplyr::group_by_at(.withincall)

  # If .resolve is a string, drop all inconsistencies
  if (is.character(.resolve)) {
    # for each element of .var, drop inconsistent obs
    for (v in .varcall) {
      .df <- .df %>%
        dplyr::arrange_at(v) %>%
        dplyr::filter_at(v, function(y) dplyr::first(y) == dplyr::last(y))
    }

    # Rearrange in original order and drop origorder
    .df <- .df %>% dplyr::arrange_at(origorder)
  } else {
    # otherwise, use .resolve to, uh, resolve inconsistencies
    if (is.character(.flag)) {
      databkup <- .df
    }

    .df <- .df %>%
      dplyr::mutate_at(.varcall, .funs = .resolve)

    if (is.character(.flag)) {
      # check if every column matches. If not, flag 'em
      newflag <- rowSums(matrix(Vectorize(isTRUE)(.df == databkup),
        nrow = nrow(.df)
      ) +
        (is.na(.df) & is.na(databkup))) < ncol(.df)
      .df <- .df %>%
        dplyr::mutate(!!.flag := !!newflag)
      rm(databkup)
    }
  }

  .df <- .df %>%
    dplyr::select(-!!origorder)

  # Restore grouping, if present
  if (!is.na(origgroups)) {
    .df <- .df %>% dplyr::group_by_at(origgroups)
  }
  else {
    .df <- .df %>% dplyr::ungroup()
  }

  return(.df)
}


#' Calculate the mode, and use original order to break ties
#'
#' \code{mode_order()} calculates the mode of a vector, mostly used as the default \code{.resolve} option in \code{fixed_force()}
#'
#' In the case of ties, the first-ordered value in the vector wins.

#' @param x Vector to calculate the mode of.
#' @examples
#'
#' x <- c(1, 2, 2, NA, 5, 3, 4)
#' mode_order(x)
#'
#' # Ties are broken by order
#' x <- c(2, 2, 1, 1)
#' mode_order(x)
#' @export
mode_order <- function(x) {
  unique(x)[which.max(tabulate(match(x, unique(x))))]
}
