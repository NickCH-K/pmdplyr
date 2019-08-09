#' Join two data frames safely
#'
#' This function is a wrapper for the standard \code{dplyr} \code{join} functions and the \code{pmdplyr} \code{inexact_join} functions.
#'
#' When performing a join, we generally expect that one or both of the joined data sets is uniquely identified by the set of joining variables.
#'
#' If this is not true, the results of the join will often not be what you expect. Unfortunately, \code{join} does not warn you that you may have just done something strange.
#'
#' This issue is especially likely to arise with panel data, where you may have multiple different data sets at different observation levels.
#'
#' \code{safe_join} forces you to specify which of your data sets you think are uniquely identified by the joining variables. If you are wrong, it will return an error. If you are right, it will pass you on to your preferred \code{join} function, given in \code{join}. If \code{join} is not specified, it will just return \code{TRUE}.
#'
#' @param x,y The left and right data sets to join.
#' @param expect Either \code{'x'}, \code{'y'}, or \code{c('x','y')} - the data sets you expect to be uniquely identified by the joining variables. You can alternately use \code{expect = "1:m"} instead of \code{"x"} for one-to-many merge, \code{"m:1"} instead of \code{"y"} for many-to-one, or \code{"1:1"} instead of \code{c("x", "y")} for one-to-one.
#' @param join A \code{join} or \code{inexact_join} function to run if \code{safe_join} determines your join is safe. By default, simply returns \code{TRUE} instead of running the join.
#' @param ... Other arguments to be passed to the function specified in \code{join}. If performing an \code{inexact_join}, put the \code{var} and \code{jvar} arguments in as quoted variables.
#'
#' @examples
#' # left is panel data and i does not uniquely identify observations
#' left <- data.frame(
#'   i = c(1, 1, 2, 2),
#'   t = c(1, 2, 1, 2),
#'   a = 1:4
#' )
#' # right is individual-level data uniquely identified by i
#' right <- data.frame(
#'   i = c(1, 2),
#'   b = 1:2
#' )
#'
#' # I think that I can do a one-to-one merge on i
#' # Forgetting that left is identified by i and t together
#' # So, this produces an error
#' \dontrun{
#' safe_join(left, right, expect = c("x", "y"), join = left_join)
#' }
#'
#' # If I realize I'm doing a many-to-one merge, that is correct,
#' # so safe_join will perform it for us
#' safe_join(left, right, expect = "y", join = left_join)
#' @export

safe_join <- function(x, y, expect = NULL, join = NULL, ...) {
  if (!is.character(expect)) {
    stop("expect must be specified as a character.")
  }

  if (max(expect == "1:m") == 1) {
    expect <- "x"
  }
  if (max(expect == "m:1") == 1) {
    expect <- "y"
  }
  if (max(expect == "1:1") == 1) {
    expect <- c("x", "y")
  }

  if (!("x" %in% expect) & !("y" %in% expect)) {
    stop("expect must contain the strings 'x' and/or 'y'")
  }
  if (!is.null(join) & !is.function(join)) {
    stop("join must be NULL or one of the dplyr::join or pmdplyr::inexact_join functions.")
  }

  dots <- list(...)

  # Get the list of matching variables
  if (!is.null(dots[["by"]])) {
    matchvars <- dots[["by"]]
  } else {
    matchvars <- intersect(names(x), names(y))
  }

  # Do the checking for x and y
  errormessagex <- NULL
  errormessagey <- NULL

  if ("x" %in% expect) {

    # If we're doing an inexact_join, there may be a var to consider
    matchvarsx <- matchvars

    if (!rlang::is_empty(dots[["var"]])) {
      matchvarsx <- c(matchvars, dots[["var"]])
    }

    plural <- " "
    if (length(matchvarsx) > 1) {
      plural <- "s "
    }

    if (x %>%
        dplyr::select_at(matchvarsx) %>%
        anyDuplicated() > 0) {
      errormessagex <- paste("The left-hand data set x is not uniquely identified by the matching variable",
        plural, paste0(matchvarsx, collapse = ", "), ".",
        sep = ""
      )
    }
  }
  if ("y" %in% expect) {

    # If we're doing an inexact_join, there may be a var to consider
    matchvarsy <- matchvars
    if (!is.null(dots[["jvar"]])) {
      matchvarsy <- c(matchvars, dots[["jvar"]])
    }

    plural <- " "
    if (length(matchvarsy) > 1) {
      plural <- "s "
    }

    if (y %>%
        dplyr::select_at(matchvarsy) %>%
        anyDuplicated() > 0) {
      errormessagey <- paste("The right-hand data set y is not uniquely identified by the matching variable",
        plural, paste0(matchvarsy, collapse = ", "), ".",
        sep = ""
      )
    }
  }

  # If we found a problem, stop with an error
  if (!is.null(errormessagex) | !is.null(errormessagey)) {
    stop(paste(errormessagex, errormessagey, sep = "\n"))
  }

  # If we're fine, and join is null, just return TRUE.
  if (is.null(join)) {
    return(TRUE)
  }

  # Otherwise, pass on to the function
  return(join(x, y, ...))
}

#' Join two data frames inexactly
#'
#' These functions are modifications of the standard \code{dplyr} \code{join} functions, except that it allows a variable of an ordered type (like date or numeric) in \code{x} to be matched in inexact ways to variables in \code{y}.
#'
#' This allows matching, for example, if one data set contains data from multiple days in the week, while the other data set is weekly. Another example might be matching an observation in one data set to the *most recent* previous observation in the other.
#'
#' The available methods for matching are:
#'
#' \itemize{
#'   \item \code{method = 'last'} matches \code{var} to the closest value of \code{jvar} that is *lower*.
#'   \item \code{method = 'next'} matches \code{var} to the closest value of \code{jvar} that is *higher*.
#'   \item \code{method = 'closest'} matches \code{var} to the closest value of \code{jvar}, above or below. If equidistant between two values, picks the lower of the two.
#'   \item \code{method = 'between'} requires two variables in \code{jvar} which constitute the beginning and end of a range, and matches \code{var} to the range it is in. Make sure that the ranges are non-overlapping within the joining variables, or else you will get strange results (specifically, it should join to the earliest-starting range). If the end of one range is the exact start of another, \code{exact = c(TRUE,FALSE)} or \code{exact = c(FALSE,TRUE)} is recommended to avoid overlaps. Defaults to \code{exact = c(TRUE,FALSE)}.
#' }
#'
#' Note that if, given the method, \code{var} finds no proper match, it will be merged with any \code{is.na(jvar[1])} values.
#'
#' @param x,y,by,copy,suffix,keep,name,... Arguments to be passed to the relevant \code{join} function.
#' @param var Quoted or unquoted variable from the \code{x} data frame which is to be indirectly matched.
#' @param jvar Quoted or unquoted variable(s) from the \code{y} data frame which are to be indirectly matched. These cannot be variable names also in \code{x} or \code{var}.
#' @param method The approach to be taken in performing the indirect matching.
#' @param exact A logical, where \code{TRUE} indicates that exact matches are acceptable. For example, if \code{method = 'last'}, \code{x} contains \code{var = 2}, and \code{y} contains \code{jvar = 1} and \code{jvar = 2}, then \code{exact = TRUE} will match with the \code{jvar = 2} observation, and \code{exact = FALSE} will match with the \code{jvar = 1} observation. If \code{jvar} contains two variables and you want them treated differently, set to \code{c(TRUE,FALSE)} or \code{c(FALSE,TRUE)}.
#' @name inexact_join
#'
#' @examples
#'
#' data(Scorecard)
#' # We also have this data on the December unemployment rate for US college grads nationally
#' # but only every other year
#' unemp_data <- data.frame(
#'   unemp_year = c(2006, 2008, 2010, 2012, 2014, 2016, 2018),
#'   unemp = c(.017, .036, .048, .040, .028, .025, .020)
#' )
#' # I want to match the most recent unemployment data I have to each college
#' Scorecard <- Scorecard %>%
#'   inexact_left_join(unemp_data,
#'     method = "last",
#'     var = year,
#'     jvar = unemp_year
#'   )
#'
#' # Or perhaps I want to find the most recent lagged value (i.e. no exact matches, only recent ones)
#' data(Scorecard)
#' Scorecard <- Scorecard %>%
#'   inexact_left_join(unemp_data,
#'     method = "last",
#'     var = year,
#'     jvar = unemp_year,
#'     exact = FALSE
#'   )
#'
#' # Another way to do the same thing would be to specify the range of unemp_years I want exactly
#' data(Scorecard)
#' unemp_data$unemp_year2 <- unemp_data$unemp_year + 2
#' Scorecard <- Scorecard %>%
#'   inexact_left_join(unemp_data,
#'     method = "between",
#'     var = year,
#'     jvar = c(unemp_year, unemp_year2)
#'   )
NULL

#' @rdname inexact_join
#' @export
inexact_inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = suffix, var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::inner_join(x, y, by = matchvars, copy = copy, suffix = suffix, ...))
  }
  else {
    return(dplyr::inner_join(x, y, copy = copy, suffix = suffix, ...))
  }
}

#' @rdname inexact_join
#' @export
inexact_left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = suffix, var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::left_join(x, y, by = matchvars, copy = copy, suffix = suffix, ...))
  }
  else {
    return(dplyr::left_join(x, y, copy = copy, suffix = suffix, ...))
  }
}

#' @rdname inexact_join
#' @export
inexact_right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }


  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = suffix, var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::right_join(x, y, by = matchvars, copy = copy, suffix = suffix, ...))
  }
  else {
    return(dplyr::right_join(x, y, copy = copy, suffix = suffix, ...))
  }
}

#' @rdname inexact_join
#' @export
inexact_full_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = suffix, var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::full_join(x, y, by = matchvars, copy = copy, suffix = suffix, ...))
  }
  else {
    return(dplyr::full_join(x, y, copy = copy, suffix = suffix, ...))
  }
}

#' @rdname inexact_join
#' @export
inexact_semi_join <- function(x, y, by = NULL, copy = FALSE, ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = c(".x", ".y"), var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::semi_join(x, y, by = matchvars, copy = copy, ...))
  }
  else {
    return(dplyr::semi_join(x, y, copy = copy, ...))
  }
}


#' @rdname inexact_join
#' @export
inexact_nest_join <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = c(".x", ".y"), var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::nest_join(x, y, by = matchvars, copy = copy, keep = FALSE, name = NULL, ...))
  }
  else {
    return(dplyr::nest_join(x, y, copy = copy, keep = FALSE, name = NULL, ...))
  }
}

#' @rdname inexact_join
#' @export
inexact_anti_join <- function(x, y, by = NULL, copy = FALSE, ..., var = NULL, jvar = NULL, method, exact = TRUE) {
  # Pull out variable names
  jvarcall <- tidyselect::vars_select(names(y), {{ jvar }})
  if (length(jvarcall) == 0) {
    jvarcall <- NA_character_
  }
  varcall <- tidyselect::vars_select(names(x), {{ var }})
  if (length(varcall) == 0) {
    varcall <- NA_character_
  }

  # Get the proper matching variable in x
  x <- inexact_join_prep(x = x, y = y, by = by, copy = copy, suffix = c(".x", ".y"), var = varcall, jvar = jvarcall, method = method, exact = exact)

  # If by was specified, extend our list of matching variables. Then join!
  # then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by, jvarcall[1])
    return(dplyr::anti_join(x, y, by = matchvars, copy = copy, ...))
  }
  else {
    return(dplyr::anti_join(x, y, copy = copy, ...))
  }
}

# This is just the function used to find out which observation in y each obs in x goes to
find_matching_list <- function(z, yidname, i, jvar, range) {
  suppressWarnings(
    max(
      z[z[[yidname]] == i & range, ][[jvar[1]]],
      na.rm = TRUE
    )
  )
}

# This is the shared preprocessing shared by all join functions
inexact_join_prep <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), var, jvar, method, exact = TRUE) {
  if (!is.character(var)) {
    stop("Internal error: var should be a character variable with the variable name by this point. Please report errors on https://github.com/NickCH-K/pmdplyr")
  }
  if (!is.character(jvar)) {
    stop("Internal error: jvar should be a character variable with the variable name by this point. Please report errors on https://github.com/NickCH-K/pmdplyr")
  }
  if (length(jvar) > 2) {
    stop("jvar can contain no more than two variables.")
  }
  if (!is.character(method)) {
    stop("method must be a character variable.")
  }
  if (!is.logical(exact)) {
    stop("exact must be logical.")
  }
  if (max(var %in% jvar) == 1) {
    stop("The variable names in var and jvar should not match.")
  }
  if (max(jvar %in% names(x)) == 1) {
    stop("The variable names in jvar should not be in x")
  }
  if (method == "closest" & is.character(x[[var]])) {
    stop("The 'closest' method requires var/jvar to be a variable type that supports subtraction, like numeric or Date.")
  }
  if (method == "closest" & !exact) {
    warning("exact=FALSE is ignored for the 'closest' method.")
  }
  if (!method %in% c("last", "next", "closest", "between")) {
    stop("Acceptable values of 'method' are 'last', 'next', 'closest', and 'between'.")
  }

  # set .exact length properly
  if (length(exact) == 1 & method == "between") {
    if (exact == TRUE) {
      exact <- c(TRUE, FALSE)
    } else {
      stop("For method = 'between', exact must have two elements, or leave as default to get exact = c(TRUE,FALSE)")
    }
  }

  # Get the list of matching variables
  if (!is.null(by)) {
    matchvars <- by
  } else {
    matchvars <- intersect(names(x), names(y))
  }

  # If there aren't any matchvars, create one that's identical for everyone
  if (length(matchvars) == 0) {
    # Figure out longest variable name in either data frame and expand it so we don't overwrite names
    bothnames <- c(names(x), names(y))
    matchvars <- paste(utils::tail(bothnames[order(nchar(bothnames))],1), ".1", sep="")

    x <- x %>%
      dplyr::mutate(!!matchvars := 1)
    y <- y %>%
      dplyr::mutate(!!matchvars := 1)

    # flag the matchvar variable to remove it
    rem_flag <- TRUE
  } else {
    rem_flag <- FALSE
  }

  # We need a unique data set of Group by the set of matching variables and the jvars
  yidname <- uniqname(y)

  z <- y %>%
    dplyr::select_at(c(matchvars, jvar)) %>%
    # collapse all the matchvars into one
    dplyr::mutate(!!yidname := id_variable(.[, matchvars], .method = "character")) %>%
    dplyr::select(-!!matchvars) %>%
    dplyr::ungroup() %>%
    # one observation each
    dplyr::distinct()

  # similar one-column ID for x
  xidname <- uniqname(x)

  x <- x %>%
    dplyr::mutate(!!xidname := id_variable(x[, matchvars], .method = "character"),
                  # We'll be matching additionally on jvar[1]
                  !!jvar[1] := NA)

  # findIntervals lets us do the 'last' method quickly
  if (method == "last") {
    for (i in unique(z[[yidname]])) {
      # get list of jvar values present for this id
      vals <- z %>%
        dplyr::filter_at(yidname, dplyr::any_vars(. == i)) %>%
        dplyr::arrange_at(jvar) %>%
        dplyr::pull(!!jvar)

      # find, by index, which interval each observation fits in
      intervals <- findInterval(x %>%
                                  dplyr::filter_at(xidname, dplyr::any_vars(. == !!i)) %>%
                                  dplyr::pull(!!var),
                                vals, left.open = !exact)

      # Create a version without 0s so it doesn't mess up indexing
      intervalsno0 <- ifelse(intervals == 0, 1, intervals)

      # If it's 0, that's a NA. Otherwise, map back to actual jvar values
      x[x[[xidname]] == i, jvar] <- ifelse(intervals == 0, NA,
        vals[intervalsno0]
      )
    }
  } else if (method == "next") {
    for (i in unique(z[[yidname]])) {
      # get list of jvar values present for this id
      vals <- z %>%
        dplyr::filter_at(yidname, dplyr::any_vars(. == i)) %>%
        dplyr::arrange_at(jvar) %>%
        dplyr::pull(!!jvar)

      # find, by index, which interval each observation fits in
      intervals <- findInterval(x %>%
                                  dplyr::filter_at(xidname, dplyr::any_vars(. == !!i)) %>%
                                  dplyr::pull(!!var),
                                vals, left.open = TRUE)
      # If it's an exact match but exact == FALSE, findInterval will shunt you to the gap BELOW
      # but we want you in the gap ABOVE
      # also, we want to shift the index by 1 so as to match that upper-end number
      if (exact) {
        intervals <- intervals + 1
      } else {
        intervals <- intervals + sapply(
          x %>%
            dplyr::filter_at(xidname, dplyr::any_vars(. == !!i)) %>%
            dplyr::pull(!!var),
          function(w) ifelse(max(w %in% vals) == 1, 2, 1)
        )
      }

      # If it's above the highest number, that will mess things up.
      intervalsno0 <- ifelse(intervals > length(vals), 1, intervals)

      # If it's 0, that's a NA. Otherwise, map back to actual jvar values
      x[x[[xidname]] == i, jvar] <- ifelse(intervals > length(vals), NA,
        vals[intervalsno0]
      )
    }
  } else if (method == "closest") {
    for (i in unique(z[[yidname]])) {
      # get list of jvar values present for this id
      vals <- z %>%
        dplyr::filter_at(yidname, dplyr::any_vars(. == i)) %>%
        dplyr::arrange_at(jvar) %>%
        dplyr::pull(!!jvar)

      # find, by index, which interval each observation fits in
      intervals <- findInterval(x %>%
                                   dplyr::filter_at(xidname, dplyr::any_vars(. == !!i)) %>%
                                   dplyr::pull(!!var), vals)
      # Create a version without 0s so it doesn't mess up indexing
      intervalsno0 <- ifelse(intervals == 0, 1, intervals)

      # Get the appropriate NA type for case_when
      naw <- NA
      class(naw) <- class(vals)

      # fill in appropriate values
      x[x[[xidname]] == i, jvar] <- dplyr::case_when(
        # If x is missing, y is too
        is.na(intervals) ~ naw,
        # if it's 0, take the first value
        intervals == 0 ~ vals[1],
        # if it's beyond the last, take the last value
        intervals >= length(vals) ~ vals[length(vals)],
        # If it's anywhere in the middle, then if the lower one is closer, take that
        abs(vals[intervalsno0] - x[x[[xidname]] == i, var]) <= abs(vals[intervalsno0 + 1] - x[x[[xidname]] == i, var]) ~ vals[intervalsno0],
        # otherwise, take the higher
        TRUE ~ vals[intervalsno0 + 1]
      )
    }
  } else if (method == "between") {
    z <- z %>%
      dplyr::arrange_at(jvar[1])

    # 'between' unfortunately needs this slower version because of potential corner cases
    # Go through each of the values in z and add them to the appropriate x rows
    for (i in 1:nrow(z)) {
      # If it's the same ID, hasn't been matched yet, and x is between the two y's we have our match
      x[Vectorize(isTRUE)(is.na(x[[jvar[1]]]) & x[[xidname]] == z[i, yidname] &
        (x[[var]] > z[i, jvar[1]] | (exact[1] == TRUE & x[[var]] == z[i, jvar[1]])) &
        (x[[var]] < z[i, jvar[2]] | (exact[2] == TRUE & x[[var]] == z[i, jvar[2]]))), jvar[1]] <- z[i, jvar[1]]
    }
  }
  # Get rid of the ID variable we were using
  x <- x %>% select(-!!xidname)

  # Get rid of the matchvar if we created a fake one
  if (rem_flag == TRUE) {
    x <- x %>% select(-!!matchvars)
  }

  return(x)
}
