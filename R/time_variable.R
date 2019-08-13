#' Create a single integer time period index variable
#'
#' This function takes either multiple time variables, or a single Date-class variable, and creates a single integer time variable easily usable with functions in \code{pmdplyr} and other packages like \code{plm} and \code{panelr}.
#'
#' The \code{pmdplyr} library accepts only two kinds of time variables:
#'
#' 1. Ordinal time variables: Variables of any ordered type (\code{numeric}, \code{Date}, \code{character}) where the size of the gap between one value and the next does not matter. So if someone has two observations - one in period 3 and one in period 1, the period immediately before 3 is period 1, and two periods before 3 is missing. Set \code{.d=0} in your data to use this.
#'
#' 2. Cardinal time variables: Numeric variables with a fixed gap between one observation and the next, where the size of that gap is given by \code{.d}. So if \code{.d=1} and someone has two observations - one in period 3 and one in period 1, the period immediately before 3 is missing, and two periods before 3 is period 1.
#'
#' If you would like to have a cardinal time variable but your data is not currently in that format, \code{time_variable()} will help you create a new variable that works with a setting of \code{.d=1}, the default.
#'
#' If you have a date variable that is not in \code{Date} format (perhaps it's a string) and would like to use one of the \code{Date}-reliant methods below, I recommend converting it to \code{Date} using the convenient \code{ymd()}, \code{mdy()}, etc. functions from the \code{lubridate} package. If you only have partial date information (i.e. only year and month) and so converting to a \code{Date} doesn't work, see the \code{.datepos} option below.
#'
#' Methods available include:
#'
#' \itemize{
#'   \item \code{.method="present"} will assume that, even if each individual may have some missing periods, each period is present in your data *somewhere*, and so simply numbers, in order, all the time periods observed in the data.
#'   \item \code{.method="year"} can be used with a single \code{Date}/\code{POSIX}/etc.-type variable (anything that allows \code{lubridate::date()}) and will extract the year from it. Or, use it with a character or numeric variable and indicate with \code{.datepos} the character/digit positions that hold the year in YY or YYYY format.  If combined with \code{.breaks} or \code{.skip}, will instead set the earliest year in the data to 1 rather than returning the actual year.
#'   \item \code{.method="month"} can be used with a single \code{Date}/\code{POSIX}/etc.-type variable (anything that allows \code{lubridate::date()}). It will give the earliest-observed month in the data set a value of \code{1}, and will increment from there. Or, use it with a character or numeric variable and indicate with \code{.datepos} the character/digit positions that hold the year and month in YYMM or YYYYMM format (note that if your variable is in MMYYYY format, for example, you can just give a \code{.datepos} argument like \code{c(3:6,1:2)}). Months turn over on the \code{.start} day of the month, which is by default 1.
#'   \item \code{.method="week"} can be used with a single \code{Date}/\code{POSIX}/etc.-type variable (anything that allows \code{lubridate::date()}). It will give the earliest-observed week in the data set a value of \code{1}, and will increment from there. Weeks turn over on the \code{.start} day, which is by default 1 (Monday). Note that this method always starts weeks on the same day of the week, which is different from standard \code{lubridate} procedure of counting sets of 7 days starting from January 1.
#'   \item \code{.method="day"} can be used with a single \code{Date}/\code{POSIX}/etc.-type variable (anything that allows \code{lubridate::date()}). It will give the earliest-observed day in the data set a value of \code{1}, and increment from there. Or, use it with a character or numeric variable and indicate with \code{.datepos} the character/digit positions that hold the year and month in YYMMDD or YYYYMMDD format. To skip certain days of the week, such as weekends, use the \code{.skip} option.
#'   \item \code{.method="turnover"} can be used when you have more than one variable in variable and they are all numeric nonnegative integers. Set the \code{.turnover} option to indicate the highest value each variable takes before it starts over, and set \code{.turnover_start} to indicate what value it takes when it starts over. Cannot be combined with \code{.skip} or \code{.breaks}. Doesn't work with any variable for which the turnover values change, i.e. it doesn't play well with days-in-month - if you'd like to do something like year-month-day-hour, I recommend running \code{.method="day"} once with just the year-month-day variable, and then taking the result and combining *that* with hour in \code{.method="turnover"}.
#' }
#'
#' @param ... variables (vectors) to be used to generate the time variable, in order of increasing specificity. So if you have a variable each for year, month, and day (with the names year, month, and day), you would use \code{year,month,day} (if a data set containing those variables has been attached using \code{with} or \code{dplyr}) or \code{data$year,data$month,data$day} (if not).
#' @param .method The approach that will be taken to create your variable. See below for the options. By default, this is \code{.method = "present"}.
#' @param .datepos A numeric vector containing the character/digit positions, in order, of the YY or YYYY year (or year/month in YYMM or YYYYMM format, or year/month/day in YYMMDD or YYYYMMDD) for the \code{.method="year"}, \code{.method="month"}, or \code{.method="day"} options, respectively. Give it only the data it needs - if you give \code{.method="year"} YYMM information, it will assume you're giving it YYYY and mess up. For example, if dates are stored as a character variable in the format '2013-07-21' and you want the year and month, you might specify \code{.datepos=c(1:4,6:7)}. If two-digit year is given, \code{.datepos} uses the \code{lubridate} package to determine century.
#' @param .start A numeric variable indicating the day of the week/month that begins a new week/month, if \code{.method="week"} or \code{.method="month"} is used. By default, 1, where for \code{.method=week} 1 is Monday, 7 Sunday. If used with \code{.method="month"}, the time data should include day as well.
#' @param .skip A numeric vector containing the values of year, month, or day-of-week (where Monday = 1, Sunday = 7, no matter what value \code{.start} takes) you'd like to skip over (for \code{.method="year","month","week","day"}, respectively). For example, with \code{.method="month"} and \code{.skip=12}, an observation in January would be determined to come one period after November. Commonly this might be \code{.skip=c(6,7)} with \code{.method="day"} to skip weekends so that Monday immediately follows Friday. If \code{.breaks} is also specified, select the values of \code{.breaks} you would like to skip, but do be aware that combining \code{.skip} and \code{.breaks} can be tricky.
#' @param .breaks A numeric vector containing the starting breakpoints of year or month you'd like to clump together (for \code{.method="year','month"}, respectively). Commonly, this might be \code{.breaks=c(1,4,7,10)} with \code{.method="month"} to go by quarter-year. The first element of \code{.breaks} should usually be 1.
#' @param .turnover A numeric vector the same length as the number of variables included indicating the maximum value that the corresponding variable in the list of variables takes, where NA indicates no maximum value, for use with \code{.method="turnover"} and required for that method. For example, if the variable list is \code{year,month} then you might have \code{.turnover=c(NA,12)}. Or if the variable list is \code{days-since-jan1-1970,hour,minute,second} you might have \code{.turnover=c(NA,23,59,59)}. Defaults to the maximum observed value of each variable if not specified, and NA for the first variable. Note that in almost all cases, the first element of \code{.turnover} should be \code{NA}, and all others should be non-NA.
#' @param .turnover_start A numeric vector the same length as the number of variables included indicating the minimum value that the corresponding variable in the list of variables takes, where NA indicates no minimum value, for use with \code{method="turnover"}. For example, if the variable list is \code{year,month} then you might have \code{.turnover=c(NA,1)}. Or if the variable list is \code{days-since-jan1-1970,hour,minute,second} you might have \code{.turnover=c(NA,0,0,0)}. By default this is a vector of 1s the same length as the number of variables, except for the first element, which is NA. Note that in almost all cases, the first element of \code{.turnover_start} should be \code{NA}, and all others should be non-NA.
#' @examples
#'
#' data(SPrail)
#'
#' # Since we have a date variable, we can easily create integers that increment for each
#' # year, or for each month, etc.
#' # Likely we'd only really need one of these four, depending on our purposes
#' SPrail <- SPrail %>%
#'   dplyr::mutate(
#'     year_time_id = time_variable(insert_date, .method = "year"),
#'     month_time_id = time_variable(insert_date, .method = "month"),
#'     week_time_id = time_variable(insert_date, .method = "week"),
#'     day_time_id = time_variable(insert_date, .method = "day")
#'   )
#'
#' # Perhaps I'd like quarterly data
#' # (although in this case there are only two months, not much variation there)
#' SPrail <- SPrail %>%
#'   dplyr::mutate(quarter_time_id = time_variable(insert_date,
#'     .method = "month",
#'     .breaks = c(1, 4, 7, 10)
#'   ))
#' table(SPrail$month_time_id, SPrail$quarter_time_id)
#'
#' # Maybe I'd like Monday to come immediately after Friday!
#' SPrail <- SPrail %>%
#'   dplyr::mutate(weekday_id = time_variable(insert_date,
#'     .method = "day",
#'     .skip = c(6, 7)
#'   ))
#'
#' # Perhaps I'm interested in ANY time period in the data and just want to enumerate them in order
#' SPrail <- SPrail %>%
#'   dplyr::mutate(any_present_time_id = time_variable(insert_date,
#'     .method = "present"
#'   ))
#'
#'
#' # Maybe instead of being given a nice time variable, I was given it in string form
#' SPrail <- SPrail %>% dplyr::mutate(time_string = as.character(insert_date))
#' # As long as the character positions are consistent we can still use it
#' SPrail <- SPrail %>%
#'   dplyr::mutate(day_from_string_id = time_variable(time_string,
#'     .method = "day",
#'     .datepos = c(3, 4, 6, 7, 9, 10)
#'   ))
#' # Results are identical
#' cor(SPrail$day_time_id, SPrail$day_from_string_id)
#'
#'
#' # Or, maybe instead of being given a nice time variable, we have separate year and month variables
#' SPrail <- SPrail %>%
#'   dplyr::mutate(
#'     year = lubridate::year(insert_date),
#'     month = lubridate::month(insert_date)
#'   )
#' # We can use the turnover method to tell it that there are 12 months in a year,
#' # and get an integer year-month variable
#' SPrail <- SPrail %>%
#'   dplyr::mutate(month_from_two_vars_id = time_variable(year, month,
#'     .method = "turnover",
#'     .turnover = c(NA, 12)
#'   ))
#' # Results are identical
#' cor(SPrail$month_time_id, SPrail$month_from_two_vars_id)
#'
#' # I could also use turnover to make the data hourly.
#' # Note that I'm using the day variable from earlier to avoid having
#' # to specify when day turns over (since that could be 28, 30, or 31)
#' SPrail <- SPrail %>%
#'   dplyr::mutate(hour_id = time_variable(day_time_id, lubridate::hour(insert_date),
#'     .method = "turnover",
#'     .turnover = c(NA, 23),
#'     .turnover_start = c(NA, 0)
#'   ))
#' # This could be easily extended to make the data by-minute, by-second, etc.
#' @export

time_variable <- function(..., .method = "present", .datepos = NA, .start = 1, .skip = NA, .breaks = NA, .turnover = NA, .turnover_start = NA) {

  # R seems to think these are global variables because they are created by dplyr
  # avoid notes
  rawdate <- NA
  timevarM <- NA
  wksb4 <- NA

  ########################################## CHECK INPUTS
  if (!(.method %in% c("present", "year", "month", "week", "day", "turnover"))) {
    stop("Unrecognized time_variable .method.")
  }

  data <- tibble::tibble(...)
  var <- names(data)

  ########################################## METHOD = PRESENT
  if (.method == "present") {
    timevar <- data
    origordername <- uniqname(timevar)
    timevar <- timevar %>%
      dplyr::mutate(!!origordername := 1:nrow(.)) %>%
      dplyr::arrange_at(var) %>%
      # Identify new dates
      dplyr::mutate_at(var, function(x) x != dplyr::lag(x) | is.na(x) & is.na(dplyr::lag(x)) | dplyr::row_number() == 1)

    newdate <- uniqname(timevar)

    if (length(var) == 1) {
      timevar <- timevar %>%
        dplyr::select_at(c(origordername, var)) %>%
        dplyr::rename_at(2, function(x) newdate)
    }
    else {
      timevar <- timevar %>%
        # Any new date on any var
        dplyr::mutate(!!newdate := rowSums(timevar %>% dplyr::select(!!var)) > 0) %>%
        dplyr::select_at(c(origordername, newdate))
    }

    timevar <- timevar %>%
      # and count up how many there have been
      dplyr::mutate(timevar = cumsum(.[[newdate]])) %>%
      dplyr::arrange_at(origordername)

    timevar <- timevar$timevar
  }
  ########################################## METHOD = YEAR
  else if (.method == "year") {
    # Standard date prep
    timevar <- date_methods_prep(data, var, .method, .start, .datepos, .skip, .breaks)

    # extract the year and place it in a data frame.
    # Before any options are implemented, timevar is just the year
    td <- tibble::tibble(
      rawyear = lubridate::year(timevar),
      timevar = lubridate::year(timevar)
    )

    # Now implement all the options!!
    # For year we only have .skip and .breaks to contend with
    # Now do .breaks
    if (!anyNA(.breaks)) {

      # Check that .breaks are integers
      if (min(as.integer(.breaks) == .breaks) == 0) {
        stop("All elements of .breaks must be integers. These numbers represent years.")
      }

      # first, check if you're starting your .breaks after the beginning of years in the data and issue a warning
      if (min(td$timevar, na.rm = TRUE) < min(.breaks)) {
        warning("The first break in .breaks is after the first year in the data.
Years earlier than the first break will be given a missing time value.")
        td <- td %>%
          dplyr::mutate(timevar = dplyr::case_when(
            timevar >= min(.breaks, na.rm = TRUE) ~ timevar
          ))
      }


      # Create a lookup table so that there is a gap of 1 between different .breaks
      breakstb <- data.frame(
        .breaks = .breaks,
        timevar = 1:length(.breaks)
      )
      # Set your year value to the most recent break, and merge with the lookup table
      td <- td %>%
        dplyr::mutate(.breaks = sapply(td$timevar, FUN = function(x) max(.breaks[.breaks <= x]))) %>%
        # get rid of timevar and reform
        dplyr::select(-timevar) %>%
        dplyr::left_join(breakstb, by = ".breaks")
    }
    # Do .skip afterwards so that it can integrate break-.skips.
    if (!anyNA(.skip)) {

      # Check that .skips are integers
      if (min(as.integer(.skip) == .skip) == 0) {
        stop("All elements of .skip must be integers. These numbers represent years.")
      }

      # First, if .breaks has already been run, convert the .skips vector into the current numbering
      if (!anyNA(.breaks)) {
        .skip <- data.frame(rawyear = .skip) %>%
          dplyr::left_join(td, by = "rawyear") %>%
          dplyr::pull(timevar)
        if (anyNA(.skip)) {
          stop("Use of the .breaks option means you're trying to .skip years that are no longer there. Check specification.")
        } else {
          warning("Combining .breaks with .skip will skip the whole break.")
        }
      }

      # then, check if you're .skipping any years that are present in the data and issue a warning
      if (sum(td$timevar %in% .skip) > 0) {
        warning(".skip includes some years that are present in data (or in the .breaks vector for a break with years in the data).
Observations in these years will be given a missing time value.")
        td <- td %>%
          dplyr::mutate(timevar = dplyr::case_when(
            !(timevar %in% .skip) ~ timevar
          ))
      }

      # Finally, for each year we have, count up the number of lesser .skipped years
      # and subtract that many
      td <- td %>%
        dplyr::mutate(timevar = sapply(td$timevar, FUN = function(x) x - sum(x > .skip))) %>%
        # then, since the year variable itself is meaningless now, reorient to start at 1
        # ungroup just in case
        dplyr::ungroup() %>%
        dplyr::mutate(timevar = timevar - min(timevar, na.rm = TRUE) + 1)
    }

    # And extract our completed timevar
    timevar <- td$timevar
  }
  ########################################## METHOD = MONTH
  else if (.method == "month") {
    # Standard date prep
    timevar <- date_methods_prep(data, var, .method, .start, .datepos, .skip, .breaks)

    # Apply the ".start" option
    timevar <- dplyr::case_when(
      lubridate::day(timevar) < .start ~ timevar %m-% months(1),
      TRUE ~ timevar
    )

    # extract the year and place it in a data frame.
    # Before any options are implemented, timevar is just the date
    td <- data.frame(
      rawdate = timevar,
      timevarY = lubridate::year(timevar),
      timevarM = lubridate::month(timevar)
    )

    # To start out with there are 12 months in a year, may be reduced by .breaks or .skips
    nmonths <- 12

    # Now implement all the options!!

    # Now do .breaks
    if (!anyNA(.breaks)) {
      # Check that .breaks are numbers 1 to 12
      if (min(.breaks %in% 1:12) == 0) {
        stop("All elements of .breaks must be integers 1 to 12. These numbers represent months.")
      }
      # first, check if you're starting your .breaks after the beginning of months in the data and issue a warning
      if (min(td$timevarM, na.rm = TRUE) < min(.breaks, na.rm = TRUE)) {
        warning("The first break in .breaks is after the first month of the year present in data.
Months earlier than the first break will be given a missing time value.
Note that when .method='month', the first element of .breaks should usually be 1.")
        td <- td %>%
          dplyr::mutate(timevarM = dplyr::case_when(
            timevarM >= min(.breaks, na.rm = TRUE) ~ timevarM
          ))
      }


      # Create a lookup table so that there is a gap of 1 between different .breaks
      breakstb <- data.frame(
        .breaks = .breaks,
        timevarM = 1:length(.breaks)
      )
      # Now this is how many months we have
      nmonths <- length(.breaks)

      # Set your month value to the most recent break, and merge with the lookup table
      td <- td %>%
        dplyr::mutate(.breaks = sapply(td$timevarM, FUN = function(x) {
          if (is.na(x)) {
            NA
          } else {
            max(.breaks[.breaks <= x])
          }
        })) %>%
        # get rid of timevar and reform
        dplyr::select(-timevarM) %>%
        dplyr::left_join(breakstb, by = ".breaks")
    }

    # Do .skip afterwards so that it can integrate break-.skips.
    if (!anyNA(.skip)) {

      # Check that .skips are numbers 1 to 12
      if (min(.skip %in% 1:12) == 0) {
        stop("All elements of .skip must be integers 1 to 12. These numbers represent months.")
      }

      # First, if .breaks has already been run, convert the .skips vector into the current numbering
      if (!anyNA(.breaks)) {
        .skip <- data.frame(rawM = .skip) %>%
          dplyr::left_join(td %>%
            dplyr::mutate(rawM = lubridate::month(rawdate)),
          by = "rawM"
          ) %>%
          dplyr::pull(timevarM)
        if (anyNA(.skip)) {
          stop("Use of the .breaks option means you're trying to .skip months that are no longer there. Check specification.")
        } else {
          warning("Combining .breaks with .skip will skip the whole break.")
        }
      }

      # then, check if you're .skipping any years that are present in the data and issue a warning
      if (sum(td$timevarM %in% .skip) > 0) {
        warning(".skip includes some months that are present in data (or in the .breaks vector for a break with months in the data).
Observations in these months will be given a missing time value.")
        td <- td %>%
          dplyr::mutate(timevarM = dplyr::case_when(
            !(timevarM %in% .skip) ~ timevarM
          ))
      }

      # Finally, for each month we have, count up the number of lesser .skipped months
      # and subtract that many
      td <- td %>%
        dplyr::mutate(timevarM = sapply(td$timevarM, FUN = function(x) x - sum(x > .skip))) %>%
        # then, since the year variable itself is meaningless now, reorient to start at 1
        # ungroup just in case
        dplyr::ungroup() %>%
        dplyr::mutate(timevarM = timevarM - min(timevarM, na.rm = TRUE) + 1)

      # the new number of months we're dealing with
      nmonths <- nmonths - length(.skip)
    }

    # NOW implement turnover method using years and months
    timevar <- time_variable_turnover(td$timevarY, td$timevarM, .turnover = c(NA, nmonths), .turnover_start = c(NA, 1))
  }
  ########################################## METHOD = WEEK
  else if (.method == "week") {
    if (min(is.na(.skip)) == 0 | min(is.na(.breaks)) == 0) {
      warning(".skip and .breaks options not available with .method='week'. Proceeding, ignoring these options.")
    }

    # Standard date prep
    timevar <- date_methods_prep(data, var, .method, .start, .datepos, .skip, .breaks)

    # Find the first date
    firstday <- min(timevar)

    # Find the first day of the week that first day is on, in days since origin
    fdfw <- as.numeric(firstday - (lubridate::wday(firstday) - .start))

    # Convert timevar to days since origin
    timevar <- as.numeric(timevar)

    # Count in sets of 7 from that first day of the first week
    timevar <- floor((timevar - fdfw) / 7) + 1
  }
  ########################################## METHOD = DAY
  else if (.method == "day") {
    if (min(is.na(.breaks)) == 0) {
      warning(".breaks option not available with .method='day'. Proceeding, ignoring this option.")
    }

    # Standard date prep
    timevar <- date_methods_prep(data, var, .method, .start, .datepos, .skip, .breaks)

    # implement .skips
    if (min(is.na(.skip)) == 0) {
      # Find the first day of the first week in the data, in days from origin
      firstday <- min(timevar)
      # Find the Monday of the week that first day is on, in days since origin
      fdfw <- as.numeric(firstday - (lubridate::wday(firstday) - 1))

      td <- data.frame(timevar = timevar) %>%
        # get day of week, and number of weeks in data BEFORE this one
        dplyr::mutate(
          wday = lubridate::wday(timevar),
          wksb4 = floor((as.numeric(timevar) - fdfw) / 7)
        )
      # For each dow .skipped, subtract one day for each week in the data before
      # and ALSO subtract one day for each week in THIS week if the .skip comes before  (the sapply)
      td <- td %>%
        dplyr::mutate(timevar = timevar - length(.skip) * wksb4 - sapply(td$wday, function(x) sum(.skip < x)))

      # If the day you're on was .skipped, you should be NA
      if (sum(td$wday %in% .skip) > 0) {
        warning(".skip includes some days of week that are present in the data.
Observations in these weekdays will be given a missing time value.")
        td <- td %>%
          dplyr::mutate(timevar = dplyr::case_when(
            !(wday %in% .skip) ~ timevar
          ))
      }
      timevar <- td$timevar
      rm(td)
    }
    # Turn numeric and reorient to start at 1
    timevar <- as.numeric(timevar)
    timevar <- timevar - min(timevar, na.rm = TRUE) + 1
  }
  ########################################## METHOD = TURNOVER
  else if (.method == "turnover") {
    # This method is set out in its own function so it can be called by others.
    timevar <- time_variable_turnover(data, .turnover = .turnover, .turnover_start = .turnover_start)
  }

  return(as.integer(timevar))
}


################### THE TURNOVER METHOD
# also a subfunction to make the month method work.
time_variable_turnover <- function(..., .turnover = NA, .turnover_start = NA) {
  data <- data.frame(...)
  var <- names(data)

  # Fill in if missing
  if (min(is.na(.turnover_start)) == 1) {
    .turnover_start <- c(NA, rep(1, length(var) - 1))
  }
  if (min(is.na(.turnover)) == 1) {
    .turnover <- t(as.vector((data %>%
      dplyr::select_at(var) %>%
      dplyr::summarize_all(max))[1, ]))
    .turnover[1] <- NA
  }
  # Check inputs
  if (!is.na(.turnover[1]) | max(is.na(.turnover[-1])) == 1) {
    warning("The turnover .method may produce strange results if the first element of .turnover is not NA, or if any of the later elements are NA. Proceeding, but be cautious...")
  }
  # Check that there's more than one variable in var, and that those vars are present in the data.
  if (length(var) < 2) {
    stop("To use the turnover .method, there must be more than one variable in the variable list.")
  }
  if (min(sapply(data, is.numeric)) == 0) {
    stop("To use the turnover .method, all variables in the variable list must be numeric.")
  }
  if (min(sapply(data, function(x) as.integer(x) == x), na.rm = TRUE) == 0 | min(data, na.rm = TRUE) < 0) {
    stop("To use the turnover .method, all variables in the variable list must be nonnegative integers.")
  }
  if (length(.turnover) != length(var) | length(.turnover_start) != length(var)) {
    stop("To use the turnover .method, the number of variables in the variable list, .turnover, and .turnover_start must all be the same.")
  }
  if (sum(dplyr::summarize_all(data, max) > .turnover, na.rm = TRUE) > 0 | sum(dplyr::summarize_all(data, min) < .turnover_start, na.rm = TRUE) > 0) {
    stop("All values must be within the minima and maxima given in .turnover_start and .turnover")
  }

  # do a copy
  td <- data

  # Set everything to start at 1
  td <- data.frame(sapply(1:length(var), function(x) {
    if (is.na(.turnover[x])) {
      td[, x] - min(td[, x], na.rm = TRUE) + 1
    }
    else {
      td[, x] - .turnover_start[x] + 1
    }
  }))

  # And reduce the top-end to match
  .turnover <- ifelse(is.na(.turnover), NA, .turnover - .turnover_start + 1)

  # Give something to look ahead to that won't change the power
  .turnover[length(var) + 1] <- 1

  # Multiply by product of possibilities downstream to make room for every period
  td <- sapply(1:length(var), function(x) td[, x] * prod(.turnover[(x + 1):(length(var) + 1)]))

  timevar <- rowSums(td)

  # Start at 1
  timevar <- timevar - min(timevar, na.rm = TRUE) + 1
  return(timevar)
}

#### Basic input checking for all the date-based methods, performs date extraction, and applies .datepos if necessary
date_methods_prep <- function(data, var, .method, .start, .datepos, .skip, .breaks) {
  if (length(var) > 1) {
    stop("Date-based .methods allow only one variable in var.")
  }
  if (!lubridate::is.timepoint(data[[var]]) & min(is.na(.datepos)) == 1) {
    stop("Date-based .methods require a timepoint-class (Date, POSIX, etc.) variable as input, or that the .datepos argument be specified.")
  }
  if (!is.na(.start) & !is.numeric(.start)) {
    stop(".start must be numeric.")
  }
  if (min(is.na(.datepos)) == 0 & !is.numeric(.datepos)) {
    stop(".datepos must be a numeric vector.")
  }
  if (min(is.na(.datepos)) == 0 & max(is.na(.datepos)) == 1) {
    stop(".datepos must not contain any missing values.")
  }
  if (min(is.na(.skip)) == 0 & !is.numeric(.skip)) {
    stop(".skip must be a numeric vector.")
  }
  if (min(is.na(.breaks)) == 0 & !is.numeric(.breaks)) {
    stop(".breaks must be a numeric vector.")
  }
  if (.method == "week" & (min(is.na(.datepos)) == 0 | !lubridate::is.timepoint(data[[var]]))) {
    stop("The week .method requires a timepoint-class (Date, POSIX, etc.) class variable as input, and does not allow .datepos.")
  }
  if (min(is.na(.skip)) == 0 & .method == "day") {
    if (sum(.skip %in% 1:7) < length(.skip)) {
      stop("When .method='day', All elements of .skip must be integers from 1 to 7. These represent days of the week from Monday (1) to Sunday (7).")
    }
  }
  # .skips and .breaks should be unique
  .breaks <- unique(.breaks)
  .skip <- unique(.skip)

  # If there's a .datepos, extract the date
  # If not, just take the date variable
  if (min(is.na(.datepos)) == 0) {
    # Date as string
    timevar <- sapply(data[[var]], function(x) paste0(substring(x, .datepos, .datepos), collapse = ""))

    # Fill out date for lubridate conversion
    if (.method == "year") {
      if (!(length(.datepos) %in% c(2, 4))) {
        stop("With .method='year', .datepos must be in YY or YYMM format.")
      }
      timevar <- paste(timevar, "0101", sep = "")
    }
    if (.method == "month") {
      if (!(length(.datepos) %in% c(4, 6))) {
        stop("With .method='month', .datepos must be in YYMM or YYYYMM format.")
      }
      timevar <- paste(timevar, "01", sep = "")
    }
    if (.method == "day") {
      if (!(length(.datepos) %in% c(6, 8))) {
        stop("With .method='day', .datepos must be in YYMMDD or YYYYMMDD format.")
      }
    }

    # and convert
    timevar <- lubridate::ymd(timevar)
  }
  else {
    # use date here so as to get rid of h/m/s
    timevar <- lubridate::date(data[[var]])
  }

  return(timevar)
}
