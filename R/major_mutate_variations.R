#' Function to perform mutate one time period at a time ('Cascading mutate')
#'
#' WARNING: FOR NOW, ALWAYS SPECIFY THE PANEL STRUCTURE EVERY TIME YOU CALL THIS FUNCTION. PDECLARE STATUS IS DROPPED BY MOST FUNCTIONS.
#'
#' This function is a wrapper for \code{dplyr::mutate()} which performs \code{mutate} one time period at a time, allowing each period's calculation to complete before moving on to the next. This allows changes in one period to 'cascade down' to later periods. This is (number of time periods) slower than regular \code{mutate()} and, generally, is only used for mutations where an existing variable is being defined in terms of its own \code{lag()} or \code{tlag()}. This is similar in concept to (and also slower than) \code{cumsum} but is much more flexible, and works with data that has multiple observations per individual-period using \code{tlag()}. For example, this could be used to calculate the current value of a savings account given a variable with each period's deposits, withdrawals, and interest, or could calculate the cumulative number of credits a student has taken across all classes.
#'
#' To apply \code{mutate_cascade()} to non-panel data and without any grouping (perhaps to mimic standard Stata \code{replace} functionality), add a variable to your data indicating the order you'd like \code{mutate} performed in (perhaps using \code{dplyr::row_number()}) and \code{.t} equal to that new variable.
#'
#' @param .df Data frame or tibble.
#' @param ... Specification to be passed to \code{mutate()}.
#' @param .skip Set to \code{TRUE} to skip the first period present in the data (or present within each group for grouped data) when applying \code{mutate()}. Since most uses of \code{mutate_cascade()} will involve a \code{lag()} or \code{tlag()}, this avoids creating an \code{NA} in the first period that then cascades down. By default this is TRUE. If you set this to FALSE you should probably have some method for avoiding a first-period \code{NA} in your \code{...} entry, perhaps using the \code{default} option in \code{dplyr::lag} or the \code{.default} option in \code{tlag}.
#' @param .backwards Set to \code{TRUE} to run \code{mutate_cascade()} from the last period to the first, rather than from the first to the last.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{mutate_cascade} will group the data by \code{.i}, ignoring any grouping already implemented (although the original grouping structure will be returned at the end). Set \code{.group_i = FALSE} to avoid this.
#' @param .i Character or character vector with the variable names that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{pdeclare()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @param .setpanel Logical parameter. Set to FALSE to return data with the same \code{.i}, \code{.t}, \code{.d} attributes it came in with, even if those are null. TRUE by default, but ignored if \code{.i}, \code{.t}, and \code{.d} are all NA.
#' @examples
#'
#' library(magrittr)
#' data(Scorecard)
#' #I'd like to build a decaying function that remembers previous earnings but at a declining rate
#' #Let's only use nonmissing earnings
#' Scorecard <- Scorecard %>%
#'     dplyr::filter(!is.na(earnings_med))
#' Scorecard <- Scorecard %>%
#'     #Almost all instances involve a variable being set to a function of a lag of itself
#'     #we don't want to overwrite so let's make another
#'     dplyr::mutate(decay_earnings = earnings_med) %>%
#'     #Now we can cascade
#'     dplyr::mutate(decay_earnings = decay_earnings +
#'     .5*tlag(decay_earnings,Scorecard,.i='unitid',.t='year',.quick=TRUE))
#'
#'
#' @export

mutate_cascade <- function(.df,...,.skip=TRUE,.backwards=FALSE,.group_i=TRUE,.i=NA,.t=NA,.d=NA,.uniqcheck=FALSE,.setpanel=TRUE) {
  if (!is.logical(.backwards)) {
    stop('.backwards must be TRUE or FALSE')
  }
  if (!is.logical(.skip)) {
    stop('.skip must be TRUE or FALSE.')
  }
  if (!is.logical(.group_i)) {
    stop('.group_i must be TRUE or FALSE.')
  }

  #Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df,.i,.t,.d,.uniqcheck,.setpanel)
  if (is.na(inp$t)) {
    stop('mutate_cascade() requires that .t be declared either in the function or by pdeclare().')
  }

  #Panel-declare data if any changes have been made.
  if (min(is.na(.i)) == 0 | !is.na(.t) | !is.na(.d)) {
    .df <- pdeclare(.df,.i=.i,.t=.t,.d=.d,.uniqcheck=.uniqcheck)

    #.d might be unspecified and so inp$d is NA, but now .d is 1 from pdeclare default
    inp$d <- attr(.df,'.d')
  }

  if (.group_i == TRUE & (min(is.na(inp$i)) == 0)) {
    .df <- .df %>%
      dplyr::group_by_at(inp$i)
  }

  .df[,ncol(.df)+1] <- 1:nrow(.df)
  #Figure out (within groups if present) first period so it can be skipped
  .df[,ncol(.df)+1] <- (.df %>%
                              dplyr::mutate_at(inp$t,min))[[inp$t]]
  indexnames <- names(.df)[(ncol(.df)-1):ncol(.df)]

  #Do an explicit loop because each iteration needs to complete before moving on
  list_of_times <- sort(unique(.df[[.t]]))
  if (.backwards==TRUE) {
    list_of_times <- rev(list_of_times)
  }

  for (t in list_of_times) {
    #Perform manipulation on the whole thing (it will need access for lags)
    #but only store the current working part
    .df <- dplyr::bind_rows(
      #part of data being kept
      .df[.df[[inp$t]] != t | .df[[indexnames[2]]] == t,],
      #and the part of the data not kept, and mutated
      (.df %>% dplyr::mutate(...))[.df[[inp$t]] == t & .df[[indexnames[2]]] != t,]
    ) %>%
      dplyr::arrange_at(indexnames[1])
  }

  .df[,indexnames] <- NULL

  #If it wants the original panel setting back, do that
  if (.setpanel == FALSE) {
    attr(.df,'.i') <- inp$orig_i
    attr(.df,'.t') <- inp$orig_t
    attr(.df,'.d') <- inp$orig_d
  }

  return(.df)
}

#' Function to propogate a calculation performed on a subset of data to the rest of the data
#'
#' WARNING: FOR NOW, ALWAYS SPECIFY THE PANEL STRUCTURE WHEN USING THIS FUNCTION. PDECLARE STATUS IS DROPPED BY MOST FUNCTIONS.
#'
#' This function performs \code{dplyr::summarize} on a \code{.filter}ed subset of data. Then it applies the result to all observations (or all observations in the group, if applied to grouped data), filling in columns of the data with the summarize results, as though \code{dplyr::mutate} had been run.
#'
#' One application of this is to partially widen data. For example, if your analysis uses childhood height as a control variable in all years, \code{mutate_subset()} could be used to easily generate a \code{height_age10} variable from a \code{height} variable.
#'
#' @param .df Data frame or tibble.
#' @param ... Specification to be passed to \code{dplyr::summarize()}.
#' @param .filter A vector of the logical condition for which observations \code{dplyr::summarize()} operations are to be run on.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{mutate_cascade} will group the data by \code{.i}, overwriting any grouping already implemented. Set \code{.group_i = FALSE} to avoid this.
#' @param .i Character or character vector with the variable names that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{pdeclare()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @param .setpanel Logical parameter. Set to FALSE to return data with the same \code{.i}, \code{.t}, \code{.d} attributes it came in with, even if those are null. TRUE by default, but ignored if \code{.i}, \code{.t}, and \code{.d} are all NA.
#' @examples
#'
#' library(magrittr)
#' data(SPrail)
#' #In preparation for fitting a choice model for how people choose ticket type,
#' #I'd like to know the price of a "Promo" ticket for a given route
#' #So that I can compare each other type of ticket price to that type
#' SPrail <- SPrail %>%
#' mutate_subset(promo_price = mean(price,na.rm=TRUE),
#'              .filter=SPrail$fare=='Promo',
#'              .i = c('origin','destination'))
#'
#'
#' @export

mutate_subset <- function(.df,...,.filter,.group_i=TRUE,.i=NA,.t=NA,.d=NA,.uniqcheck=FALSE,.setpanel=TRUE) {
  ####CHECK INPUTS
  if (sum(class(.df) %in% c('data.frame','tbl','tbl_df')) == 0) {
    stop('Requires data to be a data frame or tibble.')
  }
  if (sum(class(.df) == 'data.table') > 0) {
    warning('pmdplyr functions have not been tested with data.tables')
  }
  if (!is.vector(.filter) | !is.logical(.filter)) {
    stop('.filter must be a logical vector.')
  }

  inp <- declare_in_fcn_check(.df,.i,.t,.d,.uniqcheck,.setpanel,.noneed=TRUE)

  #Panel-declare data if any changes have been made.
  if (min(is.na(.i)) == 0 | !is.na(.t) | !is.na(.d)) {
    .df <- pdeclare(.df,.i=.i,.t=.t,.d=.d,.uniqcheck=.uniqcheck)

    #.d might be unspecified and so inp$d is NA, but now .d is 1 from pdeclare default
    inp$d <- attr(.df,'.d')
  }


  if (.group_i == TRUE & (min(is.na(inp$i)) == 0)) {
    .df <- .df %>%
      dplyr::group_by_at(inp$i)
  }

  #Perform the summary on the subset
  summ <- .df[.filter,]
  summ <- summ %>%
    dplyr::summarize(...)
  #See what variables were created not counting the groupings
  #First, get the grouping variables
  groups <- names(attr(.df,"groups"))
  #Last element is .rows
  if (!is.null(groups)) {groups <- groups[1:(length(groups)-1)]}
  #now, find which variables in summ are not grouping variables
  notgroups <- names(summ)[!(names(summ) %in% groups)]
  #and drop those variables from .df, then bring in summ!
  if (is.null(groups)) {
    summdf <- as.data.frame(summ[rep(1,nrow(.df)),])
    names(summdf) <- notgroups

    suppressWarnings(try(.df <- .df %>%
                           dplyr::select(-dplyr::one_of(notgroups))))
    suppressWarnings(.df <- .df %>%
      dplyr::bind_cols(summdf))
  }
  else {
    suppressWarnings(try(.df <- .df %>%
                           dplyr::select(-dplyr::one_of(notgroups))))
    suppressWarnings(.df <- .df %>%
      dplyr::left_join(summ,by=groups))
  }

  #If it wants the original panel setting back, do that
  if (.setpanel == FALSE) {
    attr(.df,'.i') <- inp$orig_i
    attr(.df,'.t') <- inp$orig_t
    attr(.df,'.d') <- inp$orig_d
  }

  return(.df)
}
