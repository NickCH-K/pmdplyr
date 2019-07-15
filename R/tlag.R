#' Function to retrieve time-lagged data
#'
#' WARNING: FOR NOW, ALWAYS SPECIFY THE PANEL STRUCTURE EVERY TIME YOU CALL THIS FUNCTION. PDECLARE STATUS IS DROPPED BY MOST FUNCTIONS.
#'
#' This function retrieves the time-lagged values of a variable, using the time variable defined in \code{.t} in the function or by \code{as_pdeclare()}. \code{tlag()} is highly unusual among time-lag functions in that it is usable even if observations are not uniquely identified by \code{.t} (and \code{.i}, if defined).
#'
#' @param .var Vector to be lagged.
#' @param .df Data frame or tibble (usually the data frame or tibble that contains \code{.var}) which contains the panel structure variables either listed in \code{.i} and \code{.t}, or earlier declared with \code{as_pdeclare()}. If \code{tlag} is called inside of a \code{dplyr} verb, this can be omitted and the data will be picked up automatically.
#' @param .n Number of periods to lag by. 1 by default. Note that this is automatically scaled by \code{.d}. If \code{.d = 2} and \code{.n = 1}, then the lag of \code{.t = 3} will be \code{.t = 1}. Allows negative values, equivalent to \code{tlead()} with the same value but positive. Note that \code{.n} is ignored if \code{.d=0}.
#' @param .default Fill-in value used when lagged observation is not present. Defaults to NA.
#' @param .quick If \code{.i} and \code{.t} uniquely identify observations in your data, **and** there either \code{.d = 0} or there are no time gaps for any individuals (perhaps use \code{panel_fill()} first), set \code{.quick = TRUE} to improve speed. \code{tlag()} will not check if either of these things are true (except unique identification, which will be checked if \code{.uniqcheck = 1} or if \code{.i} or \code{.t} are specified in-function), so make sure they are or you will get strange results.
#' @param .resolve If there is more than one observation per individal/period, and the value of \code{.var} is identical for all of them, that's no problem. But what should \code{tlag()} do if they're not identical? Set \code{.resolve = 'error'} (or, really, any string) to throw an error in this circumstance. Or, set \code{.resolve} to a function that can be used within \code{dplyr::summarize()} to select a single value per individual/period. For example, \code{.resolve = function(x) mean(x)} to get the mean value of all observations present for that individual/period.
#' @param .group_i By default, if \code{.i} is specified or found in the data, \code{tlag()} will group the data by \code{.i}, ignoring any grouping already implemented. Set \code{.group_i = FALSE} to avoid this.
#' @param .i Character or character vector with the variable names that identify the individual cases. Note that setting any one of \code{.i}, \code{.t}, or \code{.d} will override all three already applied to the data, and will return data that is \code{as_pdeclare()}d with all three, unless \code{.setpanel=FALSE}.
#' @param .t Character variable with the single variable name indicating the time. \code{pmdplyr} accepts two kinds of time variables: numeric variables where a fixed distance \code{.d} will take you from one observation to the next, or, if \code{.d=0}, any standard variable type with an order. Consider using the \code{time_variable()} function to create the necessary variable if your data uses a \code{Date} variable for time.
#' @param .d Number indicating the gap in \code{.t} between one period and the next. For example, if \code{.t} indicates a single day but data is collected once a week, you might set \code{.d=7}. To ignore gap length and assume that "one period ago" is always the most recent prior observation in the data, set \code{.d=0}. By default, \code{.d=1}.
#' @param .uniqcheck Logical parameter. Set to TRUE to always check whether \code{.i} and \code{.t} uniquely identify observations in the data. By default this is set to FALSE and the check is only performed once per session, and only if at least one of \code{.i}, \code{.t}, or \code{.d} is set.
#' @examples
#'
#' data(Scorecard)
#'
#' #The Scorecard data is uniquely identified by unitid and year.
#' #However, there are sometimes gaps between years.
#' #In cases like this, using dplyr::lag() will still use the row before,
#' #whereas tlag() will respect the gap and give a NA, much like plm::lag()
#' #(although tlag is slower than either, sorry)
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(pmdplyr_tlag = tlag(earnings_med,.i='unitid',.t='year'))
#' Scorecard <- Scorecard %>%
#'   dplyr::arrange(year) %>%
#'   dplyr::group_by(unitid) %>%
#'   dplyr::mutate(dplyr_lag = dplyr::lag(earnings_med)) %>%
#'   dplyr::ungroup()
#'
#' #more NAs in the pmdplyr version - observations with a gap and thus no real lag present in data
#' sum(is.na(Scorecard$pmdplyr_tlag))
#' sum(is.na(Scorecard$dplyr_lag))
#'
#' #If we want to ignore gaps, or have .d = 0, and .i and .t uniquely identify observations,
#' #we can use the .quick option to match dplyr::lag()
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(pmdplyr_quick_tlag = tlag(earnings_med,.i='unitid',.t='year',.d=0,.quick=TRUE))
#' sum(Scorecard$dplyr_lag != Scorecard$pmdplyr_quick_tlag,na.rm=TRUE)
#'
#' #Where tlag shines is when you have multiple observations per .i/.t
#' #If the value of .var is constant within .i/.t, it will work just as you expect.
#' #If it's not, it will throw an error, or you can set
#' #.resolve to tell tlag how to select a single value from the many
#' #Maybe we want to get the lagged average earnings within degree award type
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(last_year_earnings_by_category =
#'   tlag(earnings_med,.i='pred_degree_awarded_ipeds',.t='year',
#'   .resolve=function(x) mean(x,na.rm=TRUE)))
#' #Or maybe I want the lagged earnings across all types - .i isn't necessary!
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(last_year_earnings_all = tlag(earnings_med,.t='year',
#'                                               .resolve=function(x) mean(x,na.rm=TRUE)))
#' #Curious why the first nonmissing obs show up in 2012?
#' #It's because there's no 2008 or 2010 in the data, so when 2009 or 2011 look back
#' #a year, they find nothing!
#' #We could get around this by setting .d = 0 to ignore gap length
#' #Note this can be a little slow
#' Scorecard <- Scorecard %>%
#'   dplyr::mutate(last_year_earnings_all = tlag(earnings_med,.t='year',.d=0,
#'                                               .resolve=function(x) mean(x,na.rm=TRUE)))
#'
#' @export

#FOR FIXING:
#https://adv-r.hadley.nz/s3.html#allowing-subclassing
#https://adv-r.hadley.nz/s3.html#allowing-subclassing
#https://vctrs.r-lib.org/articles/s3-vector.html

tlag <- function(.var,.df=get(".", envir=parent.frame()),.n=1,.default=NA,.quick=FALSE,.resolve='error',.group_i=TRUE,.i=NA,.t=NA,.d=NA,.uniqcheck=FALSE) {
  if (!is.vector(.var)) {
    stop('.var must be a vector.')
  }
  if (!is.numeric(.n) | length(.n) > 1) {
    stop('.n must be a single integer.')
  }
  if (!(as.integer(.n) == .n)) {
    stop('.n must be an integer.')
  }
  if (!is.character(.resolve) & !is.function(.resolve)) {
    stop('.resolve must be a function.')
  }
  if (!is.logical(.group_i)) {
    stop('.group_i must be TRUE or FALSE')
  }
  if (!is.logical(.quick)) {
    stop('.quick must be TRUE or FALSE')
  }
  if (length(.default) > 1) {
    stop('.default must be a single value.')
  }

  #original grouping structure
  origgroups <- names(.df %@% 'groups')
  origgroups <- origgroups[1:(length(origgroups)-1)]
  if (is.null(origgroups)) { origgroups <- NA }

  #Check inputs and pull out panel info
  inp <- declare_in_fcn_check(.df,.i,.t,.d,.uniqcheck,.setpanel=FALSE)
  if (is.na(inp$t)) {
    stop('tlag() requires that .t be declared either in the function or by as_pdeclare().')
  }

  #If changes have been made, fill in default .d
  if ((min(is.na(.i)) == 0 | !is.na(.t)) & is.na(.d)) {
    inp$d <- 1
  }

  arrnames <- c(inp$i,inp$t)
  if (.group_i == FALSE) { arrnames <- inp$t }
  arrnames <- arrnames[!is.na(arrnames)]

  if (.quick == TRUE) {
    dat <- dplyr::bind_cols(
      .df %>% dplyr::select_at(arrnames),
      data.frame(.var))
    varname <- names(dat)[ncol(dat)]
    dat[,ncol(dat)+1] <- 1:nrow(dat)
    origorder <- names(dat)[ncol(dat)]

    # #There must be a better way to do this
    # arrnames <- paste('.df[[\'',arrnames,'\']]',sep='')
    # arrnames[1] <- paste('order(',arrnames[1],sep='')
    # arrnames[length(arrnames)] <- paste(arrnames[length(arrnames)],')',sep='')
    # order <- eval(parse(text=paste0(arrnames,collapse=',')))

    return((dat %>%
              dplyr::arrange_at(arrnames) %>%
              dplyr::mutate_at(varname,.funs= function(x) dplyr::lag(x,n=.n,default=.default)) %>%
              dplyr::arrange_at(origorder))[[varname]])
  }

  #We only need these
  .df <- .df %>% dplyr::select_at(arrnames)


  #Create lookup table.
  lookup <- .df
  lookup[,ncol(lookup)+1] <- .var
  varname <- names(lookup)[ncol(lookup)]
  #Do we need to group it?
  if (!setequal(origgroups,arrnames)) {
    lookup <- lookup %>%
      dplyr::group_by_at(arrnames)
  }
  #Check if there's uniformity, if .resolve = 'error'
  if (is.character(.resolve)) {
    if (max((lookup %>%
             dplyr::mutate_at(varname,
                              .funs=function(x) dplyr::n_distinct(x)))[[varname]]) > 1) {
      stop('Values are not consistent within (.i, if specified, and) .t. See .resolve option.')
    }
    .resolve = function(x) dplyr::first(x)
  }

  #And turn into the lookup table, with adjusted time variable for linking
  lookup <- lookup %>%
    dplyr::summarize_at(varname,.resolve) %>%
    dplyr::mutate_at(inp$t,.funs=function(x) x + .n*inp$d)
  lookup[,ncol(lookup)+1] <- 1
  foundmatch <- names(lookup)[ncol(lookup)]

  #Do the linkup and return the lagged value
  #note that if .d = 0 we need to look for the most recent value
  if (inp$d > 0) {
    .df <- .df %>%
      dplyr::left_join(lookup,by=arrnames)
  } else {
    #prepare for an inexact_join. We need the name of .t in lookup to be different
    lookup[,ncol(lookup)+1] <- lookup[[inp$t]]
    jvarname <- names(lookup)[ncol(lookup)]
    #and get rid of the actual one
    lookup[[inp$t]] <- NULL

    #work separately based on whether we have .i or not
    if (length(arrnames) == 1) {
      suppressMessages(.df <- .df %>%
        inexact_left_join(lookup,var=inp$t,jvar=jvarname,method='last',exact=FALSE))
    } else {
      suppressMessages(.df <- .df %>%
                         inexact_left_join(lookup,by=arrnames[1:length(arrnames)-1],var=inp$t,
                                           jvar=jvarname,method='last',exact=FALSE))
    }
  }
  if (!is.na(.default)) {
    .df <- .df %>% dplyr::mutate_at(varname,.funs=function(x)
      ifelse(is.na(.df[[foundmatch]]),.default,.df[[varname]]))
  }

  return(.df[[varname]])

}
