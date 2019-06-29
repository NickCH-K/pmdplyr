#' Function to join two data frames inexactly
#'
#' These functions are modifications of the standard \code{dplyr} \code{join} functions, except that it allows a variable of an ordered type (like date or numeric) in \code{x} to be matched in inexact ways to variables in \code{y}.
#'
#' This allows matching, for example, if one data set contains data from multiple days in the week, while the other data set is weekly. Another example might be matching an observation in one data set to the *most recent* previous observation in the other.
#'
#' The available methods for matching are:
#'
#' \itemize{
#'   \item \code{method = 'last'} matches \code{.var} to the closest value of \code{jvar} that is *lower*.
#'   \item \code{method = 'next'} matches \code{.var} to the closest value of \code{jvar} that is *higher*.
#'   \item \code{method = 'between'} requires two variables in \code{jvar} which constitute the beginning and end of a range, and matches \code{.var} to the range it is in. Make sure that the ranges are non-overlapping within the joining variables, or else you will get strange results (specifically, it should join to the earliest-starting range). If the end of one range is the exact start of another, \code{exact = c(TRUE,FALSE)} or \code{exact = c(FALSE,TRUE)} is recommended to avoid overlaps. Defaults to \code{exact = c(TRUE,FALSE)}.
#' }
#'
#' Note that if, given the method, \code{var} finds no proper match, it will be merged with any \code{is.na(jvar[1])} values.
#'
#' @param x,y,by,copy,suffix,keep,name,... Arguments to be passed to the relevant \code{join} function.
#' @param var A character variable with the variable name from the \code{x} data frame which is to be indirectly matched.
#' @param jvar A character vector with the variable name(s) from the \code{y} data frame which are to be indirectly matched. These cannot be variable names also in \code{x} or \code{var}.
#' @param method The approach to be taken in performing the indirect matching.
#' @param exact A logical, where \code{TRUE} indicates that exact matches are acceptable. For example, if \code{method = 'last'}, \code{x} contains \code{var = 2}, and \code{y} contains \code{jvar = 1} and \code{jvar = 2}, then \code{exact = TRUE} will match with the \code{jvar = 2} observation, and \code{exact = FALSE} will match with the \code{jvar = 1} observation. If \code{jvar} contains two variables and you want them treated differently, set to \code{c(TRUE,FALSE)} or \code{c(FALSE,TRUE)}.
#' @name inexact_join
#'
#' @examples
#'
#' library(magrittr)
#' data(Scorecard)
#' #We also have this data on the December unemployment rate for US college grads nationally
#' #but only every other year
#' unemp_data <- data.frame(unemp_year = c(2006,2008,2010,2012,2014,2016,2018),
#'                          unemp = c(.017,.036,.048,.040,.028,.025,.020))
#' #I want to match the most recent unemployment data I have to each college
#' Scorecard <- Scorecard %>%
#'     inexact_left_join(unemp_data,method='last',var='year',jvar='unemp_year')
#'
#' #Or perhaps I want to find the most recent lagged value (i.e. no exact matches, only recent ones)
#' data(Scorecard)
#' Scorecard <- Scorecard %>%
#'     inexact_left_join(unemp_data,method='last',var='year',jvar='unemp_year',exact=FALSE)
#'
#' #Another way to do the same thing would be to specify the range of unemp_years I want exactly
#' data(Scorecard)
#' unemp_data$unemp_year2 <- unemp_data$unemp_year + 2
#' Scorecard <- Scorecard %>%
#'     inexact_left_join(unemp_data,method='between',var='year',jvar=c('unemp_year','unemp_year2'))
#'
NULL

#' @rdname inexact_join
#' @export
inexact_inner_join <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"),...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=suffix,var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::inner_join(x,y,by=matchvars,copy=copy,suffix=suffix,...))
  }
  else {
    return(dplyr::inner_join(x,y,copy=copy,suffix=suffix,...))
  }
}

#' @rdname inexact_join
#' @export
inexact_left_join <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"),...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=suffix,var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::left_join(x,y,by=matchvars,copy=copy,suffix=suffix,...))
  }
  else {
    return(dplyr::left_join(x,y,copy=copy,suffix=suffix,...))
  }
}

#' @rdname inexact_join
#' @export
inexact_right_join <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"),...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=suffix,var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::right_join(x,y,by=matchvars,copy=copy,suffix=suffix,...))
  }
  else {
    return(dplyr::right_join(x,y,copy=copy,suffix=suffix,...))
  }
}

#' @rdname inexact_join
#' @export
inexact_full_join <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"),...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=suffix,var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::full_join(x,y,by=matchvars,copy=copy,suffix=suffix,...))
  }
  else {
    return(dplyr::full_join(x,y,copy=copy,suffix=suffix,...))
  }
}

#' @rdname inexact_join
#' @export
inexact_semi_join <- function(x,y,by=NULL,copy=FALSE,...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=c(".x",".y"),var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::semi_join(x,y,by=matchvars,copy=copy,...))
  }
  else {
    return(dplyr::semi_join(x,y,copy=copy,...))
  }
}


#' @rdname inexact_join
#' @export
inexact_nest_join <- function(x,y,by=NULL,copy=FALSE,keep=FALSE,name=NULL,...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=c(".x",".y"),var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::nest_join(x,y,by=matchvars,copy=copy,keep=FALSE,name=NULL,...))
  }
  else {
    return(dplyr::nest_join(x,y,copy=copy,keep=FALSE,name=NULL,...))
  }
}

#easter egg
greatest_hits <- function() {
  lyrics <- c(
    'If you\'re Steve','You\'re Steve now','You\'ll be Steve','another day','Promise me,','that you won\'t fight','',
    'or attrit until you\'re old and gray...','','We\'ve measured you','so many times','Both N and T are vast','','',
    'Every second,','every moment','we\'ll melt and then recast!','','I mutate once','I mutate twice','I\'ll tidy you',
    'at any price.','I\'ll lag you now','turn now to then','You always said','adjust for trends...','','someday.'
  )

  for (l in lyrics) {
    print(l)
    Sys.sleep(1.5)
  }
}

#' @rdname inexact_join
#' @export
inexact_anti_join <- function(x,y,by=NULL,copy=FALSE,...,var,jvar,method,exact=TRUE) {
  #Get the proper matching variable in x
  x <- inexact_join_prep(x=x,y=y,by=by,copy=copy,suffix=c(".x",".y"),var=var,jvar=jvar,method=method,exact=exact)

  #If by was specified, extend our list of matching variables. Then join!
  #then run with specifying by. Do it this way to preserve the "joining by..." behavior
  if (!is.null(by)) {
    matchvars <- c(by,jvar[1])
    return(dplyr::anti_join(x,y,by=matchvars,copy=copy,...))
  }
  else {
    return(dplyr::anti_join(x,y,copy=copy,...))
  }
}

#This is just the function used to find out which observation in y each obs in x goes to
find_matching_list <- function(z,yidname,i,jvar,range) {
  suppressWarnings(
    max(
      z[z[[yidname]]==i & range,][[jvar[1]]],
      na.rm=TRUE))
}

#This is the shared preprocessing shared by all join functions
inexact_join_prep <- function(x,y,by=NULL,copy=FALSE,suffix=c(".x",".y"),var,jvar,method,exact=TRUE) {
  if (!is.character(var)) {
    stop('var must be a character variable.')
  }
  if (!is.character(jvar)) {
    stop('jvar must be a character variable or vector.')
  }
  if (length(jvar) > 2) {
    stop('jvar can contain no more than two variables.')
  }
  if (!is.character(method)) {
    stop('method must be a character variable.')
  }
  if (!is.logical(exact)) {
    stop('exact must be logical.')
  }
  if (max(var %in% jvar) == 1) {
    stop('The variable names in var and jvar should not match.')
  }
  if (max(jvar %in% names(x)) == 1) {
    stop('The variable names in jvar should not be in x')
  }

  #set .exact length properly
  if (length(exact) == 1 & method == 'between') {
    if (exact == TRUE) {
      exact <- c(TRUE,FALSE)
    } else {
      stop('For method = \'between\', exact must have two elements, or leave as default to get exact = c(TRUE,FALSE)')
    }
  }

  #Get the list of matching variables
  if (!is.null(by)) {
    matchvars <- by
  }  else {
    matchvars <- intersect(names(x),names(y))
  }

  #If there aren't any matchvars, create one that's identical for everyone
  if (length(matchvars) == 0) {
    x[,ncol(x)+1] <- 1
    matchvars <- names(x)[ncol(x)]

    if (max(matchvars %in% jvar) == 1) {
      stop('Please give your jvars non-generic names.')
    }
    y[[matchvars]] <- 1

    #flag the matchvar variable to remove it
    rem_flag <- TRUE
  } else {rem_flag <- FALSE}

  #We need a unique data set of Group by the set of matching variables and the mvars
  z <- y %>%
    dplyr::select_at(c(matchvars,jvar))

  #collapse all the matchvars into one
  z[,ncol(z)+1] <- id_variable(z %>% dplyr::select_at(matchvars),.method='character')
  yidname <- names(z)[ncol(z)]
  z[,matchvars] <- NULL
  z <- z %>%
    #and one observation each
    dplyr::distinct()
  #For these methods, go from lowest/highest to highest/lowest so we get the closest match
  if (method == 'last') {
    z <- z[order(z[[jvar]],decreasing=TRUE),]
  } else if (method == 'next' | method == 'between') {
    z <- z[order(z[[jvar[1]]]),]
  }

  #similar one-column ID for x
  x[,ncol(x)+1] <- id_variable(x %>% dplyr::select_at(matchvars),.method='character')
  xidname <- names(x)[ncol(x)]

  #We'll be matching additionally on jvar[1]
  x[,jvar[1]] <- NA

  #Go through each of the values in z and add them to the appropriate x rows
  for (i in 1:nrow(z)) {
    if (method == 'last') {
      #If it's the same ID, hasn't been matched yet, and x >= y (as appropriate) we have our match
      x[Vectorize(isTRUE)(is.na(x[[jvar]]) & x[[xidname]] == z[i,yidname] &
          (x[[var]] > z[i,jvar] | (exact == TRUE & x[[var]] == z[i,jvar]))),jvar] <- z[i,jvar]
    } else if (method == 'next') {
      #If it's the same ID, hasn't been matched yet, and x <= y (as appropriate) we have our match
      x[Vectorize(isTRUE)(is.na(x[[jvar]]) & x[[xidname]] == z[i,yidname] &
          (x[[var]] < z[i,jvar] | (exact == TRUE & x[[var]] == z[i,jvar]))),jvar] <- z[i,jvar]
    } else if (method == 'between') {
      #If it's the same ID, hasn't been matched yet, and x is between the two y's we have our match
      x[Vectorize(isTRUE)(is.na(x[[jvar[1]]]) & x[[xidname]] == z[i,yidname] &
          (x[[var]] > z[i,jvar[1]] | (exact[1] == TRUE & x[[var]] == z[i,jvar[1]])) &
            (x[[var]] < z[i,jvar[2]] | (exact[2] == TRUE & x[[var]] == z[i,jvar[2]]))),jvar[1]] <- z[i,jvar[1]]
    }
  }

  #Get rid of the ID variable we were using
  x[[xidname]] <- NULL

  #Get rid of the matchvar if we created a fake one
  if (rem_flag == TRUE) {
    x[[matchvars]] <- NULL
  }

  return(x)
}
