#' \code{pmdplyr} package
#'
#' Suite of tools extending the \code{dplyr} package to perform data manipulation. These tools are geared towards use in panel data and hierarchical data.
#'
#' Unlike other suites dealing with panel data, all functions in \code{pmdplyr} are designed to work even when considering a set of variables that do not uniquely identify rows. This is handy when working with any kind of hierarchical data, or panel data where there are multiple observations per individual per time period, like student/term/class education data.
#'
#' \code{pmdplyr} contains the following functions:
#'
#' \itemize{
#'     \item{\link[pmdplyr]{between} and \link[pmdplyr]{within}}{Standard between and within panel calculations.}
#'     \item{\link[pmdplyr]{fixed_check}}{Checks a list of variables for consistency within a panel structure.}
#'     \item{\link[pmdplyr]{fixed_force}}{Forces a list of variables to be constant within a panel structure.}
#'     \item{\link[pmdplyr]{id_variable}}{Takes a list of variables that make up an individual identifier and turns it into a single variable.}
#'     \item{\link[pmdplyr]{time_variable}}{Takes a time variable, or set of time variables, and turns them into a single well-behaved integer time variable of the kind required by most panel functions.}
#'     \item{\link[pmdplyr]{inexact_join}}{Set of wrappers for the \code{dplyr} \link[dplyr]{join} functions which allows for a variable to be matched inexactly, for example joining a time variable in \code{x} to the most recent previous value in \code{y}.}
#'     \item{\link[pmdplyr]{pdeclare} and \link[pmdplyr]{is_pdeclare}}{Set the panel structure for a data set, or check if it is already set.}
#'     \item{\link[pmdplyr]{mutate_cascade}}{A wrapper for \code{dplyr} \link[dplyr]{mutate}} which runs one period at a time, allowing changes in one period to finalize before the next period is calculated.}
#'     \item{\link[pmdplyr]{mutate_subset}}{A wrapper for \code{dplyr} \link[dplyr]{mutate}} that performs a calculation on a subset of data, and then applies the result to all the observations (within group).}
#'     \item{\link[pmdplyr]{panel_fill}}{Fills in gaps in the panel. Can also fill in at the beginning or end of the data to create a perfectly balanced panel.}
#'     \item{\link[pmdplyr]{panel_locf}}{A last-observation-carried-forward function for panels. Fills in \code{NA}s with recent nonmissing observations.}
#'     \item{\link[pmdplyr]{tlag}}{Lags a variable in time.}
#' }
#'
#' @docType package
#' @name pmdplyr
#' @importFrom magrittr %>%
#' @importFrom lubridate %m-%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
