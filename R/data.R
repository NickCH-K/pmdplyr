#' Data on Earnings and Loan Repayment in US Four-Year Colleges
#'
#' From the College Scorecard, this data set contains by-college-by-year data on how students who attended those colleges are doing.
#'
#' This data is not just limited to four-year colleges and includes a very wide variety of institutions.
#'
#' Note that the labor market (earnings, working) and repayment rate data do not refer to the same cohort of students, but rather are matched on the year in which outcomes are recorded. Labor market data refers to cohorts beginning college as undergraduates ten years prior, repayment rate data refers to cohorts entering repayment seven years prior.
#'
#' Data was downloaded using the Urban Institute's \code{educationdata} package.
#'
#' @format A data frame with 48,445 rows and 8 variables:
#' \describe{
#'   \item{unitid}{College identifiers.}
#'   \item{inst_name}{Name of the college or university.}
#'   \item{state_abbr}{Two-letter abbreviation for the state the college is in.}
#'   \item{pred_degree_awarded_ipeds}{Predominant degree awarded. 1 = less-than-two-year, 2 = two-year, 3 = four-year+}
#'   \item{year}{Year in which outcomes are measured.}
#'   \item{earnings_med}{Median earnings among students (a) who received federal financial aid, (b) who began as undergraduates at the institution ten years prior, (c) with positive yearly earnings.}
#'   \item{count_not_working}{Number of students who are (a) not working (not necessarily unemployed), (b) received federal financial aid, and (c) who began as undergraduates at the institution ten years prior.}
#'   \item{count_working}{Number of students who are (a) working, (b) who received federal financial aid, and (c) who began as undergraduates at the institution ten years prior.}
#'   \item{repay_rate}{Proportion of students who (a) received federal loans as an undergraduate at this institution, (b) entered repayment seven years ago, (c) are not in default, (d) have paid off all accrued interest, and (e) are still making progress on payment. Only available 2013-2016.}
#' }
#' @encoding UTF-8
#' @source Education Data Portal (Version 0.4.0 - Beta), Urban Institute, Center on Education Data and Policy, accessed June 28, 2019. https://educationdata.urban.org/documentation/, Scorecard.
"Scorecard"


#' Data on 2,000 Spanish train trips
#'
#' This data set is a random subsample of a much larger database of trips taken on the Spanish High Speed Train Service (Renfe AVE).
#'
#' All dates and times are European Central Time.
#'
#' The larger data set from which \code{SPrail} was sampled was compiled and released under GPL-2 public license by Pedro Muñoz and David Cañones.
#'
#' @format A data frame with 2,000 rows and 9 variables:
#' \describe{
#'   \item{insert_date}{Date and time when ticket was paid for.}
#'   \item{origin}{Origin City}
#'   \item{destination}{Destination City}
#'   \item{start_date}{Date and time for train departure.}
#'   \item{end_date}{Date and time for train arrival.}
#'   \item{train_type}{Train service name.}
#'   \item{price}{Price of ticket in Euros.}
#'   \item{train_class}{Class of ticket: tourist, business, etc.. Variable in Spanish.}
#'   \item{fare}{Type of ticket fare.}
#' }
#' @encoding UTF-8
#' @source \url{https://www.kaggle.com/thegurus/spanish-high-speed-rail-system-ticket-pricing}
"SPrail"
