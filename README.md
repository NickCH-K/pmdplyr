
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Panel Maneuvers in dplyr (pmdplyr)

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/nickch-k/pmdplyr.svg?branch=master)](https://travis-ci.org/nickch-k/pmdplyr)
[![Codecov test
coverage](https://codecov.io/gh/nickch-k/pmdplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/nickch-k/pmdplyr?branch=master)
<!-- badges: end -->

The `pmdplyr` package is an extension to `dplyr` designed for cleaning
and managing panel and hierarchical data. It contains variations on the
`dplyr` `mutate` and `join` functions that address common panel data
needs, and contains functions for managing and cleaning panel data. The
goal is to get you a nice tidy `pibble` panel data object, which you can
`panel_convert()` for use in one of the many packages that help you
*analyze* panel data.

Unlike other panel data packages, functions in `pmdplyr` are all
designed to work even if there is more than one observation per
individual per period. This comes in handy if each individual is
observed multiple times per period - for example, multiple classes per
student per term; or if you have hierarchical data - for example,
multiple companies per country.

Examples of `pmdplyr` use are below. These examples only cover some of
the functionality of the package. See the
[Reference](https://nickch-k.github.io/pmdplyr/reference/index.html)
`pkgdown` page for a full list of functions, or `help(pmdplyr)`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NickCH-K/pmdplyr")
```

## College Scorecard Example

Let’s start with the fairly straightforward `Scorecard` data, which is
uniquely identified by college ID `unitid` and year `year`, and which
describes how well students who attended that college are doing years
after attendance.

``` r
# Note that pmdplyr automatically loads dplyr as well
library(pmdplyr)

data(Scorecard)
# Let's declare it as a pibble panel-data tibble
Scorecard <- as_pibble(Scorecard, .i = unitid, .t = year)

# We also have this data on the December unemployment rate for US college grads nationally
# but only every other year
unemp_data <- data.frame(
  unemp_year = c(2006, 2008, 2010, 2012, 2014, 2016, 2018),
  unemp = c(.017, .036, .048, .040, .028, .025, .020)
)
```

I am interested in measuring the differences in ex-student earnings
`earnings_med` between two-year and four-year colleges
(`pred_degree_awarded_ipeds == 2` or `3`, respectively). But before we
can do that we need to clean the data.

``` r
Scorecard <- Scorecard %>%
  # We want pred_degree_awarded_ipeds to be consistent within college. No changers!
  # So let's drop them by using fixed_check to find inconsistencies
  anti_join(fixed_check(Scorecard,
                        .var = pred_degree_awarded_ipeds,
                        .within = unitid)[[1]]) %>%
  # Then, get rid of pred_degree_awarded_ipeds == 1
  # And simplify our terms
  filter(pred_degree_awarded_ipeds %in% c(2,3)) %>%
  mutate(FourYear = pred_degree_awarded_ipeds == 3) %>%
  # earnings_med has some missing values - let's fill them in with 
  # the most recent nonmissing observations we have
  # - panel_locf respects the panel structure declared above with as_pibble()
  mutate(earnings_med = panel_locf(earnings_med)) %>%
  # Now let's bring in that unemployment data!
  # Since it's every other year, it won't line up properly
  # in the join. So let's use inexact_join to get the MOST RECENT
  # year to join with
  inexact_left_join(unemp_data, var = year, jvar = unemp_year, method = "last") %>%
  # To adjust for state-level trends, let's also control for a tlag of
  # average earnings within state.
  # The lag is at the state level, and state-year doesn't uniquely identify,
  # But that's okay! We just pick a .resolve function to handle disagreements.
  # (We could also do this straight in the regression model itself)
  mutate(lag_state_earnings = tlag(earnings_med,
                                      .i = state_abbr,
                                      .t = year,
                                      .resolve = mean))

# Now we can run a basic regression. 

summary(lm(
  earnings_med ~ 
    FourYear +
    unemp + 
    lag_state_earnings,
  data = Scorecard
))
#> 
#> Call:
#> lm(formula = earnings_med ~ FourYear + unemp + lag_state_earnings, 
#>     data = Scorecard)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -25197  -5017   -458   4191  54110 
#> 
#> Coefficients:
#>                      Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)         7.682e+03  1.986e+03   3.869 0.000113 ***
#> FourYearTRUE        9.297e+03  3.833e+02  24.257  < 2e-16 ***
#> unemp              -8.239e+04  3.085e+04  -2.671 0.007625 ** 
#> lag_state_earnings  7.226e-01  4.453e-02  16.226  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 8262 on 2071 degrees of freedom
#>   (25564 observations deleted due to missingness)
#> Multiple R-squared:  0.342,  Adjusted R-squared:  0.341 
#> F-statistic: 358.8 on 3 and 2071 DF,  p-value: < 2.2e-16
```

We could even improve that code - why not run the `anti_join` and
`inexact_left_join` using `safe_join`? When we do the
`inexact_left_join`, for example, we’re assuming that `unemp_data` is
uniquely identified by `unemp_year` - is it really? `safe_join` would
check for us and minimize error.

## Spanish Rail Example

Let’s shift focus to the `SPrail` data set, which is not nearly as well
behaved as `Scorecard`\! This data contains the `price` of 2,000 train
trips taken on the Spanish rail system. At any given time period, there
are multiple tickets sold per route, which is identified by `origin` and
`destination`. And the `insert_date` of sale is down to the minute - far
more precise than we might actually want to use.

Let’s say we are interested in how much of a premium you’ll have to pay
for different ticket types relative to the cheapo option - a tourist
ticket with a transfer (`train_class == "Turista con enlace"`),
accounting for differences in prices between routes.

We have some difficulties to cover: making the ID and time variables
behave, accounting for the between-route differences, and figuring out
how to compare each price to the cheapo price.

``` r
data(SPrail)

SPrail <- SPrail %>%
  # We have two ID variables - origin and destination.
  # pmdplyr has no problem with this, but maybe we want to export
  # to something like plm later, which can't handle it.
  # So let's use id_variable to combine them into one
  mutate(route_ID = id_variable(origin, destination)) %>%
  # We have a time variable down to the minute. Too fine-grained!
  # Let's back things up to the daily level, and 
  # create a nice integer time variable that's easy to use
  mutate(day = time_variable(insert_date, .method = "day")) %>%
  # Now we can declare a pibble
  as_pibble(.i = route_ID, .t = day) %>%
  # We want to account for between-route differences in price,
  # so let's isolate the within variation
  mutate(price_w = within_i(price)) %>%
  # We want to compare to the cheapo option, so let's use 
  # mutate_subset to get the average price of the cheapo option
  # and propogate that to the other options for comparison
  mutate_subset(cheapo_price = mean(price, na.rm = TRUE), 
                .filter = train_class == "Turista con enlace") %>%
  mutate(premium = price - cheapo_price) %>%
  filter(train_class %in% c("Preferente", "Turista", "Turista Plus")) %>%
  # Now let's compare premia
  group_by(train_class) %>%
  summarize(premium = mean(premium, na.rm = TRUE))

SPrail
#> # A tibble: 3 x 2
#>   train_class  premium
#>   <fct>          <dbl>
#> 1 Preferente     29.9 
#> 2 Turista         8.36
#> 3 Turista Plus   14.4
```

And so there we have it - `Preferente` will really set you back relative
to the cheapo ticket on the same route.
