# pmdplyr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/nickch-k/pmdplyr.svg?branch=master)](https://travis-ci.org/nickch-k/pmdplyr) 
[![Codecov test coverage](https://codecov.io/gh/nickch-k/pmdplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/nickch-k/pmdplyr?branch=master)
<!-- badges: end -->
  
An R package for cleaning and manipulating panel and hierarchical data.

This is a suite of tools extending the `dplyr` package to perform data manipulation. These tools are geared towards use in panel data and hierarchical data.

Unlike other suites dealing with panel data, all functions in `pmdplyr` are designed to work even when considering a set of variables that do not uniquely identify rows. This is handy when working with any kind of hierarchical data, or panel data where there are multiple observations per individual per time period, like student/term/class education data.

Install this package using `devtools::install_github('NickCH-K/pmdplyr')` and use `help(pmdplyr)` for more information.

Functions included in the package:

1. `between_i` and `within_i`: Standard between and within panel calculations.
2. `fixed_check`: Checks a list of variables for consistency within a panel structure.
3. `fixed_force`: Forces a list of variables to be constant within a panel structure.
4. `id_variable`: Takes a list of variables that make up an individual identifier and turns it into a single variable.
5. `time_variable`: Takes a time variable, or set of time variables, and turns them into a single well-behaved integer time variable of the kind required by most panel functions.
6. `inexact_join`: Set of wrappers for the `dplyr` `join` functions which allows for a variable to be matched inexactly, for example joining a time variable in `x` to the most recent previous value in `y`.
7. `safe_join`: Wrapper for the `dplyr::join` and `pmdplyr::inexact_join` functions which checks before merging whether each data set is uniquely identified as expected.
8. `pdeclare`, `as_pdeclare`, and `is_pdeclare`: Set the panel structure for a data set, or check if it is already set.
9. `panel_convert`: Converts between the panel data types `pmdplyr::pibble`, `tsibble::tsibble`, `plm::pdata.frame`, and `panelr::panel_data`.
10. `mutate_cascade`: A wrapper for `dplyr::mutate` which runs one period at a time, allowing changes in one period to finalize before the next period is calculated.
11. `mutate_subset`: A wrapper for `dplyr::mutate` that performs a calculation on a subset of data, and then applies the result to all the observations (within group).
12. `panel_fill`: Fills in gaps in the panel. Can also fill in at the beginning or end of the data to create a perfectly balanced panel.
13. `panel_locf`: A last-observation-carried-forward function for panels. Fills in `NA`s with recent nonmissing observations.
14. `tlag`: Lags a variable in time.
