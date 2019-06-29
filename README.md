# pmdplyr
An R package for cleaning and manipulating panel and hierarchical data.

This is a suite of tools extending the \code{dplyr} package to perform data manipulation. These tools are geared towards use in panel data and hierarchical data.

Unlike other suites dealing with panel data, all functions in \code{pmdplyr} are designed to work even when considering a set of variables that do not uniquely identify rows. This is handy when working with any kind of hierarchical data, or panel data where there are multiple observations per individual per time period, like student/term/class education data.

Install this package using `devtools::install_github('NickCH-K/pmdplyr')` and use `help(pmdplyr)` for more information.
