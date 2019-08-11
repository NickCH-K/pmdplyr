# Internal functions that are used by functions in more than one file are to be stored here
# Exception made for some internal pibble-building functions which are still in pibble.R

# declare_in_fcn_check takes .i and .t and strings
declare_in_fcn_check <- function(.df, .i, .t, .d, .uniqcheck, .setpanel, .noneed = FALSE) {
  # Check inputs
  if (!is.na(.uniqcheck) & !is.logical(.uniqcheck)) {
    stop("uniqcheck must be TRUE or FALSE.")
  }
  if (!is.na(.setpanel) & !is.logical(.setpanel)) {
    stop("setpanel must be TRUE or FALSE.")
  }

  # Collect original panel settings, if any.
  # To be consistent with other input checking, make them NA not NULL if appropriate
  orig_i <- ifelse(is.null(.df %@% ".i"), NA, .df %@% ".i")
  orig_t <- ifelse(is.null(.df %@% ".t"), NA, .df %@% ".t")
  orig_d <- ifelse(is.null(.df %@% ".d"), NA, .df %@% ".d")
  is_tbl_pb <- is_pibble(.df, .silent = TRUE)

  # If uniqcheck is TRUE but panel is not being reset, run through check_panel_inputs
  # just to check, using already-set panel info
  if (min(is.na(.i)) > 0 & is.na(.t) & .uniqcheck == TRUE) {
    check_panel_inputs(.df, .i = orig_i, .t = orig_t, .d = orig_d, .uniqcheck = TRUE)
  }

  # If nothing was declared, use the original values
  if (min(is.na(.i)) > 0 & is.na(.t)) {
    .i <- orig_i
    .t <- orig_t
    .d <- orig_d
  }

  # If everything is still missing and you need something, error
  if (min(is.na(.i)) > 0 & is.na(.t) & .noneed == FALSE) {
    stop("Attempt to use panel indicators i and/or t, but no i or t are declared in command or stored in data.")
  }


  return(list(
    orig_i = orig_i,
    orig_t = orig_t,
    orig_d = orig_d,
    i = .i,
    t = .t,
    d = .d,
    is_tbl_pb = is_tbl_pb
  ))
}


# For generating non-conflicting variable names
uniqname <- function(df) {
  paste(utils::tail(
    names(df)[order(nchar(names(df)))], 1
  ),
  ".1",
  sep = ""
  )
}


# Regular rle treats consecutive NA values as different. Unacceptable!
# Also implements the filling-in while we're at it.
# Because of filling-in, don't use this for regular rle-but-with-NA purposes!!
# Or at least if you do get rid of the while loop
rle_na <- function(x) {
  n <- length(x)

  # Fill in NAs using previous values until there are no more NAs (except perhaps the first obs)
  # How many NAs do we lead off with?
  lead_NA <- sum(cummin(is.na(x)))
  if (lead_NA == 0) {
    lead_NA <- 1
  }
  while (sum(
    is.na(
      utils::tail(x, -lead_NA)
    )
  ) > 0) {
    x <- ifelse(is.na(x), c(NA, x[-length(x)]), x)
  }

  y <- x[-1L] != x[-n]
  i <- c(which(y |
    (is.na(y) & !is.na(c(y[-1], 1))) |
    (!is.na(x[-n]) & is.na(c(x[-c(1, n)], 1)))), n)

  structure(list(lengths = diff(c(0L, i)), values = x[i]),
    class = "rle"
  )
}
