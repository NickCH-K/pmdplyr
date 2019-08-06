#' Function to convert between panel data types
#'
#' This function takes panel data objects declared using \code{pmdplyr} (\code{pibble}/\code{tbl_pb}), \code{tsibble} (\code{tsibble}/\code{tbl_ts}), \code{plm} (\code{pdata.frame}), and \code{panelr} (\code{panel_data}) and converts to one of the other three formats for use with functions in those packages.
#'
#' Any grouping will be lost. You must have the relevant package installed to convert to the type for that package. Conversions from \code{pdata.frame} will be improved if \code{sjlabelled} is also installed.
#'
#' When using \code{panel_convert}, be aware of the requirements that each type has:
#'
#'  \tabular{lcccc}{
#'    Feature/Requirement \tab | \code{pibble}   \tab | \code{tsibble} \tab | \code{pdata.frame} \tab | \code{panel_data} \cr
#'                          \tab           \tab                \tab                 \tab \cr
#'    ID                    \tab \code{.i} \tab \code{key}     \tab \code{index[1]} \tab \code{id} \cr
#'    Time                  \tab \code{.t} \tab \code{index}   \tab \code{index[2]} \tab \code{wave} \cr
#'    Gap control           \tab \code{.d} \tab \code{regular} \tab No              \tab No  \cr
#'    ID must exist         \tab No        \tab No             \tab Yes             \tab Yes \cr
#'    Time must exist       \tab No        \tab Yes            \tab Yes             \tab Yes[1] \cr
#'    Only one ID variable  \tab No        \tab No             \tab Yes             \tab Yes \cr
#'    Unique identification \tab No        \tab Yes            \tab No[2]           \tab No[2]
#'  }
#'
#'  [1] \code{pdata.frame} does not require that time be provided, but if not provided will create it based on original ordering of the data. The \code{pdata.frame} option to set \code{index} equal to an integer for a balanced panel and have it figure out the rest by itself is not supported.
#'
#'  [2] \code{pdata.frame} and \code{panel_data} do not require that ID and time uniquely identify the observations on declaring the data, but functions in these packages may not work correctly without unique identification.
#'
#' In addition to the above, be aware that the different packages have different requirements on which variable classes can be Time variables. \code{pmdplyr::time_variable()} can build an integer variable that will work in all packages.
#'
#' You may run into some trouble if your data contains variables by the names \code{panel_convert_id}, \code{panel_convert_time}, \code{pibble_d}, or \code{panel_convert_regular}.
#'
#' @param data Data frame - a \code{pibble}, \code{tsibble}, \code{pdata.frame}, or \code{panel_data} object.
#' @param to Character variable set to \code{"pmdplyr", "pibble", "tbl_pb", "tsibble", "tbl_ts", "plm", "pdata.frame", "panelr"} or \code{"panel_data"} indicating the type/package to be converted to.
#' @param ... Additional arguments to be sent to, respectively, \code{as_pibble()}, \code{tsibble::as_tsibble()}, \code{plm::pdata.frame()}, or \code{panelr::panel_data()}.
#'
#' @examples
#' # Examples are set to not run in case you don't have the relevant target package installed.
#' if (interactive()) {
#'   data(Scorecard)
#'
#'   S_pibble <- as_pibble(Scorecard, .i = unitid, .t = year)
#'   S_tsibble <- tsibble::as_tsibble(Scorecard, key = unitid, index = year)
#'   S_pdata.frame <- plm::pdata.frame(Scorecard, index = c("unitid", "year"))
#'   S_panel_data <- panelr::panel_data(Scorecard, id = unitid, wave = year)
#'
#'   # Make everything a pibble
#'   # Note variable order may be different coming from panelr, which changes variable order
#'   S_pibble
#'   panel_convert(S_tsibble, to = "pibble")
#'   panel_convert(S_pdata.frame, to = "pibble")
#'   panel_convert(S_panel_data, to = "pibble")
#'
#'   # Make everything a tsibble
#'   panel_convert(S_pibble, to = "tsibble")
#'   S_tsibble
#'   panel_convert(S_pdata.frame, to = "tsibble")
#'   panel_convert(S_panel_data, to = "tsibble")
#'
#'   # Now for pdata.frame
#'   head(panel_convert(S_pibble, to = "plm"))
#'   head(panel_convert(S_tsibble, to = "plm"))
#'   head(S_pdata.frame)
#'   head(panel_convert(S_panel_data, to = "plm"))
#'
#'   # And finally panel_data
#'   panel_convert(S_pibble, to = "panelr")
#'   panel_convert(S_tsibble, to = "panelr")
#'   panel_convert(S_pdata.frame, to = "panelr")
#'   S_panel_data
#' }
#' @export

panel_convert <- function(data, to, ...) {
  if (!is.character(to)) {
    stop("to must be a character variable.")
  }
  if (!(to %in% c(
    "pmdplyr", "pibble", "tbl_pb",
    "tsibble", "tbl_ts",
    "plm", "pdata.frame",
    "panelr", "panel_data"
  ))) {
    stop("Invalid value of to.")
  }

  # Figure out what type we're working with and pull out relevant values
  dataclass <- class(data)

  dots <- list(...)

  # Starting with a pibble
  if ("tbl_pb" %in% dataclass) {
    if (to %in% c("pmdplyr", "pibble", "tbl_pb")) {
      stop("Already in that format.")
    }

    panel_convert_id <- data %@% ".i"
    panel_convert_time <- data %@% ".t"
    gap <- data %@% ".d"

    if (is.na(panel_convert_id)) {
      panel_convert_id <- NULL
    }
    if (is.na(panel_convert_time)) {
      panel_convert_time <- NULL
    }

    if (length(panel_convert_id) != 1 & !(to %in% c("tsibble", "tbl_ts"))) {
      stop("plm and panelr require exactly one ID variable.")
    }

    # Converting to tsibble, potentially non-1 gaps
    if (gap == 0 & to %in% c("tsibble", "tbl_ts") & is.null(dots[["regular"]])) {
      panel_convert_regular <- FALSE
    } else if (gap > 0 & to %in% c("tsibble", "tbl_ts") & is.null(dots[["regular"]])) {
      panel_convert_regular <- TRUE
    } else if (gap > 1 & !(to %in% c("tsibble", "tbl_ts"))) {
      warning("plm and panelr functions may not work as expected with gaps greater than 1.")
    }

    # as.data.frame() won't remove these
    attr(data, ".i") <- NULL
    attr(data, ".t") <- NULL
    attr(data, ".d") <- NULL

    # Converting from tsibble
  } else if ("tbl_ts" %in% dataclass) {
    if (to %in% c("tsibble", "tbl_ts")) {
      stop("Already in that format.")
    }

    panel_convert_id <- names((data %@% "key"))
    panel_convert_id <- panel_convert_id[1:(length(panel_convert_id) - 1)]
    panel_convert_time <- (data %@% "index")[1]

    if (panel_convert_id == ".rows") {
      panel_convert_id <- NULL
    }

    if (length(panel_convert_id) != 1 & !(to %in% c("pmdplyr", "pibble", "tbl_pb"))) {
      stop("plm and panelr require exactly one ID variable.")
    }

    # Potential for irregular data
    if (!tsibble::is_regular(data) & to %in% c("pmdplyr", "pibble", "tbl_pb") & is.null(dots[[".d"]])) {
      pibble_d <- 0
    } else if (!tsibble::is_regular(data) & !(to %in% c("pmdplyr", "pibble", "tbl_pb"))) {
      warning("plm and panelr functions may not work with irregular gaps.")

      # Or regular data
    } else if (tsibble::is_regular(data)) {
      # Complex time gaps
      if (sum(as.numeric(data %@% "interval") > 0) > 1) {
        warning("This time variable varies at more than one level. Non-tsibble formats may have difficulty with it, or it may not work as intended.")
      }

      # Longer-than-1 gaps
      if (max(as.numeric(data %@% "interval")) > 1 & !(to %in% c("pmdplyr", "pibble", "tbl_pb"))) {
        warning("plm and panelr functions may not work as expected with gaps greater than 1.")
      } else if (max(as.numeric(data %@% "interval")) > 1 &
        sum(as.numeric(data %@% "interval") > 0) == 1 &
        to %in% c("pmdplyr", "pibble", "tbl_pb")) {
        pibble_d <- max(as.numeric(data %@% "interval"))
      }
    }
  } else if ("pdata.frame" %in% dataclass) {
    # Now on to pdata.frame
    if (to %in% c("pdata.frame", "plm")) {
      stop("Already in that format.")
    }

    panel_convert_id <- names(data %@% "index")[1]
    panel_convert_time <- names(data %@% "index")[2]

    # Instead of the pseries we want a number back
    # If we have sjlabelled we can turn it into the original values
    if ("sjlabelled" %in% rownames(utils::installed.packages())) {
      data[[panel_convert_id]] <- sjlabelled::remove_all_labels(
        sjlabelled::as_labelled(data[[panel_convert_id]])
      )
      data[[panel_convert_time]] <- sjlabelled::remove_all_labels(
        sjlabelled::as_labelled(data[[panel_convert_time]])
      )
    } else {
      # Otherwise just make 'em valueless numbers
      data[[panel_convert_id]] <- as.numeric(data[[panel_convert_id]])
      data[[panel_convert_time]] <- as.numeric(data[[panel_convert_time]])
    }
  } else if ("panel_data" %in% dataclass) {
    # Finally, panel_data
    if (to %in% c("panel_data", "panelr")) {
      stop("Already in that format.")
    }

    panel_convert_id <- data %@% "id"
    panel_convert_time <- data %@% "wave"
  } else {
    stop("data must be a pibble, tsibble, pdata.frame, or panel_data object.")
  }

  # Now on to the conversion!
  # First, clean of original attributes
  data <- as.data.frame(data)

  # To pibble
  if (to %in% c("pmdplyr", "pibble", "tbl_pb")) {

    # Check if we made a .d (possible if coming from tsibble)
    if (exists("pibble_d")) {
      out <- as_pibble(data, .i = panel_convert_id, .t = panel_convert_time, .d = pibble_d, ...)
    } else {
      out <- as_pibble(data, .i = panel_convert_id, .t = panel_convert_time, ...)
    }
  } else if (to %in% c("tsibble", "tbl_ts")) {

    # Check if we made a regular possible if coming from pmdplyr)
    if (exists("panel_convert_regular")) {
      out <- tsibble::as_tsibble(data,
        key = panel_convert_id,
        index = panel_convert_time,
        regular = panel_convert_regular, ...
      )
    } else {
      out <- tsibble::as_tsibble(data,
        key = panel_convert_id,
        index = panel_convert_time, ...
      )
    }
  } else if (to %in% c("plm", "pdata.frame")) {
    out <- plm::pdata.frame(data, index = c(panel_convert_id, panel_convert_time), ...)
  } else if (to %in% c("panelr", "panel_data")) {
    out <- panelr::panel_data(data, id = !!panel_convert_id, wave = !!panel_convert_time, ...)
  }

  return(out)
}
