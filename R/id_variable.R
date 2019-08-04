#' Function to create a single panel ID variable out of several
#'
#' The pmdplyr library accepts the use of multiple ID variables. However, you may wish to combine these into a single variable, or renumber the single variable you already have for some reason.
#'
#' By default, id_variable() will create a unique numeric identifier out of your ID variables, sequential following the order in the original data (\code{.method='number'}). However, you may want to remove the ordering and assign IDs randomly (\code{.method='random'}), or preserve all the original information and create a single fixed-width character ID variable that contains all the original information (\code{.method='character'}).
#'
#' @param ... variables (vectors) that, together, make up the ID variables in the data and uniquely identifies the individual. Note that \code{id_variable()} will not check whether you've selected an appropriate set of variables; try running \code{as_pibble()} after getting your ID and time variables.
#' @param .method Can be \code{'number'}, \code{'random'}, or \code{'character'}, as described below.
#' @param .minwidth If \code{.method = 'character'}, omits the additional spacing that makes the ID variable fixed-width and ensures uniqueness. WARNING: This option saves space but may cause two individuals to have the same ID. Defaults to \code{FALSE}.
#' @examples
#'
#' data(SPrail)
#' # I want to identify observations at the route (origin-destination)/year level
#' # Let's make it a character variable so we can tell at a glance what route we're talking
#' SPrail <- SPrail %>%
#'   dplyr::mutate(route_id = id_variable(origin, destination, .method = "character"))
#' @export

id_variable <- function(..., .method = "number", .minwidth = FALSE) {
  ########################################## CHECK INPUTS
  if (!(.method %in% c("number", "random", "character"))) { stop("Unrecognized time_variable .method.") }
  if (!is.character(.method) | length(.method) > 1) { stop(".method must be a character variable.") }
  if (!is.logical(.minwidth)) { stop(".minwidth must be TRUE or FALSE.") }

  # What we're working with
  idf <- data.frame(...)
  var <- names(idf)
  # original order. We need two unused names
  idf[, (ncol(idf) + 1)] <- 1:nrow(idf)
  origordername <- names(idf)[ncol(idf)]
  idf[, (ncol(idf) + 1)] <- 1:nrow(idf)
  secondname <- names(idf)[ncol(idf)]
  idf[, secondname] <- NULL

  if (.method == "number") {
    # toss out origorder
    idvar <- idf
    idvar[[origordername]] <- NULL
    idvar <- idvar %>%
      # Get uniques
      unique()

    # number sequentially
    idvar[, ncol(idvar) + 1] <- 1:nrow(idvar)
    names(idvar)[ncol(idvar)] <- secondname
    idvar <- (idvar %>%
      dplyr::right_join(idf, by = var) %>%
      dplyr::arrange_at(origordername))[[secondname]]
  }
  else if (.method == "random") {
    # toss out origorder
    idvar <- idf
    idvar[[origordername]] <- NULL
    idvar <- idvar %>%
      # Get uniques
      unique()

    # how many uniques we working with? Allow random IDs up to ten times that
    idvar[, ncol(idvar) + 1] <- sample(1:(10 * nrow(idvar)), nrow(idvar), replace = FALSE)
    names(idvar)[ncol(idvar)] <- secondname
    idvar <- (idvar %>%
      dplyr::right_join(idf, by = var) %>%
      dplyr::arrange_at(origordername))[[secondname]]
  }
  else if (.method == "character") {
    # Figure out the lengths we'll need to fill in
    maxlen <- sapply(idf, function(x) max(nchar(as.character(x))))

    # Fill out to fixed width to ensure uniqueness.
    # Drop the | in there in case something ends with a period and to make clear where each ID component begins and ends
    if (.minwidth == FALSE) {
      idvar <- lapply(1:length(var), function(x) paste("|", idf[, x], "|",
          sapply(as.character(idf[, x]), function(y) paste0(rep(".", maxlen[x] - nchar(y)), collapse = "")),
          sep = ""
        ))
    }
    else {
      idvar <- lapply(idf, as.character)
    }

    # And smush 'em all together
    idvar <- apply(as.data.frame(idvar), 1, function(x) paste0(x, collapse = ""))
  }

  return(idvar)
}
