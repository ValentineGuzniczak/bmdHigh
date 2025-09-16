#' Benchmark dose estimation for high-throughput dose-response data
#'
#' Pre-processing, dose-response model fitting, model averaging, and benchmark dose estimation for high-throughput dose-response data.
#'
#' @param data A data frame containing the dose–response data. Must be long format.
#' @param dose_col Character string giving the name of the dose column.
#' @param response_type_col Character string giving the name of the
#'   endpoint/response-type column.
#' @param response_col Character string giving the name of the response
#'   value column.
#' @param factor_col Optional character string giving the name of a
#'   grouping variable (e.g. species, treatment, etc.). If \code{NULL},
#'   the data are split only by \code{response_type_col}.
#'
#' @return
#' A list (one element per unique combination of
#' \code{response_type_col} and \code{factor_col}) of model fits. #VALENTINE: return graph and/or bmdMA() value
#'
#' @details
#' This function checks that all required columns are present. Then, for each endpoint-factor combination, it performs classification,
#' monotonicity testing, trend testing, slope testing, model fitting, and plotting of model-averaged curve.
#'
#' @importFrom plyr dlply
#' @export
#'
#'@examples
#'
#' # Example using S.alba.comp dataset from drcData
#' library(drcData)
#' data("S.alba.comp")
#'
#' # The dataset must in long format
#' library(tidyr)
#' S_alba_long <- S.alba.comp %>%
#'   pivot_longer(
#'     cols      = c(drymatter, Tf, area, Fo, Fm),
#'     names_to  = "responsetype",
#'     values_to = "responsevalue"
#'   )
#'
#' # Run bmdHigh across endpoints and herbicides
#' results <- bmdHigh(
#'   data              = S_alba_long,
#'   dose_col          = "dose",
#'   response_col      = "responsevalue",
#'   response_type_col = "responsetype",
#'   factor_col        = "herbicide"
#' )
#'
#'
#'
#'
bmdHigh <- function(data,
                    dose_col,
                    response_type_col,
                    response_col,
                    factor_col = NULL) {

  # ---- Validate required columns ----
  required_cols <- c(dose_col, response_col, response_type_col)
  if (!is.null(factor_col)) {
    required_cols <- c(required_cols, factor_col)
  }

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  message("All required columns are present")

  # ---- Split data and apply master() ----
  # Use character vector of column names so plyr recognises them
  grouping_vars <- if (!is.null(factor_col)) {
    c(response_type_col, factor_col)
  } else {
    response_type_col
  }

  results <- plyr::dlply(
    .data      = data,
    .variables = grouping_vars,
    .fun       = master,
    dose_col   = dose_col,
    response_type_col = response_type_col,
    factor_col = factor_col,
    response_col = response_col
  )

  return(results)
}
