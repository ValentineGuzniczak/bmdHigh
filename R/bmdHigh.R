#' Benchmark dose estimation for high-throughput dose-response data
#'
#' Pre-processing, dose-response model fitting, model averaging, and benchmark
#' dose estimation for high-throughput dose-response data.
#'
#' @param data A data frame in long format.
#' @param dose_col Character string giving the name of the dose column.
#' @param response_type_col Character string giving the name of the
#'   endpoint/response-type column.
#' @param response_col Character string giving the name of the response value column.
#' @param factor1_col Optional character string giving the name of the first
#'   grouping variable (e.g., treatment).
#' @param factor2_col Optional character string giving the name of the second
#'   grouping variable (e.g., species).
#'
#' @return
#' A list (one element per unique combination of
#' \code{response_type_col}, \code{factor1_col}, and \code{factor2_col})
#' of model fits and a graph with the model-averaged curve.
#'
#' @details
#' This function checks that all required columns are present. Then, for each endpoint-factor combination, it performs classification,
#' monotonicity testing, trend testing, slope testing, model fitting, and plotting of model-averaged curve.
#'
#' @importFrom plyr dlply
#'
#' @examples
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
#'   factor1_col        = "herbicide"
#' )
#'
#'
#'
#' @export
bmdHigh <- function(
    data,
    dose_col,
    response_type_col,
    response_col,
    factor1_col = NULL,
    factor2_col = NULL
) {
  # ---- Validate required columns ----
  required_cols <- c(dose_col, response_col, response_type_col)
  if (!is.null(factor1_col)) required_cols <- c(required_cols, factor1_col)
  if (!is.null(factor2_col)) required_cols <- c(required_cols, factor2_col)

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  message("All required columns are present")

  # ---- Build grouping variables ----
  grouping_vars <- response_type_col
  if (!is.null(factor1_col)) grouping_vars <- c(grouping_vars, factor1_col)
  if (!is.null(factor2_col)) grouping_vars <- c(grouping_vars, factor2_col)

  # ---- Split data and apply master() ----
  results <- plyr::dlply(
    .data      = data,
    .variables = grouping_vars,
    .fun       = master,
    dose_col   = dose_col,
    response_type_col = response_type_col,
    factor1_col = factor1_col,
    factor2_col = factor2_col,
    response_col = response_col
  )

  return(results)
}
