#' Test for Negative Slope
#'
#' Fits a simple linear regression and checks whether the slope is negative.
#'
#' @param data A data frame containing the response and dose variables.
#' @param response_col Name of the response column in \code{data}.
#' @param dose_col Name of the dose column in \code{data}.
#'
#' @importFrom stats coef lm as.formula
#'
#' @return Logical. \code{TRUE} if the slope is negative, otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' df <- data.frame(Dose = c(1, 2, 3), responsevalue = c(10, 8, 5))
#' negslopetest(data=df, response_col = "responsevalue", dose_col="Dose") # TRUE
#'
negslopetest <- function(data, response_col, dose_col) {
  # Input validation
  if (!all(c(response_col, dose_col) %in% colnames(data))) {
    stop("Data must contain columns: ", response_col, " and ", dose_col)
  }

  # Try fitting a simple linear regression
  lmtest <- try(
    lm(as.formula(paste(response_col, "~", dose_col)), data = data),
    silent = TRUE
  )

  # If model fitting failed
  if (inherits(lmtest, "try-error")) {
    warning("Linear model fitting failed. Returning NA.")
    return(NA)
  }

  # Extract slope safely
  slope <- coef(lmtest)[dose_col]

  # Handle NA slope gracefully
  if (is.na(slope)) {
    warning("Slope could not be estimated. Returning NA.")
    return(NA)
  }

  # Return TRUE if slope is negative
  return(slope < 0)
}
