#' Classify Data
#'
#' Classifies a vector into one of: "binary", "count", "continuous", or "categorical".
#'
#' @param x A vector (numeric, factor, or character) to classify.
#'
#' @return A character string indicating the data type.
#' @export
#'
#' @examples
#' classify_data(c(0, 1, 1, 0))
#' classify_data(c(2, 5, 8, 10))
#' classify_data(c(2.2, 5.5, 8.8, 10.1))
#' classify_data(c("yes", "no", "yes"))
#' classify_data(c("yellow", "green", "pink"))
#'
classify_data <- function(x) {
  # Remove NAs safely
  x_no_na <- x[!is.na(x)]

  # If empty after removing NAs → unknown
  if (length(x_no_na) == 0) {
    return("unknown (all NA)")
  }

  # Handle numeric variables
  if (is.numeric(x_no_na)) {
    unique_vals <- unique(x_no_na)

    # Binary: exactly two unique values (0/1, 1/2, TRUE/FALSE encoded numerically)
    if (length(unique_vals) == 2 &&
        all(unique_vals %in% c(0, 1, 1, 2))) {
      return("binary")
    }

    # Counts: all non-negative integers
    if (all(x_no_na >= 0) && all(abs(x_no_na - round(x_no_na)) < .Machine$double.eps^0.5)) {
      return("count")
    }

    # Otherwise → continuous
    return("continuous")
  }

  # Factors & characters → categorical
  if (is.factor(x_no_na) || is.character(x_no_na)) {
    # If binary categorical (e.g., "Yes"/"No", "Male"/"Female")
    if (length(unique(x_no_na)) == 2) {
      return("binary")
    }
    return("categorical")
  }

  # Default fallback
  return(paste("unsupported data type:", class(x)))
}
