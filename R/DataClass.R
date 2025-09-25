#' Classify Data
#'
#' Classifies a vector into one of: "binary", "count", "continuous", or "categorical"; and returns a list of appropriate models.
#'
#' This function is **internal** to the package and is used by
#' \code{\link{bmdHigh}}.
#'
#' @param x A vector (numeric, factor, or character) to classify.
#'
#' @return A character string indicating the data type. + list of drc models
#'
#' @importFrom drc LL.4 W1.4 W2.4 LN.4 LL.3 W1.3 W2.3 LN.3 LL.2 W1.2 W2.2 LN.2 BC.4 BC.5
#'
#' @keywords internal
#'
#'
classify_data <- function(x) {
  # Remove NAs
  x_no_na <- x[!is.na(x)]

  # If empty after removing NAs → unknown
  if (length(x_no_na) == 0) {
    return("unknown (all NA)")
  }

  # Handle numeric variables
  if (is.numeric(x_no_na)) {
    unique_vals <- unique(x_no_na)

    # Binary: exactly two unique values (0/1, 1/2, TRUE/FALSE encoded numerically)
    if (length(unique(unique_vals)) == 2 &&
        all(unique(unique_vals) %in% c(0, 1, 2))) {
      models <- list(LL.2(), W1.2(), W2.2(), LN.2())
      datatype <- "binary"
      return(list(
        datatype = datatype,
        models = models
      ))
    }

    # Counts: all non-negative integers
    if (all(x_no_na >= 0) && all(abs(x_no_na - round(x_no_na)) < .Machine$double.eps^0.5)) {
      models <- list(LL.3(), W1.3(), W2.3(), LN.3())
      datatype <- "count"
      return(list(
        datatype = datatype,
        models = models
      ))
    }

    # Otherwise → continuous
    models <- list(LL.4(), W1.4(), W2.4(), LN.4(), BC.5())
    datatype <- "continuous"
    return(list(
      datatype = datatype,
      models = models
    ))
  }

  # Factors & characters → categorical
  if (is.factor(x_no_na) || is.character(x_no_na)) {
    # If binary categorical (e.g., "Yes"/"No", "Male"/"Female")
    if (length(unique(x_no_na)) == 2) {
      models <- list(LL.2(), W1.2(), W2.2(), LN.2())
      datatype <- "binary"
      return(list(
        datatype = datatype,
        models = models
      ))
    }
    return("categorical")
    models <- list(LL.3(), W1.3(), W2.3(), LN.3())
    datatype <- "categorical"
    return(list(
      datatype = datatype,
      models = models
    ))
  }

  # Default fallback
  return(paste("unsupported data type:", class(x)))
}

