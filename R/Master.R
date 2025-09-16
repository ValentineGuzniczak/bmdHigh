#' Master Analysis Function
#'
#' This function performs classification, monotonicity testing, trend testing,
#' slope testing, model fitting, and plotting of model-averaged curve.
#'
#' This function is **internal** to the package and is used by
#' \code{\link{bmdHigh}}.
#'
#' @param data A data frame containing dose-response data.
#' @param dose_col Name of the column with dose values.
#' @param response_col Name of the column with response values.
#' @param factor_col Optional name of the column with a grouping variable (e.g., treatment).
#' @param response_type_col Name of the column describing response type.
#'
#' @importFrom grDevices recordPlot
#' @importFrom graphics curve
#' @importFrom bmd monotonicityTest
#' @importFrom bmd trendTest
#' @importFrom bmd MACurve
#' @importFrom drc LL.4 W1.4 W2.4 LN.4
#'
#' @keywords internal
#'
master <- function(
    data,
    dose_col,
    response_col,
    factor_col,
    response_type_col
) {

  # --- Extract required columns dynamically ---
  dose <- data[[dose_col]]
  x <- sort(unique(data[[dose_col]]))
  response <- data[[response_col]]
  response_type <- data[[response_type_col]][[1]]
  factor_val <- if (!is.null(factor_col)) as.character(data[[factor_col]][1]) else NA

  # --- Display processing info ---
  if (!is.null(factor_col)) {
    cat("\n--- Processing:", response_type, "-", factor_val, "---\n")
  } else {
    cat("\n--- Processing:", response_type, "---\n")
  }

  # --- Classify response data type ---
  datainfo <- classify_data(response)
  datatype <- datainfo$datatype
  models <- datainfo$models

  # --- Monotonicity test ---
  mono <- try(
    bmd::monotonicityTest(dose, response, test = "jonckheere"),
    silent = TRUE
  )
  if (inherits(mono, "try-error")) {
    warning("Monotonicity test failed - skipping subset.")
    return(NULL)
  }
  if (!isTRUE(mono$acceptMonotonicity)) {
    cat("Non-monotonic response - analysis stopped.\n")
    return(NULL)
  }
  cat("Monotonicity accepted.\n")

  # --- Trend test ---
  trend <- try(
    bmd::trendTest(
      data = data,
      x = dose_col,
      y = response_col,
      test = "tukey",
      level = 0.05
    ),
    silent = TRUE
  )
  if (inherits(trend, "try-error") || !isTRUE(trend$acceptTrend)) {
    cat("No significant trend - analysis stopped.\n")
    return(NULL)
  }
  cat("Trend accepted.\n")

  # --- Negative slope test ---
  negslope <- negslopetest(data, response_col, dose_col)

  # --- Check data type ---
  if (datatype != "continuous") {
    cat("Data is not continuous - skipping model fitting.\n")
    return(NULL)
  }

  # --- Fit models using continuousfit() ---
  results <- continuousfit(
    data = data,
    response_col = response_col,
    dose_col = dose_col,
    models = models,
    negslope = negslope
  )

  # Remove failed models
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    warning("No valid models for this subset - skipping plot.")
    return(NULL)
  }

  # --- Plotting (sticks close to your original code) ---
  try(
    plot(
      results[[1]],
      type = "obs",
      main = paste0(
        if (!is.null(factor_col)) {
          paste0("Factor: ", results[[1]]$origData[[factor_col]][[1]], " | ")
        } else {
          ""
        },
        "Response: ", results[[1]]$origData[[response_type_col]][[1]]
      )
    ),
    silent = TRUE
  )

  # --- Model averaging curve ---
 # try(
    curve(MACurve(x, results, "AIC"),
      add  = TRUE,
      col  = "hotpink"
    )#,
   # silent = TRUE
 # )


    # Capture the plot
    pl <- recordPlot()

    # Return both models and plot
    return(list(models = results, plot = pl))

}
