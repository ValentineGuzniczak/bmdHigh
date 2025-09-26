#' Master Analysis Function
#'
#' This function performs classification, monotonicity testing, trend testing,
#' slope testing, model fitting, and plotting of model-averaged curve.
#'
#' Internal function used by \code{\link{bmdHigh}}.
#'
#' @param data A data frame containing dose-response data.
#' @param dose_col Name of the column with dose values.
#' @param response_col Name of the column with response values.
#' @param factor1_col Optional name of the first grouping variable.
#' @param factor2_col Optional name of the second grouping variable.
#' @param response_type_col Name of the column describing response type.
#'
#' @importFrom grDevices recordPlot
#' @importFrom graphics curve
#' @importFrom bmd monotonicityTest trendTest MACurve bmdMA
#' @importFrom drc LL.4 W1.4 W2.4 LN.4
#'
#' @keywords internal
#'
master <- function(
    data,
    dose_col,
    response_col,
    factor1_col = NULL,
    factor2_col = NULL,
    response_type_col
) {
  # --- Extract columns ---
  dose          <- data[[dose_col]]
  x             <- sort(unique(dose))
  response      <- data[[response_col]]
  response_type <- data[[response_type_col]][[1]]

  # Values of the optional factors (as characters so factors print nicely)
  factor1_val <- if (!is.null(factor1_col)) as.character(data[[factor1_col]][1]) else NULL
  factor2_val <- if (!is.null(factor2_col)) as.character(data[[factor2_col]][1]) else NULL

  # --- Display processing info ---
  parts <- c(response_type)
  if (!is.null(factor1_col)) parts <- c(paste0(factor1_col, ": ", factor1_val), parts)
  if (!is.null(factor2_col)) parts <- c(paste0(factor2_col, ": ", factor2_val), parts)
  cat("\n--- Processing:", paste(parts, collapse = " - "), "---\n")

  # --- Classify response type ---
  datainfo <- classify_data(response)
  datatype <- datainfo$datatype
  models   <- datainfo$models

  if(datatype=="only NAs"){
    warning("The subset only contains NAs - analysis stopped.")
    return(NULL)
  }

  # --- Monotonicity test ---
  mono <- try(bmd::monotonicityTest(dose, response, test = "jonckheere"), silent = TRUE)
  if (inherits(mono, "try-error")) {
    warning("Monotonicity test failed - analysis stopped.")
    return(NULL)
  }
  if (!isTRUE(mono$acceptMonotonicity)) {
    cat("Non-monotonic response - analysis stopped.\n")
    return(NULL)
  }
  cat("Monotonicity accepted.\n")

  # --- Trend test ---
  trend <- try(
    bmd::trendTest(data = data,
                   x    = dose_col,
                   y    = response_col,
                   test = "tukey",
                   level = 0.05),
    silent = TRUE
  )
  if (inherits(trend, "try-error") || !isTRUE(trend$acceptTrend)) {
    cat("No significant trend - analysis stopped.\n")
    return(NULL)
  }
  cat("Trend accepted.\n")

  # --- Negative slope test ---
  negslope <- negslopetest(data, response_col, dose_col)

  if (datatype != "continuous") {
    cat("Data is not continuous - skipping model fitting.\n")
    return(NULL)
  }

  # --- Fit models ---
  results <- continuousfit(
    data         = data,
    response_col = response_col,
    dose_col     = dose_col,
    models       = models,
    negslope     = negslope
  )
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) {
    warning("No valid models for this subset - skipping plot.")
    return(NULL)
  }

  # --- Build the plot title with real column names ---
  title_parts <- c()
  if (!is.null(factor1_col))
    title_parts <- c(title_parts,
                     paste0(factor1_col, ": ",
                            results[[1]]$origData[[factor1_col]][[1]]), " | ")
  if (!is.null(factor2_col))
    title_parts <- c(title_parts,
                     paste0(factor2_col, ": ",
                            results[[1]]$origData[[factor2_col]][[1]]), " | ")
  title_parts <- c(title_parts,
                   paste0("Response: ",
                          results[[1]]$origData[[response_type_col]][[1]]))
  plot_title <- paste(title_parts, collapse = " ")

  # --- Plotting ---
  try(
    plot(
      results[[1]],
      type = "obs",
      main = plot_title
    ),
    silent = TRUE
  )

  # --- Model averaging curve ---
  try(
    curve(MACurve(x, results, "AIC"), add = TRUE, col = "hotpink"),
    silent = TRUE
  )
  # --- Record and return plot---
  pl <- recordPlot()

  ## --- Model-averaged Benchmark Dose
  bmd_est <- try(
    bmd::bmdMA(modelList = results,
               modelWeights = "AIC",
               bmr = 0.1,
               backgType = "modelBased",
               def = "relative",
               type = "bootstrap"),
    silent = TRUE
  )

  if (!inherits(bmd_est, "try-error")) {
    print("Model-averaged BMD succeeded.")
  } else {
    warning("Model-averaged BMD failed, checking individual BMDs.")

    model_names <- names(results)
    if (is.null(model_names) || any(model_names == "")) model_names <- paste0("model_", seq_along(results))

    print("Testing each model with bmd() ")
    ok <- logical(length(results))
    for (i in seq_along(results)) {
      nm <- model_names[i]
      res_try <- try(
        bmd::bmd(results[[i]],
                 bmr = 0.1,
                 backgType = "modelBased",
                 def = "relative"),
        silent = TRUE
      )

      if (inherits(res_try, "try-error")) {
        err_msg <- as.character(res_try)[1]
        print(sprintf("Model %s failed: %s", nm, err_msg))
        ok[i] <- FALSE
      } else {
        print(sprintf("Model %s succeeded", nm))
        ok[i] <- TRUE
      }
    }

    valid_models <- results[ok]
    print(paste("Valid BMDs found:", length(valid_models)))

    if (length(valid_models) == 0) {
      warning("No valid models remain after filtering.")
      bmd_est <- NULL
    } else {
      print("Retrying model-averaged BMD with valid models")
      bmd_est <- try(
        bmd::bmdMA(modelList = valid_models,
                   modelWeights = "AIC",
                   bmr = 0.1,
                   backgType = "modelBased",
                   def = "relative",
                   type = "bootstrap"),
        silent = TRUE
      )
      if (inherits(bmd_est, "try-error")) {
        warning("Second attempt at model-averaged BMD failed.")
        bmd_est <- NULL
      } else {
        print("Model-averaged BMD succeeded on filtered models.")
      }
    }
  }




  ## --- Return models, plot, bmdMA ---
  list(
    models = results,
    plot   = pl,
    bmd    = bmd_est
  )
}
