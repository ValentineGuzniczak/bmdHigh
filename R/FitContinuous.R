#' Fit Continuous Dose-Response Models
#'
#' Fits a set of continuous dose-response models using the \code{drc} package.
#'
#' @param data A data frame containing the response and dose variables.
#' @param response_col Name of the response column in \code{data}.
#' @param dose_col Name of the dose column in \code{data}.
#' @param models A list of model functions from \code{drc} (e.g. LL.4(), W1.4(), BC.5()).
#' @param negslope Logical. If TRUE, adds the \code{BC.5()} model for negative slopes.
#'
#' @importFrom drc drm
#' @importFrom drc LL.4 W1.4 W2.4 LN.4 BC.5
#'
#' @return A named list of fitted models. Models that fail return \code{NULL}.
#' @export
continuousfit <- function(data,
                          response_col = "responsevalue",
                          dose_col = "Dose",
                          models,
                          negslope) {

  # --- Input validation ---
  if (!all(c(response_col, dose_col) %in% colnames(data))) {
    stop("Data must contain columns: ", response_col, " and ", dose_col)
  }

  # --- Add BC.5 model if slope is negative ---
  if (isTRUE(negslope)) {
    models <- append(models, list(drc::BC.5()))
  }

  # --- Assign model names for clarity ---
  names(models) <- sapply(models, function(m) m$name)

  # --- Fit models ---
  fittedmodels <- list()
  for (model_name in names(models)) {
    model <- models[[model_name]]

    modelfit <- try(
      drm(
        formula = as.formula(paste(response_col, "~", dose_col)),
        data = data,
        fct = model,
        type = "continuous"
      ),
      silent = TRUE
    )

    # --- Save model or NULL ---
    if (inherits(modelfit, "try-error")) {
      warning(paste("Model failed for", model_name))
      fittedmodels[[model_name]] <- NULL
    } else {
      fittedmodels[[model_name]] <- modelfit
    }
  }

  return(fittedmodels)
}
