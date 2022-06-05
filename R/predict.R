#' Get predictions from a GAM model
#'
#' Return predictions from a GAM model generated with mgcv. The output can be plotted with `plot()`.
#'
#' @param model A `gam` or `bam` model object.
#' @param length_out An integer indicating how many values to use along the
#'   numeric variables for predicting the response (the default is `10`).
#' @param values User supplied values for specific variables as a named list.
#' @param series A string specifying the variable that corresponds to the series
#'   to be plotted on the $x$-axis. If a string is given, the other numeric
#'   variables in the model are set to their mean value, unless specific values
#'   are given in `values`. If a character vector of two strings is given, the
#'   two variables will be taken as the elements of a tensor product smooth.
#'   This allows the user to plot 2D raster plots.
#' @param exclude_terms Terms to be excluded from the prediction. Term names
#'   should be given as they appear in the model summary (for example,
#'   `"s(x0,x1)"`).
#' @param ci_z The z-value for calculating the CIs (the default is `1.96` for
#'   95 percent CI).
#' @param tran_fun Function to use for transforming the predicted values and CIs.
#' @param separate Names list of factor interaction variables to be separated.
#' @param sep_by Character to separate by (the default is `\\.`).
#'
#' @return A tibble with predictions.
#' @export
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#'
#' sim_data_1 <- gamSim(1, n = 200, scale = 2)
#' model <- gam(y ~ x0 + s(I(x1^2)) + s(x2) + offset(x3), data = sim_data_1)
#' predict_gam(model)
#' predict_gam(model, values = list(x0 = mean(sim_data_1$x0)))
#' predict_gam(model, series = "x2")
#' predict_gam(model, exclude_terms = "s(I(x1^2))")
#'
#' # By-variables
#' sim_data_2 <- gamSim(4)
#' model_2 <- gam(y ~ s(x2, by = fac) + s(x0), data = sim_data_2)
#' predict_gam(model_2)
#'
#' # Poisson data
#' sim_data_3 <- sim_data_2
#' sim_data_3$y <- round(sim_data_2$y) + 20
#' model_3 <- gam(y ~ s(x2, by = fac), data = sim_data_3, family = poisson)
#' predict_gam(model_3, length_out = 50)
#' predict_gam(model_3, length_out = 50, tran_fun = exp)
#'
#' # Bivariate smooths
#' model_4 <- gam(y ~ te(x1, x2), data = sim_data_1)
#' predict_gam(model_4)
predict_gam <- function(model, length_out = 10, values = NULL,
                        series = NULL, exclude_terms = NULL,
                        ci_z = 1.96, tran_fun = NULL,
                        separate = NULL, sep_by = "\\.") {
  the_data <- insight::get_data(model)
  predictors <- insight::find_predictors(model, flatten = TRUE)
  response <- insight::find_response(model)

  terms <- model[["terms"]]
  pterms <- model[["pterms"]]
  smooths <- model[["smooth"]]

  smooths_terms <- lapply(smooths, function(x) x$label)
  smooths_vars <- lapply(smooths, function(x) x$term)

  offset_var <- NULL

  offset_idx <- attr(pterms, "offset")
  if (!is.null(offset_idx)) {
    pterms_names <- names(attr(pterms, "dataClasses"))
    offset_term <- pterms_names[offset_idx]
    offset_var <- insight::clean_names(offset_term)
  }

  if (!is.null(exclude_terms)) {
    term_idxs <- NULL
    for (term in exclude_terms) {
      term_idxs <- c(term_idxs, which(smooths_terms == term))
    }

    to_exclude <- unique(
      unlist(
        lapply(smooths_vars[term_idxs], function(x) insight::clean_names(x))
      )
    )
    to_keep <- unique(
      unlist(
        lapply(smooths_vars[-term_idxs], function(x) insight::clean_names(x))
      )
    )
    excluded_vars <- setdiff(to_exclude, to_keep)

    predictors <- predictors[-which(predictors %in% excluded_vars)]
  }

  pred_grid <- lapply(predictors, function(x) {
    if (x %in% names(values)) {
      values[[which(names(values) == x)]]
    } else if (!is.null(offset_var)) {
      if (x == offset_var) {
        # If rate ratios are used in the offset, i.e. if log() is used,
        # then the offset needs to be 1. log(0) would be -Inf.
        if (stringr::str_detect(offset_term, "log")) {
          1
        } else {
          0
        }
      } else {
        # TODO: fix code repetition (see last else in the chain)
        if (is.numeric(the_data[[x]])) {
          if (!is.null(series)) {
            if (x == series) {
              min_x <- range(the_data[[x]])[[1]]
              max_x <- range(the_data[[x]])[[2]]
              range <- max_x - min_x
              by <- range / length_out
              seq(min_x, max_x, by = by)
            } else {
              mean(the_data[[x]])
            }
          } else {
            min_x <- range(the_data[[x]])[[1]]
            max_x <- range(the_data[[x]])[[2]]
            range <- max_x - min_x
            by <- range / length_out
            seq(min_x, max_x, by = by)
          }
        } else {
          unique(the_data[[x]])
        }
      }
    } else {
      if (is.numeric(the_data[[x]])) {
        if (!is.null(series)) {
          if (x %in% series) {
            min_x <- range(the_data[[x]])[[1]]
            max_x <- range(the_data[[x]])[[2]]
            range <- max_x - min_x
            by <- range / length_out
            seq(min_x, max_x, by = by)
          } else {
            mean(the_data[[x]])
          }
        } else {
          min_x <- range(the_data[[x]])[[1]]
          max_x <- range(the_data[[x]])[[2]]
          range <- max_x - min_x
          by <- range / length_out
          seq(min_x, max_x, by = by)
        }
      } else {
        unique(the_data[[x]])
      }
    }
  })

  names(pred_grid) <- predictors
  pred_grid <- tibble::as_tibble(expand.grid(pred_grid))

  if (is.null(exclude_terms)) {
    preds <- mgcv::predict.gam(model, newdata = pred_grid, se.fit = TRUE)
    fit <- preds$fit
    se <- preds$se
  } else {
    preds_terms <- mgcv::predict.gam(model, newdata = pred_grid, se.fit = TRUE, type = "terms", exclude = exclude_terms, newdata.guaranteed = TRUE)

    preds_fit <- dplyr::mutate(
      tibble::as_tibble(preds_terms$fit),
      fit = rowSums(dplyr::across())
    )
    fit <- preds_fit$fit + attr(preds_terms, "constant")[["(Intercept)"]]

    preds_se <- dplyr::mutate(
      tibble::as_tibble(preds_terms$se.fit),
      se = rowSums(dplyr::across())
    )
    se <- preds_se$se
  }

  pred_out <- pred_grid

  if (!is.null(tran_fun)) {
    pred_out[[response]] <- tran_fun(fit)
    pred_out$se <- se
    pred_out$lower_ci <- tran_fun(fit - se * ci_z)
    pred_out$upper_ci <- tran_fun(fit + se * ci_z)
  } else {
    pred_out[[response]] <- fit
    pred_out$se <- se
    pred_out$lower_ci <- fit - se * ci_z
    pred_out$upper_ci <- fit + se * ci_z
  }

  if (!is.null(separate)) {
    for (var in 1:length(separate)) {
      pred_out <- tidyr::separate(
        pred_out,
        .data[[names(separate[var])]],
        separate[[var]],
        sep = sep_by
      )
    }
  }

  class(pred_out) <- c("tidygam", class(pred_out))
  attr(pred_out, "response") <- response
  if (is.null(series)) {
    attr(pred_out, "series") <- character()
  } else {
    attr(pred_out, "series") <- series
  }

  return(pred_out)
}
