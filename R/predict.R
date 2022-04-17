#' Title
#'
#' @param model A `gam` or `bam` model object.
#' @param length_out An integer indicating how many values to use along the
#'   numeric variables for predicting the response (the default is `10`).
#' @param values User supplied values for specific variables as a named list.
#' @param series A string specifying the variable that corresponds to the series
#'   to be plotted on the $x$-axis. If a string is given, the other numeric
#'   variables in the model are set to their mean value, unless specific values
#'   are given in `values`.
#' @param exclude_terms Terms to be excluded from the prediction. Term names
#'   should be given as they appear in the model summary (for example,
#'   `"s(x0,x1)"`).
#' @param ci_z The z-value for calculating the CIs (the default is `1.96` for
#'   95\% CI).
#'
#' @return A tibble with predictions.
#' @export
#' @examples
#' library(mgcv)
#' set.seed(10)
#' sim_data <- gamSim(1, n = 200, scale = 2)
#' model <- gam(y ~ x0 + s(I(x1^2)) + s(x2) + offset(x3), data = sim_data)
#' predict_gam(model)
#' predict_gam(model, values = list(x0 = mean(sim_data$x0)))
#' predict_gam(model, series = "x2")
#' predict_gam(model, exclude_terms = "s(I(x1^2))")
#'
#' sim_data <- gamSim(4)
#' model_2 <- gam(y ~ s(x2, by = fac) + s(x0), data = sim_data)
#' predict_gam(model_2)
predict_gam <- function(model, length_out = 10, values = NULL,
                        series = NULL, exclude_terms = NULL,
                        ci_z = 1.96) {
  the_data <- insight::get_data(model)
  predictors <- insight::find_predictors(model, flatten = TRUE)
  response <- insight::find_response(model)

  terms <- model[["terms"]]
  pterms <- model[["pterms"]]
  smooths <- model[["smooth"]]

  smooths_terms <- unlist(lapply(smooths, function(x) x$label))
  smooths_vars <- unlist(lapply(smooths, function(x) x$term))

  excluded_vars <- NULL
  offset_var <- NULL

  offset_idx <- attr(pterms, "offset")
  if (!is.null(offset_idx)) {
    pterms_names <- names(attr(pterms, "dataClasses"))
    offset_term <- pterms_names[offset_idx]
    offset_var <- insight::clean_names(offset_term)
  }

  if (!is.null(exclude_terms)) {
    for (term in exclude_terms) {
      term_idx <- which(smooths_terms == term)
      term_var <- insight::clean_names(smooths_vars[term_idx])
      excluded_vars <- c(excluded_vars, term_var)
    }

    predictors <- predictors[-which(predictors %in% excluded_vars)]
  }

  pred_grid <- lapply(predictors, function(x) {
    if (x %in% names(values)) {
      values[[which(names(values) == x)]]
    } else if (!is.null(offset_var)) {
      if (x == offset_var) {
        0
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
  pred_out[[response]] <- fit
  pred_out$se <- se
  pred_out$lower_ci <- fit - se * ci_z
  pred_out$upper_ci <- fit + se * ci_z

  class(pred_out) <- c("tidygam", class(pred_out))
  attr(pred_out, "reponse") <- response

  return(pred_out)
}
