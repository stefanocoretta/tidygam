#' Get difference between two smooths
#'
#' @param compare A named list of factor levels to compare.
#' @inheritParams predict_gam
#'
#' @return A tibble with the difference smooth.
#' @export
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ s(x2, by = fac) + s(x0), data = data)
#'
#' get_difference(model, "x2", list(fac = c("1", "2")))
get_difference <- function(model, series, compare, values = NULL,
                           exclude_terms = NULL,
                           length_out = 25, ci_z = 1.96) {
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
    } else if (x %in% names(compare)) {
      compare[[1]]
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
  pred_grid_a <- dplyr::filter(pred_grid, .data[[names(compare[1])]] == compare[[1]][1])
  pred_grid_b <- dplyr::filter(pred_grid, .data[[names(compare[1])]] == compare[[1]][2])

  pred_a <- mgcv::predict.gam(model, pred_grid_a, type = "lpmatrix")
  pred_b <- mgcv::predict.gam(model, pred_grid_b, type = "lpmatrix")

  pred_diff <- pred_a - pred_b
  diff <- as.vector(pred_diff %*% stats::coef(model))

  se <- sqrt(rowSums((pred_diff %*% stats::vcov(model)) * pred_diff))

  diff_out <- pred_grid_a
  diff_out[[names(compare[1])]] <- paste0(compare[[1]][1], "-", compare[[1]][2])

  diff_out$diff <- diff
  diff_out$se <- se
  diff_out$lower_ci <- diff - se * ci_z
  diff_out$upper_ci <- diff + se * ci_z

  n_sig <- which(!(diff_out$upper_ci >= 0 & diff_out$lower_ci <= 0))

  if (length(n_sig) == 0) {
    sig_int <- NULL
  } else {
    n_prev <- c(NA, n_sig[1:(length(n_sig) - 1)])
    n_next <- c(n_sig[2:length(n_sig)], NA)
    sig_int <- list(
      start = diff_out[[series]][n_sig[which(is.na(n_sig - n_prev) | (n_sig - n_prev) > 1)]],
      end = diff_out[[series]][n_sig[which(is.na(n_next - n_sig) | (n_next - n_sig) > 1)]]
    )
  }

  class(diff_out) <- c("tidygam.diff", class(diff_out))
  attr(diff_out, "response") <- response
  attr(diff_out, "series") <- series
  attr(diff_out, "compare") <- compare
  attr(diff_out, "sig_int") <- sig_int

  return(diff_out)
}
