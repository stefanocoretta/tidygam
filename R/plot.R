#' Plot methods for tidygam objects
#'
#' Plotting methods for `tidygam` objects.
#'
#' @param x A `tidygam` object (see [predict_gam()]).
#' @param comparison Name of a categorical predictor to compare as a string.
#' @param raster_interp Whether to linearly interpolate when plotting a tensor
#'   product smooth/interaction. It makes sense only when `series` has two
#'   variables. The default is `FALSE`.
#' @param ... Arguments passed to `plot()`.
#' @inheritParams predict_gam
#'
#' @return A `ggplot` object.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' sim_data <- gamSim(4)
#'
#' model_1 <- gam(y ~ s(x2, by = fac) + s(x0), data = sim_data)
#'
#' preds_1 <- predict_gam(model_1, length_out = 50, exclude_terms = "s(x0)")
#' plot(preds_1, "x2")
#'
#' preds_2 <- predict_gam(model_1, length_out = 100, values = list(x0 = 0))
#' plot(preds_2, "x2", "fac")
#' library(ggplot2)
#' plot(preds_2, "x2", "fac") +
#'   scale_fill_brewer(type = "qual") +
#'   scale_color_brewer(type = "qual")
#'
#' # Plotting tensor product smooths/interactions
#' model_2 <- gam(y ~ te(x0, x2, by = fac), data = sim_data)
#' preds_3 <- predict_gam(model_2)
#' preds_3 %>% plot(series = c("x0", "x2"), comparison = "fac")
plot.tidygam <- function(x, series = NULL, comparison = NULL,
                         raster_interp = FALSE, ...) {
  response <- attr(x, "response")

  if (rlang::is_empty(series)) {
    series <- attr(x, "series")
    if (rlang::is_empty(series)) {
      cli::cli_abort("Please, provide a value for the series argument.")
    }
  }

  if (length(series) == 1) {
    groupings <- dplyr::select(
      x,
      -.data$se, -.data$lower_ci, -.data$upper_ci, -.data[[series]], -.data[[response]]
    )

    # Check that there are variables to group by. If not, create a dummy
    # grouping variable with just 1s.
    if (dim(groupings)[2] > 0) {
      groupings <- groupings %>%
        dplyr::rowwise() %>%
        tidyr::unite("groupings", tidyselect::everything()) %>%
        dplyr::pull(groupings)
    } else {
      groupings <- 1
    }

    x$groupings <- groupings

    x %>%
      ggplot2::ggplot(
        ggplot2::aes(
          .data[[series]], .data[[response]],
          group_by = .data$groupings
        )
      ) +

      {if (!is.null(comparison)) {
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci, fill = .data[[comparison]]), alpha = 0.5)
      } else {
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci), alpha = 0.5)
      }} +

      {if (!is.null(comparison)) {
        ggplot2::geom_path(ggplot2::aes(colour = .data[[comparison]]))
      } else {
        ggplot2::geom_path()
      }} +

      {if (!is.null(comparison)) {
        ggplot2::geom_point(ggplot2::aes(colour = .data[[comparison]]), size = 0.5)
      } else {
        ggplot2::geom_point(size = 0.5)
      }}
  } else if (length(series) == 2) {
    x %>%
      ggplot2::ggplot(
        ggplot2::aes(
          .data[[series[1]]], .data[[series[2]]],
          fill = .data[[response]]
        )
      ) +
      ggplot2::geom_raster(interpolate = raster_interp) +
      {if (!is.null(comparison)) {
        ggplot2::facet_wrap(~ .data[[comparison]])
      }}
  } else {
    cli::cli_abort("More than two variables as series are not supported. Please specify only one or two.")
  }


}


#' Plot methods for tidygam.diff objects
#'
#' Plotting methods for `tidygam.diff` objects.
#'
#' @param x A `tidygam.diff` object (see [get_difference()]).
#' @param ... Arguments passed to `plot()`.
#' @param sig Shade the interval(s) where the difference smooth does not include 0 (default is `TRUE`).
#' @param sig_col Colour for the shading (default is `"red"`).
#' @param sig_alpha Alpha level for the shading (default is `0.25`)
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ s(x2, by = fac) + s(x0), data = data)
#'
#' model_diff <- get_difference(model, "x2", list(fac = c("1", "2")))
#' plot(model_diff)
plot.tidygam.diff <- function(x, ..., sig = TRUE, sig_col = "red", sig_alpha = 0.25) {
  response <- attr(x, "response")
  series <- attr(x, "series")
  compare <- attr(x, "compare")
  sig_int <- attr(x, "sig_int")

  x %>%
    ggplot2::ggplot(
      ggplot2::aes(
        .data[[series]],
        .data$diff
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, alpha = 0.5) +
    # If sig_int is null don't annotate
    {if (sig & !is.null(sig_int)) {
      ggplot2::annotate(
        "rect",
        xmin = sig_int$start, xmax = sig_int$end,
        ymin = -Inf, ymax = Inf, alpha = sig_alpha,
        fill = sig_col
      )
    }} +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci), alpha = 0.5) +
    ggplot2::geom_path() +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::labs(
      y = glue::glue("Difference of {compare[[1]][1]} and {compare[[1]][2]} ({names(compare[1])})")
    )
}
