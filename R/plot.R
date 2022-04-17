#' Plot methods for tidygam objects
#'
#' Plotting methods for `tidygam` objects.
#'
#' @param gam_pred A `tidygam` object (see [predict_gam()]).
#' @param comparison Name of a categorical predictor to compare as a string.
#' @inheritParams predict_gam
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' sim_data <- gamSim(4)
#'
#' model <- gam(y ~ s(x2, by = fac) + s(x0), data = sim_data)
#'
#' preds <- predict_gam(model, length_out = 50, exclude_terms = "s(x0)")
#' plot(preds, "x2")
#'
#' preds_2 <- predict_gam(model, length_out = 100, values = list(x0 = 0))
#' plot(preds_2, "x2", "fac")
plot.tidygam <- function(gam_pred, series = NULL, comparison = NULL) {
  response <- attr(gam_pred, "response")

  if (rlang::is_empty(series)) {
    series <- attr(gam_pred, "series")
    if (rlang::is_empty(series)) {
      cli::cli_abort("Please, provide a value for the series argument.")
    }
  }

  groupings <- dplyr::select(
    gam_pred,
    -se, -lower_ci, -upper_ci, -.data[[series]], -.data[[response]]
  ) %>%
    dplyr::rowwise() %>%
    tidyr::unite("groupings", everything()) %>%
    dplyr::pull(groupings)

  gam_pred$groupings <- groupings

  gam_pred %>%
    ggplot2::ggplot(
      ggplot2::aes(
        .data[[series]], .data[[response]],
        group_by = .data$groupings
      )
    ) +

    {if (!is.null(comparison)) {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci, fill = .data[[comparison]]), alpha = 0.5)
    } else {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.5)
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

}
