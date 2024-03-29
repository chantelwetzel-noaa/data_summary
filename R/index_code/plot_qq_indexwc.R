
qq_indexwc <- function(fit, file_name = "qq.png") {
  # See issue #11 regarding better QQ plots but the following code that is
  # commented out leads to R crashing. I am uncertain if this is because the
  # mcmc prediction function cannot handle delta models?
  # samples <- sdmTMBextra::predict_mle_mcmc(
  #   fit,
  #   mcmc_iter = 201,
  #   mcmc_warmup = 200
  # )
  fit_residuals <- purrr::map(
    .x = seq_along(fit[["formula"]]),
    .f = stats::residuals,
    # mcmc_samples = samples,
    # type = "mle-mcmc",
    type = "mle-laplace",
    object = fit
  )
  data <- purrr::map_df(
    .x = fit_residuals,
    .f = stats::qqnorm,
    .id = "model",
    plot.it = FALSE
  ) %>%
    dplyr::filter(!is.na(x))
  
  slopes_intercepts <- data %>%
    dplyr::select(-x) %>%
    dplyr::group_split(model, .keep = FALSE) %>%
    purrr::map_df(
      .f = ~ qqline_parameters(.x[["y"]]),
      .id = "model"
    )
  
  # Create the {ggplot2} figure with one set of points per model component
  # using color-blind friendly colors, where the first color is a dark purple
  # and is pretty close to black.
  gg <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = x,
      y = y,
      col = model
    )
  ) +
    ggplot2::geom_point() +
    viridis::scale_color_viridis(
      discrete = TRUE,
      name = "model component",
      labels = c("presence/absence", "catch rate")
    ) +
    ggplot2::xlab("Theoretical quantiles") +
    ggplot2::ylab("Sample quantiles") +
    ggplot2::labs(title = "Normal quantile-quantile plot") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position =
                     if (length(unique(data[["model"]])) > 1) {
                       c(0.15, 0.9)
                     } else {
                       "none"
                     }
    ) +
    ggplot2::geom_abline(
      mapping = ggplot2::aes(
        slope = slope,
        intercept = intercept,
        color = model
      ),
      data = slopes_intercepts
    )
  # Only save the figure if file_name is present
  if (!is.null(file_name)) {
    ggplot2::ggsave(
      filename = file_name,
      plot = gg,
      width = 7,
      height = 7
    )
  }
  return(invisible(gg))
}

#' Estimate the slope and intercept for a Q-Q plot
#'
#' The parameters are not returned from [stats::qqline()] and the line is often
#' not the same as `abline(0, 1)`. Thus, to add the line to a
#' [ggplot2::ggplot()] figure, we must first calculate the slope and intercept
#' before calling [abline()] or the equivalent ourselves.
#' @noRd
#' @author Kelli F. Johnson
qqline_parameters <- function(y) {
  y_quantiles <- as.vector(quantile(
    x = y,
    probs = c(0.25, 0.75),
    names = FALSE,
    type = 7,
    na.rm = TRUE
  ))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y_quantiles) / diff(x)
  intercept <- y_quantiles[[1L]] - slope * x[[1L]]
  return(c(intercept = intercept, slope = slope))
}