#' FigureQQPlot - Create and save QQ plots as PDF.
#'
#' Figure is a series of QQ plots of the variance decomposition model
#' standardized residual quantiles against the theoretical quantiles of the
#' standard Gaussian and standard Laplace.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param model an `lm` object; fitted variance decomposition model
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureQQPlot <- function(scan_fb, model, out_path = 'manu/figures') {
  resid_df <- cbind(model$model$log_pmmov, model$residuals) %>%
    data.frame() %>%
    magrittr::set_colnames(c('val', 'resid')) %>%
    dplyr::mutate('std_resid' = scale(resid),
                  'std_vals'  = scale(val))

  norm_qq <- ggplot(data = resid_df, ggplot2::aes(sample = std_vals)) +
    ggplot2::geom_qq(alpha = 0.5, color = 'darkred', distribution = stats::qnorm) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::labs(x = 'Theoretical Quantiles',
                  y = 'Sample Quantiles',
                  title = 'Gaussian') +
    ggplot2:: theme_bw() +
    ggplot2:: theme(aspect.ratio = 1)

  lap_qq <- ggplot2::ggplot(data = resid_df, ggplot2::aes(sample = std_vals)) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::geom_qq(alpha = 0.5, color = 'darkblue', distribution = jmuOutlier::qlaplace) +
    ggplot2::labs(x = 'Theoretical Quantiles',
                  y = 'Sample Quantiles',
                  title = 'Laplace') +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)


  ggpubr::ggarrange(norm_qq, lap_qq, ncol = 2) %>%
    ggplot2::ggsave(height = 4, width = 8, filename = file.path(out_path, 'figure_qqplot.pdf'))
}
