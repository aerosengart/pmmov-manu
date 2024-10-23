#' FigureVariance - Create and save variance decomposition figure as PDF.
#'
#' Figure is a stacked bar plot of the variance partition created from Model 3.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param var_part a data frame; variance partition for linear model (created with \code{\link{RunVarPart}})
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureVariance <- function(scan_fb, var_part, out_path = 'manu/figures') {
  var_part$name <- factor(var_part$name, levels = rev(var_part$name))
  if (nrow(var_part) == 5) {
    lm_r2 <- formatC(var_part$percent[4] * 100, digits = 2, format = 'f')
    lm_vp_plot <- ggplot2::ggplot(data = var_part,
                                  mapping = ggplot2::aes(x    = label,
                                                         y    = model,
                                                         fill = name)) +
      ggplot2::geom_bar(stat = "identity", position = 'fill', width = 0.2) +
      viridis::scale_fill_viridis(option = 'turbo', discrete = TRUE) +
      ggrepel::geom_label_repel(var_part,
                                nudge_y            = c(-0.3, 0.3, -0.3, 0.3, -0.3),
                                min.segment.length = 0.1,
                                mapping = ggplot2::aes(x     = label_y, fill = NULL,
                                                       label = paste(round(label * 100, 2), '%'))) +
      ggplot2::scale_y_discrete(labels = c(bquote(R^{2}==.(lm_r2)*"%"))) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::labs(y = NULL, x = NULL, fill = 'Covariate Group') +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = ggplot2::aes(label = ""),
                                                   reverse = TRUE)) +
      ggplot2::theme(aspect.ratio    = 1/6,
                     plot.margin     = grid::unit(c(-8, 0, -8, 0), "cm"),
                     text            = ggplot2::element_text(size = 14),
                     legend.position = 'top')
    ggplot2::ggsave(lm_vp_plot, file = file.path(out_path, 'figure_variance.pdf'), width = 10, height = 3)
  } else {
    lm_r2 <- formatC(var_part$percent[6] * 100, digits = 2, format = 'f')
    lm_vp_plot <- ggplot2::ggplot(data = var_part,
                                  mapping = ggplot2::aes(x    = label,
                                                         y    = model,
                                                         fill = name)) +
      ggplot2::geom_bar(stat = "identity", position = 'fill', width = 0.2) +
      viridis::scale_fill_viridis(option = 'turbo', discrete = TRUE) +
      ggrepel::geom_label_repel(var_part,
                                nudge_y            = c(-0.3, 0.3, -0.3, 0.3, -0.3 , 0.3, -0.3),
                                nudge_x            = c(0.0, 0.0, 0.0, -0.05, 0.0, 0.05),
                                min.segment.length = 0.1,
                                mapping = ggplot2::aes(x     = label_y,
                                                       fill  = NULL,
                                                       label = paste(name, '\n', round(label * 100, 2), '%'))) +
      ggplot2::scale_y_discrete(labels = c(bquote(R^{2}==.(lm_r2)*"%"))) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::labs(y = NULL, x = NULL, fill =  NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(aspect.ratio    = 1/6,
                     plot.margin     = grid::unit(c(-8, 0, -8, 0), "cm"),
                     text            = ggplot2::element_text(size = 14),
                     legend.position = 'none')
    ggplot2::ggsave(lm_vp_plot, file = file.path(out_path, 'figure_variance_interactions.pdf'), width = 10, height = 3)
  }
}
