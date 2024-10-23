#' FigureSiteComparison - Create and save site comparison figure as PDF.
#'
#' Figure is a grid of the predicted time series made with the individual site Bayesian models.
#' Line is the median, bands are the 95% prediction interval, and points are the observed values.
#' Rug is colored by average daily precipitation for the area.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param pred_inters a data frame; predictions from generalized Gaussian model for site(s)
#' @param prcp_inters a data frame; precipitation data for site(s)
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureSiteComparison <- function(scan_fb, pred_inters, prcp_inters, out_path = 'manu/figures') {
  pred_prcp_df <- prcp_inters %>%
    dplyr::filter(!is.na(prcp)) %>%
    dplyr::filter(prcp > 0) %>%
    dplyr::mutate(loc = factor(loc, levels = unique(pred_inters$loc)))

  pred_inters_df <- pred_inters %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(loc = factor(loc, levels = unique(pred_inters$loc)))

  pred_comp <- ggplot2::ggplot(pred_inters_df, ggplot2::aes(x = date, y = `50\\%`, group = 1)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = `2.5\\%`, ymax = `97.5\\%`),
                         fill = 'lightpink', alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = date, y = log_pmmov),
                        alpha = 0.4, color = 'darkgreen') +
    ggplot2::geom_line(mapping = ggplot2::aes(group = 1)) +
    ggplot2::scale_color_gradient(low = ggplot2::alpha("grey40", 0),
                                  high = "blue",
                                  limits = c(0, 4.5),
                                  breaks = c(0, 1, 2, 3, 4),
                                  labels = c(0, 1, 2, 3, 4)) +
    ggplot2::geom_rug(data = pred_prcp_df,
                      mapping = ggplot2::aes(x = date, color = prcp, alpha = prcp),
                      inherit.aes = FALSE,
                      length = grid::unit(0.075, "npc")) +
    ggplot2::facet_wrap(~loc, nrow = 2, scales = 'free') +
    ggplot2::labs(y = 'PMMoV (log10 gc/g dry wt)',
                  x = NULL,
                  color = 'Average Daily Precipitation (in)') +
    ggplot2::scale_x_date(date_labels = "%b %d '%y",
                          date_breaks = '1 month') +
    ggplot2::theme_bw() +
    ggplot2::guides(alpha = "none", size = "none") +
    ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 45, hjust = 0.85, size = 8),
                   strip.text      = ggplot2::element_text(size = 10),
                   legend.position = 'top',
                   plot.margin     = grid::unit(c(1, 1, 1, 3), "mm"))

  ggplot2::ggsave(pred_comp, height = 8, width = 10, filename = file.path(out_path, 'figure_site_comp.pdf'))
}
