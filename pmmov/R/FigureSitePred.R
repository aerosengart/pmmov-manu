#' FigureSitePred - Create and save site predictions as PDF.
#'
#' The site predictions are illustrated as a grid of the predicted time series made with Model 4.
#' Line is the median, bands are the 95% prediction interval, and points are the observed values.
#' Rug is colored by average daily precipitation for the area.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param pred_inters a data frame; predictions from generalized Gaussian model (Model 4) for site(s)
#' @param prcp_inters a data frame; precipitation data for site(s)
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureSitePred <- function(scan_fb, pred_inters, prcp_inters, out_path = 'manu/figures/site_preds') {
  dir.create('manu/figures/site_preds', showWarnings = FALSE)

  pred_prcp_df <- prcp_inters %>%
    dplyr::filter(!is.na(prcp)) %>%
    dplyr::filter(prcp > 0)

  pred_inters_df <- pred_inters %>%
    dplyr::mutate(date = as.Date(date))

  sites <- unique(scan_fb$site)

  for (i in 1:length(sites)) {
    site_prcp <- pred_prcp_df %>%
      dplyr::filter(site == sites[i])
    site_pred <- pred_inters_df %>%
      dplyr::filter(site == sites[i])

    pred_comp <- ggplot2::ggplot(site_pred, ggplot2::aes(x = date, y = `50\\%`, group = 1)) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = `2.5\\%`, ymax = `97.5\\%`),
                           fill = 'lightpink', alpha = 0.5) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = date, y = log_pmmov),
                          alpha = 0.4, color = 'darkgreen') +
      ggplot2::geom_line(mapping = ggplot2::aes(group = 1)) +
      ggplot2::scale_color_gradient(low = ggplot2::alpha("grey40", 0),
                                    high = "blue",
                                    limits = c(-1, 4.5),
                                    breaks = c(0, 1, 2, 3, 4),
                                    labels = c(0, 1, 2, 3, 4)) +
      ggplot2::geom_rug(data = site_prcp,
                        mapping = ggplot2::aes(x = date, color = prcp, alpha = prcp),
                        inherit.aes = FALSE,
                        length = grid::unit(0.075, "npc")) +
      ggplot2::labs(y = 'PMMoV (log10 gc/g dry wt)',
                    x = NULL,
                    color = 'Average Daily Precipitation (in)') +
      ggplot2::scale_x_date(date_labels = "%b %d '%y",
                            date_breaks = '2 month') +
      ggplot2::theme_bw() +
      ggplot2::guides(alpha = "none", size = "none") +
      ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                     strip.text      = ggplot2::element_text(size = 10),
                     legend.position = 'top',
                     plot.margin     = grid::unit(c(1, 1, 1, 3), "mm"))

    ggplot2::ggsave(pred_comp, height = 8, width = 10, filename = file.path(out_path, paste0(sites[i], '_preds.pdf')))


  }
}
