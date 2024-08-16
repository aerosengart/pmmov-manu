#' FigureBoxplot - Create and save boxplot of sites as PDF.
#'
#' Figure is a box and whisker plot of the SCAN data ordered along the x-axis by site longitude.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureBoxplot <- function(scan_fb, out_path = 'manu/figures') {
  ## create box and whisker plot for pmmov variation
  ## color by sample type, arrange by longitude
  sample_df <- scan_fb %>%
    dplyr::arrange(lng)

  site_order <- unique(sample_df$abbreviation)

  sample_df <- sample_df %>%
    dplyr::mutate(abbreviation = factor(abbreviation, levels = site_order))

  sample_df <- sample_df[sample_df$log_pmmov > stats::quantile(sample_df$log_pmmov, 0.005) &
                           sample_df$log_pmmov < stats::quantile(sample_df$log_pmmov, 0.995), ]

  annot_hi <- scan_fb %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::summarise(median = stats::median(log_pmmov)) %>%
    dplyr::arrange(-median)
  annot_hi <- annot_hi[1:3, ]
  max <- sample_df %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::summarise(max = max(log_pmmov)) %>%
    dplyr::filter(abbreviation %in% annot_hi$abbreviation)
  annot_hi <- dplyr::left_join(annot_hi, max, by = 'abbreviation')
  annot_hi$label <- c('H', 'H', 'H')

  annot_lo <- scan_fb %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::summarise(median = stats::median(log_pmmov)) %>%
    dplyr::arrange(median)
  annot_lo <- annot_lo[1:3, ]
  min <- sample_df %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::summarise(min = min(log_pmmov)) %>%
    dplyr::filter(abbreviation %in% annot_lo$abbreviation)
  annot_lo <- dplyr::left_join(annot_lo, min, by = 'abbreviation')
  annot_lo$label <- c('L', 'L', 'L')

  box_and_whisker <- ggplot2::ggplot(data = sample_df,
                                     mapping = ggplot2::aes(x = abbreviation, y = log_pmmov)) +
    ggplot2::geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, outlier.color = 'grey40', outlier.shape = 4) +
    ggplot2::stat_summary(fun = "mean", geom = "point", shape = 8, size = 1, color = "blue") +
    ggrepel::geom_label_repel(annot_hi,
                              mapping = ggplot2::aes(y = max, label = label),
                              size = 2,
                              nudge_y = 0.1,
                              nudge_x = c(0.3, 0.02, -0.02),
                              color = "darkred",
                              min.segment.length = 0.01,
                              label.padding = 0.1) +
      ggrepel::geom_label_repel(annot_lo,
                                mapping = ggplot2::aes(y = min, label = label),
                                size = 2,
                                nudge_y = -0.1,
                                nudge_x = c(0.3, -0.4, -0.1),
                                color = "darkred",
                                min.segment.length = 0.01,
                                label.padding = 0.1) +
    ggplot2::scale_color_brewer(palette = 'Dark2') +
    ggplot2::guides(color = 'none') +
    ggplot2::labs(y = "PMMoV (log10 gc/g dry wt)",
                  title = "PMMoV by Site and Longitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.title.x     = ggplot2::element_blank(),
                   axis.title.y     = ggplot2::element_text(size = 10),
                   axis.text.y      = ggplot2::element_text(size = 8),
                   axis.text.x      = ggplot2::element_text(size = 5, angle = 45, hjust = 1),
                   plot.margin      = grid::unit(c(0.25,0.25,0.25,0.25), "cm"))
  ggplot2::ggsave(box_and_whisker,
                  height = 8,
                  width = 15,
                  filename = file.path(out_path, 'figure_boxplot.pdf'))
}
