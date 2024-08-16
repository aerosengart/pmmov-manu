#' FigurePeaks - Create and save peak plot as PDF.
#'
#' Figure is a scatterplot of peak dates for each site.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param peak_trough a data frame; calculated peaks and troughs of sinusoids for each site
#' @param lambda a float; period length (in days)
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigurePeaks <- function(scan_fb, peak_trough, lambda, out_path = 'manu/figures') {
  peak_trough <- peak_trough %>%
    dplyr::mutate(peak_date   = as.Date(peak_date),
                  trough_date = as.Date(trough_date))
  prcp <- scan_fb %>%
    dplyr::group_by(site, abbreviation) %>%
    dplyr::summarise(avg_prcp = mean(avg_prcp))

  peak_trough <- dplyr::left_join(peak_trough, prcp, by = 'site') %>%
    dplyr::arrange(lng)
  abbrevs <- peak_trough$abbreviation
  peak_trough <- peak_trough %>%
    dplyr::mutate(abbreviation = factor(abbreviation, levels = rev(abbrevs)))

  if (lambda == 7) {
    peak_trough <- peak_trough %>%
      dplyr::mutate(peak_dow = weekdays(peak_date)) %>%
      dplyr::mutate(peak_dow = factor(peak_dow, levels = c('Friday', 'Saturday', 'Sunday', 'Monday',
                                                           'Tuesday', 'Wednesday', 'Thursday')))
    peaks <- ggplot2::ggplot(peak_trough, mapping = ggplot2::aes(x = peak_dow, y = abbreviation)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = 'Peak Date',
                    y = '') +
      ggplot2::lims(x = c('Friday', 'Saturday', 'Sunday', 'Monday',
                          'Tuesday', 'Wednesday', 'Thursday')) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 6),
                     axis.text.x = ggplot2::element_text(size = 8),
                     plot.margin = grid::unit(c(0.25,0.25,0.25,0.25), "cm"))
  } else {
    peaks <- ggplot2::ggplot(peak_trough, mapping = ggplot2::aes(x = peak_date, y = abbreviation)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = 'Peak Date',
                    y = '') +
      ggplot2::scale_x_date(date_labels = '%b',
                            date_breaks = '1 month') +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 6),
                     axis.text.x = ggplot2::element_text(size = 8),
                     plot.margin = grid::unit(c(0.25,0.25,0.25,0.25), "cm"))
  }

  ggplot2::ggsave(peaks,
                  height = 12,
                  width = 6,
                  filename = file.path(out_path, paste0('figure_peaks_', lambda, '.pdf')))
}
