#' FigureSewerComparison - Create and save sewer comparison figure as PDF.
#'
#' Figure is raw observations from the specified sites with a LOESS smoother to exentuate
#' sewer system type differences.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param sites a vector of strings; names of sites to plot
#' @param start a string; starting date of window for plotting
#' @param end a string; ending date of window for plotting
#' @param weather_path a string; path where cleaned weather data is saved
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureSewerComparison <- function(scan_fb,
                                  sites = c('Calera Creek Water Recycling Plant',
                                            'Oceanside Water Pollution Control Plant'),
                                  start = '2022-11-01',
                                  end = '2023-03-01',
                                  weather_path = '~/pmmov-manu/data/NCDC',
                                  out_path = 'manu/figures') {
  ## sewer system effects --- zoom in on time period with lots of rain
  period_start <- as.Date(start)
  period_end <- as.Date(end)

  data_subset <- scan_fb %>%
    dplyr::filter(site %in% sites) %>%
    dplyr::filter(date <= period_end & date >= period_start) %>%
    dplyr::mutate(sewer_name = ifelse(sewer == 1, 'Combined', 'Separate')) %>%
    dplyr::mutate(site_type = paste0(site, ' (', sewer_name, ')'),
                  date = as.Date(date))

  ## add precipitation data
  prcp_df <- c()
  for (val in sites) {
    site_df <- data_subset %>%
      dplyr::filter(site == val)
    site_prcp_data <- data.frame('DATE' = seq(as.Date(min(site_df$date)),
                                              as.Date(max(site_df$date)),
                                              by = 1))
    fips <- site_df[1, 'fipscode']
    splits_fips <- lapply(fips, FUN = function(x) strsplit(x, ",")) %>%
      unlist()
    for (j in 1:length(splits_fips)) {
      code <- splits_fips[j] %>% stringr::str_trim(side = 'both')
      site_weather <- read.csv(file.path(weather_path,
                                         paste0('noaa_ghcn_fips', code, '_clean.csv')),
                               row.names = 1) %>%
        dplyr::select(c(DATE, avg_prcp)) %>%
        dplyr::mutate(DATE = as.Date(DATE))
      site_prcp_data <- dplyr::left_join(site_prcp_data, site_weather, by = 'DATE')
    }
    ## remove date column and take row average to get average daily average precipitation
    site_prcp_no_date <- site_prcp_data %>%
      dplyr::select(-DATE)
    site_prcp_means <- rowMeans(site_prcp_no_date, na.rm = TRUE)
    site_prcp <- data.frame(date = site_prcp_data$DATE,
                            prcp = site_prcp_means,
                            site = val)
    prcp_df <- rbind(prcp_df, site_prcp)
  }

  sewer_plot <- ggplot2::ggplot(data_subset, ggplot2::aes(x = date, y = log_pmmov)) +
    ggplot2::geom_point(alpha = 0.4, color = 'darkgreen') +
    ggplot2::geom_smooth(method = "loess", span = 0.2, color = "black", fill = "#E79DD4") +
    ggplot2::scale_color_gradient(low = ggplot2::alpha("grey40", 0),
                                  high = "blue",
                                  limits = c(-1, 4.5),
                                  breaks = c(0, 1, 2, 3, 4),
                                  labels = c(0, 1, 2, 3, 4)) +
    ggplot2::geom_rug(data = prcp_df,
                      mapping = ggplot2::aes(x = date, color = prcp, alpha = prcp),
                      inherit.aes = FALSE,
                      linewidth = 0.8,
                      length = grid::unit(0.1, "npc")) +
    ggplot2::facet_wrap(~site_type, nrow = 2, scales = 'free_y') +
    ggplot2::labs(y = 'PMMoV (log10 gc/g dry wt)',
                  x = NULL,
                  color = 'Average Daily Precipitation (in)',) +
    ggplot2::scale_x_date(date_labels = "%b %d '%y",
                          date_breaks = '1 month') +
    ggplot2::ylim(7.5, 10) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   legend.position = 'top',
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   aspect.ratio = 1/2,
                   plot.margin = grid::unit(c(1, 1, 1, 3), "mm")) +
    ggplot2::guides(alpha = 'none')

  ggplot2::ggsave(sewer_plot, height = 10, width = 8, filename = file.path(out_path, 'figure_sewer_comp.pdf'))
}
