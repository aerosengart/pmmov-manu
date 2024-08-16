#' FigureComponents - Create and save site components figure as PDF.
#'
#' Figure is a grid of the predicted time series made with Bayesian median model. Rug is colored by average daily precipitation for
#' the area.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param site_name a string; name of site to plot
#' @param start a string; starting date of window for plotting
#' @param end a string; ending date of window for plotting
#' @param limits a vector; minimum and maximum values for y-axis
#' @param model_path a string; path where Stan model fit is saved
#' @param weather_path a string; path where cleaned weather data is saved
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureComponents <- function(scan_fb,
                             site_name,
                             start = '2022-01-01',
                             end   = '2023-04-01',
                             limits = c(8.125, 8.44),
                             model_path = '~/projects/pmmov-manu/stan/fits/wu1000_sep_lap_noucd/site_fits/',
                             weather_path = '~/projects/pmmov-manu/data/NCDC',
                             out_path = 'manu/figures') {
  ## read in model
  site_file <- paste0(site_name, "_stan_fit.rds")
  site_model_path <- file.path(model_path, site_file)
  model <- readRDS(site_model_path)
  #model_sum <- summary(model)$summary
  #coefs <- model_sum[grep('B', rownames(model_sum)), ] %>%
  #  magrittr::set_rownames(c('avg_prcp', 'sin1_wk', 'cos1_wk', 'sin1_yr', 'cos1_yr'))
  model_mat <- as.matrix(model) ## coerce to matrix without warmup
  coefs <- model_mat[, grep('B', colnames(model_mat))] %>%
    magrittr::set_colnames(c('avg_prcp', 'sin1_wk', 'cos1_wk', 'sin1_yr', 'cos1_yr'))
  alpha <- model_mat[, 'alpha']
  y_rep <- model_mat[, grep('y_rep', colnames(model_mat))]

  ## subset data
  site_data <- scan_fb %>%
    dplyr::filter(site == site_name) %>%
    dplyr::mutate(date = as.Date(date))

  ## get site data interval
  period_start <- as.Date(min(site_data$date))
  period_end <- as.Date(max(site_data$date))
  t_seq <- seq(min(site_data$t), max(site_data$t))
  date_seq <- seq(period_start, period_end, by = 1)

  ## get precipitation data for site
  site_prcp_data <- data.frame('DATE' = date_seq)
  fips <- site_data[1, 'fipscode']
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
  site_prcp <- data.frame(date = date_seq,
                          prcp = site_prcp_means,
                          site = site_name)

  ## get intercept
  sample_idx <- sample(nrow(model_mat), 1)
  intercept <- rep(alpha[sample_idx], length(t_seq))

  ## get yearly portion
  lambda_yr <- 365.25
  denom <- sqrt(lambda_yr / 2)
  sin_yr <- sin(2 * pi * t_seq / lambda_yr) / denom
  cos_yr <- cos(2 * pi * t_seq / lambda_yr) / denom
  yearly <- coefs[sample_idx, 'sin1_yr'] * sin_yr + coefs[sample_idx, 'cos1_yr'] * cos_yr

  ## get weekly portion
  lambda_wk <- 7
  denom <- sqrt(lambda_wk / 2)
  sin_wk <- sin(2 * pi * t_seq / lambda_wk) / denom
  cos_wk <- cos(2 * pi * t_seq / lambda_wk) / denom
  weekly <- coefs[sample_idx, 'sin1_wk'] * sin_wk + coefs[sample_idx, 'cos1_wk'] * cos_wk

  ## get precipitation portion
  precip <- coefs[sample_idx, 'avg_prcp'] * site_prcp$prcp

  ## get predictions (+ noise) from sample
  ys <- y_rep[sample_idx, ]

  ## construct data frame
  plot_df <- data.frame(date  = date_seq,
                        inter = intercept,
                        yr    = yearly + intercept,
                        wk    = weekly + yearly + intercept,
                        prcp  = precip + weekly + yearly + intercept,
                        y_rep = ys) %>%
    #dplyr::filter(date >= as.Date(start) & date <= as.Date(end)) %>%
    #tidyr::pivot_longer(cols = c('yr', 'wk', 'prcp', 'y_rep')) %>%
    #dplyr::mutate(name = factor(name, levels = c('yr', 'wk', 'prcp', 'y_rep')))
    tidyr::pivot_longer(cols = c('yr', 'wk', 'prcp')) %>%
    dplyr::mutate(name = ifelse(name == 'yr', 'Yearly Components',
                                ifelse(name == 'wk', '+ Weekly Components', '+ Precipitation'))) %>%
    dplyr::mutate(name = factor(name, levels = c('Yearly Components', '+ Weekly Components', '+ Precipitation')))

  components <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name, nrow = 3) +
    ggplot2::scale_color_gradient(low = ggplot2::alpha("grey40", 0),
                                  high = "blue",
                                  limits = c(-1, 4.5),
                                  breaks = c(0, 1, 2, 3, 4),
                                  labels = c(0, 1, 2, 3, 4)) +
    ggplot2::geom_rug(data = site_prcp,
                      mapping = ggplot2::aes(x = date, color = prcp, alpha = prcp),
                      inherit.aes = FALSE,
                      linewidth = 0.4,
                      length = grid::unit(0.1, "npc")) +
    ggplot2::labs(y = 'PMMoV (log10 gc/g dry wt)',
                  x = NULL,
                  color = 'Average Daily Precipitation (in)') +
    ggplot2::ylim(limits[1], limits[2]) +
    ggplot2::scale_x_date(date_labels = "%b %d '%y",
                          date_breaks = '3 month') +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 12),
                   legend.position = 'top',
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   aspect.ratio = 1/2,
                   plot.margin = grid::unit(c(1, 1, 1, 3), "mm")) +
    ggplot2::guides(alpha = 'none')

  ggplot2::ggsave(components, height = 12, width = 9, filename = file.path(out_path, 'figure_components.pdf'))
}

