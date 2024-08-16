#' PeakTrough - Calculate peaks and troughs of Fourier bases for each site.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param lambda a float; period for Fourier bases (non-negative)
#' @param model_path a string; path where individual Stan fits for each site are saved
#' @param out_path a string path; path to save output
#' @param plot_out_path a string path; path to save site plots
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

PeakTrough <- function(scan_fb,
                       lambda     = 365.25,
                       model_path = '~/projects/pmmov-manu/stan/wu1000_sep_lap_nucd/site_fits',
                       out_path   = '~/projects/pmmov-manu/checkpoints',
                       plot_out_path = '~/projects/pmmov-manu/manu/figures/site_peak_trough') {
  dir.create(plot_out_path, showWarnings = FALSE, recursive = TRUE)
  sites <- unique(scan_fb$site)

  ## construct time series data frame
  min_date <- min(scan_fb$date)
  max_date <- max(scan_fb$date)
  dates <- seq(as.Date(min_date), as.Date(max_date), by = 'day')

  site_pt <- data.frame()
  for (i in 1:length(sites)) {
    ## read in site model
    site_model <- file.path(model_path, paste0(sites[i], "_stan_fit.rds"))
    site_fit <- readRDS(site_model)
    site_sum <- rstan::summary(site_fit)$summary
    dates_df <- data.frame(dates, 1:(length(dates))) %>%
      magrittr::set_colnames(c('dates', 't'))

    ## use medians
    if (lambda == 7) {
      sin_coef <- site_sum['B[2]', '50%']
      cos_coef <- site_sum['B[3]', '50%']
    } else {
      sin_coef <- site_sum['B[4]', '50%']
      cos_coef <- site_sum['B[5]', '50%']
    }
    alpha  <- site_sum['alpha', '50%']
    sin_cpt <- sin_coef / (sqrt(lambda / 2)) * sin((2 * pi * dates_df$t) / lambda)
    cos_cpt <- cos_coef / (sqrt(lambda / 2)) * cos((2 * pi * dates_df$t) / lambda)
    prev_name <- colnames(dates_df)
    dates_df <- cbind(dates_df, sin_cpt + cos_cpt + alpha) %>%
      magrittr::set_colnames(c(prev_name, 'log_pmmov')) %>%
      dplyr::mutate(dates = as.Date(dates))

    ## site peaks
    peak <- (lambda / (2 * pi)) * (5 * pi / 2 - atan(cos_coef / sin_coef))
    trough <- (lambda / (2 * pi)) * (7 * pi / 2 - atan(cos_coef / sin_coef))

    ## round peaks/troughs to nearest day
    trough_t <- dates_df$dates[round(trough)]
    peak_t <- dates_df$dates[round(peak)]

    ## something weird with the coefficients? need to flip dates
    if (dates_df[which(dates_df$dates == peak_t), 'log_pmmov'] <= dates_df[which(dates_df$dates == trough_t), 'log_pmmov']) {
      temp <- peak_t
      peak_t <- trough_t
      trough_t <- temp

      temp <- peak
      peak <- trough
      trough <- temp
    }

    site_pt <- rbind(site_pt, c(sites[i], as.numeric(peak), as.numeric(trough),
                                dates_df[which(dates_df$dates == peak_t), 'log_pmmov'],
                                max(dates_df$log_pmmov),
                                dates_df[which(dates_df$dates == trough_t), 'log_pmmov'],
                                min(dates_df$log_pmmov)))

    if (lambda == 7) {
      date_lim <- c(min(trough_t, peak_t) - 2, max(trough_t, peak_t) + 2)
      site_peak_trough <- ggplot2::ggplot(dates_df,
                                          mapping = ggplot2::aes(x = dates, y = log_pmmov)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = '',
                      y = 'PMMoV (log10 gc/g dry wt)',
                      title = paste0(sites[i])) +
        ggplot2::scale_x_date(date_breaks = "1 day",
                              date_labels = "%a",
                              limits = date_lim) +
        ggplot2::geom_vline(xintercept = peak_t, linetype = "dashed", color = "red") +
        ggplot2::geom_vline(xintercept = trough_t, linetype = "dashed", color = "blue")
    } else {
      date_lim <- c(min(trough_t, peak_t) - 90, max(trough_t, peak_t) + 90)
      site_peak_trough <- ggplot2::ggplot(dates_df,
                                          mapping = ggplot2::aes(x = dates, y = log_pmmov)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = '',
                      y = 'PMMoV (log10 gc/g dry wt)',
                      title = paste0(sites[i])) +
        ggplot2::scale_x_date(date_breaks = "1 month",
                              date_labels = "%b",
                              limits = date_lim) +
        ggplot2::geom_vline(xintercept = peak_t, linetype = "dashed", color = "red") +
        ggplot2::geom_vline(xintercept = trough_t, linetype = "dashed", color = "blue")
    }
    filename <- file.path(plot_out_path, paste0(gsub(' ', '', sites[i]), '_', lambda, '.png'))
    ggplot2::ggsave(filename = filename, plot = site_peak_trough)
  }
  dates_df <- data.frame(dates, 1:(length(dates))) %>%
    magrittr::set_colnames(c('dates', 't'))
  site_pt <- data.frame(site_pt) %>%
    magrittr::set_colnames(c('site', 'peak', 'trough', 'peak_val', 'max_val', 'trough_val', 'min_val')) %>%
    dplyr::mutate(peak = as.numeric(peak),
                  trough = as.numeric(trough),
                  peak_val = as.numeric(peak_val),
                  max_val = as.numeric(max_val),
                  trough_val = as.numeric(trough_val),
                  min_val = as.numeric(min_val)) %>%
    dplyr::mutate(peak_t = round(peak),
                  trough_t = round(trough))

  peak_trough <- dplyr::left_join(site_pt, dates_df, by = c('peak_t' = 't')) %>%
    dplyr::rename(peak_date = dates) %>%
    dplyr::left_join(dates_df, by = c('trough_t' = 't')) %>%
    dplyr::rename(trough_date = dates) %>%
    dplyr::left_join(scan_fb[, c('site', 'state', 'lat', 'lng')], by = 'site') %>%
    unique()

  saveRDS(peak_trough, file = file.path(out_path, paste0('peak_trough_', lambda, '.rds')))
}
