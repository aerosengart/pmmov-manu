#' FigureACF - Create and save ACF plot as PDF.
#'
#' Figure is a plot of the weighted average autocorrelations across all sites.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param all_sites a boolean flag; create and save ACF plots for individual sites
#' @param data a boolean flag; if ACF is for residuals or raw data
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureACF <- function(scan_fb, all_sites, data, out_path) {
  ## create individual site autocorrelation plot subdirectory
  dir.create(file.path(out_path, 'site_acf'), showWarnings = FALSE)

  ## extend data frame to be daily (no imputation)
  unique_sites <- unique(scan_fb$site)
  scan_daily <- data.frame()
  for (i in 1:length(unique_sites)) {
    df <- scan_fb %>%
      dplyr::filter(site == !!unique_sites[i]) %>%
      dplyr::mutate(date = as.Date(date))
    start_date <- as.Date(min(df$date))
    end_date <- as.Date(max(df$date))
    date_seq <- seq(from = start_date, to = end_date, by = 1)
    if (!identical(df$date, date_seq)) {
      df_columns <- colnames(df)
      df_columns <- df_columns[!df_columns == 'date']
      ## create daily time steps and fill in existing data (match by date)
      imp_df <- data.frame(date = date_seq)
      imp_df <- dplyr::left_join(imp_df, df, by = 'date')
      imp_df$state <- df$state[1]
      imp_df$state_abbr <- df$state_abbr[1]
      imp_df$city <- df$city[1]
      imp_df$site <- df$site[1]
      imp_df$zipcode <- df$zipcode[1]
      imp_df$fipscode <- df$fipscode[1]
      imp_df$population <- df$population[1]
      imp_df$lat <- df$lat[1]
      imp_df$lng <- df$lng[1]
    } else {
      imp_df <- df
    }
    imp_df$t <- seq(1:length(date_seq))
    scan_daily <- rbind(scan_daily, imp_df)
  }

  unique_sites <- unique(scan_daily$site)
  acfs_used <- data.frame(matrix(0, nrow = 31, ncol = length(unique_sites)))
  sizes_used <- data.frame(matrix(0, nrow = 31, ncol = length(unique_sites)))
  pairs_used <- data.frame(matrix(0, nrow = 31, ncol = length(unique_sites)))
  colnames(acfs_used) <- colnames(sizes_used) <- colnames(pairs_used) <- unique_sites
  rownames(acfs_used) <- rownames(sizes_used) <- rownames(pairs_used) <- paste0('lag=', 0:30)

  for (s in 1:length(unique_sites)) {
    ## filter to single site
    df <- scan_daily %>%
      dplyr::filter(site == !!unique_sites[s])
    ## extract data
    resid <- df$resid
    site_acf <- c()
    ## loop through all lags up to 30 (or # of observations)
    for (k in 0:min(30, nrow(df))) {
      non_lagged <- c()
      lagged <- c()
      used_entries <- c()
      pairs <- 0
      ## calculate sample mean over entries used in numerator
      for (j in 1:(length(resid) - k)) {
        ## if value at time j and the value at time j-k are not missing, then they
        ## will contribute to the numerator
        if (!is.na(resid[j]) & !is.na(resid[j + k])) {
          if (!((j + k) %in% used_entries)) {
            used_entries <- c(used_entries, (j + k))
          }
          if (!(j %in% used_entries)) {
            used_entries <- c(used_entries, j)
          }
          non_lagged <- c(non_lagged, resid[j]) ## add non-lagged value to mean
          lagged <- c(lagged, resid[j + k]) ## add lagged value to mean
          pairs <- pairs + 1
        }
      }
      pairs_used[(k + 1), s] <- pairs

      if (length(lagged) > 0 & length(non_lagged) > 0) {
        ## calculate means/variances of lagged and non-lagged values
        mean_nl <- mean(non_lagged)
        mean_l <- mean(lagged)
        var_nl <- stats::var(non_lagged)
        var_l <- stats::var(lagged)

        ## if zero variance (only a single pair of values used), autocorrelation = 0
        if (is.na(var_nl) | is.na(var_l)) {
          k_acf <- 0
        } else if (var_nl == 0 | var_l == 0) {
          k_acf <- 0
        } else {
          center_l <- lagged - mean_l
          center_nl <- non_lagged - mean_nl
          k_acf <- stats::cov(center_l, center_nl) / sqrt(var_nl * var_l)
        }
        ## record site acf and number of entries used in the calculation at lag k
        site_acf <- rbind(site_acf, c(length(used_entries), k_acf))
      } else {
        ## record site acf and number of entries used in the calculation at lag k
        site_acf <- rbind(site_acf, c(length(used_entries), 0))
      }
    }
    acfs_used[, s] <- site_acf[, 2]
    sizes_used[, s] <- site_acf[, 1]
  }

  ## save autocorrelation plots for each individual site
  if (all_sites) {
    ## loop through sites
    for (i in 1:ncol(acfs_used)) {
      acf_site <- acfs_used[, i] %>%
        data.frame() %>%
        magrittr::set_colnames('autocorrelation')
      acf_site$lag <- 0:30
      acf_site$n <- sizes_used[, i]
      acf_site$pairs <- pairs_used[, i]
      acf_site <- acf_site %>%
        dplyr::mutate(size = ifelse(n < 10, 'small', 'large'))

      df <- scan_daily %>%
        filter(site == !!unique_sites[i])

      band1 <- data.frame(lag = 0:30, val = -stats::qnorm(1-.025/31)/sqrt(acf_site$pairs))
      band2 <- data.frame(lag = 0:30, val = stats::qnorm(1-.025/31)/sqrt(acf_site$pairs))

      plot <- ggplot2::ggplot(acf_site, ggplot2::aes(x = lag, y = autocorrelation)) +
        ggplot2::geom_bar(ggplot2::aes(fill = n), stat = "identity", width = 0.4) +
        ggplot2::scale_fill_gradient(low = "grey", high = "black") +
        ggplot2::geom_line(data = band1, ggplot2::aes(x = lag, y = val),
                           linetype = 'dashed', color = 'red', linewidth = 0.5) +
        ggplot2::geom_line(data = band2, ggplot2::aes(x = lag, y = val),
                           linetype = 'dashed', color = 'red', linewidth = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12)) +
        ggplot2::scale_x_continuous(breaks = seq(0, 30, 5)) +
        ggplot2::labs(title = unique_sites[i],
                      fill = 'Num. Obs.',
                      y    = 'Autocorrelation',
                      x    = 'Lag (days)')

      filename <- file.path(out_path, 'site_acf', paste0(gsub(' ', '', unique_sites[i]), '.png'))
      ggplot2::ggsave(filename = filename, plot = plot)
    }
  }

  overall_acfs <- c()
  for (k in 0:30) {
    sites_mean <- stats::weighted.mean(x = acfs_used[(k+1), ], w = pairs_used[(k+1), ])
    overall_acfs <- rbind(overall_acfs, c(k, sites_mean, 'used'))
  }
  overall_acfs <- data.frame(overall_acfs)
  colnames(overall_acfs) <- c('lag', 'autocorrelation', 'method')
  overall_acfs <- overall_acfs %>%
    dplyr::mutate(lag = as.integer(lag),
           autocorrelation = as.numeric(autocorrelation),
           method = factor(method, levels = c('all', 'used', 'func')))

  if (!data) {
    overall_acf_plot <- ggplot2::ggplot(overall_acfs,
                                        mapping = ggplot2::aes(x = lag, y = autocorrelation)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity", fill = 'navy') +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 60, 5)) +
      ggplot2::labs(title    = 'Residual Autocorrelation',
                    subtitle = 'Average Over Sites Weighted by Number of Observations',
                    y        = 'Autocorrelation',
                    x        = 'Lag (days)')
    ggplot2::ggsave(filename = file.path(out_path, 'figure_resid_acf.pdf'),
                    plot     = overall_acf_plot, height = 6, width = 8)
  } else {
    overall_acf_plot <- ggplot2::ggplot(overall_acfs,
                                        mapping = ggplot2::aes(x = lag, y = autocorrelation)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity", fill = 'darkred') +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10)) +
      ggplot2::scale_x_continuous(breaks = seq(0, 60, 5)) +
      ggplot2::labs(title    = 'Autocorrelation',
                    subtitle = 'Average Over Sites Weighted by Number of Observations',
                    y        = 'Autocorrelation',
                    x        = 'Lag (days)')
    ggplot2::ggsave(filename = file.path(out_path, 'figure_data_acf.pdf'),
                    plot     = overall_acf_plot, height = 6, width = 8)
  }

}
