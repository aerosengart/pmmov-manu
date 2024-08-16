#' RunPred - Create predictions and bands for model fit with Stan.
#'
#' @param out_path a string path; path to save output
#' @param weather_path a string path; path to cleaned weather data .csv files from NCDC
#' @param model_path a string path; path to RDS files for stan fits for each site
#' @param prefix a string; prefix for saved RDS files
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @param beta a scalar; exponent for generalized Gaussian distribution
#' @param sites a vector of strings; names of sites to predict for
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

RunPred <- function(out_path, weather_path, model_path, scan_fb, beta, sites,
                    prefix = 'cross_us') {
  ## create prediction intervals for mixed model
  site_preds <- PredInterval(scan_fb = scan_fb,
                             beta = beta,
                             model_path = model_path,
                             sites = sites)

  site_preds <- site_preds %>%
    dplyr::mutate(loc = paste0(site, ' (', state_abbr, ')'))

  ## add precipitation data
  unique_fips <- unique(site_preds$fipscode)
  unique_sites <- unique(site_preds$site)
  pred_prcp_df <- c()
  for (val in unique_sites) {
    site_df <- site_preds %>%
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
                            site = val,
                            loc  = site_df[1, 'loc'])
    pred_prcp_df <- rbind(pred_prcp_df, site_prcp)
  }

  saveRDS(site_preds, file = file.path(out_path, paste0(prefix, '_prediction_intervals.rds')))
  saveRDS(pred_prcp_df, file = file.path(out_path, paste0(prefix, '_precipitation.rds')))
}
