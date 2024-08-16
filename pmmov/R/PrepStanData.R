#' PrepStanData - Arrange data frame for Stan sampling.
#'
#' @param site_data a data frame; site-specific SCAN data
#' @param weather_path a string; path to saved cleaned weather data .csv files
#'
#' @return a data frame; the prepared SCAN data used for the Stan sampling of response
#'
#' @importFrom magrittr %>%
#'
#' @export

PrepStanData <- function(site_data, weather_path = 'data/NCDC') {
  start <- as.Date(min(site_data$date))
  end <- as.Date(max(site_data$date))
  t_seq <- seq(min(site_data$t), max(site_data$t))
  date_seq <- seq(start, end, by = 1)
  
  ## get precipitation data for site
  site_prcp_data <- data.frame('DATE' = date_seq,
                               't'    = t_seq)
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
  site_stan_df <- data.frame(date = date_seq,
                             t    = t_seq,
                             site = site_data$site[1],
                             lat = site_data$lat[1],
                             lng = site_data$lng[1],
                             avg_prcp = site_prcp_means)
  ## add basis functions
  ## drops constant basis function (idx = 1)
  yr <- fda::create.fourier.basis(rangeval    = c(min(t_seq), max(t_seq)),
                                  nbasis      = 2,
                                  period      = 365.25,
                                  dropind     = 1)
  wk <- fda::create.fourier.basis(rangeval    = c(min(t_seq), max(t_seq)),
                                  nbasis      = 2,
                                  period      = 7,
                                  dropind     = 1)
  
  eval_yr <- fda::eval.basis(t_seq, yr) %>%
    as.data.frame()
  eval_wk <- fda::eval.basis(t_seq, wk) %>%
    as.data.frame()
  colnames(eval_yr) <- paste0(colnames(eval_yr), '_yr')
  colnames(eval_wk) <- paste0(colnames(eval_wk), '_wk')
  site_stan_df <- cbind(site_stan_df, eval_yr, eval_wk)
  
  return (site_stan_df)
}