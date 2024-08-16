#' PrepData - Clean and arrange data frame for PMMoV manuscript.
#'
#' @param out_path a string; path to save cleaned data .csv file
#' @param raw_path a string; path to raw WastewaterSCAN data .csv
#' @param weather_path a string; path to saved cleaned weather data .csv files
#' @param clean_ncdc a boolean flag; whether to aggregate the NCDC weather data
#'
#' @return a data frame; the cleaned SCAN data used for the analyses
#'
#' @importFrom magrittr %>%
#'
#' @export

PrepData <- function(out_path, raw_path = 'data/scan', weather_path = 'data/NCDC', clean_ncdc = FALSE) {
  ## load raw SCAN data
  scan_raw <- read.csv(file.path(raw_path, 'data.csv'))

  ## load SCAN sewer system information
  sewer <- xlsx::read.xlsx(file       = file.path(raw_path, 'WWSCAN_SiteInfo_PMMoV.xlsx'),
                           sheetIndex = 1) %>%
    dplyr::select('Site.Name', 'Sample.Type', 'Sewer.Type') %>%
    dplyr::rename(site   = Site.Name,
                  sample = Sample.Type,
                  sewer  = Sewer.Type)

  ## clean the raw weather data - if desired
  fipscodes <- unique(scan_raw$County_FIPS)
  if (clean_ncdc) {
    CleanNCDC(weather_path = weather_path,
              fipscodes    = fipscodes)
  }

  ## clean data
  scan_raw_preproc <- PreprocRawSCAN(df = scan_raw)
  scan_raw_weather <- AddWeatherSCAN(df           = scan_raw_preproc,
                                     weather_path = file.path(weather_path, 'clean'))

  ## restrict to sites with at least 30 observations
  counts <- scan_raw_weather %>%
    dplyr::group_by(site, city) %>%
    dplyr::summarise(total_count = n()) %>%
    dplyr::filter(total_count >= 30) %>%
    dplyr::mutate(site_city = paste0(site, ', ', city))
  scan_postproc <- scan_raw_weather %>%
    dplyr::mutate(site_city = paste0(site, ', ', city)) %>%
    dplyr::filter(site_city %in% counts$site_city) %>%
    dplyr::select(-site_city)

  ## add sewer system information
  scan_postproc <- AddSewerSCAN(df       = scan_postproc,
                                sewer_df = sewer)

  ## add Fourier basis functions
  scan_fb <- AddBasisSCAN(df        = scan_postproc,
                          num_basis = 1)

  ## add weekend indicator
  scan_fb <- scan_fb %>%
    dplyr::mutate(dow = weekdays(as.Date(date))) %>%
    dplyr::mutate(wknd = ifelse(dow %in% c('Saturday', 'Sunday'), 1, 0))  %>%
    dplyr::filter(site != 'UC Davis') ## remove UC Davis

  ## save data for easy loading
  write.csv(scan_fb, file = file.path(out_path, 'scan_clean_df.csv'))

  ## create data frame for Stan sampling
  unique_sites <- unique(scan_fb$site)
  stan_df <- c()
  for (i in 1:length(unique_sites)) {
    site_name <- unique_sites[i]
    site_data <- scan_fb %>%
      dplyr::filter(site == site_name)

    site_stan_df <- PrepStanData(site_data, weather_path)
    stan_df <- rbind(stan_df, site_stan_df)
  }
  ## save data for easy loading
  write.csv(stan_df, file = file.path(out_path, 'stan_data.csv'))

  return (scan_fb)
}
