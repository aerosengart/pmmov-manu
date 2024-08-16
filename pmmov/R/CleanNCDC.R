#' CleanNCDC - Clean .csv file from NOAA NCDC GHNC (yearly) direct download.
#'
#' Cleans data sourced from [the NOAA NCDC](https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year/)
#' daily climate data records by year. These were downloaded by hand.
#' Saves cleaned files as .csv with 'clean' prepended to file name.
#'
#' @param weather_path a string; path to directory containing raw NOAA NCDC GHNC .csv files
#' @param fipscodes a vector of strings; list of fipscodes covered by SCAN dataset
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export


CleanNCDC <- function(weather_path, fipscodes) {
  unique_fips <- fipscodes %>%
    lapply(FUN = function(x) strsplit(x, ",")) %>%
    unlist() %>%
    lapply(FUN = function(x) stringr::str_trim(x, side = 'both')) %>%
    unlist() %>%
    unique()

  for (i in 1:length(unique_fips)) {
    fips <- unique_fips[i]
    noaa_data <- utils::read.csv(paste0(weather_path, '/noaa_ghcn_fips', fips, '.csv'), row.names = 1)
    contains_vec <- c('PRCP', 'SNOW', 'SNWD', 'TAVG', 'TMAX', 'TMIN') %in% colnames(noaa_data)

    if (sum(contains_vec) == 6) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE),
                         avg_sfll = mean(SNOW, na.rm = TRUE),
                         avg_sdep = mean(SNWD, na.rm = TRUE),
                         avg_temp = mean(TAVG, na.rm = TRUE),
                         avg_tmax = mean(TMAX, na.rm = TRUE),
                         avg_tmin = mean(TMIN, na.rm = TRUE))
    } else if (sum(contains_vec[-4]) == 5) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE),
                         avg_sfll = mean(SNOW, na.rm = TRUE),
                         avg_sdep = mean(SNWD, na.rm = TRUE),
                         avg_tmax = mean(TMAX, na.rm = TRUE),
                         avg_tmin = mean(TMIN, na.rm = TRUE))
    } else if (sum(contains_vec[-3] == 5)) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE),
                         avg_sfll = mean(SNOW, na.rm = TRUE),
                         avg_temp = mean(TAVG, na.rm = TRUE),
                         avg_tmax = mean(TMAX, na.rm = TRUE),
                         avg_tmin = mean(TMIN, na.rm = TRUE))
    } else if (sum(contains_vec[c(1, 4, 5, 6)]) == 4) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE),
                         avg_temp = mean(TAVG, na.rm = TRUE),
                         avg_tmax = mean(TMAX, na.rm = TRUE),
                         avg_tmin = mean(TMIN, na.rm = TRUE))
    } else if (sum(contains_vec[c(1, 4)]) == 2) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE),
                         avg_temp = mean(TAVG, na.rm = TRUE))
    } else if (contains_vec[1]) {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_prcp = mean(PRCP, na.rm = TRUE))
    } else {
      noaa_summary <- noaa_data %>%
        dplyr::group_by(DATE) %>%
        dplyr::summarise(avg_temp = mean(TAVG, na.rm = TRUE))
    }
    noaa_summary$fipscode <- fips

    dir.create(file.path(weather_path, 'clean'), showWarnings = FALSE)
    utils::write.csv(noaa_summary, file = paste0(weather_path, '/clean/clean_noaa_ghcn_fips', fips, '.csv'))
  }
}
