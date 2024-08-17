#' AddWeatherSCAN - add weather information to raw SCAN dataset
#'
#' The following changes/edits are made:
#'     - Add columns for average temperature/precipitation to scan data frame (average taken within day and over all counties served by that site)
#'     - Add column for season (based upon meteorological season)
#'     - Removes rows with NA in columns of interest (see below) to avoid imputation
#'
#' @param df a data frame for SCAN
#' @param weather_path a string; path to directory containing NOAA NCDC GHCN cleaned precipitation data by FIPS codes (no trailing '/')
#'
#' @return a data frame; subset and cleaned raw data from SCAN dataset
#'
#' @importFrom magrittr %>%
#'
#' @export

AddWeatherSCAN <- function(df, weather_path = 'data/NCDC') {
  unique_fips <- unique(df$fipscode)
  scan_postproc <- data.frame()

  for (i in 1:length(unique_fips)) {
    fips <- unique_fips[i]
    ## get all fipscodes and split them apart
    splits_fips <- lapply(fips, FUN = function(x) strsplit(x, ",")) %>%
      unlist()
    ## get observations associated with that/those fipscode(s)
    scan_subset <- df %>%
      dplyr::filter(fipscode == fips)
    fips_data <- data.frame(matrix(nrow = 0, ncol = 8)) %>%
      magrittr::set_colnames(c("DATE", "avg_prcp", "avg_sfll", "avg_sdep",
                               "avg_temp", "avg_tmax", "avg_tmin", "fipscode"))
    ## loop over all fipscodes in split list
    for (j in 1:length(splits_fips)) {
      code <- splits_fips[j] %>%
        stringr::str_trim(side = 'both')
      ## load in weather data
      noaa_data <- utils::read.csv(paste0(weather_path, '/clean_noaa_ghcn_fips', code, '.csv'), row.names = 1)
      ## add weather data
      missing_cols <- setdiff(colnames(fips_data), colnames(noaa_data))
      ## if some variable(s) were not available for a fipscode, supply NA
      if (length(missing_cols) > 0 ) {
        for (k in 1:length(missing_cols)) {
          old_col_names <- colnames(noaa_data)
          noaa_data <- cbind(noaa_data, NA)
          colnames(noaa_data) <- c(old_col_names, missing_cols[k])
        }
      }
      fips_data <- fips_data %>%
        dplyr::select(c("DATE", "avg_prcp", "avg_sfll", "avg_sdep",
                        "avg_temp", "avg_tmax", "avg_tmin", "fipscode"))
      fips_data <- rbind(fips_data, noaa_data)
    }
    ## take mean over day
    fips_data <- fips_data %>%
      dplyr::group_by(DATE) %>%
      dplyr::summarise(avg_prcp = mean(avg_prcp, na.rm = TRUE),
                       avg_sfll = mean(avg_sfll, na.rm = TRUE),
                       avg_sdep = mean(avg_sdep, na.rm = TRUE),
                       avg_temp = mean(avg_temp, na.rm = TRUE),
                       avg_tmax = mean(avg_tmax, na.rm = TRUE),
                       avg_tmin = mean(avg_tmin, na.rm = TRUE))
    ## replace NaN and blanks with NA
    fips_data[is.na(fips_data)] <- NA
    ## join weather and scan data together
    scan_subset <- dplyr::left_join(scan_subset, fips_data, by = c('date' = 'DATE'))
    scan_postproc <- rbind(scan_postproc, scan_subset)
  }

  ## seasons are by meteorological seasons
  label_season <- function(date) {
    month <- lubridate::month(date)
    if (month %in% c(12, 1, 2)) {
      return ('winter')
    } else if (month %in% c(3, 4, 5)) {
      return ('spring')
    } else if (month %in% c(6, 7, 8)) {
      return ('summer')
    } else {
      return ('fall')
    }
  }
  scan_postproc$season <- lapply(scan_postproc$date, label_season)
  scan_postproc <- scan_postproc %>%
    dplyr::mutate(season = factor(season, levels = c('spring', 'summer', 'winter', 'fall')))

  ## remove observations that have NA for the variables of interest so that model comparison is fair
  scan_postproc_nona <- scan_postproc %>%
    dplyr::filter(!is.na(avg_prcp) & !is.na(lat) & !is.na(lng) & !is.na(pmmov) & !is.na(log_pmmov))

  return (scan_postproc_nona)
}


