#' AddSewerSCAN - add sewer type information to SCAN dataset
#'
#' Some changes are made manually:
#'      - Oceanside Water Pollution Control Plant is combined
#'      - Southeast San Francisco is [combined](https://sfpuc.org/about-us/our-systems/sewer-system/our-combined-sewer)
#'      - CODIGA is [separate?](https://suwater.stanford.edu/stormwater)
#'      - UC Davis is [separate?](https://sustainability.ucdavis.edu/goals/water) (see also [here](https://facilities.ucdavis.edu/utilities-operations/campus-infrastructure-%26-services#waste-water))
#'
#' @param df a data frame for SCAN
#' @param sewer_df a data frame of SCAN sewer type information
#'
#' @return a data frame; SCAN dataset with sewer type information cleaned and appended
#'
#' @importFrom magrittr %>%
#'
#' @export

AddSewerSCAN <- function(df, sewer_df) {
  scan_sewer <- dplyr::left_join(df, sewer_df, by = 'site') %>%
    dplyr::mutate(sample = ifelse(is.na(sample), 'Unknown', sample),
                  sewer  = ifelse(is.na(sewer), 'Unknown', sewer))

  scan_sewer <- scan_sewer %>%
    dplyr::mutate(sewer = ifelse(site == 'Oceanside Water Pollution Control Plant',
                                 'Combined',
                                 sewer)) %>%
    dplyr::mutate(sewer = ifelse(site == 'Southeast San Francisco',
                                 'Combined',
                                 sewer)) %>%
    dplyr::mutate(sewer = ifelse(site == 'CODIGA',
                                 'Separated',
                                 sewer)) %>%
    dplyr::mutate(sewer = ifelse(site == 'UC Davis',
                                 'Separated',
                                 sewer))

  scan_fb <- scan_sewer %>%
    dplyr::filter(sewer != 'Unknown') %>% ## there are none
    dplyr::mutate(sewer = ifelse(sewer == 'Separated', 0, 1))

  return (scan_fb)
}
