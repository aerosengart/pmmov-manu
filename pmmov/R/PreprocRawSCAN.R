#' PreprocRawSCAN - preprocessing of raw SCAN dataset
#'
#' The following changes/edits are made:
#'     - Several entries are missing zip/FIPS codes. These are added manually.
#'     - Removes sample 'COD110099 influent'
#'     - Collection_Date is cast as 'Date' type
#'     - County_FIPS is renamed as fipscode
#'     - Collection_Date is renamed as date
#'     - missing FIPS codes are changed to 'NA'
#'     - select relevant columns (sampling location/time metadata, SC2 N gene, PMMoV)
#'     - add latitude and longitude of sites by zipcode
#'     - filters data to after change in [lab methods](https://docs.google.com/document/d/1qiNq3wh0H8GrELesgLUrDZF9oTdanI40CZ6tEZSUbic/edit)
#'     - add column for log10 PMMoV
#'
#' @param df a data frame for SCAN
#'
#' @return a data frame; subset and cleaned raw data from SCAN dataset
#'
#' @importFrom magrittr %>%
#'
#' @export

PreprocRawSCAN <- function(df) {
  ## manually fill missing FIPS codes for known cities
  df$County_FIPS[df$County_FIPS == "" & df$City == "Fairfield"] <- "06095"
  df$County_FIPS[df$County_FIPS == "" & df$City == "Davis"] <- "06113"
  df$County_FIPS[df$County_FIPS == "" & df$City == "San Francisco"] <- "06075"
  df$County_FIPS[df$County_FIPS == "Alameda"] <- "06001"
  ## CODIGA is at Stanford University, which is in Santa Clara County
  df$County_FIPS[df$County_FIPS == "" & df$Site_Name == "CODIGA"] <- "06085"

  ## add leading 0 to zip codes
  df$Zipcode <- stringr::str_pad(df$Zipcode, 5, side = "left", pad = "0")

  ## manually fill missing zip codes for known cities
  df$Zipcode[is.na(df$Zipcode) & df$Site_Name %in% c("UC Davis","Davis")] <- "95616"
  df$Zipcode[is.na(df$Zipcode) & df$Site_Name == "Southeast San Francisco"] <- "94124"
  df$Zipcode[is.na(df$Zipcode) & df$Site_Name == "CODIGA"] <- "94305"

  ## 31902 Columbus GA is PO box only
  df$Zipcode[df$Zipcode == "31902"] <- "31901"

  scan_preproc <- df %>%
    ## empty string -> NA
    dplyr::mutate(County_FIPS = ifelse(County_FIPS == "", NA, County_FIPS)) %>%
    ## select relevant columns
    dplyr::select(c('Sample_ID', 'State', 'State_Abbr', 'City', 'Site_Name', 'Zipcode',
                    'Population_Served', 'County_FIPS', 'Collection_Date',
                    'PMMoV_gc_g_dry_weight', 'SC2_N_gc_g_dry_weight',
                    'SC2_S_gc_g_dry_weight')) %>%
    dplyr::rename(id = Sample_ID,
                  state = State,
                  state_abbr = State_Abbr,
                  city = City,
                  site = Site_Name,
                  zipcode = Zipcode,
                  population = Population_Served,
                  fipscode = County_FIPS,
                  date = Collection_Date,
                  pmmov = PMMoV_gc_g_dry_weight,
                  sc2_n = SC2_N_gc_g_dry_weight,
                  sc2_s = SC2_S_gc_g_dry_weight) %>%
    dplyr::mutate(log_pmmov = log(pmmov, base = 10))

  ## get zipcode centroid positioning
  centroids <- zipcodeR::geocode_zip(unique(scan_preproc$zipcode))
  ## add latitude and longitude by zipcode to data frame
  scan_preproc <- scan_preproc %>%
    dplyr::left_join(centroids, by = c('zipcode' = 'zipcode'))
  ## change in method on 2021-05-29 that affected concentrations (but not ratios)
  ## https://docs.google.com/document/d/1qiNq3wh0H8GrELesgLUrDZF9oTdanI40CZ6tEZSUbic/edit
  scan_preproc <- scan_preproc %>%
    dplyr::filter(date >= '2021-05-29') %>%
    dplyr::filter(id != 'COD110099 influent')

  return (scan_preproc)
}
