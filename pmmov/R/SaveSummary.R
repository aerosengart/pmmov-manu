#' SaveSummary - save summary statistics for SCAN dataset to .csv file.
#'
#' @param out_path a string; path to save .csv file
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

SaveSummary <- function(out_path, scan_fb) {
  ## overall
  log_pmmov <- scan_fb %>%
    dplyr::reframe(min  = min(log_pmmov),
                   max  = max(log_pmmov),
                   med  = median(log_pmmov),
                   mean = mean(log_pmmov))

  prcp <- scan_fb %>%
    dplyr::reframe(min  = min(avg_prcp),
                   max  = max(avg_prcp),
                   med  = median(avg_prcp),
                   mean = mean(avg_prcp))

  obs <- scan_fb %>%
    dplyr::group_by(site) %>%
    dplyr::reframe(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-site) %>%
    dplyr::mutate(min  = min(count),
                  max  = max(count),
                  med  = median(count),
                  mean = mean(count)) %>%
    dplyr::select(-count) %>%
    unique()

  formatting <- function(x) {
    formatC(x, digits = 2, format = 'f')
  }
  data_summary <- rbind(obs, log_pmmov, prcp) %>%
    t() %>%
    apply(2, formatting) %>%
    magrittr::set_rownames(c('Min.', 'Max.', 'Med.', 'Mean')) %>%
    magrittr::set_colnames(c('Obs.', 'PMMoV (log gc/g)', 'Avg. Prcp. (in)')) %>%
    write.csv(file = file.path(out_path, 'data-summary-table.csv'), row.names = TRUE)

  ## by site
  log_pmmov <- scan_fb %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::reframe(min  = min(log_pmmov),
                   max  = max(log_pmmov),
                   med  = median(log_pmmov),
                   mean = mean(log_pmmov))

  prcp <- scan_fb %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::reframe(min  = min(avg_prcp),
                   max  = max(avg_prcp),
                   med  = median(avg_prcp),
                   mean = mean(avg_prcp))

  obs <- scan_fb %>%
    dplyr::group_by(abbreviation) %>%
    dplyr::reframe(count = n())

  pop <- scan_fb %>%
    dplyr::select(abbreviation, state, city, sewer, state_abbr, population) %>%
    dplyr::mutate(location = paste0(city, ', ', state_abbr)) %>%
    dplyr::select(abbreviation, state, city, location, sewer, population) %>%
    unique() %>%
    dplyr::arrange(abbreviation) %>%
    dplyr::select(-c(state, city))

  formatting <- function(x) {
    formatC(x, digits = 2, format = 'f')
  }

  pmmov_summary <- dplyr::left_join(pop, obs, by = 'abbreviation') %>%
    dplyr::left_join(log_pmmov, by = 'abbreviation') %>%
    dplyr::left_join(prcp, by = 'abbreviation') %>%
    dplyr::mutate(sewer = ifelse(sewer == 1, 'Combined', 'Separated')) %>%
    magrittr::set_colnames(c('Site', 'Location', 'Sewer Type', 'Pop. Served', 'Num. of Obs.',
                             'Min. PMMoV', 'Max. PMMoV', 'Med. PMMoV', 'Mean PMMoV',
                             'Min. Prcp.', 'Max. Prcp.', 'Med. Prcp.', 'Mean Prcp.'))
  pmmov_summary[, 5:ncol(pmmov_summary)] <- apply(pmmov_summary[, 5:ncol(pmmov_summary)], 2, formatting)
  write.csv(pmmov_summary, file = file.path(out_path, 'data-summary-table-by-site.csv'), row.names = TRUE)
}
