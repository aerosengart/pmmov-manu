#' AddBasisSCAN - add Fourier basis functions to SCAN dataset
#'
#' @param df a data frame for SCAN
#' @param num_basis an integer; the number of pairs of basis functions to add for year AND week
#'
#' @return a data frame; SCAN dataset with additional columns for basis function values
#'
#' @importFrom magrittr %>%
#'
#' @export

AddBasisSCAN <- function(df, num_basis) {
  ## add date as integers
  min_date <- min(df$date)
  max_date <- max(df$date)
  dates <- seq(as.Date(min_date), as.Date(max_date), by = 'day')
  dates_df <- data.frame(dates, 1:(length(dates))) %>%
    magrittr::set_colnames(c('dates', 't'))
  df_date <- df %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::left_join(dates_df, by = c('date' ='dates'))

  ## drops constant basis function (idx = 1)
  fb <- fda::create.fourier.basis(rangeval  = c(min(df_date$t), max(df_date$t)),
                                  nbasis    = num_basis + 1,
                                  period    = 365.25,
                                  dropind   = 1)
  fb2 <- fda::create.fourier.basis(rangeval = c(min(df_date$t), max(df_date$t)),
                                   nbasis   = num_basis + 1,
                                   period   = 7,
                                   dropind  = 1)

  eval_fb <- fda::eval.basis(seq(min(df_date$t), max(df_date$t), 1), fb) %>%
    as.data.frame()
  eval_fb2 <- fda::eval.basis(seq(min(df_date$t), max(df_date$t), 1), fb2) %>%
    as.data.frame()
  eval_fb$t <- seq(min(df_date$t), max(df_date$t))
  eval_fb2$t <- seq(min(df_date$t), max(df_date$t))
  colnames(eval_fb) <- paste0(colnames(eval_fb), '_yr')
  colnames(eval_fb2) <- paste0(colnames(eval_fb2), '_wk')

  scan_fb <- dplyr::left_join(df_date, eval_fb, by = c('t' = 't_yr')) %>%
    dplyr::left_join(eval_fb2, by = c('t' = 't_wk'))

  return (scan_fb)
}
