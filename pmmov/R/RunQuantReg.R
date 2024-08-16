#' RunQuantReg - Fit and save linear quantile regression models.
#'
#' @param out_path a string; path to save fitted models
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

RunQuantReg <- function(out_path, scan_fb) {
  ## Linear Quantile Regression ------------------------------------------------
  ## very simple --- latitude and longitude
  qr_s <- quantreg::rq(formula = log_pmmov ~ lat + lng,
                       tau     = 0.5,
                       data    = scan_fb,
                       method  = 'br')
  qr_s_sum <- summary(qr_s, se = 'boot', cluster = scan_fb$site)
  saveRDS(qr_s, file = file.path(out_path, 'quantreg_simple.rds'))
  saveRDS(qr_s_sum, file = file.path(out_path, 'quantreg_simple_sum.rds'))

  ## complex linear model --- use other covariates
  ## 9 parameters
  qr_c <- quantreg::rq(formula = log_pmmov ~ lat + lng + avg_prcp + sewer +
                                   avg_prcp:sewer + sin1_yr + cos1_yr +
                                   sin1_wk + cos1_wk,
                       tau     = 0.5,
                       data    = scan_fb,
                       method  = 'br')
  # qr_c_bic <- 9 * log(nrow(scan_fb)) - 2 * logLik(qr_c)
  ## 8 parameters
  # qr_c2 <- quantreg::rq(formula = log_pmmov ~ lat + lng + avg_prcp + sewer +
  #                        avg_prcp:sewer + sin1_yr + cos1_yr +
  #                        wknd,
  #                       tau     = 0.5,
  #                       data    = scan_fb,
  #                       method  = 'br')
  # qr_c2_bic <- 8 * log(nrow(scan_fb)) - 2 * logLik(qr_c2)
  qr_c_sum <- summary(qr_c, se = 'boot', cluster = scan_fb$site)
  saveRDS(qr_c, file = file.path(out_path, 'quantreg_complex.rds'))
  saveRDS(qr_c_sum, file = file.path(out_path, 'quantreg_complex_sum.rds'))
}


