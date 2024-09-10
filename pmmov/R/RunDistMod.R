#' RunDistMod - Fit and save quantile regression model with distance covariates.
#'
#' @param out_path a string; path to save fitted models
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export


RunDistMod <- function(out_path, scan_fb) {
  ## approximate longitude/latitude for verily life science and el paso
  verily_life_science <- c(-122.4, 37.7)
  el_paso <- c(-106.5, 31.8)

  ## copy and add distance variables
  scan_fb2 <- scan_fb
  scan_fb2$vls_dist <- geosphere::distm(verily_life_science, scan_fb[, c('lng', 'lat')], fun = geosphere::distHaversine) %>%
    t()
  scan_fb2$ep_dist <- geosphere::distm(el_paso, scan_fb[, c('lng', 'lat')], fun = geosphere::distHaversine) %>%
    t()

  corr_df <- scan_fb2 %>%
    dplyr::select(site, vls_dist, ep_dist) %>%
    unique()
  print(paste0('Pearson Correlation: ', cor(corr_df$vls_dist, corr_df$ep_dist)))

  ## fit quantile regression model (laplace likelihood)
  lqr_unres <- lqmm::lqm(formula = log_pmmov ~ vls_dist + ep_dist + avg_prcp + sewer +
                                               avg_prcp:sewer + sin1_yr + cos1_yr +
                                               sin1_wk + cos1_wk,
                         data = scan_fb2,
                         tau = 0.5)
  lqr_unres_sum <- summary(lqr_unres, method = "boot", R = 100, alpha = 0.05)
  saveRDS(lqr_unres, file = file.path(out_path, 'distance_model.rds'))
  saveRDS(lqr_unres_sum, file = file.path(out_path, 'distance_model_sum.rds'))
}
