#' SiteResid - Calculate residuals from Stan models.
#'
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @param model_path a string path; path to RDS files for stan fits for each site
#' @param out_path a string path; path to save data frame of residuals to
#' @return none
#'
#' @importFrom magrittr %>%
#'
#' @export

SiteResid <- function(scan_fb, model_path, out_path = '~/pmmov-manu/checkpoints') {
  ## prepare data for predictions
  data <- scan_fb
  sites <- unique(scan_fb$site)

  site_preds <- c()
  ## center continuous variables
  scale2 <- function(x) scale(x, center = TRUE, scale = TRUE)
  coef <- c('avg_prcp', 'sin1_wk', 'cos1_wk', 'sin1_yr', 'cos1_yr')
  for (i in 1:length(sites)) {

    site_data <- data %>%
      dplyr::filter(site == sites[i]) %>%
      dplyr::mutate_at(coef, scale2)

    ## read in model
    site_file <- paste0(sites[i], "_stan_fit.rds")
    site_model_path <- file.path(model_path, site_file)
    model <- readRDS(site_model_path)
    model_sum <- rstan::summary(model, probs = c(0.5))$summary ## just median
    site_sum <- data.frame(model_sum[grep('y_rep', rownames(model_sum)), '50%'])

    site_md <- site_data[, c('site', 'date', 'city', 'state_abbr', 'state',
                             'zipcode', 'fipscode', 'log_pmmov')]
    site_sum$date <- seq(min(site_md$date), max(site_md$date), by = 1)
    site_sum <- site_sum %>%
      magrittr::set_colnames(c('pred', 'date')) %>%
      dplyr::mutate(date = as.Date(date))
    one_site_preds <- dplyr::left_join(site_md, site_sum, by = 'date')
    one_site_preds$resid <- one_site_preds$log_pmmov - one_site_preds$pred
    site_preds <- rbind(site_preds, one_site_preds)
  }

  ## save the data frame (takes a long time to make)
  saveRDS(site_preds, file = file.path(out_path, 'site_resids.RDS'))
}







