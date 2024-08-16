#' PredInterval - Create predictions and bands for model fit with Stan.
#'
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @param beta a scalar; exponent for generalized Gaussian distribution
#' @param model_path a string path; path to RDS files for stan fits for each site
#' @param sites a vector of strings; names of sites to predict for
#' @return a data frame of site metadata as well as quantiles and mean of predictions
#' using a random sample of coefficient estimates across all chains from Stan fit
#'
#' @importFrom magrittr %>%
#'
#' @export

PredInterval <- function(scan_fb, beta, model_path, sites) {
  ## prepare data for predictions
  data <- scan_fb %>%
    dplyr::filter(site %in% sites)
  coef <- c('avg_prcp', 'sin1_wk', 'cos1_wk', 'sin1_yr', 'cos1_yr')

  site_preds <- c()
  for (i in 1:length(sites)) {
    ## center continuous variables
    scale2 <- function(x) scale(x, center = TRUE, scale = TRUE)

    site_data <- data %>%
      dplyr::filter(site == sites[i]) %>%
      dplyr::mutate_at(coef, scale2)

    ## read in model
    site_file <- paste0(sites[i], "_stan_fit.rds")
    site_model_path <- file.path(model_path, site_file)
    model <- readRDS(site_model_path)
    model_sum <- rstan::summary(model, probs = c(0.025, 0.05, 0.5, 0.95, 0.975))[['summary']]
    site_sum <- model_sum[grep('y_rep', rownames(model_sum)), c('2.5%', '5%', '50%', '95%', '97.5%')] %>%
      data.frame()
    one_site_preds <- site_sum %>%
      magrittr::set_colnames(c('2.5\\%', '5\\%', '50\\%', '95\\%', '97.5\\%')) %>%
      dplyr::mutate(site = site_data[1, 'site'],
                    city = site_data[1, 'city'],
                    state_abbr = site_data[1, 'state_abbr'],
                    state = site_data[1, 'state'],
                    zipcode = site_data[1, 'zipcode'],
                    fipscode = site_data[1, 'fipscode'])
    one_site_preds$date <- seq(as.Date(min(site_data$date)), as.Date(max(site_data$date)), by = 1)
    site_md <- site_data[, c('date', 'log_pmmov')] %>%
      dplyr::mutate(date = as.Date(date))
    one_site_preds <- dplyr::left_join(one_site_preds, site_md, by = 'date')
    site_preds <- rbind(site_preds, one_site_preds)
  }
  return (site_preds)
}







