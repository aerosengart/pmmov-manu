
sink('stan_output.txt')
library(dplyr)
library(rstan)

data <- read.csv('../../../data/scan_clean_df.csv')
stan_data <- read.csv('../../../data/stan_data.csv')
sites <- unique(data$site)
coef <- c('avg_prcp', 'sin1_wk', 'cos1_wk', 'sin1_yr', 'cos1_yr')

for (i in 1:length(sites)) {
  ## center continuous variables
  scale2 <- function(x) scale(x, center = TRUE, scale = TRUE)

  site_data <- data %>%
    dplyr::filter(site == sites[i]) %>%
    dplyr::mutate_at(coef, scale2)

  site_stan <- stan_data %>%
    dplyr::filter(site == sites[i]) %>%
    dplyr::mutate_at(coef, scale2)

  print(nrow(site_stan))

  covars <- site_data %>%
    dplyr::select(all_of(coef))
  covars_stan <- site_stan %>%
    dplyr::select(all_of(coef))


  pmmov_data <- list(
    beta = 1.0,
    obs = nrow(site_data),
    p = length(coef),
 
    X = covars,
    y = site_data$log_pmmov,

    obs_rep = nrow(site_stan),
    X_rep = covars_stan
  )

  fit <- stan(
    file   = 'pmmov.stan',
    data   = pmmov_data,
    iter   = 2000,
    cores  = 4,
    include = TRUE,
    pars = c('sigma', 'alpha', 'sigma_coef', 'B', 'y_rep'),
    control = list(adapt_delta = 0.9, 
                   stepsize = 0.5) 
  )
  site_file <- paste0(sites[i], '_stan_fit.rds')
  saveRDS(fit, file = file.path('site_fits', site_file))
}
sink()


