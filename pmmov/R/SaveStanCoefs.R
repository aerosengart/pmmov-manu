#' SaveStanCoefs - save summaries of model parameters for all sites
#'
#' @param scan_fb a data frame; cleaned WWTP data framed
#' @param model_path a string; path to saved Stan model fits
#' @param out_path a string; path to save .csv file
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

SaveStanCoefs <- function(scan_fb, model_path, out_path) {
  ## make abbreviations map
  abbreviations <- scan_fb[, c('site', 'abbreviation')] %>%
    unique()

  unique_sites <- unique(scan_fb$site)
  intercepts <- c()
  sigmas <- c()
  prcp_coefs <- c()
  sinwk_coefs <- c()
  coswk_coefs <- c()
  sinyr_coefs <- c()
  cosyr_coefs <- c()
  for (i in 1:length(unique_sites)) {
    model <- readRDS(file.path(model_path, paste0(unique_sites[i], "_stan_fit.rds")))
    mod_sum <- rstan::summary(model, probs = c(0.025, 0.5, 0.975))$summary[c('sigma', 'alpha', 'B[1]', 'B[2]', 'B[3]', 'B[4]', 'B[5]'),] %>%
      t()
    sigmas      <- rbind(sigmas,      c(unique_sites[i], mod_sum[, 'sigma']))
    intercepts  <- rbind(intercepts,  c(unique_sites[i], mod_sum[, 'alpha']))
    prcp_coefs  <- rbind(prcp_coefs,  c(unique_sites[i], mod_sum[, 'B[1]']))
    sinwk_coefs <- rbind(sinwk_coefs, c(unique_sites[i], mod_sum[, 'B[2]']))
    coswk_coefs <- rbind(coswk_coefs, c(unique_sites[i], mod_sum[, 'B[3]']))
    sinyr_coefs <- rbind(sinyr_coefs, c(unique_sites[i], mod_sum[, 'B[4]']))
    cosyr_coefs <- rbind(cosyr_coefs, c(unique_sites[i], mod_sum[, 'B[5]']))
  }

  MakeDF <- function(param_mat) {
    param_df <- data.frame(param_mat) %>%
      magrittr::set_colnames(c('site', 'mean', 'mcse', 'sd', 'lo', 'med', 'hi', 'n_eff', 'rhat'))
    param_df[, 2:ncol(param_df)] <- apply(param_df[, 2:ncol(param_df)], 2, as.numeric)
    param_df <- param_df %>%
      dplyr::mutate(mean  = formatC(mean, format = 'E', digits = 3),
                    mcse  = formatC(mcse, format = 'E', digits = 3),
                    sd    = formatC(sd, format = 'E', digits = 3),
                    lo    = formatC(lo, format = 'E', digits = 3),
                    med   = formatC(med, format = 'E', digits = 3),
                    hi    = formatC(hi, format = 'E', digits = 3),
                    n_eff = formatC(n_eff, format = 'E', digits = 3),
                    rhat  = formatC(rhat, format = 'E', digits = 3))
    param_df$sig <- ifelse(as.numeric(param_df$lo) < 0 & as.numeric(param_df$hi) > 0, 0, 1)
    param_df[, 2:(ncol(param_df) - 1)] <- apply(param_df[, 2:(ncol(param_df) - 1)], 2, Formatting)
    param_df$lo <- gsub("\\$", '', param_df$lo)
    param_df$hi <- gsub("\\$", '', param_df$hi)
    param_df$ci <- ifelse(param_df$sig == 1,
                          paste0('$\\mathbf{(', param_df$lo, ', ', param_df$hi, ')}$'),
                          paste0('$(', param_df$lo, ', ', param_df$hi, ')$'))
    param_df <- param_df %>%
      dplyr::left_join(abbreviations, by = 'site') %>%
      dplyr::select(abbreviation, mean, mcse, sd, med, ci, n_eff, rhat) %>%
      magrittr::set_colnames(c('Site', 'Mean', 'MCSE', 'SD', 'Median', '95\\% CI', 'ESS', 'R Hat')) %>%
      dplyr::arrange(Site)

    return (param_df)
  }

  sigma_df <- MakeDF(sigmas)
  inter_df <- MakeDF(intercepts)
  prcp_df  <- MakeDF(prcp_coefs)
  sinwk_df <- MakeDF(sinwk_coefs)
  coswk_df <- MakeDF(coswk_coefs)
  sinyr_df <- MakeDF(sinyr_coefs)
  cosyr_df <- MakeDF(cosyr_coefs)

  dir.create(file.path(out_path, 'csv'), showWarnings = FALSE)
  write.csv(sigma_df, file = file.path(out_path, 'csv', 'sigma_fits.csv'), row.names = FALSE)
  write.csv(inter_df, file = file.path(out_path, 'csv', 'alpha_fits.csv'), row.names = FALSE)
  write.csv(prcp_df,  file = file.path(out_path, 'csv', 'prcp_fits.csv'),  row.names = FALSE)
  write.csv(sinwk_df, file = file.path(out_path, 'csv', 'sinwk_fits.csv'), row.names = FALSE)
  write.csv(coswk_df, file = file.path(out_path, 'csv', 'coswk_fits.csv'), row.names = FALSE)
  write.csv(sinyr_df, file = file.path(out_path, 'csv', 'sinyr_fits.csv'), row.names = FALSE)
  write.csv(cosyr_df, file = file.path(out_path, 'csv', 'cosyr_fits.csv'), row.names = FALSE)
}



