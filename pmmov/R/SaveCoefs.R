#' SaveCoefs - Save model coefficients as .csv
#'
#' @param model a model
#' @param out_path a string; path to save .csv of coefficients
#' @param file_name a string; name of .csv file (no extension)
#' @param index_fix a vector; contains strings for names of fixed effect variables
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

SaveCoefs <- function(model, out_path, file_name,
                      index_fix = c('lat', 'lng', 'avg_prcp', 'sewer',
                                    'avg_prcp:sewer', 'sin1_wk', 'cos1_wk',
                                    'sin2_wk', 'cos2_wk')) {
  ## find which type of model
  mod_class <- class(model)[1]
  if (mod_class == 'lm') { ## linear model
    mod_coef_tab <- summary(model)$coefficients[, c('Estimate', 'Std. Error', 'Pr(>|t|)')] %>%
      formatC(format = 'E', digits = 3)
  } else if (mod_class == 'stanfit') { ## stan fit
    mod_coef_tab <- rstan::summary(model)
    mod_coef_tab <- mod_coef_tab$summary[, c('2.5%', '50%', '97.5%')]
    mod_coef_tab <- mod_coef_tab[grep('U', rownames(mod_coef_tab), invert = TRUE), ] %>%
      formatC(format = 'E', digits = 3)
  } else if (mod_class == 'summary.rq') { ## quantile regression
    mod_coef_tab <- model$coefficients[, c('Value', 'Std. Error', 'Pr(>|t|)')] %>%
      formatC(format = 'E', digits = 3)
  } else if (mod_class == 'lmerMod') { ## mixed model
    ## extract estimates and standard errors for fixed/random effects
    all <- lme4::fixef(model)
    fix_eff <- all[names(all) %in% index_fix]
    rand_eff <- all[!(names(all) %in% index_fix)]
    fix_std_err <- summary(model)$coefficients[names(fix_eff), 'Std. Error']
    rand_std_err <- summary(model)$coefficients[names(rand_eff), 'Std. Error']
    ## extract estimated variance components (random effects and residual)
    rand_est_var <- c(diag(summary(model)$varcor$site)[names(rand_eff)],
                      summary(model)$sigma^2)
    ## combine into data frames
    fix_df <- data.frame('Estimate' = formatC(fix_eff,
                                              format = 'E', digits = 3),
                         'Std. Err.' = formatC(fix_std_err,
                                               format = 'E', digits = 3)) %>%
      t()
    rand_df <- data.frame('Cond. Mode' = c(formatC(rand_eff,
                                                   format = 'E', digits = 3),
                                           '-'),
                          'Std. Err.' = c(formatC(rand_std_err,
                                                  format = 'E', digits = 3),
                                          '-'),
                          'Estim. Var.' = formatC(rand_est_var,
                                                  format = 'E', digits = 3)) %>%
      t() %>%
      magrittr::set_colnames(c(names(rand_std_err), 'resid'))
  } else if (mod_class == 'geeglm') { ## GEEs
    gee_exch <- summary(model)$coef[, c('Estimate', 'Std.err', 'Pr(>|W|)')]
    mod_coef_tab <- data.frame(formatC(gee_exch$Estimate,
                                        format = 'E', digits = 3),
                                formatC(gee_exch$Std.err,
                                        format = 'E', digits = 3),
                                formatC(gee_exch$`Pr(>|W|)`,
                                        format = 'E', digits = 3)) %>%
      magrittr::set_rownames(rownames(gee_exch)) %>%
      magrittr::set_colnames(colnames(gee_exch))
  } else if (mod_class == 'summary.lqm') { ## LQM
    mod_coef_tab <- model$tTable[, c('Value', 'Std. Error', 'Pr(>|t|)')]%>%
      formatC(format = 'E', digits = 3)
  } else {
    stop('Model class not supported.')
  }

  Formatting <- function(x) {
    new_strs <- c()
    for (i in 1:length(x)) {
      substrs <- strsplit(x[i], split = "E")[[1]]
      if (abs(as.numeric(substrs[1]) * 10^as.numeric(substrs[2])) < 10^(-15)) {
        new_strs <- c(new_strs, "$\\mathbf{< 1.000 \\times 10^{-15}}$")
      } else if (as.numeric(substrs[2]) == 0) {
        new_strs <- c(new_strs, paste0("$", substrs[1], "$"))
      } else if (as.numeric(substrs[2]) == 1) {
        new_strs <- c(new_strs, paste0("$", substrs[1], " \\times 10$"))
      } else if (as.numeric(substrs[2]) == -1) {
        new_strs <- c(new_strs, paste0("$", format(as.numeric(substrs[1]) / 10, digits = 3, nsmall = 3), "$"))
      } else {
        new_strs <- c(new_strs, paste0("$", substrs[1], " \\times 10^{", as.numeric(substrs[2]), "}$"))
      }
    }
    return (new_strs)
  }

  row_names <- data.frame(rownames(mod_coef_tab)) %>%
    magrittr::set_colnames('row_names') %>%
    dplyr::mutate(new_name = dplyr::case_when(row_names == '(Intercept)' ~ 'Intercept',
                                              row_names == 'vls_dist' ~ 'Dist. to Lab',
                                              row_names == 'ep_dist' ~ 'Dist. to El Paso',
                                              row_names == 'avg_prcp' ~ 'Avg. Prcp.',
                                              row_names == 'sewer' ~ 'Sewer',
                                              row_names == 'lat' ~ 'Lat.',
                                              row_names == 'lng' ~ 'Lng.',
                                              row_names == 'sin1_wk' ~ '$\\psi_{7}^{\\text{sin}}$',
                                              row_names == 'cos1_wk' ~ '$\\psi_{7}^{\\text{cos}}$',
                                              row_names == 'sin1_yr' ~ '$\\psi_{365.25}^{\\text{sin}}$',
                                              row_names == 'cos1_yr' ~ '$\\psi_{365.25}^{\\text{cos}}$',
                                              row_names == 'avg_prcp:sewer' ~ 'Avg. Prcp./Sewer',
                                              TRUE ~ row_names))
  mod_coef_tab2 <- apply(mod_coef_tab, 2, Formatting)
  rownames(mod_coef_tab2) <- row_names$new_name

  if (mod_class == 'stanfit') {
    mod_coef_tab3 <- as.data.frame(mod_coef_tab2) %>%
      dplyr::mutate('95% Interval' = paste0('(', `2.5%`, ', ', `97.5%`, ')')) %>%
      dplyr::select(c('50%', '95% Interval'))
    mod_coef_tab <- as.data.frame(mod_coef_tab) %>%
      dplyr::mutate('95% Interval' = paste0('(', `2.5%`, ', ', `97.5%`, ')')) %>%
      dplyr::select(c('50%', '95% Interval'))
    mod_coef_tab <- rbind(mod_coef_tab3, mod_coef_tab)
  } else {
    mod_coef_tab <- rbind(mod_coef_tab2, mod_coef_tab)
  }


  ## save as .csv
  dir.create(file.path(out_path, 'csv'), showWarnings = FALSE)
  if (mod_class != 'lmerMod') {
    write.csv(mod_coef_tab,
              file      = file.path(out_path, 'csv', paste0(file_name, '.csv')),
              row.names = TRUE)
  } else { ## if mixed model, save fixed/random estimates separately
    write.csv(fix_df,
              file      = file.path(out_path, 'csv', paste0(file_name, '_fix.csv')),
              row.names = TRUE)
    write.csv(rand_df,
              file      = file.path(out_path, 'csv', paste0(file_name, '_rand.csv')),
              row.names = TRUE)
  }
}
