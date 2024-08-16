#' RunVarPart - Calculate variance partitions for linear model.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param out_path a string; path to save .csv of coefficients
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

RunVarPart <- function(scan_fb, out_path) {
  ## linear model R2
  lm1 <- lm(log_pmmov ~ lat + lng, data = scan_fb)
  lm2 <- lm(log_pmmov ~ lat + lng + sewer + avg_prcp + sewer:avg_prcp, data = scan_fb)
  lm3 <- lm(log_pmmov ~ lat + lng + sewer + avg_prcp + sewer:avg_prcp + site, data = scan_fb)
  lm4 <- lm(log_pmmov ~ lat + lng + sewer + avg_prcp + sewer:avg_prcp + site
            + sin1_wk + cos1_wk + sin1_yr + cos1_yr,
            data = scan_fb)
  lm5 <- lm(log_pmmov ~ lat + lng + sewer + avg_prcp + sewer:avg_prcp + site
            + sin1_wk + cos1_wk + sin1_yr + cos1_yr
            + lat:sin1_wk + lat:cos1_wk + lat:sin1_yr + lat:cos1_yr
            + lng:sin1_wk + lng:cos1_wk + lng:sin1_yr + lng:cos1_yr,
            data = scan_fb)
  lm6 <- lm(log_pmmov ~ lat + lng + sewer + avg_prcp + sewer:avg_prcp + site
            + sin1_wk + cos1_wk + sin1_yr + cos1_yr
            + lat:sin1_wk + lat:cos1_wk + lat:sin1_yr + lat:cos1_yr
            + lng:sin1_wk + lng:cos1_wk + lng:sin1_yr + lng:cos1_yr
            + site:sin1_wk + site:cos1_wk + site:sin1_yr + site:cos1_yr,
            data = scan_fb)

  ## no extended interactions
  percent <- c(summary(lm1)$r.squared,
               summary(lm2)$r.squared,
               summary(lm3)$r.squared,
               summary(lm4)$r.squared,
               1)
  label <- c(percent[1],
             percent[2] - percent[1],
             percent[3] - percent[2],
             percent[4] - percent[3],
             1 - percent[4])
  name <- c('Spatial', 'Sewer & Precipitation', 'Site', 'Temporal', 'Residual')
  model <- rep('total', 5)
  lm_vp_df <- data.frame(percent, label, name, model)
  lm_vp_df$name <- factor(name, levels = rev(name))
  lm_vp_df$label_y <- percent - 0.5 * label
  dir.create(file.path(out_path, 'csv'), showWarnings = FALSE)
  write.csv(lm_vp_df, file = file.path(out_path, 'csv', 'lm_var_part.csv'), row.names = FALSE)

  ## add interactions
  percent <- c(summary(lm1)$r.squared,
               summary(lm2)$r.squared,
               summary(lm3)$r.squared,
               summary(lm4)$r.squared,
               summary(lm5)$r.squared,
               summary(lm6)$r.squared,
               1)
  label <- c(percent[1],
             percent[2] - percent[1],
             percent[3] - percent[2],
             percent[4] - percent[3],
             percent[5] - percent[4],
             percent[6] - percent[5],
             1 - percent[6])
  name <- c('Spatial', 'Sewer & Precipitation', 'Site', 'Temporal',
            'Spatio-Temporal Interaction', 'Site-Temporal Interaction', 'Residual')
  model <- rep('total', 7)
  lm_vp_df <- data.frame(percent, label, name, model)
  lm_vp_df$name <- factor(name, levels = rev(name))
  lm_vp_df$label_y <- percent - 0.5 * label
  dir.create(file.path(out_path, 'csv'), showWarnings = FALSE)
  write.csv(lm_vp_df,
            file = file.path(out_path, 'csv', 'lm_var_part_interactions.csv'),
            row.names = FALSE)
}
