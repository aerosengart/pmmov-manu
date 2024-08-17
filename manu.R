# SET UP ----
## libraries
library(concaveman)
library(cowplot)
library(dplyr)
library(fda)
library(ggplot2)
library(ggpubr)
library(quantreg)
library(scales)
library(sf)
library(stringr)
library(xlsx)
library(zipcodeR)

## reprod?
set.seed(767473278)
library(pmmov)
setwd('../')

## clean data
scan_fb <- PrepData(out_path     = 'data',
                    raw_path     = 'data/scan',
                    weather_path = 'data/NCDC',
                    clean_ncdc   = FALSE)

## save site abbreviations
abbrev <- SiteAbbrev(scan_fb, out_path = 'checkpoints/csv')
scan_fb <- dplyr::left_join(scan_fb, abbrev, by = c('site' = 'site name'))

################################################################################
##  CALCULATIONS                                                              ##
################################################################################
## fit linear quantile models
RunQuantReg(out_path = 'checkpoints',
            scan_fb  = scan_fb)

## calculate variance partitions
RunVarPart(scan_fb = scan_fb, out_path = 'checkpoints')

## create prediction intervals for visualization
RunPred(out_path     = 'checkpoints',
        weather_path = 'data/NCDC',
        beta         = 1,
        model_path   = 'stan/wu1000_sep_lap_noucd/site_fits',
        sites        = c('Southeast San Francisco',
                         "City of Coeur d'Alene Water Resource Recovery Facility",
                         'City of Garland Rowlett Creek WWTP',
                         'Capital Region Water AWTF'),
        scan_fb      = scan_fb)

## create predictions for all sites
RunPred(out_path     = 'checkpoints',
        weather_path = 'data/NCDC',
        beta         = 1,
        model_path   = 'stan/wu1000_sep_lap_noucd/site_fits',
        sites        = unique(scan_fb$site),
        prefix       = 'all_sites',
        scan_fb      = scan_fb)

## find peaks and troughs of yearly time components
PeakTrough(scan_fb,
           lambda     = 365.25,
           model_path = 'stan/wu1000_sep_lap_noucd/site_fits')
PeakTrough(scan_fb,
           lambda     = 7,
           model_path = 'stan/wu1000_sep_lap_noucd/site_fits')

## find residuals for each site
SiteResid(scan_fb,
          model_path   = 'stan/wu1000_sep_lap_noucd/site_fits',
          out_path     = 'checkpoints')


################################################################################
##  TABLES                                                                    ##
################################################################################
## summary statistics for data set
SaveSummary(scan_fb = scan_fb, out_path = 'checkpoints/csv')

## simple median model
qr_s <- readRDS(file = file.path('checkpoints', 'quantreg_simple.rds'))
qr_s_sum <- readRDS(file = file.path('checkpoints', 'quantreg_simple_sum.rds'))
SaveCoefs(model = qr_s_sum, out_path = 'checkpoints', file_name = 'qr_simple')

## detailed median model
qr_c <- readRDS(file = file.path('checkpoints', 'quantreg_complex.rds'))
qr_c_sum <- readRDS(file = file.path('checkpoints', 'quantreg_complex_sum.rds'))
SaveCoefs(model = qr_c_sum, out_path = 'checkpoints', file_name = 'qr_complex')

## Bayesian median models
SaveStanCoefs(scan_fb,
              model_path = 'stan/wu1000_sep_lap_noucd/site_fits',
              out_path = 'checkpoints')

################################################################################
##  FIGURES                                                                   ##
################################################################################
## US Map with Gradient
FigureGradient(scan_fb, qr_s, out_path = 'manu/figures')

## Variance Partition - no interactions
var_part <- read.csv(file.path('checkpoints', 'csv', 'lm_var_part.csv'))
FigureVariance(scan_fb, var_part, out_path = 'manu/figures')

## Variance Partition - with interactions
var_part <- read.csv(file.path('checkpoints', 'csv', 'lm_var_part_interactions.csv'))
FigureVariance(scan_fb, var_part, out_path = 'manu/figures')

## Site Comparison
pred_inters <- readRDS(file.path('checkpoints', 'cross_us_prediction_intervals.rds'))
prcp_inters <- readRDS(file.path('checkpoints', 'cross_us_precipitation.rds'))
FigureSiteComparison(scan_fb, pred_inters, prcp_inters, out_path = 'manu/figures')

## Boxplots
FigureBoxplot(scan_fb, out_path = 'manu/figures')

## QQ Plots
FigureQQPlot(scan_fb, qr_c, out_path = 'manu/figures')

## Residual ACF Figure
resid_df <- readRDS(file.path('checkpoints', 'site_resids.rds'))
FigureACF(resid_df, all_sites = FALSE, data = FALSE, out_path = 'manu/figures')

## Data ACF Figure
scan_df <- scan_fb %>%
  dplyr::mutate(resid = log_pmmov)
FigureACF(scan_df, all_sites = FALSE, data = TRUE, out_path = 'manu/figures')

## Individual Site Predictions
pred_inters <- readRDS(file.path('checkpoints', 'all_sites_prediction_intervals.rds'))
prcp_inters <- readRDS(file.path('checkpoints', 'all_sites_precipitation.rds'))
FigureSitePred(scan_fb, pred_inters, prcp_inters, out_path = 'manu/figures/site_preds')

## Sewer Differences
FigureSewerComparison(scan_fb, weather_path = 'data/NCDC', out_path = 'manu/figures')

## Site Component Effects
FigureComponents(scan_fb,
                 site_name = 'Loxahatchee River Environmental Control District',
                 start = '2021-06-01', end = '2023-08-01',
                 limits = c(8.25, 8.5),
                 model_path = '~/projects/pmmov-manu/stan/wu1000_sep_lap_noucd/site_fits',
                 weather_path = '~/projects/pmmov-manu/data/NCDC')

## Site Peaks
peak_trough <- readRDS('checkpoints/peak_trough_365.25.rds')
FigurePeaks(scan_fb, peak_trough, lambda = 365.25, out_path = 'manu/figures')

peak_trough <- readRDS('checkpoints/peak_trough_7.rds')
FigurePeaks(scan_fb, peak_trough, lambda = 7, out_path = 'manu/figures')
