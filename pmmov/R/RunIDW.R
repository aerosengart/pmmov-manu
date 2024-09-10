#' RunIDW - Do IDW spatial interpolation.
#'
#' Code is a (very slightly) modified version of the code from the `phylin` package.
#'
#' Tarroso, P., Velo-Anton, G., & Carvalho, S. B. (2015). PHYLIN: an R
#' package for phylogeographic interpolation. Molecular Ecology Resources, 15(2), 349-357.
#'
#' Tarroso, P., Carvalho, S. B. & Velo-Anton, G. (2019). PHYLIN 2.0:
#' Extending the phylogeographic interpolation method to include
#' uncertainty and user-defined distance metrics. Molecular Ecology Resources, 19(2), 1081-1094.
#'
#' There is an error when calculating the distance matrix where the matrix is cast to a list
#' which throws an indexing error.
#'
#' The power parameter will be chosen by LOOCV if no value is provided.
#'
#' @param out_path a string; path to save fitted models
#' @param scan_fb a data frame; cleaned WWTP data frame
#' @param pred_grid a data frame; coordinates at which to interpolate (should contain columns for latitude and longitude)
#' @param power a positive scalar; power to use for IDW
#' @return a data frame of interpolated values at the locations specified by `pred_grid`
#'
#' @importFrom magrittr %>%
#'
#' @export

RunIDW <- function(out_path, scan_fb, pred_grid, power = NA) {
  ## collapse down to just medians
  scan_fb_med <- scan_fb %>%
    dplyr::group_by(site) %>%
    dplyr::mutate(med_log_pmmov = median(log_pmmov),
                  med_pmmov = median(pmmov)) %>%
    dplyr::select('state', 'state_abbr', 'city', 'site', 'abbreviation',
                  'population', 'zipcode', 'fipscode', 'lat', 'lng',
                  'med_log_pmmov', 'med_pmmov') %>%
    unique()
  scan_coords <- scan_fb_med[, c('lat', 'lng')]
  scan_vals <- scan_fb_med[, 'med_log_pmmov']

  ## from https://github.com/ptarroso/phylin/blob/master/source/R/geo.dist.R
  ## need to unlist replacement and cast to numeric
  geo.dist <- function(from, to) {
    dst <- matrix(NA, nrow=nrow(from), ncol=nrow(to))
    dimnames(dst) <- list(rownames(from), rownames(to))
    for (i in 1:nrow(from)) {
      dst[i, ] <- unlist(((to[, 1] - as.numeric(from[i, 1]))^2 + (to[, 2] - as.numeric(from[i, 2]))^2)^0.5)
    }
    return(dst)
  }

  ## from https://github.com/ptarroso/phylin/blob/master/source/R/idw.R
  ## removed unnecessary arguments and lines
  idw <- function(values, coords, grid, pow) {
    ## calculate distances for all points
    d.real <- geo.dist(from = grid, to = coords)
    dimensions <- dim(d.real)

    if (is.na(pow)) {
      ## added in cross-validation for picking power
      powers <- seq(0.001, 4, 0.1)
      mse_result <- NULL
      for (power in powers) {
        print(paste0('Running ', power, '. . .'))
        mse <- 0
        for (i in 1:nrow(scan_fb_med)) {
          ## test/train set
          test_coords <- scan_coords[i, ]
          test_val <- scan_vals[i, ]
          train_coords <- scan_coords[-i, ]
          train_vals <- scan_vals[-i, ]

          test_dst <- geo.dist(from = test_coords, to = train_coords)
          dimensions <- dim(test_dst)
          w <- 1/test_dst**power

          ## To allow the idw to act on points with same coordinate, rows
          ## are checked for infinite weights. When found, points with Inf
          ## are 1 and all others have 0 weight
          for (i in 1:nrow(w)) {
            if (sum(is.infinite(w[i, ])) > 0) {
              w[i, !is.infinite(w[i, ])] <- 0
              w[i, is.infinite(w[i, ])] <- 1
            }
          }

          ## interpolation
          w.sum <- apply(w, 1, sum, na.rm = TRUE)
          wx <- w %*% diag(unlist(train_vals))
          ux <- apply(wx / w.sum, 1, sum, na.rm = TRUE)
          mse <- mse + (unlist(test_val) - ux)^2
        }
        mse_result <- c(mse_result, mse / nrow(scan_fb_med))
      }
      ## get best power by minimum mse
      pow <- powers[which.min(mse_result)]
      print(paste0('OPTIMAL POWER IS ', pow))
    }

    ## do interpolation
    w <- 1/d.real**pow

    ## To allow the idw to act on points with same coordinate, rows
    ## are checked for infinite weights. When found, points with Inf
    ## are 1 and all others have 0 weight
    for (i in 1:nrow(w)) {
      if (sum(is.infinite(w[i, ])) > 0) {
        w[i, !is.infinite(w[i, ])] <- 0
        w[i, is.infinite(w[i, ])] <- 1
      }
    }

    ## interpolation
    w.sum <- apply(w, 1, sum, na.rm = TRUE)
    wx <- w %*% diag(unlist(values))
    ux <- apply(wx / w.sum, 1, sum, na.rm = TRUE)

    data.frame(pred = ux)
  }

  scan_intpol <- idw(values = scan_vals,
                     coords = scan_coords,
                     grid = pred_grid,
                     pow = power)
  scan_intpol$lat <- pred_grid$lat
  scan_intpol$lng <- pred_grid$lng

  saveRDS(scan_intpol, file = file.path(out_path, 'idw_intpol.rds'))
  return(scan_intpol)
}





