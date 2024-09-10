#' FigureGradient - Create and save gradient map figure as PDF.
#'
#' Figure is a map of the contiguous USA with a color gradient made with
#' cubic regression spline generalized additive model.
#'
#' @param scan_fb a data frame; cleaned SCAN data
#' @param power a positive scalar; power to use for IDW
#' @param out_path a string; path to save figure
#'
#' @return None
#'
#' @importFrom magrittr %>%
#'
#' @export

FigureGradient <- function(scan_fb, power = NA, out_path = 'manu/figures') {
  ## create map of US --- state outline data
  state_outline_data <- ggplot2::map_data("state")
  lat_interval <- seq(min(state_outline_data$lat),
                      max(state_outline_data$lat),
                      0.2)
  long_interval <- seq(min(state_outline_data$long),
                       max(state_outline_data$long),
                       0.2)
  us_outline_data <- ggplot2::map_data("usa")
  us_outline_sf <- sf::st_as_sf(us_outline_data, coords = c("lat", "long"))
  ## in case points are out of order
  us_polygon <- concaveman::concaveman(us_outline_sf)
  grid <- expand.grid(lat_interval, long_interval) %>%
    dplyr::rename(lat = Var1,
                  lng = Var2)
  ## do spatial interpolation
  pred_grid <- RunIDW(out_path = 'checkpoints', scan_fb = scan_fb, pred_grid = grid, power = power)
  pred_grid_sf <- sf::st_as_sf(pred_grid, coords = c("lat", "lng"))
  pred_in_us <- sf::st_filter(pred_grid_sf, us_polygon) %>%
    dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                  lng = sf::st_coordinates(.)[,2]) %>%
    sf::st_drop_geometry()

  ## create white rectangle to get rid of outside colors
  outer <- matrix(c(-140, 20, -60, 20, -60, 55, -140, 55, -140, 20),
                  ncol = 2,
                  byrow = TRUE)
  outer_ls <- list(outer)
  map_pgon <- sf::st_polygon(outer_ls)
  for (i in 1:length(unique(us_outline_data$group))) {
    hole <- us_outline_data %>%
      dplyr::filter(group == i) %>%
      dplyr::select(c(1, 2)) %>%
      as.matrix()
    hole_close <- rbind(hole, hole[1, ])
    hole_ls <- list(hole_close)
    hole_pgon <- sf::st_polygon(hole_ls)
    hole_buff <- sf::st_buffer(hole_pgon, dist = 0)
    map_pgon <- sf::st_difference(map_pgon, hole_buff)
  }

  ## US with gradient
  site_map_df <- scan_fb %>%
    dplyr::group_by(site, city) %>%
    dplyr::mutate(mean_pmmov = mean(pmmov),
                  med_pmmov  = median(pmmov)) %>%
    ungroup() %>%
    dplyr::select(c(state, state_abbr, city, site, zipcode, fipscode, lat, lng,
                    sewer, mean_pmmov, med_pmmov)) %>%
    unique() %>%
    dplyr::mutate(sewer = ifelse(sewer == 0,
                                 'Separated',
                                 ifelse(sewer == 1,
                                        'Combined',
                                        'Unknown'))) %>% ## there are 0 unknown
    dplyr::filter(state != 'Alaska')

  ## create ggplot object
  break_seq <- c(min(pred_in_us$pred),
                 min(pred_in_us$pred) + (max(pred_in_us$pred) - min(pred_in_us$pred)) / 2,
                 max(pred_in_us$pred))
  labels_seq <- round(break_seq, 2)
  site_map <- ggplot2::ggplot() +
    ggplot2::geom_point(data    = pred_in_us,
                        mapping = ggplot2::aes(x     = lng,
                                               y     = lat,
                                               group  = 1,
                                               color  = pred,
                                               stroke = 0)) +
    ggplot2::geom_polygon(data      = state_outline_data,
                          mapping   = ggplot2::aes(x     = long,
                                                   y     = lat,
                                                   group = group),
                          colour    = "black",
                          fill      = NA,
                          linewidth = 0.2) +
    ggplot2::geom_sf(map_pgon,
                     colour    = "black",
                     mapping = ggplot2::aes(group = 2),
                     fill    = 'white') +
    ggplot2::annotate(geom = "segment", x = -123.5, xend = -120,
                      y = 36.5, yend = 36.5, colour = "green3") +
    ggplot2::annotate(geom = "segment", x = -123.5, xend = -120,
                      y = 39, yend = 39, colour = "green3") +
    ggplot2::annotate(geom = "segment", x = -123.5, xend = -123.5,
                      y = 36.5, yend = 39, colour = "green3") +
    ggplot2::annotate(geom = "segment", x = -120, xend = -120,
                      y = 36.5, yend = 39, colour = "green3") +
    ggplot2::geom_point(data    = site_map_df,
                        mapping = ggplot2::aes(x     = lng,
                                               y     = lat,
                                               shape = sewer,
                                               alpha = 0.95,
                                               size  = sewer,
                                               group = 1),
                        fill    = 'white',
                        alpha   = 0.6) +
    ggplot2::scale_shape_manual(values = c(24, 21)) +
    ggplot2::scale_size_manual(values = c(3.5, 2), guide = 'none') +
    ggplot2::scale_alpha(guide = 'none') +
    ggplot2::scale_colour_gradient2(low = "#001DFF",
                                    mid = "#800F80",
                                    high = "#FF0000",
                                    midpoint = labels_seq[2],
                                    breaks = labels_seq,
                                    labels = labels_seq,
                                    limits = c(min(pred_in_us$pred) - 0.01, max(pred_in_us$pred) + 0.01)) +
    ggplot2::theme_bw() +
    ggplot2::coord_sf(xlim        = c(-125, -67),
                      ylim        = c(24, 50),
                      default_crs = sf::st_crs(4326)) +
    ggplot2::labs(color = 'PMMoV (log10 gc/g dry wt)',
                  y     = 'Latitude',
                  x     = 'Longitude',
                  shape = 'Sewer Type') +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position  = 'right',
                   text             = ggplot2::element_text(size = 10),
                   aspect.ratio     = 6/10)

  ## get just california sites
  ca_counties <- c('alameda', 'napa', 'santa clara', 'contra costa',
                   'san francisco', 'solano', 'marin', 'san mateo',
                   'sonoma', 'yolo', 'sacramento', 'san joaquin',
                   'santa clara', 'santa cruz', 'stanislaus', 'santa cruz',
                   'merced')
  ca_zips <- c()
  for (county in ca_counties) {
    ca_zips <- c(ca_zips,
                 zipcodeR::search_county(county, 'CA', similar = TRUE)$zipcode)
  }
  ca_sites <- site_map_df %>%
    dplyr::filter(zipcode %in% ca_zips)
  ca_outline_data <- ggplot2::map_data("county") %>%
    dplyr::filter(region == 'california') %>%
    dplyr::filter(subregion %in% ca_counties)
  ca_site_map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data      = ca_outline_data,
                          mapping   = ggplot2::aes(x     = long,
                                                   y     = lat,
                                                   group = group),
                          colour    = "black",
                          alpha     = 0.5,
                          fill      = '#EAD7A4',
                          linewidth = 0.2) +
    ggplot2:: geom_point(data    = ca_sites,
                         mapping = ggplot2::aes(x     = lng,
                                                y     = lat,
                                                shape = sewer,
                                                alpha = 0.95,
                                                group = 1),
                         fill    = 'grey50') +
    ggplot2::scale_shape_manual(values = c(24, 21), guide = 'none') +
    ggplot2::scale_size_manual(values = c(3.5, 2), guide = 'none') +
    ggplot2::scale_alpha(guide = 'none') +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Longitude', y = 'Latitude') +
    ggplot2::coord_sf() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_rect(fill  = 'transparent',
                                                            color = NA),
                   panel.background = ggplot2::element_rect(fill  = 'transparent',
                                                            color = NA),
                   axis.ticks       = ggplot2::element_blank(),
                   text             = ggplot2::element_blank(),
                   plot.margin      = grid::unit(c(0, 0, 0, 0), "mm"),
                   aspect.ratio     = 7/8)

  grid_ab <- cowplot::ggdraw() +
    cowplot::draw_plot(site_map) +
    cowplot::draw_plot(ca_site_map, x = -0.35, y = -0.27, scale = 0.25)
  ggplot2::ggsave(grid_ab, filename = file.path(out_path, 'figure_gradient.pdf'), height = 5, width = 10)
}
