# TODO list
# * Only pull data once per species and then combine the pull results with the
#   configuration matrix again
# * Fix how format_data returns an object without the nwfscSurvey class instead
#   it is class(data) > [1] "tbl_df" "tbl" "data.frame"
# * for vessel_year, might want a different level scaling things might not have
#   to give this to grid
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc")
load("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/wcgbt_catch_filtered.Rdata")

#library(indexwc)
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc")

# redbanded singular
# rerun greenstripe with increased knots (existing was a 100 test run)
# rosethorn turn off the spatiotemporal for model 1
# sharpchin change to lognormal or gamma dist
# splitnose turn off the spatiotemporal for model 1

to_run <- c(
  #"aurora rockfish"
  #"blackgill rockfish",
  #"darkblotched rockfish"
  #"greenstriped rockfish",
  #"redbanded rockfish" 
  #"rosethorn rockfish",
  #"sharpchin rockfish",
  #"splitnose rockfish",
  #"stripetail rockfish",
  #"widow rockfish",
  #"yellowtail rockfish",
  #"longspine thornyhead",
  #"Pacific sanddab"
  "yelloweye rockfish"
  #"shortspine thornyhead"
  #"lingcod"
)

for (i in 1:length(to_run)){

species_data <- catch[which(catch$Common_name %in% to_run[i]), ]
configuration <- tibble::as_tibble(read.csv(
  file.path("data-raw", "configuration.csv")
))

configuration <- configuration[which(configuration$species %in% to_run[i])[2], ]

data <-configuration %>%
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() %>%
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(format_data(data = species_data)),
    data_filtered = list(data_raw %>%
                           dplyr::filter(
                             depth <= min_depth, depth >= max_depth,
                             latitude >= min_latitude, latitude <= max_latitude,
                             year >= min_year, year <= max_year
                           ))
  ) %>%
  dplyr::ungroup()

best <- data %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy,
        n_knots = knots,
        spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list)
      ),
      .f = indexwc::run_sdmtmb
    )
  )

} # end of species loop


keep <- which(species_data$Depth_m < 450 & species_data$Latitude_dd < 40.167)
data_filtered <- format_data(species_data[keep, ])

run_sdmtmb(
  dir_main = getwd(),
  data = data_filtered,
  family = delta_gamma(),
  formula = configuration$formula,
  knots = configuration$knots,
  anisotropy = configuration$anisotropy, 
  spatiotemporal = "iid"
  )

index <- calc_index_areas(
  data = data_truncated,
  fit = fit,
  prediction_grid = grid,
  dir = dir_index
)


setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc/lingcod/wcgbts/delta_gamma/index")
load("sdmTMB_fit.RData")
index <- calc_index_areas(
  data, fit, grid, 
  dir = getwd()
)

index <- calc_index_areas(
  data, fit, grid, 
  dir = getwd(), 
  boundaries = boundaries_data[5:6]
)

indices <- dir(
  file.path("canary_rockfish", "wcgbts"),
  pattern = "sdmTMB_save",
  recursive = TRUE,
  full.names = TRUE
) %>%
  unlist() %>%
  purrr::map_df(
    .f = function(x) {
      load(x)
      if (exists("index_areas")) {
        return(data.frame(i = x, index_areas))
      } else {
        return(NULL)
      }
    }
  )
gg <- ggplot2::ggplot(
  data = indices %>%
    dplyr::mutate(
      dist = basename(dirname(dirname(i)))
    ) %>%
    dplyr::filter(area == "coastwide"),
  ggplot2::aes(x = year, y = est, lty = dist, col = dist, group = i)
) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()
ggsave(gg, filename = "indexwc_copper_rockfish.png")


best <- data[2, ] %>%
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy
      ),
      spatiotemporal = list("iid", "off"),
      n_knots = 200,
      .f = indexwc::run_sdmtmb
    )
  )
