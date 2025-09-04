library(dplyr)
library(indexwc)

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/indices")
load("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/wcgbt_catch_filtered.Rdata")

wcgbt_species <- nwfscSurvey::GetSpp.fn(species = nwfscSurvey::GetSppDefault.fn())
wcgbt_species[which(wcgbt_species$common == "rougheye rockfish"), "common"] <- "rougheye and blackspotted rockfish"
species_to_run <- unique(wcgbt_species[which(wcgbt_species[, "common"] %in% catch$Common_name), "common"])
species_to_run <- c(species_to_run[species_to_run != "lingcod"]
  "lingcod south", "lingcod north")

find <- which(catch$Common_name == "lingcod" & catch$Latitude_dd < 40.17)
catch$Common_name[find] <- "lingcod south"
find <- which(catch$Common_name == "lingcod")
catch$Common_name[find] <- "lingcod north"

remove <- which(catch$Common_name == "yellowtail rockfish" & catch$Latitude_dd < 40.17)
catch <- catch[-remove, ]

for (sp in species_to_run){

  species_data <- catch[catch$Common_name == sp, ]
  data <- format_data(data = species_data)

  # Run a single species
  fit <- indexwc::run_sdmtmb(
    data = data,
    family = sdmTMB::delta_gamma(),
    formula = catch_weight ~ 0 + fyear + pass_scaled,
    n_knots = 250
  )

  sp_out <- gsub(" ", "_", sp)
  sp_out <- gsub(" ", "_", sp_out)
  out_dir <- file.path(getwd(), tolower(sp_out), "wcgbts", "delta_gamma",
                        tolower(sp_out), "wcgbts", "delta_gamma", "index")
  write.csv(rbind(c("pos_def_hessian", fit[[1]]$pos_def_hessian),
                  c("bad_eig", fit[[1]]$bad_eig)),
            file = file.path(out_dir, paste0(sp_out, "_pos_def_hessian_", fit[[1]]$pos_def_hessian, ".csv")))
  
}

# Species with non-positive definite hessians gamma
# canary rockfish catch_weight ~ 0 + fyear + pass_scaled
# darkblotched rockfish catch_weight ~ 0 + fyear + pass_scaled
# greenspotted rockfish
# longspine thornyhead
# Pacific spiny dogfish (converged) but should be rerun with lognormal
# redbanded rockfish
# sharpchin rockfish

# Other issues
# shortspine thornyhead
# ✔ Range parameters don't look unreasonably large
# `anisotropy = FALSE` in `sdmTMB()`; no anisotropy figure is available.
# • Running sdmTMB for shortspine thornyhead
# Error in `purrr::map2()`:
# ℹ In index: 1.
# Caused by error in `solve.default()`:
# ! system is computationally singular: reciprocal condition number = 1.27956e-18
# Run `rlang::last_trace()` to see where the error occurred.


# The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
# which was just loaded, will retire in October 2023.
# Please refer to R-spatial evolution reports for details, especially
# https://r-spatial.org/r/2023/05/15/evolution4.html.
# It may be desirable to make the sf package available;
# package maintainers should consider adding sf to Suggests:.
# The sp package is now running under evolution status 2
# (status 2 uses the sf package in place of rgdal)

