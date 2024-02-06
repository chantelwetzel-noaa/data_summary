library(dplyr)
library(indexwc)

#remotes::install_github("pfmc-assessments/indexwc@sap-indices")
utm_zone_10 <- 32610
source("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc/data-raw/grid_nwfscSurvey.R")

#data <- nwfscSurvey::pull_catch(
#  common_name = "Pacific ocean perch",
#  survey = "NWFSC.Combo"
#)
#pulled_catch <- data

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/indices")

load("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/wcgbt_catch_filtered.Rdata")

wcgbt_species <- nwfscSurvey::GetSpp.fn(species = nwfscSurvey::GetSppDefault.fn())
species_to_remove <- c("copper rockfish", "quillback rockfish", "brown rockfish", "squarespot rockfish", "bank rockfish")
wcgbt_species <- wcgbt_species[which(!wcgbt_species$common %in% species_to_remove), ]
wcgbt_species[which(wcgbt_species$common == "rougheye rockfish"), "common"] <- "rougheye and blackspotted rockfish"
species_to_run <- unique(wcgbt_species[which(wcgbt_species[, "common"] %in% catch$Common_name), "common"])


for (sp in species_to_run){

  species_data <- catch[catch$Common_name == sp, ]
  data <- format_data(data = species_data)

  # Run a single species
  fit <- indexwc::run(
    data = data,
    family = sdmTMB::delta_lognormal(),
    formula = catch_weight ~ 0 + fyear + pass_scaled
  )

  sp_out <- gsub(" ", "_", sp)
  sp_out <- gsub(" ", "_", sp_out)
  out_dir <- file.path(getwd(), tolower(sp_out), "wcgbts", "delta_lognormal",
                        tolower(sp_out), "wcgbts", "delta_lognormal", "index")
  write.csv(rbind(c("pos_def_hessian", fit[[1]]$pos_def_hessian),
                  c("bad_eig", fit[[1]]$bad_eig)),
            file = file.path(out_dir, paste0(sp_out, "_pos_def_hessian_", fit[[1]]$pos_def_hessian, ".csv")))
  
}

# Species with non-positive definite hessians lognormal year + pass


# The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
# which was just loaded, will retire in October 2023.
# Please refer to R-spatial evolution reports for details, especially
# https://r-spatial.org/r/2023/05/15/evolution4.html.
# It may be desirable to make the sf package available;
# package maintainers should consider adding sf to Suggests:.
# The sp package is now running under evolution status 2
# (status 2 uses the sf package in place of rgdal)

