# Set-up to run VAST for survey species
#
# requires library(VASTWestCoast)
#
# @ dir directory to save the results
# @ sci_name the scientific species name to run the vast model on
# @ dist distribution for the model
# @ strat stratificition to calculate area based indices (ca_or, ca, wa, wa_or, north_west, coast) 
#
# Written by Chantel Wetzel
#
do_vast_fn <- function(dir, common_name, dist, strat){

  if(strat == "ca_or"){
    strata.limits = data.frame('STRATA' = c("ca_or", "ca", "or"), 
                                'south_border'   = c(32.0, 32.0, 42.0),
                                'north_border'   = c(46.0, 42.0, 46.0), 
                                'shallow_border' = c(  55,   55,   55), 
                                'deep_border'    = c(1280, 1280, 1280) )
  }
  
  if(strat == "ca"){
    strata.limits = data.frame('STRATA' = c("ca"), 
                              'south_border'   = c(32.0),
                              'north_border'   = c(42.0), 
                              'shallow_border' = c(  55), 
                              'deep_border'    = c(1280) )
  }

  if(strat == "wa"){
    strata.limits = data.frame('STRATA' = c("wa"), 
                              'south_border'   = c(46.0),
                              'north_border'   = c(49.0), 
                              'shallow_border' = c(  55), 
                              'deep_border'    = c(1280) )
  }
  
  if(strat == "wa_or"){
    strata.limits = data.frame('STRATA' = c("wa_or", "wa", "or"), 
                              'south_border'   = c(42.0, 46.0, 42.0),
                              'north_border'   = c(49.0, 49.0, 46.0), 
                              'shallow_border' = c(  55,   55,   55), 
                              'deep_border'    = c(1280,  183, 1280) )
  }

  if(strat == "north_south"){
    strata.limits = data.frame( 'STRATA' = c("coastwide", "south",  "north"), 
                                'north_border'   = c(49.0, 40.1667,    49.0), 
                                'south_border'   = c(32.0,    32.0, 40.1667),
                                'shallow_border' = c(  55,      55,      55), 
                                'deep_border'    = c(1280,    1280,    1280) )
  }
  
  if(strat == "coast"){
      strata.limits = data.frame( 'STRATA' = c("coastwide", "ca",  "or", "wa"), 
                              'north_border'   = c(49.0, 42.0, 46.0, 49.0), 
                              'south_border'   = c(32.0, 32.0, 42.0, 46.0),
                              'shallow_border' = c(  55,   55,   55,   55), 
                              'deep_border'    = c(1280, 1280, 1280, 1280) )
  }

  if(strat == "sablefish"){
      strata.limits = data.frame( 'STRATA' = c("coastwide", "south",  "north"), 
                              'north_border'   = c(49.0, 36.0, 49.0), 
                              'south_border'   = c(32.0, 32.0, 36.0),
                              'shallow_border' = c(  55,   55,   55), 
                              'deep_border'    = c(1280, 1280, 1280) )
  }

  #strata.limits = nwfscSurvey::GetStrata.fn(area = strat)
 
  fine_scale = TRUE 
  if (dist == "lognormal"){ 
    obs_model = c(1,0)
    rho_config = NULL
    field = NULL
  }
  if (dist == "gamma")    { 
    obs_model = c(2,0)
    rho_config = NULL
    field = NULL
  }
  if (dist == "tweedie")  { 
    obs_model = c(10, 2)
    rho_config = c(Beta1 = 3, Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)
    field = c(Omega1 = 0, Epsilon1 = 0, Omega2 = "IID", Epsilon2 = "IID")
  }
  #field = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 0)

  #if (common_name %in% c("longspine thornyhead", "rougheye rockfish")){
  #  field = c(Omega1 = 0, Epsilon1 = 0, Omega2 = "IID", Epsilon2 = 0) 
  #}

  data = nwfscSurvey::PullCatch.fn(Name = common_name, 
    SurveyName = "NWFSC.Combo",
    SaveFile = TRUE, Dir = dir)

  #data = nwfscSurvey::PullCatch.fn(Name =  c(common_name, "blackspotted rockfish", 
  #  "rougheye and blackspotted rockfish"), 
  #  SurveyName = "NWFSC.Combo",
  #  SaveFile = TRUE, Dir = dir)

  #d <-  
  #  data %>%
  #  group_by(Trawl_id) %>%
  #  summarise(
  #    Year = unique(Year),
  #    Common_name = "rougheye rockfish",
  #    Project = unique(Project), 
  #    Pass = unique(Pass),       
  #    Vessel = unique(Vessel), 
  #    Tow = unique(Tow),        
  #    Date = unique(Date),
  #    Area_Swept_ha = unique(Area_Swept_ha),
  #    total_catch_numbers = sum(total_catch_numbers),
  #    total_catch_wt_kg = sum(total_catch_wt_kg),
  #    Latitude_dd = unique(Latitude_dd),
  #    Longitude_dd = unique(Longitude_dd),
  #    Depth_m = unique(Depth_m),
  #    Subsample_count = sum(Subsample_count),
  #    Subsample_wt_kg = sum(Subsample_wt_kg),
  #    cpue_kg_km2 = 100 * sum(total_catch_wt_kg)/unique(Area_Swept_ha)
  #  )

  #data = data.frame(d)

  #if (common_name %in% c("rougheye rockfish")){
  #  keep = which(data$Common_name == common_name)
  #  data = data[keep, ]
  #  # rougheye tow
  #  #data = data[data$Trawl_id != 202103010021, ]
  #  # longspine_thornyhead
  #  # data = data[data$Trawl_id != 202103010077, ]
  #}

  # petrale sole 2021 giant tow
  #remove = which(data$Trawl_id == 202103017117)

  settings <- list("Species" = paste0("WCGBTS_", sci_name), 
                   "ObsModelcondition" = obs_model,
                   "nknots" = 500,
                   "strata" = strata.limits,
                   "Passcondition" = TRUE,
                   "rho" = rho_config,
                   "field" = field,
                   overdispersion = c(eta1 = 0, eta2 = 0), #<-- fixes the L_eta1_z issue below
                   #"overdisperion" = overdisperion,
                   # use_anistotropy turned off in VAST_do
                   #use_anisotropy = FALSE, # <-- ln_h is related to anisotropy
                   fine_scale = fine_scale)

  test <- VASTWestCoast::VAST_condition(conditiondir = dir,
                         settings = settings, 
                         spp = settings$Species,
                         data = data) 

}

