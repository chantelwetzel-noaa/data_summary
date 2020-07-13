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
do_vast_fn <- function(dir, sci_name, dist, strat){

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
  
  if (dist == "lognormal"){ obs_model = c(1,0)}
  if (dist == "gamma")    { obs_model = c(2,0)}
  
  Sim_Settings <- list( "Species" = paste0("WCGBTS_", sci_name), 
                        "ObsModelcondition" = obs_model,
                        "nknots" = 250,
                        "depth" = c("no", "linear", "squared")[1],
                        "strata" = strata.limits,
                        "version" = "VAST_v5_4_0",
                        "Passcondition" = TRUE
                        )
    
  test <- VAST_condition(conditiondir = dir,
                         settings = Sim_Settings, 
                         spp = Sim_Settings$Species,
                         datadir = dir,
                         overdispersion = NULL)
  
  VAST_diagnostics(dir)
}
