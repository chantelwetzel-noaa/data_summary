#
# Create strata for the survey data design-based estimates
# and composition data expansion.
# requires nwfscSurvey package
#
# @ areas the strata selected
#
# writen by Chantel Wetzel
#
create_strata_fn <- function(area = NULL){
	if (area == "ca"){
	strata = CreateStrataDF.fn(names=c("shallow_ca", "deep_ca"), 
                           depths.shallow = c( 55,  183),
                           depths.deep    = c(183,  549),
                           lats.south     = c(32.0, 32.0),
                           lats.north     = c(42.0, 42.0))
	}

	if (area == "wa"){
	strata = CreateStrataDF.fn(names=c("shallow_wa",  "deep_wa"), 
                           depths.shallow = c( 55,  183),
                           depths.deep    = c(183,  549),
                           lats.south     = c(46.0, 46.0),
                           lats.north     = c(49.0, 49.0))
	}

	if (area == "wa_or"){
	strata = CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "deep_wa", "deep_or"), 
                           depths.shallow = c( 55,   55, 183, 183),
                           depths.deep    = c(183,  183, 549, 549),
                           lats.south     = c(46.0, 42.0, 46.0, 42.0),
                           lats.north     = c(49.0, 46.0, 49.0, 46.0))
	}


	if (area == "coast"){
	strata = CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "shallow_ca",  
                           			   "deep_wa", "deep_or", "deep_ca"), 
                           depths.shallow = c( 55,   55,   55,  183,  183, 183),
                           depths.deep    = c(183,  183,  183,  549,  549, 549),
                           lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                           lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
	}

	if(area == "north_south"){
	strata = CreateStrataDF.fn(names=c("shallow_north", "shallow_south"), 
                           depths.shallow = c( 55,   55),
                           depths.deep    = c(300,  300),
                           lats.south     = c(40.5, 32.0),
                           lats.north     = c(49.0, 40.5))
	}

	if(area == "coast_deep"){
	strata = CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "shallow_ca", 
									   "medium_wa", "medium_or", "medium_ca", 
                           			   "deep_wa", "deep_or", "deep_ca"), 
                           depths.shallow = c( 55,   55,   55,  183,  183, 183, 549,  549, 549),
                           depths.deep    = c(183,  183,  183,  549,  549, 549, 1280, 1280, 1280),
                           lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                           lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
	}
	
	return(strata)
}