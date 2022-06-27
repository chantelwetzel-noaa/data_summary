
wd = "C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary"
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/VASTWestCoast")
setwd(wd)

#library(nwfscSurvey)
#library(VASTWestCoast)
#library(rgeos)
#library(sdmTMB)

species = GetSppDefault.fn()
species_areas = GetSpp.fn(species)
#species = c('rosethorn_rockfish', 'stripetail_rockfish', 'curlfin_sole')
#find = which(species_areas$input %in% species)
#species_areas = species_areas[find, ]

for (s in species){
	dir = file.path(wd, "vast", s)
	dir.create(dir)
	strat = species_areas[species_areas$input == s, "strata"]
	sci_name = species_areas[species_areas$input == s, "scientific_name"]
	common_name = species_areas[species_areas$common_name == s, "common"]	
	do_vast_fn(dir = dir, common_name = common_name, dist = "tweedie", strat = strat)
}

for (s in species){
	dir = file.path(wd, "vast", s)
	common_name = species_areas[species_areas$input == s, "common"]	
	create_survey_comps(
		dir = dir, 
		species = s, 
		common_name = common_name,
		load_catch = TRUE,
		load_bio = TRUE)
}

# redo the index plots
for (s in species){
	dir = file.path(wd, "vast", s)
	common_name = species_areas[species_areas$input == s, "common"]	

	fileindex <- file.path(dir, "Table_for_SS3.csv")
	if (file.exists(fileindex)){
		index = read.csv(fileindex)
		#write.csv(index, file = file.path(dir, "Table_for_SS3_master.csv"))
		indexdata <- data.frame(
   			Year = index[, "Year"],
   			Unit = 'mt',
   			Fleet = index[, "Fleet"],
   			Estimate_metric_tons = index[, "Estimate_metric_tons"] / 1000,
   			SD_mt = index[, "SD_mt"] / 1000,
   			SD_log = index[, "SD_log"]
   		)
	} else {
		index = read.csv(file.path(dir, "Index.csv"))
		#write.csv(index, file = file.path(dir, "Index_master.csv"))
		indexdata <- data.frame(
   		Year = index[, "Time"],
   		Unit = 'mt',
   		Fleet = index[, "Stratum"],
   		Estimate_metric_tons = index[, "Estimate"] / 1000,
   		SD_log = index[, "Std..Error.for.Estimate"] / 1000,
   		SD_mt = index[, "Std..Error.for.ln.Estimate."]
   	)
   }
   if (c("north") %in% indexdata$Fleet){
    	if (s == "sablefish"){
    		indexdata$Fleet[indexdata$Fleet == "north"] = "North 36"
    		indexdata$Fleet[indexdata$Fleet == "south"] = "South 36"
    	} else {
    		indexdata$Fleet[indexdata$Fleet == "north"] = "North 40 10"
    		indexdata$Fleet[indexdata$Fleet == "south"] = "South 40 10"
    	}

    }
    write.csv(indexdata, file = file.path(dir, "Table_for_SS3.csv"))

	plot_ss(file.in = fileindex,
    	lab.survey = "NWFSC.Combo",
    	lab.spp = common_name)

}

# redo the index plots
species = species_areas[species_areas$strata %in% c("north_south", "sablefish"), "input"]
for (s in species){
	dir = file.path(wd, "vast", s)
	common_name = species_areas[species_areas$input == s, "common"]	

	fileindex <- file.path(dir, "Table_for_SS3.csv")
	indexdata = read.csv(fileindex)
    if (c("north") %in% indexdata$Fleet){
    	if (s == "sablefish"){
    		indexdata$Fleet[indexdata$Fleet == "north"] = "North 36"
    		indexdata$Fleet[indexdata$Fleet == "south"] = "South 36"
    	} else {
    		indexdata$Fleet[indexdata$Fleet == "north"] = "North 40 10"
    		indexdata$Fleet[indexdata$Fleet == "south"] = "South 40 10"
    	}

    }
    write.csv(indexdata, file = file.path(dir, "Table_for_SS3.csv"))

	plot_ss(file.in = fileindex,
    	lab.survey = "NWFSC.Combo",
    	lab.spp = common_name)

}