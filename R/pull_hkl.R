

pull_hkl <- function(
	file = "qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version_04042021.csv", 
	return = TRUE, load = FALSE){
	
	species_names <- unlist(all_species())
	dir <- here::here("data-raw")

	if (!load) {
		cols <- c("common_name", "year", "site_number", "set_id", "sex", "otolith_number", "age_years",
					"length_cm", "weight_kg")

		hkl <- read.csv(file.path(dir, file = file))
		hkl_data <- hkl[,cols]
	
		keep <- which(tolower(hkl_data$common_name) %in% tolower(species_names))
		sub_hkl <- hkl_data[keep, ]

		find = which(sub_hkl$otolith_number == "")
		sub_hkl[find,"otolith_number"] = NA
	
		#save(hkl_data, file = file.path(dir, "hkl_all.Rda"))
		save(sub_hkl, file = file.path(dir, "hkl_filtered.Rdata"))		
	}

	if (load) {
		load(file.path(dir, "hkl_all.Rda"))
		load(file.path(dir, "hkl_filtered.Rda"))
	}

	if(return){
		hkl <- sub_hkl
		return(hkl)
	}
}

	