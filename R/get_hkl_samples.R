
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")
library(dplyr)
library(tidyr)
load(file.path(getwd(), "data", "hkl_filtered.Rdata"))
data <- sub_hkl

get_hkl_samples <- function(data){
	# table of year, pos tows, lengths, otoliths by species
	dir <- here::here("data")

	data$unaged_fish  <- 0
	use <- which(!is.na(data$otolith_number) & is.na(data$age_years))
	data$unaged_fish[use] <- 1 

	data[data$common_name == "Vermilion Rockfish", "common_name"] = "Vermilion and Sunset Rockfish"
	data[data$common_name == "Blue Rockfish", "common_name"] = "Blue and Deacon Rockfish"

	hkl_summary <-  
		data %>%
		group_by(common_name, year) %>%
		summarise(
			positive_sites = length(unique(set_id)),
			sexed_fish = sum(sex %in% c("M", "F") & !is.na(length_cm)),
			unsexed_fish = sum(sex == "U" & !is.na(length_cm)),
			lengthed = sum(!is.na(length_cm)),
			aged = sum(!is.na(age_years)),
			otoliths = sum(unaged_fish)
		) 

	hkl_summary$common_name <- tolower(hkl_summary$common_name)
	hkl_summary$data_type <- "nwfsc_hkl"
	hkl_summary$state <- "C"
	colnames(hkl_summary)[2] <- 'sample_year'

	out <- hkl_summary[,
		c('common_name', 'state', 'sample_year', 'positive_sites', 'sexed_fish', 'unsexed_fish',
		  'lengthed', 'aged', 'otoliths', 'data_type')] 

	write.csv(out, file.path(dir, "hkl_summary.csv"), row.names = FALSE)
	save(out,  file = file.path(getwd(), "data", "summarized_hkl_data.rdat"))
	return(out)

}