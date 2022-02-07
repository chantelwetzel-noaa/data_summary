
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")
library(dplyr)
library(tidyr)
load(file.path('data', 'bio_wcgbt_filtered.Rda'))
data <- sub_bio
species <- unlist(all_species())

get_wcgbt_samples <- function(data, species){
	# table of year, pos tows, lengths, otoliths by species
	dir <- here::here("data")

	data$unaged_fish <-0
	use <- which(!is.na(data$Oto_id) & is.na(data$Age))
	data$unaged_fish[use] <- 1 

	wcgbts_summary <-  
		data %>%
		group_by(Common_name, Year) %>%
		summarise(
			positive_sites = length(unique(Trawl_id)),
			sexed_fish = sum(Sex %in% c("M", "F")),
			unsexed_fish = sum(Sex == "U"),
			lengthed = sum(!is.na(Length_cm)),
			aged = sum(!is.na(Age)),
			otoliths = sum(unaged_fish),
			data_type = 'nwfsc_wcgbt'
		) 

	wcgbts_summary$state = NA
	wcgbts_summary$Common_name = tolower(wcgbts_summary$Common_name)
	colnames(wcgbts_summary)[1:2] = c("common_name", "sample_year")
	# there are only 7 tows total across and for 3 species that did not have
	# biological samples collected that year. The species and years where there 
	# was a positive tow but not biological samples are:
	# species	        year    positive tows	
	# speckled rockfish	2008	1
	# starry flounder	2003	5
	# starry rockfish	2014	1	
	# however, there are a years where some tows are sampled and others not
	# example: arrowtooth flounder bio data collected from 191 tows but 
	# there were 193 tows that encountered af	
	# wcgbts_positive_tows <- 
	# 	data$catch %>%
	# 	group_by(Common_name, Year) %>%
	# 	summarise(
	# 		positive_tow = sum(positive_tow)
	# 	) %>%
	# 	filter(positive_tow > 0)

	out <- wcgbts_summary[,
		c('common_name', 'state', 'sample_year', 'positive_sites', 'sexed_fish', 'unsexed_fish',
		  'lengthed', 'aged', 'otoliths', 'data_type')] 

	write.csv(out, file.path(dir, "wcgbts_summary.csv"), row.names = FALSE)
	save(out,  file = paste0("data/summarized_wcgbt_data.rdat"))
	#write.csv(wcgbts_positive_tows, file.path(dir, "wcgbts_positive_tows.csv"), row.names = FALSE)

	return(out)

}