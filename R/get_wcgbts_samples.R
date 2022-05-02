
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")
library(dplyr)
library(tidyr)
load(file.path('data-raw', 'bio_wcgbt_filtered.Rda'))
source("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/R/all_species.R")
data <- sub_bio
species <- unlist(all_species())
#get_wcgbt_samples(data = data, species = species)

get_wcgbt_samples <- function(data, species){
	# table of year, pos tows, lengths, otoliths by species
	dir <- here::here("data")

	data$unaged_fish <-0
	use <- which(!is.na(data$Oto_id) & is.na(data$Age))
	data$unaged_fish[use] <- 1 

	# set any NA sexed fish to U
	data[is.na(data$Sex), "Sex"] = "U"

	wcgbts_summary <-  
		data %>%
		group_by(Common_name, Year) %>%
		summarise(
			positive_sites = length(unique(Trawl_id)),
			sexed_fish = sum(Sex %in% c("M", "F") & !is.na(Length_cm)),
			unsexed_fish = sum(Sex == "U" & !is.na(Length_cm)),
			lengthed = sum(!is.na(Length_cm)),
			aged = sum(!is.na(Age)),
			otoliths = sum(unaged_fish),
			data_type = 'nwfsc_wcgbt'
		) 

	wcgbts_summary$state = NA
	wcgbts_summary$Common_name = tolower(wcgbts_summary$Common_name)
	colnames(wcgbts_summary)[1:2] = c("common_name", "sample_year")

	#          common_name state sample_year positive_sites sexed_fish unsexed_fish lengthed aged otoliths   data_type otoliths.y                 name
	#          chilipepper  <NA>        2006             70       2630           NA     2696  611        0 NWFSC WCGBT         NA Chilipepper rockfish
	# longspine thornyhead  <NA>        2004            151       2790           NA    11173    0      742 NWFSC WCGBT         NA Longspine thornyhead
	# longspine thornyhead  <NA>        2008            261       2775           NA     3635    0      989 NWFSC WCGBT         NA Longspine thornyhead
	# longspine thornyhead  <NA>        2011            254       4074           NA     5188    0     1002 NWFSC WCGBT         NA Longspine thornyhead
	#   splitnose rockfish  <NA>        2007            158       2775           NA     3363  495      389 NWFSC WCGBT         NA   Splitnose rockfish
	#   splitnose rockfish  <NA>        2008            153       2539           NA     2918  488      342 NWFSC WCGBT         NA   Splitnose rockfish
	
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
	save(out,  file = file.path(getwd(), "data", "summarized_wcgbt_data.rdat"))
	#write.csv(wcgbts_positive_tows, file.path(dir, "wcgbts_positive_tows.csv"), row.names = FALSE)

	return(out)

}