library(dplyr)
library(tidyr)
# Otolith summary
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")

# Recreational
ca_oto = read.csv(file.path("data", "ca_recreational_samples_otoliths_2022.csv"))
or_oto = read.csv(file.path("data", "OR_recreational_samples.csv"))
wa_oto = read.csv(file.path("data", "wa_recreational_samples_otoliths.csv"))

# Recreational data
ca_rec = read.csv(file.path("data", "SD501-CALIFORNIA-1983---2020.csv"))
or_rec = read.csv(file.path("data", "SD501-OREGON-1983---2020.csv"))
wa_rec = read.csv(file.path("data", "SD501-WASHINGTON-1983---2020.csv"))

# The samples in RecFIN do not match those provided directly from WDFW
wa_rec = wa_oto

species = read.csv(file.path("data", "species_names.csv"))
species_list = c(species[,1], species[,2], species[,3], species[,4])
species_list = species_list[species_list != -99]

or_oto$aged <- or_oto$Aged 
or_oto$otoliths <- or_oto$Available
or_oto$SPECIES_NAME <- or_oto$Species
or_oto$RECFIN_YEAR <- or_oto$Year

ca_oto$aged <- 0
ca_oto$otoliths <- ca_oto$Samples
ca_oto$SPECIES_NAME <- toupper(ca_oto$Species)
ca_oto$RECFIN_YEAR <- ca_oto$Year

filter_species <- function(data){
	key <- NULL
	for(a in 1:length(species_list)){
		key <- c(key, grep(species_list[a], data, ignore.case = TRUE))
	}
	return(key)
}

summarize_rec <- function(data){
	summary <-  
		data %>%
		group_by(SPECIES_NAME, RECFIN_YEAR) %>%
		summarise(
			sexed_fish = sum(FISH_SEX %in% c("M", "F", 1, 2)),
			unsexed_fish = sum(FISH_SEX %in% c(8, 3, "U")),
			lengthed = sum(!is.na(RECFIN_LENGTH_MM))#,
			#aged = 0,
			#otoliths = 0
		) 

	key <- filter_species(data = summary$SPECIES_NAME)

	return(summary[key,])
}

or_rec_summ <- summarize_rec(data = or_rec)
ca_rec_summ <- summarize_rec(data = ca_rec)
#wa_rec_summ <- summarize_rec(data = wa_rec)

wa_rec$unsexed <- wa_rec$CountOffish_length - wa_rec$CountOfsex_code
wa_rec$unaged <- wa_rec$CountOfage_structure - wa_rec$CountOfbest_age
wa_rec_summ <-  
		wa_rec %>%
		group_by(species_name, sample_year) %>%
		summarise(
			state = "W",
			sexed_fish = sum(CountOfsex_code),
			unsexed_fish = sum(unsexed),
			lengthed = sum(CountOffish_length),
			aged = sum(CountOfbest_age),
			otoliths = sum(unaged)
		)

key <- filter_species(data = wa_rec_summ$species_name)
wa_all <- wa_rec_summ[key,]


keep = c("SPECIES_NAME", "RECFIN_YEAR", "sexed_fish", "unsexed_fish", "lengthed", "aged", "otoliths")
or_all = dplyr::left_join(or_rec_summ, or_oto)
or_all = or_all[, colnames(or_all) %in% keep]
or_all[is.na(or_all)] = 0
or_all$state = "O"
rename = c("SPECIES_NAME", "RECFIN_YEAR")
colnames(or_all)[colnames(or_all) %in% rename] = c("species_name", "sample_year")

ca_all = dplyr::left_join(ca_rec_summ, ca_oto)
ca_all = ca_all[, colnames(ca_all) %in% keep]
ca_all[is.na(ca_all)] = 0
ca_all$state = "C"
colnames(ca_all)[colnames(ca_all) %in% rename] = c("species_name", "sample_year")

write.csv(ca_all, file.path("tables", "recreational_ca.csv"), row.names = FALSE)
write.csv(or_all, file.path("tables", "recreational_or.csv"), row.names = FALSE)
write.csv(wa_all, file.path("tables", "recreational_wa.csv"), row.names = FALSE)


# combine all together in a standardized format
combine <- rbind(ca_all, or_all, wa_all)
combine$common_name <- tolower(combine$species_name)
combine$positive_sites <- NA
combine$data_type <- "recreational"

# put columns in the right order
out <- 
	combine[, c("common_name", "state", "sample_year", "positive_sites", "sexed_fish", "unsexed_fish", 
	"lengthed",  "aged", "otoliths", 'data_type')]

save(out, file = paste0("data/summarized_recreational_data.rdat"))