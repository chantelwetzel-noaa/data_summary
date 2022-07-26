library(dplyr)
library(tidyr)
# Otolith summary
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")

# Note: WDFW and ODFW each sent separate spreadsheets reflecting the number of
# recreational aged and otoliths collected. According to RecFIN, there are no
# recreational samples with ages for California. Need to reach out to CDFW in 
# the 2024 data summary process to see if this is still correct. 

# Species list =============================================================
species <- unlist(all_species())

# Filter function ==========================================================
filter_species <- function(data, species_list){
	key <- NULL
	for(a in 1:length(species_list)){
		key <- c(key, grep(species_list[a], data, ignore.case = TRUE))
	}
	return(key)
}
#==============================================================================

# Recreational
ca_oto = read.csv(file.path("data", "ca_recreational_samples_otoliths_2022.csv"))
or_oto = read.csv(file.path("data", "OR_recreational_samples.csv"))
wa_oto = read.csv(file.path("data", "wa_recreational_samples_otoliths.csv"))

# Recreational data
ca_rec = read.csv(file.path("data-raw", "SD501-CALIFORNIA-1983---2020.csv"))
or_rec = read.csv(file.path("data-raw", "SD501-OREGON-1983---2020.csv"))
wa_rec = read.csv(file.path("data-raw", "SD501-WASHINGTON-1983---2020.csv"))

# 2020 - 2021 data updates 
new_data = read.csv(file.path("data-raw", "SD501--2020---2021.csv"))

# add to existing files
remove = which(ca_rec$RECFIN_YEAR >= 2020)
ca_rec = rbind(ca_rec[-remove, ], 
	new_data[new_data$STATE_NAME == "CALIFORNIA", ])
remove = which(or_rec$RECFIN_YEAR >= 2020)
or_rec = rbind(or_rec[-remove, ], 
	new_data[new_data$STATE_NAME == "OREGON", ])

or_rec = combine_species(data = or_rec, name = "SPECIES_NAME")
ca_rec = combine_species(data = ca_rec, name = "SPECIES_NAME")

# The samples in RecFIN do not match those provided directly from WDFW
wa_rec = wa_oto
wa_rec = combine_species(data = wa_rec, name = "species_name")

# Filter species
species = unlist(all_species()) #read.csv(file.path("data", "species_names.csv"))
find = filter_species(data = or_rec$SPECIES_NAME, species)
or_rec = or_rec[find, ]
find = filter_species(data = ca_rec$SPECIES_NAME, species)
ca_rec = ca_rec[find, ]
find = filter_species(data = wa_rec$species_name, species)
wa_rec = wa_rec[find, ]

# ================================================================
or_oto$aged <- or_oto$Aged 
or_oto$otoliths <- or_oto$Available
or_oto$SPECIES_NAME <- or_oto$Species
or_oto$RECFIN_YEAR <- or_oto$Year
or_oto = combine_species(data = or_oto, name = "SPECIES_NAME")

or_oto_sum <-  
		or_oto %>%
		group_by(SPECIES_NAME, RECFIN_YEAR) %>%
		summarise(
			state = "O",
			aged = sum(aged),
			otoliths = sum(otoliths)
		)

# ===============================================================================
ca_oto$aged <- 0
ca_oto$otoliths <- ca_oto$Samples
ca_oto$SPECIES_NAME <- toupper(ca_oto$Species)
ca_oto$RECFIN_YEAR <- ca_oto$Year
ca_oto = combine_species(data = ca_oto, name = "Species")

ca_oto_sum <-  
		ca_oto %>%
		group_by(SPECIES_NAME, RECFIN_YEAR) %>%
		summarise(
			state = "C",
			aged = sum(aged),
			otoliths = sum(otoliths)
		)


summarize_rec <- function(data){
	summary <-  
		data %>%
		group_by(SPECIES_NAME, RECFIN_YEAR) %>%
		summarise(
			sexed_fish = sum(FISH_SEX %in% c("M", "F", 1, 2) & !is.na(RECFIN_LENGTH_MM)),
			unsexed_fish = sum(FISH_SEX %in% c(8, 3, "U") & !is.na(RECFIN_LENGTH_MM)),
			lengthed = sum(!is.na(RECFIN_LENGTH_MM))#,
		) 
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

keep = c("SPECIES_NAME", "RECFIN_YEAR", "sexed_fish", "unsexed_fish", "lengthed", "aged", "otoliths")
or_all = dplyr::left_join(or_rec_summ, or_oto_sum)
or_all = or_all[, colnames(or_all) %in% keep]
or_all[is.na(or_all)] = 0
or_all$state = "O"
rename = c("SPECIES_NAME", "RECFIN_YEAR")
colnames(or_all)[colnames(or_all) %in% rename] = c("species_name", "sample_year")

ca_rec_summ$SPECIES_NAME = tolower(ca_rec_summ$SPECIES_NAME)
ca_oto_sum$SPECIES_NAME	= tolower(ca_oto_sum$SPECIES_NAME)
ca_all = dplyr::left_join(ca_rec_summ, ca_oto_sum)
ca_all = ca_all[, colnames(ca_all) %in% keep]
ca_all[is.na(ca_all)] = 0
ca_all$state = "C"
colnames(ca_all)[colnames(ca_all) %in% rename] = c("species_name", "sample_year")

wa_all = wa_rec_summ

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