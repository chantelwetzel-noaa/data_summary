
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")

data_dir <- file.path("data")

# Load all of the summarized data sets =====================================
load(file.path('data', 'summarized_wcgbt_data.rdat'))
wcgbt = out
load(file.path('data', 'summarized_hkl_data.rdat'))
hkl = out
load(file.path('data', 'summarized_commercial_data.rdat'))
com = combine
load(file.path('data', 'summarized_recreational_data.rdat'))
rec = out

# Species list =============================================================
species <- unlist(all_species())
pacfin_species <- read.csv(file.path('data', 'pacfin_species_names.csv'))

# Filter function ==========================================================
filter_species <- function(data, species_list){
	key <- NULL
	for(a in 1:length(species_list)){
		key <- c(key, grep(species_list[a], data, ignore.case = TRUE))
	}
	return(key)
}


# Read in state specific otolith data and join that with the commercial data =======
ca_oto <- read.csv(file.path('data', 'ca_commerical_samples_otoliths_2022.csv'))
ca_oto$common_name <- NA
ca_oto$sample_year <- ca_oto$year
all <- sort(unique(ca_oto$species))
for(a in 1:length(all)){
	find = which(all[a] == pacfin_species$pacfin.code)
	ca_oto$common_name[ca_oto$species == all[a]] <- pacfin_species[find,"species"]
}

key <- filter_species(data = ca_oto$common_name, species_list = species)
ca_oto_summarized <- ca_oto[key,] %>%
		group_by(common_name, sample_year) %>%
		summarise(state = "C",
				  otoliths = sum(SumOftotal) )
com_int = left_join(com, ca_oto_summarized, by = c("sample_year", "common_name", "state"))
find = which(!is.na(com_int$otoliths.y))
com_int[find, "otoliths.x"] = com_int[find, "otoliths.y"] 

# Need to check the WA otoliths against the info in PacFIN
wa_oto <- read.csv(file.path('data', 'wa_commercial_samples_otoliths.csv'))
wa_oto$unaged <- wa_oto$CountOfage_structure - wa_oto$CountOfbest_age
wa_oto$common_name = tolower(wa_oto$species_name)
key <- filter_species(data = wa_oto$species_name, species_list = species)
wa_oto_summarized <- wa_oto[key,] %>%
		group_by(common_name, sample_year) %>%
		summarise(state = "W",
				  otoliths = sum(unaged) )
com_final = left_join(com_int, wa_oto_summarized, by = c("sample_year", "common_name", "state"))
find = which(!is.na(com_final$otoliths.y))
com_final[find, "otoliths.x"] = com_final[find, "otoliths.y"] 

colnames(com_final)[colnames(com_final) == "otoliths.x"] = "otoliths"
com_final = com_final[, -ncol(com_final)]

# Bring it all together ============================================================

all_data <- rbind(com_final, rec, wcgbt, hkl)
#com_final[find,] %>% tbl_df %>% print(n=200)


# Combine names for cryptic species and decide on capitalization structure =========

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

all_data$name = firstup(all_data$common_name)	

find = which(all_data$common_name == "blue rockfish" | all_data$common_name	 == "deacon rockfish" |
		all_data$common_name == "blue/deacon rockfish" | all_data$common_name == "deacon/blue rockfish unknown")
all_data[find, "name"] = "Blue/Deacon rockfish"

find = which(all_data$common_name == "vermilion rockfish" | all_data$common_name == "sunset rockfish" |
		all_data$common_name == "vermilion and sunset rockfish")
all_data[find, "name"] = "Vermilion/Sunset rockfish"

find = which(all_data$common_name == "gopher rockfish" | all_data$common_name == "black and yellow rockfish")
all_data[find, 'name'] = "Gopher/Black and yellow rockfish"

find = which(all_data$common_name == "blackspotted rockfish" | all_data$common_name == "rougheye rockfish" |
		all_data$common_name == "rougheye and blackspotted rockfish")
all_data[find, 'name'] = "Rougheye/Blackspotted rockfish"

find = which(all_data$common_name == "spiny dogfish" | all_data$common_name == 'spiny dogfish shark')
all_data[find, 'name'] = 'Pacific spiny dogfish'

find = which(all_data$common_name == "cowcod rockfish")
all_data[find, 'name'] = "Cowcod"

find = which(all_data$common_name == "chilipepper")
all_data[find, 'name'] = "Chilipepper rockfish"

find = which(all_data$common_name == "silvergrey rockfish")
all_data[find, 'name'] = "Silvergray rockfish"

#Change data type names
find = which(all_data$data_type == "commercial")
all_data$data_type[find] = "commercial fisheries"
find = which(all_data$data_type	 == "recreational")
all_data$data_type[find] = "recreational fisheries"
find = which(all_data$data_type	== "nwfsc_hkl")
all_data$data_type[find] = "NWFSC HKL"
find = which(all_data$data_type	== "nwfsc_wcgbt")
all_data$data_type[find] = "NWFSC WCGBT"

save(all_data, file = file.path(data_dir, "summarized_all_data.rdat"))
