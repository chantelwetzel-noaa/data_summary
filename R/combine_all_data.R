
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")
source(file.path(getwd(), "R", "all_species.R"))
library(dplyr)
library(tidyr)

data_dir <- file.path(getwd(), "data")

# Load all of the summarized data sets =====================================
load(file.path(data_dir, 'summarized_wcgbt_data.rdat'))
wcgbt = out
# blue rockfish
# deacon rockfish
# rougheye and blackspotted
# vermilion and sunset
load(file.path(data_dir, 'summarized_hkl_data.rdat'))
hkl = out
# blue rockfish
# vermilion rockfish
load(file.path(data_dir, 'summarized_commercial_data.rdat'))
com = combine
# blue rockfish
# rougheye rockfish
# blackspotted rockfish
# vermilion rockfish
load(file.path(data_dir, 'summarized_recreational_data.rdat'))
rec = out
# blue rockfish
# blue/deacon rockfish
# rougheye rockfish
# vermilion rockfish
# Species list =============================================================
species <- unlist(all_species())
pacfin_species <- read.csv(file.path(data_dir, 'pacfin_species_names.csv'))
remove <- c("gopher/black and yellow rockfish", "black and yellow rockfish",
	"gopher rockfish", "spiny dogfish", "dogfish shark",
	"rougheye rockfish", "blackspotted rockfish", "rougheye/blackspotted rockfish",
  "blue rockfish", "deacon rockfish", "blue/deacon rockfish",
  "vermilion rockfish", "sunset rockfish", "vermilion/sunset rockfish")

# Filter function ==========================================================
filter_species <- function(data, species_list){
	key <- NULL
	for(a in 1:length(species_list)){
		key <- c(key, grep(species_list[a], data, ignore.case = TRUE))
	}
	return(key)
}


# Read in state specific otolith data and join that with the commercial data =======
ca_oto <- read.csv(file.path(data_dir, 'ca_commerical_samples_otoliths_2022.csv'))
ca_oto$common_name <- NA
ca_oto$sample_year <- ca_oto$year
# filter out unneeded species
ca_oto = ca_oto[ca_oto$species %in% pacfin_species$pacfin.code, ]
all <- sort(unique(ca_oto$species))
for(a in 1:length(all)){
	find = which(all[a] == pacfin_species$pacfin.code)
	ca_oto$common_name[ca_oto$species == all[a]] <- pacfin_species[find,"species"]
}

find = which(ca_oto$common_name %in% c("blue rockfish", "deacon rockfish"))
ca_oto[find, "common_name"] = "blue and deacon rockfish"
find = which(ca_oto$common_name %in% c("black and yellow rockfish", "gopher rockfish"))
ca_oto[find, "common_name"] = "gopher and black and yellow rockfish"
find = which(ca_oto$common_name %in% c("vermilion rockfish", "sunset rockfish"))
ca_oto[find, "common_name"] = "vermilion and sunset rockfish"
find = which(ca_oto$common_name %in% c("rougheye rockfish", "blackspotted rockfish"))
ca_oto[find, "common_name"] = "rougheye and blackspotted rockfish"

key <- filter_species(data = ca_oto$common_name, species_list = species[!species %in% remove])
ca_oto_summarized <- ca_oto[key,] %>%
		group_by(common_name, sample_year) %>%
		summarise(state = "C",
				  otoliths = sum(SumOftotal) )
com_int = left_join(com, ca_oto_summarized, by = c("sample_year", "common_name", "state"))
find = which(!is.na(com_int$otoliths.y))
com_int[find, "otoliths.x"] = com_int[find, "otoliths.y"] 
colnames(com_int)[which(colnames(com_int) == "otoliths.x")] = "otoliths"
com_int <- com_int[ ,colnames(com_int) != "otoliths.y"]


# Need to check the WA otoliths against the info in PacFIN
wa_oto <- read.csv(file.path('data', 'wa_commercial_samples_otoliths.csv'))
wa_oto$unaged <- wa_oto$CountOfage_structure - wa_oto$CountOfbest_age
wa_oto$common_name = tolower(wa_oto$species_name)
find = which(wa_oto$common_name %in% c("rougheye rockfish", "blackspotted rockfish"))
wa_oto[find, "common_name"] = "rougheye and blackspotted rockfish"
key <- filter_species(data = wa_oto$common_name, species_list = species[!species %in% remove])
wa_oto_summarized <- wa_oto[key,] %>%
		group_by(common_name, sample_year) %>%
		summarise(state = "W",
				  otoliths = sum(unaged) )
com_final = left_join(com_int, wa_oto_summarized, by = c("sample_year", "common_name", "state"))
find = which(!is.na(com_final$otoliths.y))
com_final[find, "otoliths.x"] = com_final[find, "otoliths.y"] 

colnames(com_final)[which(colnames(com_final) == "otoliths.x")]  = "otoliths"
com_final = com_final[, colnames(com_final) != "otoliths.y"]


# Bring it all together ============================================================

all_data <- rbind(com_final, rec, wcgbt, hkl)
#com_final[find,] %>% tbl_df %>% print(n=200)


# Combine names for cryptic species and decide on capitalization structure =========

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

all_data$name = firstup(all_data$common_name)	

# fix gopher for hkl and wcgbt
find = which(all_data$common_name == "gopher rockfish" & all_data$data_type %in% c("nwfsc_hkl", "nwfsc_wcgbt"))
all_data$common_name[find] = "gopher and black and yellow rockfish"

find = which(all_data$common_name == "spiny dogfish" | all_data$common_name == 'spiny dogfish shark')
all_data[find, 'common_name'] = 'pacific spiny dogfish'

find = which(all_data$common_name == "cowcod rockfish")
all_data[find, 'common_name'] = "cowcod"

find = which(all_data$common_name == "chilipepper")
all_data[find, 'common_name'] = "chilipepper rockfish"

find = which(all_data$common_name == "silvergrey rockfish")
all_data[find, 'common_name'] = "ailvergray rockfish"

fix <- which(all_data$common_name == "southern rock sole")
all_data[fix, "common_name"] = "rock sole"

#Change data type names
find = which(all_data$data_type == "commercial")
all_data$data_type[find] = "commercial fisheries"
find = which(all_data$data_type	 == "recreational")
all_data$data_type[find] = "recreational fisheries"
find = which(all_data$data_type	== "nwfsc_hkl")
all_data$data_type[find] = "NWFSC HKL"
find = which(all_data$data_type	== "nwfsc_wcgbt")
all_data$data_type[find] = "NWFSC WCGBT"


all_data = data.frame(all_data)

save(all_data, file = file.path(data_dir, "summarized_all_data.rdat"))

