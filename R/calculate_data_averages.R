
library(tidyverse)
library(glue)

dir <- "C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary"
source(file.path(dir, "R", "get_species_data.R"))
source(file.path(dir, "R", "get_source.R"))

load(file.path(dir, 'data', 'summarized_all_data.rdat'))
data <- all_data

fix <- which(all_data$name == "Southern rock sole")
data[fix, "name"] = "Rock sole"
species_data <- get_species_data()

# Fix NA in unsexed
replace = which(is.na(data$unsexed_fish))
data[replace, 'unsexed_fish'] = data[replace, 'lengthed'] - data[replace, 'sexed_fish']
set_to_zero = which(data[, 'unsexed_fish'] < 0)
data[set_to_zero, 'unsexed_fish'] = 0

# Add data to use to the data matrix
#data$use_data <- NA
#for (a in 1:nrow(species_data)){
#	key <- grep(species_data[a, "species_name"], data$name, ignore.case = TRUE)
#	data[key, "use_data"] <- species_data[a, "data"]
#}

# Filter down for only the species we are examining
find = NULL
for (a in 1:nrow(species_data)){
	find <- c(find, grep(species_data[a, "species_name"], data$name, ignore.case = TRUE))
}
data <- data[find, ]

# Calculate averages by species for WCGBT
find = which(data$data_type == "NWFSC WCGBT")
tmp = data[find, ]
n = length(unique(tmp$sample_year))

wcgbt_ave <-  
	tmp %>%
	group_by(name) %>%
	summarise(
		positive_sites = round(sum(positive_sites) / n, 0), 
		sexed_fish = round(sum(sexed_fish) / n, 0),
		unsexed_fish = round(sum(unsexed_fish) / n, 0),
		lengthed = round(sum(lengthed) / n, 0),
		aged = round(sum(aged) / n, 0),
		otoliths = round(sum(otoliths) / n, 0)
	)

write.csv(wcgbt_ave, file.path(dir, 'tables', 'wcgbt_average_samples.csv'), row.names = FALSE)

# Calculate averages by species for HKL

find = which(data$data_type == "NWFSC HKL")
tmp = data[find, ]
n = length(unique(tmp$sample_year))

hkl_ave <-  
	tmp %>%
	group_by(name) %>%
	summarise(
		positive_sites = round(sum(positive_sites) / n, 0), 
		sexed_fish = round(sum(sexed_fish) / n, 0),
		unsexed_fish = round(sum(unsexed_fish) / n, 0),
		lengthed = round(sum(lengthed) / n, 0),
		aged = round(sum(aged) / n, 0),
		otoliths = round(sum(otoliths) / n, 0)
	)

write.csv(hkl_ave, file.path(dir, 'tables', 'hkl_average_samples.csv'), row.names = FALSE)


# Calculate averages by species for commercial 2001 - 2020

find = which(data$data_type == "commercial fisheries" & data$sample_year > 2000)
tmp = data[find, ]
n = length(unique(tmp$sample_year))

com_ave <-  
	tmp %>%
	group_by(name, state) %>%
	summarise(
		sexed_fish = round(sum(sexed_fish) / n, 0),
		unsexed_fish = round(sum(unsexed_fish) / n, 0),
		lengthed = round(sum(lengthed) / n, 0),
		aged = round(sum(aged) / n, 0),
		otoliths = round(sum(otoliths) / n, 0) ) %>%
	ungroup(name, state) %>%
	complete(name, state, fill = list(0, 0, 0, 0, 0))

com_ave[is.na(com_ave)] = 0

write.csv(com_ave, file.path(dir, 'tables', 'commercial_2001-2020_average_samples.csv'), row.names = FALSE)


# Calculate averages by species for commercial 1980 - 2000
n = length(1980:2000)
find = which(data$data_type == "commercial fisheries" & data$sample_year < 2001 )
tmp = data[find, ]

com_ave <-  
	tmp %>%
	group_by(name, state) %>%
	summarise(
		sexed_fish = round(sum(sexed_fish) / n, 0),
		unsexed_fish = round(sum(unsexed_fish) / n, 0),
		lengthed = round(sum(lengthed) / n, 0),
		aged = round(sum(aged) / n, 0),
		otoliths = round(sum(otoliths) / n, 0) ) %>%
	ungroup(name, state) %>%
	complete(name, state, fill = list(0, 0, 0, 0, 0))
com_ave[is.na(com_ave)] = 0

write.csv(com_ave, file.path(dir, 'tables', 'commercial_1980-2000_average_samples.csv'), row.names = FALSE)


# Calculate averages by species for recreational 2003 - 2020

find = which(data$data_type == "recreational fisheries" & data$sample_year > 2002)
tmp = data[find, ]
n = length(unique(tmp$sample_year))

rec_ave <-  
	tmp %>%
	group_by(name, state) %>%
	summarise(
		sexed_fish = round(sum(sexed_fish) / n, 0),
		unsexed_fish = round(sum(unsexed_fish) / n, 0),
		lengthed = round(sum(lengthed) / n, 0),
		aged = round(sum(aged) / n, 0),
		otoliths = round(sum(otoliths) / n, 0)) %>%
	ungroup(name, state) %>%
	complete(name, state, fill = list(0, 0, 0, 0, 0))
rec_ave[is.na(rec_ave)] = 0

write.csv(rec_ave, file.path(dir, 'tables', 'recreational_2003-2020_average_samples.csv'), row.names = FALSE)
