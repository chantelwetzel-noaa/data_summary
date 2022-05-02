#' Function to summarize the number of clusters sampled, lengths,
#' ages, and otoliths available by year
#'
#' @param data_names The file name from the cut down R object from the
#' cut_commercial_bds(). This can be a single file name or a vector of strings
#' 
#' 
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' 
#' 
#' data_names <- c("Confidential-BDS501-1980---2020_california.rdat",
#' 				"Confidential-BDS501-1980---2020_oregon.rdat",
#' 				"Confidential-BDS501-1980---2020_washington.rdat")
#' 
summarize_commercial_bds <- function(data_names) {

	data_matrix <- NULL
	for(i in 1:length(data_names)){
		load(paste0("data-raw/", data_names[i]))
		data_matrix <- rbind(data_matrix, bds_data)
	}

	data_matrix$common_name <- gsub(" ", " ", tolower(data_matrix$PACFIN_SPECIES_COMMON_NAME))	

	data_matrix$collected_oto <- 0
	grab <- which(is.na(data_matrix$AGE_IN_YEARS) & data_matrix$AGE_STRUCTURE_CODE %in% c("O", "SP", "V"))
	data_matrix$collected_oto[grab] = 1	

	find = which(data_matrix$common_name %in% c("vermilion rockfish", "sunset rockfish"))
	data_matrix[find, "common_name"] = "vermilion and sunset rockfish"

	find = which(data_matrix$common_name %in% c("gopher rockfish", "black and yellow rockfish"))
	data_matrix[find, "common_name"] = "gopher and black and yellow rockfish"

	find = which(data_matrix$common_name %in% c("rougheye rockfish", "blackspotted rockfish"))
	data_matrix[find, "common_name"] = "rougheye and blackspotted rockfish"

	find = which(data_matrix$common_name %in% c("blue rockfish", "deacon rockfish"))
	data_matrix[find, "common_name"] = "blue and deacon rockfish"

	combine <-  
		data_matrix %>%
		group_by(common_name, AGENCY_CODE, SAMPLE_YEAR) %>%
		summarise(
			positive_sites = 0, 
			sexed_fish = sum(SEX_CODE %in% c("M", "F", 1, 2) & !is.na(FISH_LENGTH)),
			unsexed_fish = sum(!SEX_CODE %in% c("M", "F", 1, 2) & !is.na(FISH_LENGTH)),
			lengthed = sum(!is.na(FISH_LENGTH)),
			aged = sum(!is.na(AGE_IN_YEARS)),
			otoliths = sum(collected_oto) #sum(AGE_STRUCTURE_CODE %in% c("O", "SP", "V"))
		)

	combine$data_type <- "commercial"

	colnames(combine)[2:3] = c("state", "sample_year")
	save(combine, file = paste0("data/summarized_commercial_data.rdat"))	
}