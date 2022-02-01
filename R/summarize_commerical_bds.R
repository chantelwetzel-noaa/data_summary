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
		load(paste0("data/", data_names[i]))
		data_matrix <- rbind(data_matrix, bds_data)
	}

	data_matrix$common_name <- gsub(" ", " ", tolower(data_matrix$PACFIN_SPECIES_COMMON_NAME))	

	#lengths <- aggregate(FISH_LENGTH ~ AGENCY_CODE + species + SAMPLE_YEAR, 
	#	data_matrix[!is.na(data_matrix$"FISH_LENGTH"), ],
	#	function(x) length(x), 
	#	drop = FALSE)
#
	#ages <- aggregate(AGE_IN_YEARS ~ AGENCY_CODE + species + SAMPLE_YEAR, 
	#	data_matrix,
	#	function(x) length(!is.na(x)), 
	#	drop = FALSE)
	data_matrix$collected_oto <- 0
	grab <- which(is.na(data_matrix$AGE_IN_YEARS) & data_matrix$AGE_STRUCTURE_CODE %in% c("O", "SP", "V"))
	data_matrix$collected_oto[grab] = 1	

	combine <-  
		data_matrix %>%
		group_by(common_name, AGENCY_CODE, SAMPLE_YEAR) %>%
		summarise(
			positive_sites = 0, 
			sexed_fish = sum(SEX_CODE %in% c("M", "F", 1, 2)),
			unsexed_fish = sum(!SEX_CODE %in% c("M", "F", 1, 2)),
			lengthed = sum(!is.na(FISH_LENGTH)),
			aged = sum(!is.na(AGE_IN_YEARS)),
			otoliths = sum(collected_oto) #sum(AGE_STRUCTURE_CODE %in% c("O", "SP", "V"))
		)

	combine$data_type <- "commercial"
	#combine <- dplyr::left_join(lengths, ages)
	#combine$FISH_LENGTH[which(is.na(combine$FISH_LENGTH))] <- 0
	#combine$AGE_IN_YEARS[which(is.na(combine$AGE_IN_YEARS))] <- 0
	#order <- sort(combine$species, index.return = TRUE)$ix
	#combine <- combine[order, ]

	#out_matrix <- NULL
	#for(s in sort(unique(combine$species))) {
	#	temp <- combine[combine$species == s, ]

	#	ca <- which(temp$AGENCY_CODE == "C")
	#	or <- which(temp$AGENCY_CODE == "O")
	#	wa <- which(temp$AGENCY_CODE == "W")	

	#	mat <- cbind(temp$species[ca], 
	#		temp$SAMPLE_YEAR[ca],
	#		temp$FISH_LENGTH[ca],
	#		temp$FISH_LENGTH[or],
	#		temp$FISH_LENGTH[wa],
	#		temp$AGE_IN_YEARS[ca],
	#		temp$AGE_IN_YEARS[or],
	#		temp$AGE_IN_YEARS[wa]
	#	)

	#	ind <- sort(as.numeric(mat[,2]), index.return = TRUE)$ix
	#	out_matrix <- rbind(out_matrix, mat[ind, ])
	#}

	#out_matrix <- as.data.frame(out_matrix)
	#colnames(out_matrix) <- c("common_name", "year", 
	#	"length_ca", "length_or", "length_wa",
	#	"age_ca", "age_or", "age_wa")
	#write.csv(out_matrix, "data/processed_commercial_samples.csv", row.names = FALSE)

	colnames(combine)[2:3] = c("state", "sample_year")
	save(combine, file = paste0("data/summarized_commercial_data.rdat"))	
}