#' Function to cut down the number of columns from the  
#' PacFIN SD501 table. This function also filters out any
#' non-market or non-random samples. The original SD501 data
#' file contains confidential data but the saved R object created
#' by this function cuts out the confidential fields.
#'
#' @param data_name The file name from the downloaded SD501 csv file
#' @param add_cols Additional columns to retain
#'
#' @author Chantel Wetzel
#' @export
#' @md
#' 
#' 
#' data_name <- "Confidential-BDS501-1980---2020_california.csv" 
#' data_name <- "Confidential-BDS501-1980---2020_oregon.csv" 
#' data_name <- "Confidential-BDS501-1980---2020_washington.csv" 
#'
cut_commercial_bds <- function(data_name, add_cols = NULL) {

	data <- read.csv(paste0("data/", data_name))

	cols <- c("AGENCY_CODE", "SAMPLE_YEAR", "PACFIN_SPECIES_CODE", "PACFIN_SPECIES_COMMON_NAME",
		"SAMPLE_TYPE", "SAMPLE_TYPE_DESC",
		"SAMPLE_METHOD_CODE", "SAMPLE_METHOD_DESC", "DATA_TYPE", "CLUSTER_ID",
		"FISH_LENGTH_TYPE_CODE", "FISH_LENGTH_TYPE_DESC", "FISH_LENGTH", "FORK_LENGTH", "FORK_LENGTH_IS_ESTIMATED",
		"SEX_CODE", "AGE_METHOD_CODE", "AGE_IN_YEARS", "AGE_STRUCTURE_CODE", "AGENCY_AGE_STRUCTURE_CODE", "AGE_STRUCTURE_DESC"
	)

	if(!is.null(add_cols)) {
		cols <- c(cols, add_cols)
	}
	cut_data <- data[,cols]

	if(unique(cut_data$AGENCY_CODE) == "C") {
		good_samples <- which(cut_data$SAMPLE_METHOD_CODE == "R")
	} else {
		good_samples <- which(cut_data$SAMPLE_TYPE == "M" & cut_data$SAMPLE_METHOD_CODE == "R")
	}
	cut_data <- cut_data[good_samples, ] 

	# Summarize only break and burn reads
	not_bb_read <- which(!cut_data$AGE_METHOD_CODE %in% c(1, 4, "B", "BB", "", "TS"))
	cut_data[not_bb_read, c("AGE_IN_YEARS")] <- NA

	pacfin_names <- read.csv("data/pacfin_species_names.csv")
	keep_species <- which(cut_data$PACFIN_SPECIES_CODE %in% pacfin_names[,1])
	bds_data <- cut_data[keep_species, ]

	new_name <- gsub('.{4}$', '', data_name)
	save(bds_data, file = paste0("data/", new_name, ".rdat"))
}
