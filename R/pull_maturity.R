

pull_maturity <- function(return = TRUE, file = "maturity_reads.csv"){

	species_names <- all_species()
	dir <- here::here("data")

	mat_data <- as.data.frame(read.csv(file.path(dir, file = file)))

	keep <- which(tolower(mat_data$species) %in% tolower(species_names))
	sub_mat <- mat_data[keep, ]

	mat_summary <- matrix(NA, length(unique(mat_data$species)), 2)
   	ind <- sort(unique(mat_data$species))

   	for(a in 1:length(ind)) {		
		mat_summary[a, 1] <- sum(mat_data[mat_data$species == ind[a], "total"])   		
   		mat_summary[a, 2] <- sum(mat_data[mat_data$species == ind[a] & mat_data$read == "yes", "total"])  		
   	}
   	rownames(mat_summary) <- ind
   	colnames(mat_summary) <- c("collected", "read")

   	write.csv(mat_summary, file = file.path(dir, "maturity_summary.csv"))

	if(return){
		mat <- sub_mat
		return(mat)
	}
}