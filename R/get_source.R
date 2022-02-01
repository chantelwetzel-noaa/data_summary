
get_source <- function(data){

	use <- unique(data$use_data)
	if("all" %in% use){
		sources <- c("commercial fisheries", "recreational fisheries", 
					 "NWFSC WCGBT", "NWFSC HKL")
	}
	if("com" %in% use){
		sources <- c("commercial fisheries")		
	}
	if("rec" %in% use){
		sources <- c("recreational fisheries")		
	}
	if("com_rec" %in% use){
		sources <- c("commercial fisheries", "recreational fisheries")		
	}
	if("com_wcgbt" %in% use){
		sources <- c("commercial fisheries",
					 "NWFSC WCGBT")		
	}
	if("com_hkl" %in% use){
		sources <- c("commercial fisheries",
					 "NWFSC HKL")		
	}
	if("rec_hkl" %in% use){
		sources <- c("recreational fisheries", 
					 "NWFSC HKL")		
	}
	if("com_rec_wcgbt" %in% use){
		sources <- c("commercial fisheries", "recreational fisheries", 
					 "NWFSC WCGBT")		
	}
	if("com_rec_hkl" %in% use){
		sources <- c("commercial fisheries", "recreational fisheries", 
					 "NWFSC HKL")		
	}
	if("com_wcgbt_hkl" %in% use){
		sources <- c("commercial fisheries", 
					 "NWFSC WCGBT", "NWFSC HKL")		
	}
	if("rec_wcgbt_hkl" %in% use){
		sources <- c("recreational fisheries", 
					 "NWFSC WCGBT", "NWFSC HKL")		
	}
	return(sources)
}


