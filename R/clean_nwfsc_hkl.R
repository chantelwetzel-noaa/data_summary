#' Function to remove all unneeded species data and to format column names as needed
#' for the NWFS HKL data.
#' 
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param data Data frame of NWFSC HKL data
#' 
#' @author Chantel Wetzel
#' @export
#' @md
#'
#'
clean_nwfsc_hkl <- function(
    dir = here::here("data-processed"), 
    data, 
    species){
  
    #cols <- c("common_name", "year", "site_number", "set_id", "sex", "otolith_number", "age_years",
    #          "length_cm", "weight_kg")
    #
    #data <- data[,cols]
    
    data$set_tow_id <- data$set_id
    
    data <- data[which(tolower(data$common_name) %in% species[,1]), ]
    data$common_name <- tolower(data$common_name)
    data$Common_name <- NA
    for (a in 1:dim(species)[1]){
      find <- grep(species[a, "name"], data[, "common_name"])
      data[find, "Common_name"] <- species[a, "use_name"]
    }
    
    data <- data[data$Common_name != "southern rock sole", ]
    
    data$State <- "California"
    
    find = which(!is.na(data$otolith_number) & is.na(data$age_years))
    data$Otolith <- 0
    data$Otolith[find] <- 1
    
    data$Lengthed <- 1
    data$Length_cm <- data$length_cm
    data$Age <- data$age_years
    
    data$Sex <- data$sex
    data$Source <- "NWFSC HKL"
    
    data$Fleet <- NA
    
    data$Year <- data$year
    
    save(data, file = file.path(dir, "nwfsc_hkl_filtered.Rdata"))		
    return(data)
}

