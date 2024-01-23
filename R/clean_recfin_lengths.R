#' 
#' 
#' 
#' @param dir Directory location to save the cleaned data frame
#' @param species 
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
#' @import nwfscSurvey
#'
clean_recfin_lengths <- function(dir, or_data = NULL, wa_data= NULL, ca_data= NULL, species){
  
  all_data <- rbind(wa_data, or_data, ca_data)
  all_data$species_name <- tolower(all_data$SPECIES_NAME)
  all_data$Common_name <- NA
  #data <- combine_species(data = all_data, name = "Common_name")
  
  data <- all_data[which(all_data$species_name %in% species[,1]), ]
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "species_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  
  data$State <- tolower(data$STATE_NAME)
  substr(data$State, 1, 1) <- toupper(substr(data$State, 1, 1))
  data$Source <- "RecFIN"
  data$State_Source <- paste0("RecFIN-", data$State)
  
  data$Year <- data$RECFIN_YEAR
  
  keep <- which(data$AGENCY_WATER_AREA_NAME %in% c(
    "NOT KNOWN", # All Washington records are not know so keeping this across the board
    "OCEAN", 
    "OCEAN <= 3 MILES", 
    "OCEAN <= 3 MILES (AREAB AND P1B IMPORT)",
    "OCEAN > 3 MILES",
    "OCEAN > 3 MILES (AREAB AND P1B IMPORT)"))
  data <- data[keep, ]
  
  remove <- which(is.na(data$RECFIN_LENGTH_MM)) 
  if(length(remove) > 0){
    data <- data[-remove, ]    
  }
  remove <- which(data$RECFIN_LENGTH_MM == 0)
  if(length(remove) > 0) {
    data <- data[-remove, ]
  }
  remove <- which(data$RECFIN_LENGTH_MM > 2000)
  if(length(remove) > 0){
    data <- data[-remove, ]
  }
  
  data$Length_cm <- data$RECFIN_LENGTH_MM / 10
  data$Lengthed <- 1
  
  data$Sex <- data$RECFIN_SEX_CODE
  data$Sex[which(!data$Sex %in% c("F", "M"))] <- "U"
  
  data$set_tow_id <- NA
  
  data$Age <- NA
  data$Otolith <- NA
  
  data$Fleet <- data$RECFIN_MODE_NAME
  
  #cols_to_keep <- c("Year", "State", "Source", "Common_name", "Fleet", "Lengthed", "Otolith", "Sex", "set_tow_id", "Length_cm", "Age")
  #data <- data[, cols_to_keep]
  
  save(data, file = file.path(dir, "recfin_lengths_filtered.Rdata"))
  return(data)
  
}