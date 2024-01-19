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
clean_recfin_ages <- function(dir, species, data){
  
  data$species_name <- tolower(data$RECFIN_SPECIES_NAME)
  data$Common_name <- NA
  
  data <- data[which(data$species_name %in% species[,1]), ]
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "species_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  

  data$State <- ifelse(data$SAMPLING_AGENCY_NAME == "WDFW", "Washington", 
                       ifelse(data$SAMPLING_AGENCY_NAME == "ODFW", "Oregon", NA))
  data$Source <- "RecFIN"
  data$State_Source <- paste0("RecFIN-", data$State)
  
  data$Year <- data$SAMPLE_YEAR
  
  data$Length_cm <- NA
  data$Length_cm[which(data$LENGTH_UNITS %in% c("C", ""))] <- 
    data[which(data$LENGTH_UNITS %in% c("C", "")), "MEASURED_LENGTH"]
  data$Length_cm[which(data$LENGTH_UNITS %in% c("M", "MM"))] <- 
    data[which(data$LENGTH_UNITS %in% c("M", "MM")), "MEASURED_LENGTH"] / 10
  
  data$Lengthed <- 1
  data$Lengthed[is.na(data$Length_cm)] <- 0
  
  data$Sex <- data$RECFIN_SEX_CODE
  data$Sex[which(!data$Sex %in% c("F", "M"))] <- "U"
  
  data$set_tow_id <- NA
  
  data$Age <- data$USE_THIS_AGE
  data$Otolith <- 0
  data$Otolith[which(data$RECFIN_STRUCTURE_DESCRIPTION != "" & is.na(data$Age))] <- 1
  
  data$Fleet <- data$RECFIN_MODE_NAME
  
  #cols_to_keep <- c("Year", "State", "Source", "Common_name", "Fleet", "Lengthed", "Otolith", "Sex", "set_tow_id", "Length_cm", "Age")
  #data <- data[, cols_to_keep]
  
  save(data, file = file.path(dir, "recfin_ages_filtered.Rdata"))
  return(data)
  
}
