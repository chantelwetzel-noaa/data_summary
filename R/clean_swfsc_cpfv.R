#' 
#' @param data
#' @param species 
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
#'
clean_coop_ages <- function(data, species){
  
  data$SPECIES_NAME <- tolower(data$SPECIES_NAME)
  
  keep <- which(data[, "SPECIES_NAME"] %in% species[, "name"])
  data <- data[keep, ] 
  
  data$Common_name <- NA
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "SPECIES_NAME"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  
  data$State <- "California"
  data$Source <- "SWFSC-CPFV Coop."
  data$State_Source <- paste0("SWFSC-CPFV Coop-", data$State)
  data$Sex <- "B"
  data$Lengthed <- data[, "Lengths"]
  data$Otolith <- data[, "Unread_Otoliths"]
  data$Fleet <- "CPFV"
  data$set_tow_id <- NA 
  data$Length_cm <- NA 
  data$Age <- NA
  
  return(data)
}