#' 
#' @param dir Directory location to save the cleaned data frame
#' @param species 
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
#'
clean_ccfrp <- function(dir, species, data){

  data$Common_name <- NA
  data$species_name <- tolower(data$Common_Name)

  data <- data[which(data$species_name %in% species[,1]), ]
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "species_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  
  # Remove Farralon samples due to only being sampled a couple of years
  data <- data[which(!data$Area %in% c("Farallon Islands", "Point Conception", "Trinidad", "Laguna Beach")), ]
  
  data$Lengthed <- 1
  data$State <- "California"
  data$Source <- "CCFRP"
  data$Fleet <- NA
  data$set_tow_id <- 0
  data$Otolith <- 0
  data$Aged <- 0
  data$Age <- NA
  data$Sex <- "U"

  return(data)
}