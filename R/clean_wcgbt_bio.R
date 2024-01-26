#' 
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param data List of catch and bio data to clean up
#' @author Chantel Wetzel
#' @export
#' @md
#'
#' @import nwfscSurvey
#' @import here
#'
clean_wcgbt_bio <- function(dir = here::here("data-processed"), species, data){
  
  # Biological data
  bio <- data$bio
  bio$set_tow_id <- bio$Trawl_id

  bio <- bio[bio$Common_name != "southern rock sole", ]
  
  find <- which(bio$Common_name %in% c("tree rockfish"))
  bio[find, "Common_name"] <- "treefish"
  
  find <- which(bio$Common_name %in% c("rougheye rockfish", "blackspotted rockfish"))
  bio[find, "Common_name"] <- "rougheye and blackspotted rockfish"
  
  find <- which(bio$Common_name %in% c("blue / deacon rockfish", "blue rockfish", "deacon rockfish"))
  bio[find, "Common_name"] <- "blue and deacon rockfish"
  
  find <- which(bio$Common_name %in% c("vermilion rockfish", "Sunset rockfish"))
  bio[find, "Common_name"] <- "vermilion and sunset rockfish"
  
  find <- which(bio$Common_name %in% c("gopher rockfish"))
  bio[find, "Common_name"] <- "gopher and black and yellow rockfish"
  
  # filter down to only the species considered in the prioritization process
  keep <- which(bio$Common_name %in% unique(species[,"use_name"]))
  bio_sub <- bio[keep, ]
  
  bio_sub$Source <- "NWFSC WCGBT"
  
  bio_sub$State_area <- ifelse(
    bio_sub$Latitude_dd > 46, "WA", ifelse(
      bio_sub$Latitude_dd > 42 & bio_sub$Latitude_dd < 46, "OR", ifelse(
        bio_sub$Latitude_dd < 42 & bio_sub$Latitude_dd < 40.17, "NCA", ifelse(
          bio_sub$Latitude_dd < 40.17 & bio_sub$Latitude_dd > 34.47, "CCA", "SCA"
        )
      )
    )
  )
  
  bio_sub$State <- ifelse(
    bio_sub$Latitude_dd > 46, "Washington", ifelse(
      bio_sub$Latitude_dd > 42 & bio_sub$Latitude_dd < 46, "Oregon", "California"
    )
  )
  
  bio_sub <- bio_sub[!is.na(bio_sub$Length_cm), ]
  bio_sub$Lengthed <- 1
  
  bio_sub$Sex[is.na(bio_sub$Sex)] <- "U"
  
  bio_sub$Otolith <- 0
  bio_sub[which(!is.na(bio_sub$Otosag_id) & is.na(bio_sub$Age)), "Otolith"] <- 1
  
  bio_sub$Fleet <- NA
  
  
  save(bio_sub, file = file.path(dir, "wcgbt_bio_filtered.Rdata"))	
  
  return(bio_sub)
}

