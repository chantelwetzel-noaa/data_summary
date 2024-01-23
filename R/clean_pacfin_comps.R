
#' Data pulled from pacfin apex report rather than using code from PacFIN.Utilities
#' I originally pulled using the PacFIN.Utlities function and encountered two issues:
#' 1) The majority of records did not have a length
#' 2) When using the cleanPacFIN function the common names were removed and only SPID remained.

clean_pacfin_comps <- function(dir, bds_pacfin, species, spid_key){
  
  Pdata <- bds_pacfin[which(bds_pacfin$FISH_LENGTH_UNITS != "UNK"), ]
  data <- PacFIN.Utilities::cleanPacFIN(
    Pdata = Pdata,
    CLEAN = TRUE,
    keep_age_method = c("B", "S", "T"),
    verbose = FALSE)
  cleaned_pacfin_bds <- data
  save(cleaned_pacfin_bds, file = here::here("data-raw", "cleaned_pacfin_bds.Rdata"))
  
  data$spid_name <- NA
  #spid_key <- read.csv(file.path(dir, "pacfin_species_codes.csv"))
  for (a in 1:dim(spid_key)[1]){
    find <- grep(spid_key[a, "pacfin.code"], data[, "SPID"])
    data[find, "spid_name"] <- spid_key[a, "species"]
  }
  
  data <- data[!is.na(data$spid_name), ]
  
  data$Common_name <- NA
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "spid_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  
  data <- data[!is.na(data$Common_name), ]
  
  data$State <- NA
  data$State[which(data$state == "CA")] <- "California"
  data$State[which(data$state == "OR")] <- "Oregon"
  data$State[which(data$state == "WA")] <- "Washington"
  data$Source <- "PacFIN"
  data$State_Source <- paste0("PacFIN-", data$State)
  
  data$Fleet = "Non-trawl"
  data$Fleet[which(data$geargroup %in% c("TWL", "TWS"))] <- "Trawl"
  
  data$Year <- data$year
  
  data$Sex <- data$SEX
  
  data$Length_cm <- data$lengthcm

  data$Lengthed <- 1
  
  data$Otolith <- 0
  # AGE_STRUCTURE_DESC1
  find <- which(!is.na(data$AGE_STRUCTURE_CODE1) & data$AGE_STRUCTURE_CODE1 != "L" & is.na(data$Age))
  data[find, "Otolith"] <- 1
  find <- which(!is.na(data$AGE_STRUCTURE_CODE2) & data$AGE_STRUCTURE_CODE1 != "L" & is.na(data$Age))
  data[find, "Otolith"] <- 1
  find <- which(!is.na(data$AGE_STRUCTURE_CODE3) & data$AGE_STRUCTURE_CODE1 != "L" & is.na(data$Age))
  data[find, "Otolith"] <- 1
  
  data$set_tow_id <- NA
                  
  save(data, file = file.path(dir, "pacfin_filtered.Rdata"))
  return(data)
}