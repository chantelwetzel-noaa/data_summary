#'
#' @param dir Directory location to save the cleaned data frame
#' @param bds_pacfin
#' @param species
#' @param year
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
#'
clean_pacfin_comps <- function(dir, bds_pacfin, species, spid_key, year = 1980){

  Pdata <- bds_pacfin[which(bds_pacfin$FISH_LENGTH_UNITS != "UNK" & bds_pacfin$SAMPLE_YEAR >= year), ]

  data <- PacFIN.Utilities::cleanPacFIN(
    Pdata = Pdata,
    CLEAN = TRUE,
    keep_age_method = c("B", "S", "T"),
    verbose = TRUE)
  cleaned_pacfin_bds <- data
  save(cleaned_pacfin_bds, file = here::here("data-raw", "cleaned_pacfin_bds.Rdata"))

  data$spid_name <- NA
  for (a in 1:dim(spid_key)[1]){
    find <- grep(spid_key[a, "pacfin.code"], data[, "SPID"])
    data[find, "spid_name"] <- spid_key[a, "species"]
  }
  data <- data[!is.na(data$spid_name), ]

  data$Common_name <- NA
  for (a in 1:dim(species)[1]){
    find <- grep(species[a, "name"], data[, "spid_name"], ignore.case = TRUE)
    data[find, "Common_name"] <- species[a, "use_name"]
  }
  data <- data[!is.na(data$Common_name), ]

  # Overwrite age structures for all records where there wasn't an age method recorded and hence
  # the Age column was set to NA even if there was a value in FISH_AGE_CODE_FINAL
  fix <- which(!is.na(data$FISH_AGE_YEARS_FINAL) & is.na(data$age_method))
  data[fix, c("AGE_STRUCTURE_CODE1", "AGE_STRUCTURE_CODE2", "AGE_STRUCTURE_CODE3")] <- "L"

  data$State <- NA
  data$State[which(data$state == "CA")] <- "California"
  data$State[which(data$state == "OR")] <- "Oregon"
  data$State[which(data$state == "WA")] <- "Washington"
  data$Source <- "Commercial"
  data$State_Source <- paste0(data$Source, "-", data$State)

  data$Fleet = "Non-trawl"
  data$Fleet[which(data$geargroup %in% c("TWL", "TWS"))] <- "Trawl"

  data$Year <- data$year

  data$Sex <- data$SEX

  data$Length_cm <- data$lengthcm

  data$Lengthed <- 1

  data$Aged <- 0
  data$Aged[!is.na(data$Age)] <- 1

  data$Otolith <- 0
  # AGE_STRUCTURE_DESC1
  find <- which(!is.na(data$AGE_STRUCTURE_CODE1) & data$AGE_STRUCTURE_CODE1 != "L" & is.na(data$Age))
  if(length(find) > 0 ){
    data[find, "Otolith"] <- 1
  }

  find <- which(!is.na(data$AGE_STRUCTURE_CODE2) & data$AGE_STRUCTURE_CODE2 != "L" & is.na(data$Age))
  if(length(find) > 0 ){
    data[find, "Otolith"] <- 1
  }
  find <- which(!is.na(data$AGE_STRUCTURE_CODE3) & data$AGE_STRUCTURE_CODE3 != "L" & is.na(data$Age))
  if(length(find) > 0 ){
    data[find, "Otolith"] <- 1
  }

  data$set_tow_id <- 0

  save(data, file = file.path(dir, "pacfin_filtered.Rdata"))
  return(data)
}
