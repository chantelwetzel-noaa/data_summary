#' 
#' 
#' 
#' @param dir Directory location to save the compbiend data frame
#' @param wcgbt
#' @param nwfsc_hkl
#' @param pacfin
#' @param recfin_lengths
#' @param recfin_age
#' @param ca_rec_oto
#' @param ca_com_oto
#' @param wa_com_oto
#' @param coop_rec
#' @param ccfrp
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
combine_all_data <- function(
  dir = here::here("data-processed"),
  wcgbt,
  nwfsc_hkl,
  pacfin,
  recfin_lengths,
  recfin_ages,
  ca_rec_oto = NULL,
  ca_com_oto = NULL,
  wa_com_oto = NULL,
  coop_rec,
  ccfrp = NULL)
{
  
  if(!is.null(wa_com_oto)){
    find <- which(pacfin$State == "Washington" & pacfin$Source == "PacFIN")
    pacfin[find, "Otolith"] <- 0
  }
  
  #Combine data sets into a single data frame
  cols_to_keep <- c("Year", "State", "Source", "Common_name", "Fleet", "set_tow_id", "Lengthed", "Otolith", "Age", "Aged")
  data <- rbind(
    wcgbt[, cols_to_keep],
    nwfsc_hkl[, cols_to_keep],
    pacfin[, cols_to_keep],
    recfin_lengths[, cols_to_keep],
    recfin_ages[, cols_to_keep],
    coop_rec[, cols_to_keep],
    wa_com_oto[, cols_to_keep],
    ca_com_oto[, cols_to_keep],
    ca_rec_oto[, cols_to_keep]
  )
  
  if(!is.null(ccfrp)){
    data <- rbind(data, ccfrp[, cols_to_keep])
  }
  
  save(data, file = file.path(dir, "combined_data.Rdata"))
  #data$read_age <- 0
  #data$read_age[!is.na(data$Age)] <- 1
  #data[is.na(data)] <- 0
  
  group_vars = c("Common_name", "State", "Source")
  data_total <-  
    data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(Aged),
      total_otoliths = sum(Otolith),
      n_years = dplyr::n_distinct(Year),
      ave_set_tows = floor(dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(Aged) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    )
  data_total <- as.data.frame(data_total)
  
  group_vars <- c("Common_name", "State", "Source", "Year")
  data_total_by_year <-  
    data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(Aged),
      total_otoliths = sum(Otolith)
    )
  data_total_by_year <- as.data.frame(data_total_by_year)
  
  write.csv(data_total, file.path(dir, "data_summaries.csv"), row.names = FALSE)
  write.csv(data_total_by_year, file.path(dir, "data_summaries_by_year.csv"), row.names = FALSE)
  
  save(data_total_by_year, file = file.path(dir, "data_total_by_year.Rdata"))
  
  return(data_total_by_year)
}