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
  
  #Combine data sets into a single data frame
  cols_to_keep <- c("Year", "State", "Source", "Common_name", "Fleet", "Lengthed", "Otolith", "Sex", "set_tow_id", "Length_cm", "Age")
  data <- rbind(
    wcgbt[, cols_to_keep],
    nwfsc_hkl[, cols_to_keep],
    pacfin[, cols_to_keep],
    recfin_lengths[, cols_to_keep],
    recfin_ages[, cols_to_keep],
    coop_rec[, cols_to_keep]
  )
  
  if(!is.null(ccfrp)){
    data <- rbind(data, ccfrp)
  }
  
  
  group_vars = c("Common_name", "State", "Source")
  data_total <-  
    data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Age)),
      total_otoliths = sum(Otolith),
      n_years = dplyr::n_distinct(Year),
      ave_set_tows = floor(dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Age)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    )
  
  
  data_total <- as.data.frame(data_total)
  
  group_vars = c("Common_name", "State", "Source", "Year")
  data_total_by_year <-  
    data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Age)),
      total_otoliths = sum(Otolith)
    )
  
  data_total_by_year <- as.data.frame(data_total_by_year)
  
  if(!is.null(ca_rec_oto)){
    group_vars = c("Common_name", "State", "Source")
    oto_rec_total <-  
      ca_rec_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count),
        n_years = dplyr::n_distinct(Year)
      )
    
    oto_rec_total <- as.data.frame(oto_rec_total)
    oto_rec_total <- oto_rec_total[oto_rec_total$total_otoliths > 0, ]
    
    group_vars = c("Common_name", "State", "Source", "Year")
    oto_rec_total_by_year <-  
      ca_rec_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count)
      )
    
    oto_rec_total_by_year <- as.data.frame(oto_rec_total_by_year)
    oto_rec_total_by_year <- oto_rec_total_by_year[oto_rec_total_by_year$total_otoliths > 0, ]
    
    for(a in 1:dim(oto_rec_total_by_year)[1]){
      find <- which(data_total_by_year[, "Common_name"] == oto_rec_total_by_year[a, "Common_name"] &
                    data_total_by_year[, "Year"] == oto_rec_total_by_year[a, "Year"] &
                    data_total_by_year[, "State"] == oto_rec_total_by_year[a, "State"] &
                    data_total_by_year[, "Source"] == oto_rec_total_by_year[a, "Source"])
      if(length(find) > 0 ){
        data_total_by_year[find, "total_otoliths"] <- oto_rec_total_by_year[a, "total_otoliths"]        
      }
    }
    
    for(a in 1:dim(oto_rec_total)[1]){
      find <- which(data_total[, "Common_name"] == oto_rec_total[a, "Common_name"] &
                    data_total[, "State"] == oto_rec_total[a, "State"] &
                    data_total[, "Source"] == oto_rec_total[a, "Source"])
      if (length(find) > 0){
        data_total[find, "total_otoliths"] <- oto_rec_total[a, "total_otoliths"]        
      }
    }
  }
  
  if(!is.null(wa_com_oto)){
    find <- which(data$State == "Washington" & data$Source == "PacFIN")
    data[find, "Otolith"] <- 0
    
    group_vars <- c("Common_name", "State", "Source")
    oto_com_total <-  
      wa_com_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count),
        n_years = dplyr::n_distinct(Year)
      )
    oto_com_total <- as.data.frame(oto_com_total)
    oto_com_total <- oto_com_total[oto_com_total$total_otoliths > 0, ]
    
    group_vars <- c("Common_name", "State", "Source", "Year")
    oto_com_total_by_year <-  
      wa_com_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count)
      )
    oto_com_total_by_year <- as.data.frame(oto_com_total_by_year)
    oto_com_total_by_year <- oto_com_total_by_year[oto_com_total_by_year$total_otoliths > 0 , ]
    
    for(a in 1:dim(oto_com_total_by_year)[1]){
      find <- which(data_total_by_year[, "Common_name"] == oto_com_total_by_year[a, "Common_name"] &
                      data_total_by_year[, "Year"] == oto_com_total_by_year[a, "Year"] &
                      data_total_by_year[, "State"] == oto_com_total_by_year[a, "State"] &
                      data_total_by_year[, "Source"] == oto_com_total_by_year[a, "Source"])
      if (length(find) > 0){
        data_total_by_year[find, "total_otoliths"] <- oto_com_total_by_year[a, "total_otoliths"]  
      }
    }
    
    for(a in 1:dim(oto_com_total)[1]){
      find <- which(data_total[, "Common_name"] == oto_com_total[a, "Common_name"] &
                    data_total[, "State"] == oto_com_total[a, "Common_name"] & 
                    data_total[, "Source"] == oto_com_total[a, "Source"])
      if(length(find) > 0){
        data_total[find, "total_otoliths"] <- oto_com_total[a, "total_otoliths"]       
      }
    }
  }
  
  if(!is.null(ca_com_oto)){
    group_vars <- c("Common_name", "State", "Source")
    oto_com_total <-  
      ca_com_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count),
        n_years = dplyr::n_distinct(Year)
      )
    oto_com_total <- as.data.frame(oto_com_total)
    oto_com_total <- oto_com_total[oto_com_total$total_otoliths > 0, ]
    
    group_vars <- c("Common_name", "State", "Source", "Year")
    oto_com_total_by_year <-  
      ca_com_oto |>
      dplyr::group_by_at(group_vars) |>
      dplyr::summarise(
        total_otoliths = sum(Count)
      )
    oto_com_total_by_year <- as.data.frame(oto_com_total_by_year)
    oto_com_total_by_year <- oto_com_total_by_year[oto_com_total_by_year$total_otoliths > 0 , ]
    
    for(a in 1:dim(oto_com_total_by_year)[1]){
      find <- which(data_total_by_year[, "Common_name"] == oto_com_total_by_year[a, "Common_name"] &
                      data_total_by_year[, "Year"] == oto_com_total_by_year[a, "Year"] &
                      data_total_by_year[, "State"] == oto_com_total_by_year[a, "State"] &
                      data_total_by_year[, "Source"] == oto_com_total_by_year[a, "Source"])
      if (length(find) > 0){
        data_total_by_year[find, "total_otoliths"] <- oto_com_total_by_year[a, "total_otoliths"]  
      }
    }
    
    for(a in 1:dim(oto_com_total)[1]){
      find <- which(data_total[, "Common_name"] == oto_com_total[a, "Common_name"] &
                      data_total[, "State"] == oto_com_total[a, "Common_name"] & 
                      data_total[, "Source"] == oto_com_total[a, "Source"])
      if(length(find) > 0){
        data_total[find, "total_otoliths"] <- oto_com_total[a, "total_otoliths"]       
      }
    }
  }
  
  data_total[, c("total_lengths", "total_ages", "total_otoliths")][is.na(data_total[, c("total_lengths", "total_ages", "total_otoliths")])] <- 0
  data_total_by_year[, c("total_lengths", "total_ages", "total_otoliths")][is.na(data_total_by_year[, c("total_lengths", "total_ages", "total_otoliths")])] <- 0
  
  write.csv(data_total, file.path(dir, "data_summaries.csv"), row.names = FALSE)
  write.csv(data_total_by_year, file.path(dir, "data_summaries_by_year.csv"), row.names = FALSE)
  write.csv(data_total_by_year[data_total_by_year$State == "Washington" & data_total_by_year$Source == "PacFIN", ],
            file.path(dir, "data_summaries_by_year_washington.csv"), row.names = FALSE)
  write.csv(data_total_by_year[data_total_by_year$State == "California" & data_total_by_year$Source == "PacFIN", ],
            file.path(dir, "data_summaries_by_year_california.csv"), row.names = FALSE)
  write.csv(data_total_by_year[data_total_by_year$State == "Oregon" & data_total_by_year$Source == "PacFIN", ],
            file.path(dir, "data_summaries_by_year_oregon.csv"), row.names = FALSE)
  
  return(data_total_by_year)
}