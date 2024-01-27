#' 
#' @param data
#' @param source 
#' @param state
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
#'
format_cdfw_otolith_files <- function(data, source = NA, state = NA){
  
  data[is.na(data)] <- 0
  find <- grep("species", colnames(data),  ignore.case = TRUE)
  
  all_cols <- 1:dim(data)[2]
  cols_to_rotate <- all_cols[-find]
  
  col_names <- gsub("X", "", colnames(data))
  
  data_out <- NULL
  for(a in cols_to_rotate){
    tmp <- cbind(NA, data[, find], col_names[a], data[, a])
    data_out <- rbind(data_out, tmp)
  }
  data_out[is.na(data_out[, 4]), 4] <- 0
  
  out <- as.data.frame(data_out)
  colnames(out) <- c("Common_name", "Species", "Year", "Otolith")
  out$Year <- as.numeric(out$Year)
  out$Otolith <- as.numeric(out$Otolith)
  
  species_names <- get_species_list()
  
  for (a in 1:dim(species_names)[1]){
    find <- grep(species_names[a, "name"], out[, "Species"], ignore.case = TRUE)
    out[find, "Common_name"] <- species_names[a, "use_name"]
  }
  
  out <- out[which(out$Common_name %in% species_names), ]
  
  out$State <- state
  out$Source <- source
  out$State_Source <- paste0(source, "-", out$State)
  
  out$set_tow_id <- 0
  out$Lengthed <- 0
  out$Age <- NA
  out$Fleet <- NA
  
  return(out)
  
}