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
clean_wcgbt_catch <- function(dir = here::here("data-processed"), species, data){
  

  # Catch data
  catch <- data$catch
  
  # Note the only complex species with multiple entry names found on 12/13/2023 were:
  # blue / deacon rockfish (n = 1) and deacon rockfish (n = 1) in different tows in different years 2019 and 2022
  # rougheye and blackspotted rockfish (n = 510) and rougheye rockfish (n = 47) were the years do not overlap with
  # only rougheye rockfish occurring in 2021 and 2022
  # I did not find multiple name entries for vermilion/sunset, chilipepper (vs. chilipepper rockfish),
  # gopher rockfish of Pacific spiny dogfish
  
  catch$positive_tow <- 0
  catch[catch$total_catch_wt_kg > 0, "positive_tow"] <- 1 
  catch$set_tow_id <- catch$Trawl_id
  
  # Change rougheye rockfish to rougheye and blackspotted rockfish
  find = which(catch$Common_name %in% "rougheye rockfish")
  catch[find, "Common_name"] <- "rougheye and blackspotted rockfish"
  # Change the name and integrate the data together
  find <- which(catch$Common_name %in% c("blue / deacon rockfish") & catch$positive_tow == 1)
  trawl_id <- catch[find, "Trawl_id"]
  find_trawl <- which(catch$Common_name %in% c("deacon rockfish") & catch$Trawl_id == trawl_id)
  catch <- catch[-find_trawl, ]
  find <- which(catch$Common_name %in% c("blue / deacon rockfish", "deacon rockfish"))
  catch[find, "Common_name"] <- "blue and deacon rockfish"
  find <- which(catch$Common_name %in% c("tree rockfish"))
  catch[find, "Common_name"] <- "treefish"  
  
  
  catch <- catch[which(catch$Common_name %in% unique(species[,"name"])), ]
  
  catch$Source <- "NWFSC WCGBT" 
  catch$State <- ifelse(
    catch$Latitude_dd > 46, "WA", ifelse(
      catch$Latitude_dd > 42 & catch$Latitude_dd < 46, "OR", ifelse(
        catch$Latitude_dd < 42 & catch$Latitude_dd < 40.17, "NCA", ifelse(
          catch$Latitude_dd < 40.17 & catch$Latitude_dd > 34.47, "CCA", "SCA"
        )
      )
    )
  )
  
  catch_areas <- 
    catch |>
    dplyr::filter(positive_tow == 1) |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      min_depth = min(Depth_m),
      min_depth_01 = quantile(Depth_m, 0.01),
      max_depth = max(Depth_m),
      max_depth_99 = quantile(Depth_m, 0.99),
      min_lat = min(Latitude_dd),
      min_lat_01 = quantile(Latitude_dd, 0.01), 
      max_lat = max(Latitude_dd),
      max_lat_99 = quantile(Latitude_dd, 0.99)      
    )

  write.csv(catch_areas, here::here("data-processed", "wcgbt_catch_areas.csv"), row.names = FALSE)
  save(catch, file = file.path(dir, "wcgbt_catch_filtered.Rdata"))
  return(catch)
}
