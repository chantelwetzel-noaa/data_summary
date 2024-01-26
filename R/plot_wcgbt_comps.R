#' 
#' 
#' 
#' @param dir 
#' @param catch_data
#' @param biodata
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
plot_wcgbt_comps <- function(
	dir = here::here(), 
	wcgbt_catch,
	wcgbt_bio )
{
	
  
  species <- nwfscSurvey::GetSpp.fn(species = nwfscSurvey::GetSppDefault.fn())
  species[which(species$common == "rougheye rockfish"), "common"] <- "rougheye and blackspotted rockfish"
  
  species_to_plot <- unique(species[which(species[, "common"] %in% wcgbt_bio$Common_name), "common"])
  

  for(sp in species_to_plot){
  
    catch <- wcgbt_catch[wcgbt_catch$Common_name == sp, ]
    bio <- wcgbt_bio[wcgbt_bio$Common_name == sp, ]
  
	  # Create a generic strata
	  strata <-  nwfscSurvey::CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "shallow_ca",  
                             			   "deep_wa", "deep_or", "deep_ca"), 
                             depths.shallow = c( 55,   55,   55,  183,  183, 183),
                             depths.deep    = c(183,  183,  183,  549,  549, 549),
                             lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                             lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
  
	  if (sp %in% c("Dover sole", "longspine thornyhead", "shortspine thornyhead", "sablefish")){
	  strata <- nwfscSurvey::CreateStrataDF.fn(names = c("shallow_wa", "shallow_or", "shallow_ca", 
	  								                 "medium_wa", "medium_or", "medium_ca", 
                             			   "deep_wa", "deep_or", "deep_ca"), 
                             depths.shallow = c( 55,   55,   55,  183,  183, 183, 549,  549, 549),
                             depths.deep    = c(183,  183,  183,  549,  549, 549, 1280, 1280, 1280),
                             lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                             lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
	  }
  
	  #biomass <- nwfscSurvey::Biomass.fn(
	  #	dat = catch,  
	  #	strat.df = strata, 
	  #	outputMedian = TRUE) 
  
	  ## Calculate the observations by length and age
	  if(length(bio$Length_cm) > 0) {
	  	
	  	ind <- !is.na(bio$Length_cm)
	  	min_len <- ifelse(floor(min(bio$Length_cm[ind])) > 10, floor(min(bio$Length_cm[ind])) , 10)
	  	max_len <- floor(max(bio$Length_cm[ind]))
	  	bin_size <- ifelse(max_len - min_len > 60, 4, 2)
	  	len.bins <- seq(min_len , max_len - 2*bin_size, bin_size)
  
	  	bio$Sex = "U"
	  	if(sp %in% c("blue and deacon rockfish", "rougheye and blackspotted rockfish")){
	  	  catch <- catch |>
	  	    dplyr::distinct(Trawl_id, .keep_all = TRUE)
	  	  catch <- as.data.frame(catch)
	  	}
	  	
	  	# Calculate and plot the length-frequencies based on the default strata
	  	lfs <- nwfscSurvey::SurveyLFs.fn(
	  		datL = bio, 
	  		datTows = catch,  
	  		strat.df = strata, 
	  		lgthBins = len.bins, 
	  		sex = 0, 
	  		verbose = FALSE)
  
	  	nwfscSurvey::plot_comps(
	  	  dir = dir,
	  	  add_0_ylim = FALSE,
	  	  add_save_name = sp,
	  	  data = lfs,
	  	  plot = 1
	  	)
	  	
	  	if(sum(0:4 %in% bio$Age) >  3 ){

	  	  if(sum(bio$Age == 2, na.rm = TRUE) > 0){
	  	    max_age_len <- quantile(bio[which(bio$Age == 4), "Length_cm"], 0.99)
	  	  }	  	  
	  	  if(sum(bio$Age == 1, na.rm = TRUE) > 0){
	  	    max_age_len <- quantile(bio[which(bio$Age == 3), "Length_cm"], 0.99)
	  	  }
	  	  if(sum(bio$Age == 0, na.rm = TRUE) > 0){
	  	    max_age_len <- quantile(bio[which(bio$Age == 2), "Length_cm"], 0.99)
	  	  }
	  	  
	  	  sub_bio <- bio[bio$Length_cm <= max_age_len, ]
	  	  ind <- !is.na(sub_bio$Length_cm)
	  	  min_len <- ifelse(floor(min(sub_bio$Length_cm[ind])) > 10, floor(min(sub_bio$Length_cm[ind])) , 10)
	  	  max_len <- floor(max(sub_bio$Length_cm[ind]))
	  	  bin_size <- ifelse(max_len - min_len > 60, 4, 2)
	  	  len.bins <- seq(min_len , max_len + 2, bin_size)
	  	  
	  	  # Calculate and plot the length-frequencies based on the default strata
	  	  lfs <- nwfscSurvey::SurveyLFs.fn(
	  	    datL = sub_bio, 
	  	    datTows = catch,  
	  	    strat.df = strata, 
	  	    lgthBins = len.bins, 
	  	    sex = 0, 
	  	    verbose = FALSE)
	  	  
	  	  nwfscSurvey::plot_comps(
	  	    dir = dir,
	  	    add_0_ylim = FALSE,
	  	    add_save_name = paste0(sp, "_young_fish_"),
	  	    data = lfs,
	  	    plot = 1
	  	  )
	  	  
	  	}
	  } # lengths loop
  } # species loop
}


