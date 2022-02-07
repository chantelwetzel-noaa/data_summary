#' 
#' 
#' @param survey A character entry from one of the following options that
#' specifies which survey to pull the data for:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf,
#'   * NWFSC.Hypoxia,
#'   * NWFSC.Santa.Barb.Basin,
#'   * NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both are not working),
#'   * NWFSC.Video.
#' @param save A logical value specifying whether or not the the data should
#' be saved to a file in `Dir`. Must change from the default of `FALSE` to save a file.
#' @param return
#' @param verbose A logical value specifying whether or not to pring out
#' message statements to the screen while using this function.
#' The default is to print all messages, i.e., `verbose = TRUE`.
#'
#' @author Chantel Wetzel based on code by John Wallace
#' @export
#' @md
#'
#' @import nwfscSurvey
#' @import here
#'
pull_wcgbts <- function(survey = 'NWFSC.Combo', save = FALSE, return = TRUE, verbose = FALSE){

	species_names <- all_species()

	dir <- here::here("data")
	catch <- nwfscSurvey::PullCatch.fn(SurveyName = survey, 
				SaveFile = save, Dir = dir, verbose = verbose) 

	keep <- which(catch$Common_name %in% unique(species_names))
	sub_catch <- catch[keep, ]
	sub_catch$positive_tow <- 0
	sub_catch[sub_catch$total_catch_wt_kg > 0, "positive_tow"] <- 1 

	bio <- nwfscSurvey::PullBio.fn(SurveyName = survey, 
				SaveFile = save, Dir = dir, verbose = verbose) 
	
	# deal with the naming of vermilion in select years
	bio[which(bio$Common_name == "vermilion rockfish"), "Common_name"] = 'vermilion and sunset rockfish'

	# filter down to only the species considered in the prioritization process
	keep <- which(bio$Common_name %in% unique(unlist(species_names)))
	sub_bio <- bio[keep, ]

	save(sub_catch, file = file.path(dir, "catch_wcgbt_filtered.Rda"))
	save(sub_bio, file = file.path(dir, "bio_wcgbt_filtered.Rda"))	

	if(return){
		wcgbts <- list()
		wcgbts$catch <- sub_catch
		wcgbts$bio <- sub_bio
		return(wcgbts)
	}
}

