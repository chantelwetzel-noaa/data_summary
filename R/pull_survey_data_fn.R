#
# @ dir directory to find or save pulled data
# @ name species name as needed by the nwfscSurvey code
# @ load.data TRUE/FALSE pull or load saved data 
#

pull_survey_data_fn <- function(dir, name, load.data = FALSE){
	if (load.data == FALSE){
		# Pull the catches and biological data by species
		catch = PullCatch.fn(Name = name, SurveyName = "NWFSC.Combo", SaveFile = TRUE, Dir = dir) 
		save(catch, file = paste0(dir, "/", name, "_catch.rda"))
		bio   = PullBio.fn(Name = name, SurveyName = "NWFSC.Combo", SaveFile = TRUE, Dir = dir)
		save(bio, file = paste0(dir, "/", name, "_bio.rda"))
	} else {
		files = list.files(dir)
		find = grep(paste0(name, "_catch"), files, ignore.case = TRUE)
		load(paste0(dir, "/", files[find]))
		
		find = grep(paste0(name, "_bio"), files, ignore.case = TRUE)
		load(paste0(dir, "/", files[find]))
	}

	out <- list()
	out$catch = catch
	out$bio = bio
	return(out)
}