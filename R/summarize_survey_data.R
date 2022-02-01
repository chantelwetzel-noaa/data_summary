

setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfsc-survey-summary")
here::i_am("R/summarize_survey_data.R")

summarize_survey_data(){
		
	species_names(){
		# read in species csv
	}

	pull_survey_data(){

		pull_wcgbts()

		pull_hkl()
		
		pull_maturity()
	}
	

	# should name data files so the next 2 functions
	# could be combined
	get_hkl_samples(){
		# table of year, length, otoliths by species
	}

	get_wcgbt_samples(){
		# table of year, pos tows, lengths, otoliths by species
	}

	plot_survey_data(){
		
	}
}