
multiplot <- function(species_name){
	
	glue::glue(" \n# {species_name} \n \n") %>% cat()

	sub_data <- data %>%
		filter(name == species_name)
	sources <- get_source(data = sub_data)
	data_to_show <- unique(sub_data$use_data)

	assess <- assess_data %>% filter(species == species_name)
	ass_yr <- ifelse(is.na(assess$year) & assess$type == "data-limited", 2010, assess$year)

	if(assess$type %in% c("full", "update", "data-moderate", "data-limited")) {
		glue::glue('The most recent assessment of {species_name} ',
			'was a {assess$type} assessment conducted in {ass_yr}. '
 			) %>% cat()	
 	} else { 
		glue::glue('To date, no assessment or analysis has been conducted on {species_name}. '
 			) %>% cat()	
 	}

 	glue::glue("Across available data, {species_name} ",
 		"have been observed and sampled generally by ")	%>% cat()	

	if(data_to_show == "all"){
		glue::glue("both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys. ") %>% cat()
	}

	if(data_to_show == "com_wcgbt_hkl") {
		glue::glue("commercial fisheries and the NWFSC WCGBT and HKL surveys. ") %>% cat()
	}
		
	if(data_to_show == "rec_wcgbt_hkl"){
		glue::glue("recreational fisheries and the both the NWFSC WCGBT and HKL surveys. ") %>% cat()
	}
	
	if(data_to_show == "com_rec_wcgbt"){
		glue::glue("both the commercial and recreational fisheries and the NWFSC WCGBT survey. ") %>% cat()
	}

	if(data_to_show == "com_rec_hkl"){
		glue::glue("both the commercial and recreational fisheries and the NWFSC HKL survey. ") %>% cat()
	}

	if(data_to_show == "com_wcgbt") {
		glue::glue("commercial fisheries and the NWFSC WCGBT survey. ") %>% cat()
	}

	if(data_to_show == "com_hkl") {
	    glue::glue("commercial fisheries and the NWFSC HKL survey. ") %>% cat()
	}
	if(data_to_show == "com_rec") {
		glue::glue("both commercial and recreational fisheries. ") %>% cat()
	}
	if(data_to_show == "rec_hkl") {
		glue::glue("recreational fisheries and the NWFSC HKL survey. ") %>% cat()
	}	
	if(data_to_show == "rec_wcgbt") {
		glue::glue("recreational fisheries and the NWFSC WCGBT survey. ") %>% cat()
	}	

	glue::glue(" \n \n \n \n") %>% cat()

	for(ss in sources){	

		find = which(sub_data$data_type == ss)
		plural <- ifelse(ss %in% c("commercial fisheries", "recreational fisheries"), "have", "has")
		survey <- ifelse(ss %in% c("NWFSC WCGBT", "NWFSC HKL"), "survey ","")
		the <- ifelse(ss %in% c("NWFSC WCGBT", "NWFSC HKL"), "the ","")

		glue::glue('Across all years of available data, ', 
			 '{the}{ss} {survey}{plural} collected ',
 			 'a total of {length} length observations, ',
 			 '{age} age readings, ',
 			 'and {otolith} otoliths that are available to be aged. ',
 			 name = ss,
 			 length = prettyNum(sum(sub_data$lengthed[find]), big.mark = ",", scientific = FALSE),
 			 age = prettyNum(sum(sub_data$aged[find]), big.mark = ",", scientific = FALSE),
 			 otolith = prettyNum(sum(sub_data$otoliths[find]), big.mark = ",", scientific = FALSE)
 			) %>% cat()	

 		if(ss %in% c("commercial fisheries", "recreational fisheries")){
 			yr <- ifelse(ss == "commercial fisheries", 2000, 2003)
 			tmp <- sub_data[find, ]
 			find <- which(tmp$sample_year >= yr)
			#glue::glue('Since {yr}, {ss} have collected ',
 			#	 'a {length} length observations, ',
 			#	 '{age} age readings, ',
 			#	 'and {otolith} otoliths. ',
 			#	 name = ss,
 			#	 length = prettyNum(sum(tmp$lengthed[find]), big.mark = ",", scientific = FALSE),
 			#	 age = prettyNum(sum(tmp$aged[find]), big.mark = ",", scientific = FALSE),
 			#	 otolith = prettyNum(sum(tmp$otoliths[find]), big.mark = ",", scientific = FALSE)
 			#	) %>% cat() 

			tmp2 <- tmp[find,]
			use_state <- as.matrix(unique(tmp2[tmp2$data_type == ss, "state"]))

 			for (aa in 1:length(use_state)) {
 				tmp_state <- use_state[aa]
 				find <- which(tmp2$state == tmp_state)
 				state <- ifelse(tmp_state == "C", "California", 
 					     ifelse(tmp_state == "O", "Oregon", "Washington"))
 				
 				glue::glue('In {state}, since {yr}, ',
 				 'a total of {length} length observations, ',
 				 '{age} age readings, ',
 				 'and {otolith} otoliths have been collected. ',
 				 state = state,
 				 length = prettyNum(sum(tmp2[find, "lengthed"]), big.mark = ",", scientific = FALSE),
 				 age = prettyNum(sum(tmp2[find, "aged"]), big.mark = ",", scientific = FALSE),
 				 otolith = prettyNum(sum(tmp2$otoliths[find]), big.mark = ",", scientific = FALSE)
 				) %>% cat() 

 			} # state loop	
			glue::glue(" \n \n") %>% cat()
			glue::glue(" \n \n") %>% cat()	
 		} # fishery loop		
	} # data source loop        

	glue::glue(" \n \n") %>% cat()
	glue::glue(" \n \n") %>% cat()			

	for(tt in 1:length(sources)) {

		glue::glue(" \n## {sources[tt]} \n \n") %>% cat()

		if(sources[tt] %in% c("commercial fisheries", "recreational fisheries")){			
			use_state <- as.matrix(unique(sub_data[sub_data$data_type == sources[tt], "state"]))
			for (aa in 1:length(use_state)) {
				tmp_state <- use_state[aa]
				find <- which(sub_data$data_type == sources[tt] & sub_data$state == tmp_state)
				tab = as.data.frame(sub_data[find, ])
				state <- ifelse(tmp_state == "C", "California", 
 					     ifelse(tmp_state == "O", "Oregon", "Washington"))
				col_names = c("State", "Year", "Sexed Fish", "Unsexed Fish", "Lengths", "Ages", "Otoliths")
				caption = paste0("Data collected annually from the ", sources[tt], " in ", state,".")
				t = table_format(x = tab[,c(2,3,5:9)], 
					caption = caption,
					label = 'tab-label',
					col_names = col_names,
					align = 'l')
				print(t)
			}
		}

		if(sources[tt] %in% c("NWFSC WCGBT", "NWFSC HKL")){
			tab = as.data.frame(sub_data[sub_data$data_type == sources[tt], ])
			col_names = c("Year", "Positive Sites/Tows", "Sexed Fish", "Unsexed Fish", "Lengths", "Ages", "Otoliths")
			caption = paste0("Data collected annually from the ", sources[tt], " survey.")
			t = table_format(x = tab[,3:9], 
				caption = caption,
				label = 'tab-label',
				col_names = col_names,
				align = 'l')
			print(t)
		}		

	}
  
  	cat("  \n  \n")
}