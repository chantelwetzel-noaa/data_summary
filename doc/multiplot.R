
multiplot <- function(species_name){
	
	glue::glue(" \n# {species_name} \n \n") %>% cat()

	sub_data <- data %>%
		filter(name == species_name)
	sources <- get_source(data = sub_data)
	data_to_show <- unique(sub_data$use_data)

	wcgbt_filename = GetSppDefault.fn()
	#remove = which(wcgbt_filename %in% c("bank_rockfish", "brown_rockfish", "copper_rockfish", "quillback_rockfish", "squarespot_rockfish"))
	#wcgbt_filename = wcgbt_filename[-remove]
	wcgbt_name = gsub("_", " ", firstup(wcgbt_filename))
	wcgbt_name[wcgbt_name == "Chilipepper"] = "Chilipepper rockfish"
	wcgbt_name[wcgbt_name == "Rougheye rockfish"] = "Rougheye and blackspotted rockfish"

	hkl_filename = gsub(" ", "_", tolower(get_hkl_species()))
	hkl_name = gsub("_", " ", firstup(hkl_filename))
	hkl_name[hkl_name == "Chilipepper"] = "Chilipepper rockfish"
	hkl_name[hkl_name == "Vermilion rockfish"] = "Vermilion and sunset rockfish"

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

	if(data_to_show == "com") {
		glue::glue("only commercial fisheries. ") %>% cat()
	}
	if(data_to_show == "rec") {
		glue::glue("only recreational fisheries. ") %>% cat()
	}	

	#glue::glue(" \n \n \n \n") %>% cat()
	glue::glue(" \n \n") %>% cat()
	glue::glue(" \n \n") %>% cat()	
	cat("\n")
 	cat("\n")

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
 			cat("\n")
 			cat("\n")
			#glue::glue(" \n \n") %>% cat()
			#glue::glue(" \n \n") %>% cat()	
 		} # fishery loop		
	} # data source loop        
	

	for(tt in 1:length(sources)) {
		cat("\n")
		cat("\n")
		glue::glue(" \n## {sources[tt]} \n \n") %>% cat()

		if(sources[tt] %in% c("commercial fisheries", "recreational fisheries")){			
			use_state <- as.matrix(unique(sub_data[sub_data$data_type == sources[tt], "state"]))
			for (aa in 1:length(use_state)) {
				tmp_state <- use_state[aa]
				find <- which(sub_data$data_type == sources[tt] & sub_data$state == tmp_state)
				tab = data.frame(sub_data[find, ])
				state <- ifelse(tmp_state == "C", "California", 
 					     ifelse(tmp_state == "O", "Oregon", "Washington"))
				col_names = c("State", "Year", "Sexed Fish", "Unsexed Fish", "Lengths", "Ages", "Unread Otoliths")
				caption = paste0("Data collected annually from the ", sources[tt], " in ", state,".")
				rownames(tab) = NULL
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
			col_names = c("Year", "Positive Tows", "Sexed Fish", "Unsexed Fish", "Lengths", "Ages", "Unread Otoliths")
			if (sources[tt] == "NWFSC HKL"){
				col_names = c("Year", "Positive Sites", "Sexed Fish", "Unsexed Fish", "Lengths", "Ages", "Unread Otoliths")
			}
			caption = paste0("Data collected annually from the ", sources[tt], " survey.")
			rownames(tab) = NULL
			t = table_format(x = tab[,3:9], 
				caption = caption,
				label = 'tab-label',
				col_names = col_names,
				align = 'l')
			print(t)
			#cat("\n")
 			#cat("\n")
			#print("<P style='page-break-before: always'>")
			cat("\n\n\\pagebreak\n")
		}

		if(sources[tt] %in% "NWFSC WCGBT" & species_name %in% wcgbt_name){
			ind = which(wcgbt_name == species_name)
			add_figure(
				filein = file.path(vast_dir, wcgbt_filename[ind], "VASTWestCoast_Index_2021.png"), 
				caption = "Index of abundance from the NWFSC WCGBT survey from 2003-2021 (excluding 2020) for the full area (black line with circles) with area-specific estimates (shown in either red, purple, or blue). A loess smoother line was fit to full area estimate and is denoted by the grey dashed line.",
				label = paste0('index-', ind),
				width = 57,
				height = 57)

			add_figure(
				filein = file.path(vast_dir, wcgbt_filename[ind], "plots", paste0(wcgbt_filename[ind], "_Length_Frequency.png")), 
				caption = "Length (cm) compostion data from the NWFSC WCGBT survey. Size of the circles within a year indicate higher (larger circles) and lower (smaller circles) proportion observed by length bin",
				label = paste0('lengths-', ind),
				width = 55,
				height = 55)

			#print("<P style='page-break-before: always'>") #pagebreak())
			cat("\n\n\\pagebreak\n")
			#cat("\n")
 			#cat("\n")
		}

		if(sources[tt] %in% "NWFSC HKL" & species_name %in% hkl_name){
			ind = which(hkl_name == species_name)
			add_figure(
				filein = file.path(hkl_dir, hkl_filename[ind], paste0("HKL_GLM_", hkl_filename[ind], ".png")), 
				caption = "Index of abundance from the NWFSC HKL survey from 2003-2021 (excluding 2020). A loess smoother line was fit to full area estimate and is denoted by the grey dashed line.",
				label = paste0('index-hkl-', ind),
				width = 55,
				height = 55)

			add_figure(
				filein = file.path(hkl_dir, hkl_filename[ind], "plots", paste0(hkl_filename[ind], "_Length_Frequency.png")), 
				caption = "Length (cm) compostion data from the NWFSC HKL survey. Size of the circles within a year indicate higher (larger circles) and lower (smaller circles) proportion observed by length bin",
				label = paste0('lengths-hkl-', ind),
				width = 55,
				height = 55)

			#print("<P style='page-break-before: always'>") #pagebreak())
			cat("\n\n\\pagebreak\n")
		}	

	}
  
  	cat("  \n  \n")
}