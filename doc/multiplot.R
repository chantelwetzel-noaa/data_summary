
multiplot <- function(species_name){
  
  section_name <- firstup(species_name)
	
	glue::glue(" \n# {section_name} {{-}}\n \n") |> cat( )

	sub_data <- data |>
		dplyr::filter(Common_name == species_name)
	
	sources <- get_source(data = sub_data)
	data_to_show <- unique(sub_data$sources_to_use)

	assess <- assess_data |> 
	  dplyr::filter(species == species_name)
	
	ass_yr <- ifelse(is.na(assess$year) & assess$type == "data-limited", 2010, assess$year)

	if(assess$type %in% c("benchmark", "update", "data-moderate", "data-limited")) {
	  a_an <- ifelse(assess$type == "update", "an", "a")
		glue::glue('The most recent assessment of {species_name} ',
			'was {a_an} {assess$type} assessment conducted in {ass_yr}. '
 			) |> cat()	
 	} else { 
		glue::glue('To date, no assessment or analysis has been conducted on {species_name}. '
 			) |> cat()	
 	}

 	glue::glue("Across available data, {species_name} ",
 		"have been observed and sampled by ") |> cat()	

	if(data_to_show == "all"){
		glue::glue("both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys. ") |> cat()
	}

	if(data_to_show == "com_wcgbt_hkl") {
		glue::glue("commercial fisheries and the NWFSC WCGBT and HKL surveys. ") |> cat()
	}
		
	if(data_to_show == "rec_wcgbt_hkl"){
		glue::glue("recreational fisheries and the both the NWFSC WCGBT and HKL surveys. ") |> cat()
	}
	
	if(data_to_show == "com_rec_wcgbt"){
		glue::glue("both the commercial and recreational fisheries and the NWFSC WCGBT survey. ") |> cat()
	}

	if(data_to_show == "com_rec_hkl"){
		glue::glue("both the commercial and recreational fisheries and the NWFSC HKL survey. ") |> cat()
	}

	if(data_to_show == "com_wcgbt") {
		glue::glue("commercial fisheries and the NWFSC WCGBT survey. ") |> cat()
	}

	if(data_to_show == "com_hkl") {
	    glue::glue("commercial fisheries and the NWFSC HKL survey. ") |> cat()
	}
	if(data_to_show == "com_rec") {
		glue::glue("both commercial and recreational fisheries. ") |> cat()
	}
	if(data_to_show == "rec_hkl") {
		glue::glue("recreational fisheries and the NWFSC HKL survey. ") |> cat()
	}	
	if(data_to_show == "rec_wcgbt") {
		glue::glue("recreational fisheries and the NWFSC WCGBT survey. ") |> cat()
	}

	if(data_to_show == "com") {
		glue::glue("only commercial fisheries. ") |> cat()
	}
	if(data_to_show == "rec") {
		glue::glue("only recreational fisheries. ") |> cat()
	}	
 	
 	tows_per_year <- sets_per_year <- 0
 	if("NWFSC WCGBT" %in% sub_data$Source){
 	  tows_per_year <- round(
 	    sum(sub_data[sub_data$Source == "NWFSC WCGBT", "set_tows"]) / length(c(2003:2019, 2021:2023)), 0)
 	}
 	if("NWFSC HKL" %in% sub_data$Source){
 	  sets_per_year <- round(
 	    sum(sub_data[sub_data$Source == "NWFSC HKL", "set_tows"]) / length(c(2004:2019, 2021:2023)), 0)
 	}
 	average_sets <- data.frame(
 	  Source = c("NWFSC WCGBT", "NWFSC HKL"),
 	  sets = c(tows_per_year, sets_per_year)
 	)
 	
 	if(sum(average_sets$sets != 0) == 1){
 	  if(average_sets$sets[1] != 0) {
 	    number <- average_sets[average_sets$Source == "NWFSC WCGBT", "sets"]
 	    glue::glue("The NWFSC WCGBT survey has an average of 
 	               {number} positive tows per year.") |> cat()
 	  }
 	  if(average_sets$sets[2] != 0) {
 	      number <- average_sets[average_sets$Source == "NWFSC HKL", "sets"]
 	      glue::glue("The NWFSC HKL survey has an average of 
 	                 {number} positive sets per year.") |> cat()
 	  }
 	}
 	
 	if(sum(average_sets$sets != 0) == 2){
 	    number <- average_sets[, "sets"]
 	    glue::glue("The NWFSC WCGBT has a coastwide average of {number[1]} positive tows per
 	    year and the NWFSC HKL survey has an average of {number[2]} positive sets per year the 
 	               area south of Point Conception in California.") |> cat()    
 	}

	#glue::glue(" \n \n \n \n") %>% cat()
	glue::glue(" \n \n") |> cat()
	glue::glue(" \n \n") |> cat()	
	cat("\n")
 	cat("\n")
 	
 	#total <- sub_data |>
 	#  dplyr::group_by(State, Source) |>
 	#  dplyr::summarise(
 	#    Lengths = sum(total_lengths),
 	#    Ages = sum(total_ages),
 	#    `Age Structures` = sum(total_otoliths)
 	#  ) |>
 	#  gt::gt() |>
 	#  gtsummary::tbl_summary() |>
 	#  gt::as_latex()
 	#print(total)
 	
 	total <- sub_data |>
 	  dplyr::group_by(State, Source) |>
 	  dplyr::summarise(
 	    Lengths = sum(total_lengths),
 	    Ages = sum(total_ages),
 	    `Age Structures` = sum(total_otoliths)
 	  )
 	total <- as.data.frame(total)
 	
 	
 	
 	caption <- glue::glue('Total number of available lengths, read ages, and unread age structures by data source and
 	state between 2000-2023 for {species_name}.')
 	t <- table_format(x = total, 
 	                  caption = caption,
                  	digits = 0, 
 	                  format.args = list(big.mark = ",", scientific = FALSE), 
 	                  custom_width = TRUE,
 	                  col_to_adjust = 1:5,
 	                  width = c('2cm', '3.5cm', '2cm', '2cm', '2cm'),
 	                  align = 'r')
 	print(t)
 	
 	
 	# This should be the data figures by state
 	add_figure(
 	   filein = file.path('C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/plots', paste0(species_name, "_state_compositions.png")), 
 	   caption = glue::glue("Total number of available lengths, read ages, and unread age structures by data source by year for {species_name}.  Note the y-axis is unique for the number of lengths plot row compared to 
 	                      the number of age and age structure plot rows."),
 	   label = paste0('sample-table-', species_name),
 	   width = 100,
 	   height = 100)
 	 
 	cat("\n\n\\pagebreak\n")

	if(file.exists(here::here("plots", paste0(species_name, "_length_frequency_sex_0.png")))){ 
		#add_figure(
		#	filein = file.path(vast_dir, wcgbt_filename[ind], "VASTWestCoast_Index_2021.png"), 
		#	caption = "Index of abundance from the NWFSC WCGBT survey from 2003-2021 (excluding 2020) for the full area (black line with circles) with area-specific estimates (shown in either red, purple, or blue). A loess smoother line was fit to full area estimate and is denoted by the grey dashed line.",
		#	label = paste0('index-', ind),
		#	width = 57,
		#	height = 57)

		add_figure(
			filein = file.path("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/plots", paste0(species_name, "_length_frequency_sex_0.png")), 
			caption = glue::glue("Length (cm) compostion data from the NWFSC West Coast Groundfish Bottom Trawl survey for {species_name}. 
			Size of the circles within a year indicate higher (larger circles) and lower (smaller circles) 
			proportion observed by length bin."),
			label = paste0('wcgbt-lengths-', species_name),
			width = 100,
			height = 100)

		cat("\n\n\\pagebreak\n")
	}
 	
 	if(species_name %in% age_species$Common_name){
 	  
 	  plot_files <- list.files(here::here("plots"), pattern = "_r4ss_frequency_sex_0.png")
 	  file <- plot_files[grep(species_name, plot_files)]
 	  age <- as.numeric(age_species[age_species$Common_name == species_name, "age_20"])
 	  age_text <- ifelse(age == 0, "", " or younger")
 	  
 	  print_text <- rec_text[rec_text$Common_name == species_name, "text"]
 	  
 	  add_figure(
 	    filein = file.path("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/plots", file), 
 	    caption = glue::glue("Length (cm) compostion data from the NWFSC West Coast Groundfish Bottom Trawl survey with 
 	                         fish associated associated with age {age}{age_text} for {species_name}. {print_text}"),
 	    label = paste0('wcgbt-young-lengths-', species_name),
 	    width = 100,
 	    height = 100)
 	  
 	  age_length <- aggregate(Length_cm ~ Age, 
 	                          wcgbt_bio[which(wcgbt_bio$Common_name == species_name & wcgbt_bio$Age <= age), ], 
 	                          function(x) quantile(x, 0.50, na.rm = TRUE))
 	  
 	  
 	  caption <- glue::glue("The median length (cm) associated with fish age {age}{age_text} for {species_name} based on aged fish from the NWFSC West Coast Groundfish Bottom Trawl survey.")
 	  t <- table_format(x = age_length, 
 	                    col_names = c("Age", "Length (cm)"),
 	                    caption = caption,
 	                    align = 'r')
 	  print(t)
 	  
 	  cat("\n\n\\pagebreak\n")
 	  
 	}
 	
 
 	
 	if(file.exists(here::here("plots", paste0(species_name, "_nwfsc_hkl_length_frequency_sex_0.png")))){ 
 	  
 	  add_figure(
 	    filein = file.path("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/plots", paste0(species_name, "_nwfsc_hkl_length_frequency_sex_0.png")), 
 	    caption = glue::glue("Length (cm) compostion data from the NWFSC Hook and Line survey for {species_name}. 
			Size of the circles within a year indicate higher (larger circles) and lower (smaller circles) 
			proportion observed by length bin."),
 	    label = paste0('hkl-lengths-', species_name),
 	    width = 100,
 	    height = 100)
 	  
 	  cat("\n\n\\pagebreak\n")
 	}

	#if(file.exists(here::here("plots-index", paste0(species_name, "_negbinom index.png")))){
	#	add_figure(
	#		filein = file.path("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/plots-index", paste0(species_name, "_negbinom index.png")),
	#		caption = glue::glue("Index of abundance from the NWFSC Hook and Line survey from 2003-2023 (excluding 2020) for {species_name}."),
	#		label = paste0('index-hkl-', species_name),
	#		width = 100,
	#		height = 100)

	#	#print("<P style='page-break-before: always'>") #pagebreak())
	#	cat("\n\n\\pagebreak\n")
	#}	
  
  	cat("  \n  \n")
}