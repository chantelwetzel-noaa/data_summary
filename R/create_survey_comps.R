
create_survey_comps <- function(
	dir, 
	species, 
	common_name, 
	load_catch = FALSE,
	load_bio = FALSE)
{

	pngfun <- function(wd, file,w=7,h=7,pt=12){
	  file <- file.path(wd, file)
	  cat('writing PNG to',file,'\n')
	  png(filename=file,
	      width=w,height=h,
	      units='in',res=300,pointsize=pt)
	}
	
	bw   <- adjustcolor(1, alpha.f<- 0.10)
	gr   <- adjustcolor(1, alpha.f<- 0.50)
	red  <- adjustcolor("red", alpha.f<- 0.10)
	blue <- adjustcolor("blue", alpha.f<- 0.10)
	
	if (load_catch == FALSE){
		# Pull the catches and biological data by species
		catch = PullCatch.fn(
			Name = common_name, 
			SurveyName = "NWFSC.Combo", 
			SaveFile = TRUE, 
			Dir = dir) 
		save(catch, file = paste0(dir, "/", common_name, "_catch.rda"))
	} else {
		files = list.files(dir)
		find = grep("Catch__NWFSC.Combo", files)
		load(paste0(dir, "/", files[find[1]]))
		catch = Out
	}

	if (load_bio == FALSE){
		bio   = PullBio.fn(
			Name = common_name, 
			SurveyName = "NWFSC.Combo", 
			SaveFile = TRUE, 
			Dir = dir)
		save(bio, file = paste0(dir, "/", common_name, "_bio.rda"))
	} else {
		files = list.files(dir)
		find = grep("Bio_All_NWFSC.Combo", files)
		load(paste0(dir, "/", files[max(find)]))
		bio = Data
	}

	# remove = which(catch$Trawl_id == 202103017117)
	# catch = catch[-remove,]
	# remove = which(bio$Trawl_id == 202103017117)
	# bio = bio[-remove,]

	# Create a generic strata
	strata = CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "shallow_ca",  
                           			   "deep_wa", "deep_or", "deep_ca"), 
                           depths.shallow = c( 55,   55,   55,  183,  183, 183),
                           depths.deep    = c(183,  183,  183,  549,  549, 549),
                           lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                           lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))

	if (common_name %in% c("Dover sole", "longspine thornyhead", "shortspine thornyhead", "sablefish")){
	strata = CreateStrataDF.fn(names=c("shallow_wa", "shallow_or", "shallow_ca", 
									   "medium_wa", "medium_or", "medium_ca", 
                           			   "deep_wa", "deep_or", "deep_ca"), 
                           depths.shallow = c( 55,   55,   55,  183,  183, 183, 549,  549, 549),
                           depths.deep    = c(183,  183,  183,  549,  549, 549, 1280, 1280, 1280),
                           lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
                           lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
	}

	## Calculate the observations by strata
	num.strata = CheckStrata.fn(
		dat = catch, 
		strat.vars = c("Depth_m","Latitude_dd"), 
		strat.df = strata, 
		verbose = TRUE)

	biomass = Biomass.fn(
		dir = dir,
		dat = catch,  
		strat.df = strata, 
		printfolder = "forSS", 
		outputMedian = T) 

	PlotBio.fn(dir = dir, dat = biomass, 
		main = paste("NWFSC WCGBTS:", common_name), 
		save_name = species, 
		dopng = TRUE)

	PlotBioStrata.fn(dir = dir, dat = biomass, mfrow.in = c(3,3), 
		gap = 0.01, sameylim = TRUE, ylim = c(0, 40), dopng = TRUE)

	write.csv(num.strata$PositiveTows, 
	 file = file.path(dir, "forSS", paste0(species, "_positive_tows.csv")), row.names = FALSE)


	## Calculate the observations by length and age
	if(length(bio$Length_cm) > 0) {
		
		ind = !is.na(bio$Length_cm)
		min_len = ifelse(floor(min(bio$Length_cm[ind])) > 10, floor(min(bio$Length_cm[ind])) , 10)
		max_len = floor(max(bio$Length_cm[ind]))
		bin_size = ifelse(max_len - min_len > 60, 4, 2)
		len.bins = seq(min_len , max_len - 2*bin_size, bin_size)
		
		# Summarize the observation and writ to csv
		n = GetN.fn(
			dat = bio, 
			type = "length", 
			species = "all", 
			output = "summary", 
			verbose = FALSE)

		write.csv(n[,1:5], 
			file = file.path(dir, "forSS", paste0(species, "_lengths_samples.csv")), 
			row.names = FALSE)

		# Calculate and plot the length-frequencies based on the default strata
		lfs <- SurveyLFs.fn(
			datL = bio, 
			datTows = catch,  
			strat.df = strata, 
			lgthBins = len.bins, 
			sex = 0, 
			verbose = FALSE)

		write.csv(lfs, 
			file = file.path(dir, "forSS", paste0(species, "_Lengths.csv")), row.names = FALSE)

		ymax = which(len.bins == (min_len + bin_size*8))
		PlotFreqData.fn(
			dir = dir, 
			dat = lfs, w = 10, h = 7, #ylim=c(0, (min_len + bin_size*9)), ymax = ymax, 
			save_name = species,
			main = paste("NWFSC WCGBTS:", common_name), 
			yaxs="i", 
			ylab="Length (cm)",
			cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, 
			dopng = TRUE)

	}

}


