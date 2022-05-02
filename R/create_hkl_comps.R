
create_hkl_comps <- function(
	dir, 
	data,
	species, 
	common_name)
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
	
	obs = aggregate(length_cm~year, data, function(x) length(x))	
	write.csv(obs, 
	 file = file.path(dir, paste0(species, "_observations.csv")), row.names = FALSE)

	# put in the write column names
	data$Length_cm = data$length_cm
	data$Year = data$year
	data$Sex = "U"
	data$Age = data$age_years

	## Calculate the observations by length and age
	if(length(data$length_cm) > 0) {
		
		ind = !is.na(data$length_cm)
		min_len = ifelse(floor(min(data$length_cm[ind])) > 10, floor(min(data$length_cm[ind])) , 10)
		max_len = floor(max(data$length_cm[ind]))
		bin_size = ifelse(max_len - min_len > 60, 4, 2)
		len.bins = seq(min_len , max_len - 2*bin_size, bin_size)
		

		# Calculate and plot the length-frequencies based on the default strata
		lfs <- UnexpandedLFs.fn(
			datL = data, 
			lgthBins = len.bins, 
			sex = 0, 
			verbose = FALSE)

		prop_lfs = data.frame(lfs)
		prop_lfs[,-c(1:6)] = prop_lfs[,-c(1:6)] / prop_lfs[,"comps.Nsamp"]
		prop_lfs = cbind(prop_lfs, prop_lfs[, -c(1:6)])

		colnames(prop_lfs) = c("year", "month", "fleet", "sex",	
			"partition", "Nsamp", paste0("U", len.bins),
		     paste0("U1.", len.bins))

		write.csv(lfs, 
			file = file.path(dir, "forSS", paste0(species, "_Lengths.csv")), row.names = FALSE)

		ymax = which(len.bins == (min_len + bin_size*8))
		PlotFreqData.fn(
			dir = dir, 
			dat = prop_lfs, 
			w = 10, h = 7, #ylim=c(0, (min_len + bin_size*9)), ymax = ymax, 
			save_name = species,
			main = paste("NWFSC HKL:", common_name), 
			yaxs="i", 
			ylab="Length (cm)",
			cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, 
			dopng = TRUE)

	}

}
