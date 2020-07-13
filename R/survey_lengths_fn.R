

survey_lengths_fn <- function(bio, catch, all.dir, name, table.dir, comp.dir, plot = "all"){

  ## Calculate the observations by length and age
  if(length(bio$Length_cm)>0){
     ind = !is.na(bio$Length_cm)
     min_len = ifelse(floor(min(bio$Length_cm[ind])) > 10, floor(min(bio$Length_cm[ind])) , 10)
     max_len = floor(max(bio$Length_cm[ind]))
     bin_size = ifelse(max_len - min_len > 60, 4, 2)
     len.bins = seq(min_len , max_len - 2*bin_size, bin_size)

     # Summarize the observation and writ to csv
     n = GetN.fn(dat = bio, type = "length", species = "all", output = summary, verbose = FALSE)
     write.csv(n[,1:5], file = paste0(table.dir, name, "_lengths_samples.csv"), row.names = FALSE)

     # Calculate and plot the length-frequencies based on the default strata
     lfs <- SurveyLFs.fn(datL = bio, datTows = catch,  strat.df = strata, lgthBins = len.bins, 
                                         gender = 0, verbose = FALSE)
     write.csv(lfs, file = paste0(table.dir, name, "_length_comps.csv"), row.names = FALSE)
     ymax = which(len.bins == (min_len + bin_size*8))

     PlotFreqData.fn(dir = comp.dir, dat = lfs, w = 10, h = 7, 
            main = paste0("NWFSC_WCGBTS: ", name), yaxs="i", ylab="Length (cm)",
            cex.main = 2, cex.axis = 1.5, cex.lab = 2, dopng = TRUE)
  }

  # Code below is moved into the plot_cpue function
  # CPUE by depth
  #pngfun(wd = comp.dir, file = paste0(name, '_NWFSC_CPUE_Depth.png'), h = 8.5)
  #ind = catch$cpue_kg_km2 != 0
  #plot(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), type = "n", pch = 20,  xlab = "Depth (m)", ylab = "log(CPUE kg/km2)", 
  #     main = paste0("NWFSC WCGBTS: ", name[a]), las = 1)
  #points(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
  #dev.off()
  
  # Code below is moved into the plot_cpue function
  # CPUE by latitude
  #pngfun(wd = comp.dir, file = paste0(name, '_NWFSC_CPUE_Lat.png'), h = 8.5)
  #ind = catch$cpue_kg_km2 != 0
  #plot(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), type = "n", pch = 20,  xlab = "Latitude", ylab = "log(CPUE kg/km2)", 
  #     main = paste0("NWFSC WCGBTS: ", name), las = 1)
  #points(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
  #dev.off()

  # Cut out in lieu of specific call
  # Create map
  #PlotMap.fn(dir = comp.dir, dat = catch, main = paste("NWFSC WCGBTS: ", name), dopng = TRUE)
  #file.rename(paste0(comp.dir, "NWFSC_combo_CPUE_Map_Year.png"), paste0(comp.dir, name,"_NWFSC_combo_CPUE_Map_Year.png"))
  #file.rename(paste0(comp.dir, "NWFSC_combo_CPUE_Map.png"), paste0(comp.dir, name,"_NWFSC_combo_CPUE_Map.png"))

  # Being done in the plot_summary_fn instead
  # Calculate values for histogram plots for % positive tows, lengths and ages sampled
  # pngfun(wd = comp.dir, file = paste0(name, '_survey_stats.png'), h = 12, w = 9)
  # par(mfrow = c(3,1), oma = c(1,1,0,0)); cex.axis = 1.5; cex.names = 1.5; main = 2
  # pos  = aggregate(total_catch_wt_kg~Year, data = catch, function(x) sum(x > 0))
  # tow  = aggregate(Trawl_id~Year, data = catch, function(x) length(unique(x)))
  # barplot(pos$total_catch_wt_kg/tow$Trawl_id, cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0,1), main = reg.name[a], xlab = "Year", ylab = "% Positive Tows", names.arg = min(catch$Year):max(catch$Year), col = 'purple')
  # abline(h = mean(pos$total_catch_wt_kg/tow$Trawl_id), lty = 2, col = 'grey18')
  # find = grep(name, oto.dat$common_name, ignore.case = TRUE)
  # tmp = oto.dat[find,]
  # tab = aggregate(length_cm~year, data = tmp, FUN = function(x) sum(x > 0) )
  # ymax = round_any(max(tab$length_cm), 100, f = ceiling)
  # barplot(tab$length_cm, ylim = c(0, ymax), cex.lab = cex.axis, cex.main = main, cex.names = cex.names, xlab = "Year", ylab = "# of Length Collected", names.arg = min(tab$year):max(tab$year), col = 'red')
  # abline(h = mean(tab$length_cm), lty = 2, col = 'grey18')
  # oto = aggregate(otosag_id~year, data = tmp, FUN = function(x) length(unique(x)))
  # barplot(oto$otosag_id/tab$length_cm, xlab = "Year", cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0, 1), ylab = "% of Otoliths Collected", names.arg = min(tab$year):max(tab$year), col = 'blue')
  # abline(h = mean(oto$otosag_id/tab$length_cm), lty = 2, col = 'grey18')
  # dev.off()
	
}