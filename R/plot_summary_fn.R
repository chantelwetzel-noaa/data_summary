

plot_summary_fn <- function(dir, name, catch, all.dat, survey)
{
  reg.name = gsub("_", " ", name)
  cex.axis = 1.5; cex.names = 1.5; main = 2

  if (survey == "trawl"){
    #Calculate values for histogram plots
    pngfun(wd = dir, file = paste0(name, '_survey_stats.png'), h = 12, w = 9)
    par(mfrow = c(3,1), oma = c(1,1,0,0))
    pos  = aggregate(total_catch_wt_kg~Year, data = catch, function(x) sum(x > 0))
    tow  = aggregate(Trawl_id~Year, data = catch, function(x) length(unique(x)))
    barplot(pos$total_catch_wt_kg/tow$Trawl_id, cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0,1), main = reg.name, xlab = "Year", ylab = "% Positive Tows", names.arg = min(catch$Year):max(catch$Year), col = 'purple')
    abline(h = mean(pos$total_catch_wt_kg/tow$Trawl_id), lty = 2, col = 'grey18')
    find = grep(reg.name, all.dat$common_name, ignore.case = TRUE)
    tmp = all.dat[find,]
    tab = aggregate((width_cm | length_cm)~year, data = tmp, FUN = function(x) sum(x > 0) )
    ldf = data.frame(year = min(catch$Year):max(catch$Year), length = 0)
    y = 0
    for(ii in sort(unique(catch$Year))){ 
      y = y + 1
      ind = which(tab$year == ii)
      if(length(ind) > 0){
        ldf[y,2] = tab[ind,2]
      }
    }
    ymax = plyr::round_any(max(ldf$length, rm.na = TRUE), 100, f = ceiling)
    barplot(ldf$length, ylim = c(0, ymax), cex.lab = cex.axis, cex.main = main, cex.names = cex.names, xlab = "Year", ylab = "# of Length Collected", names.arg = min(tab$year):max(tab$year), col = 'red')
    abline(h = mean(ldf$length), lty = 2, col = 'grey18')
    
    oto = aggregate(otosag_id~year, data = tmp, FUN = function(x) length(unique(x))) 
    odf = data.frame(year = min(catch$Year):max(catch$Year), otolith = 0)
    y = 0
    for(ii in sort(unique(catch$Year))){ 
      y = y + 1
      ind = which(oto$year == ii)
      if(length(ind) > 0){
        odf[y,2] = oto[ind,2]
      }
    }
    barplot(odf$otolith/ldf$length, xlab = "Year", cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0, 1), ylab = "% of Otoliths/Lengths", names.arg = min(tab$year):max(tab$year), col = 'blue')
    abline(h = mean(odf$otolith/ldf$length), lty = 2, col = 'grey18')
    dev.off()
  }

  if (survey == "hkl"){
    #Calculate values for histogram plots
    pngfun(wd = dir, file = paste0(name, '_hkl_stats.png'), h = 12, w = 9)
    par(mfrow = c(3,1), oma = c(1,1,0,0))
    pos  = aggregate(COMNAME ~ YEAR, data = catch, function(x) sum(x == name))
    tow  = table(catch$YEAR)
    barplot(pos$COMNAME/tow, cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0,0.5), main = reg.name, xlab = "Year", ylab = "% Positive Hooks", names.arg = min(catch$YEAR):max(catch$YEAR), col = 'purple')
    abline(h = mean(pos$COMNAME/tow), lty = 2, col = 'grey18')
    find = grep(name, catch$COMNAME, ignore.case = TRUE)
    tmp = catch[find,]
    ldf = data.frame(year = min(catch$YEAR):max(catch$YEAR), length = 0)
    y = 0
    for(ii in sort(unique(catch$YEAR))){ 
      y = y + 1
      ind = which(tmp$YEAR == ii)
      if(length(ind) > 0){
        ldf[y,2] = sum(!is.na(tmp[ind,"LENGTH"]))
      }
    }
    ymax = plyr::round_any(max(ldf$length, rm.na = TRUE), 100, f = ceiling)
    barplot(ldf$length, ylim = c(0, ymax), cex.lab = cex.axis, cex.main = main, cex.names = cex.names, xlab = "Year", ylab = "# of Length Collected", names.arg = min(ldf$year):max(ldf$year), col = 'red')
    abline(h = mean(ldf$length), lty = 2, col = 'grey18')
    
    oto = aggregate(OTIENUM~YEAR, data = tmp, FUN = function(x) length(unique(x))) 
    odf = data.frame(year = min(catch$YEAR):max(catch$YEAR), otolith = 0)
    y = 0
    for(ii in sort(unique(catch$YEAR))){ 
      y = y + 1
      ind = which(oto$YEAR == ii)
      if(length(ind) > 0){
        odf[y,2] = oto[ind,2]
      }
    }
    barplot(odf$otolith/ldf$length, xlab = "Year", cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0, 1), ylab = "% of Otoliths/Lengths", names.arg = min(ldf$year):max(ldf$year), col = 'blue')
    abline(h = mean(odf$otolith/ldf$length), lty = 2, col = 'grey18')
    dev.off()
  }

}