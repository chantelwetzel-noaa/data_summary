

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
    file.name = tolower(gsub(" ", "_", name))
    #Calculate values for histogram plots
    pngfun(wd = dir, file = paste0(file.name, '_hkl_stats.png'), h = 12, w = 9)
    par(mfrow = c(3,1), oma = c(1,1,0,0))
    sets = aggregate(Set.ID ~ YEAR, data = catch, FUN = function(x) length(unique(x)), drop = FALSE)
    pos = aggregate(SITENAME  ~ YEAR + COMNAME , data = catch, FUN = function(x) length(unique(x)), drop = FALSE)
    length   = aggregate(LENGTH  ~ YEAR + COMNAME , data = catch, FUN = function(x) length(x), drop = FALSE)
    age = aggregate(OTIENUM ~ YEAR + COMNAME, data = catch, FUN = function(x) sum(x != ""), drop = FALSE)
    pos[is.na(pos_sites$SITENAME), "SITENAME"] = 0 
    length[is.na(length$LENGTH), "LENGTH"] = 0 
    age[is.na(length$OTIENUM), "OTIENUM"] = 0 
    pos = pos[pos$COMNAME == name,] ; length = length[length$COMNAME == name,] ; age = age[age$COMNAME == name,]  
    yr = min(catch$YEAR):max(catch$YEAR)
    barplot(pos$SITENAME/sets$Set.ID, cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0, 1), main = reg.name, xlab = "Year", ylab = "% Positive Sites", names.arg = yr, col = 'purple')
    abline(h = mean(pos$SITENAME/sets$Set.ID), lty = 2, col = 'grey18')

    ymax = plyr::round_any(max(length$LENGTH, rm.na = TRUE), 100, f = ceiling)
    barplot(length$LENGTH, ylim = c(0, ymax), cex.lab = cex.axis, cex.main = main, cex.names = cex.names, xlab = "Year", ylab = "# of Length Collected", names.arg = yr, col = 'red')
    abline(h = mean(length$LENGTH), lty = 2, col = 'grey18')
    barplot(age$OTIENUM/length$LENGTH, xlab = "Year", cex.lab = cex.axis, cex.main = main, cex.names = cex.names, ylim = c(0, 1), ylab = "% of Otoliths/Lengths", names.arg = yr, col = 'blue')
    abline(h = mean(age$OTIENUM/length$LENGTH), lty = 2, col = 'grey18')

    dev.off()
  }

}