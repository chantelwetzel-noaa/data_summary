#


plot_cpue_fn <- function(dir, name, catch, bio, plot = 1:3, n = 20000){
  # plot 1 = cpue by latitude and depth 
  # plot 2 = cpue by latitude by year
  # plot 3 = cpue by depth by year
  bw   <- adjustcolor(1, alpha.f<- 0.10)
  gr   <- adjustcolor(1, alpha.f<- 0.50)
  red  <- adjustcolor("red",  alpha.f<- 0.03)
  blue <- adjustcolor("blue", alpha.f<- 0.03)
  grey <- adjustcolor("grey", alpha.f<- 0.1)

  pn = 1
  if (pn %in% plot){
  
  pngfun(wd = dir, file = paste0(name, '_NWFSC_CPUE_Depth_Latitud.png'), h = 7, w = 12)
  par(mfrow = c(2, 2), mar = c(1, 1, 1, 1), oma = c(4,4,3,3))
    # CPUE by depth
    ind = catch$cpue_kg_km2 != 0
    plot(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), axes = F, type = "n", pch = 20,  xlab = "", ylab = "", las = 1)
    points(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
    axis(side = 2, las = 1); mtext(side = 2, "log(CPUE kg/km2)", line = 3); box()
    mtext(side = 3, paste0("NWFSC WCGBTS: ", name), outer = TRUE)

    # CPUE by latitude
    ind = catch$cpue_kg_km2 != 0
    plot(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), axes = F, type = "n", pch = 20,  xlab = "", ylab = "", las = 1)
    points(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
    box()

    # Length by Depth
    ind = bio$Length_cm > 0; bio$depth_bin = plyr::round_any(bio$Depth_m, 50, f = floor)
    plot(bio$Depth_m[ind], bio$Length_cm[ind], axes = F, type = "n", pch = 20,  xlab = "", ylab = "", las = 1)
    axis(side = 2, las = 1); mtext (side = 2, "Length (cm)", line = 3); box()
    axis(side = 1); mtext (side = 1, "Depth (m)", line = 3)
    female = which(bio$Sex[ind] == "F")
    if(length(female) > 0){
      pred.f = aggregate(Length_cm ~ depth_bin, data = bio[female,], FUN = mean) 
      if (length(female) > n) { female = sample(female, n) } 
      points(bio$Depth_m[female], bio$Length_cm[female], col= red, pch = 1, cex = 0.7)  }
    male = which(bio$Sex[ind] == "M")
    if(length(male) > 0 ) {
      pred.m = aggregate(Length_cm ~ depth_bin, data = bio[male,], FUN = mean) 
      if (length(male) > n) { male = sample(male, n)  }
      points(bio$Depth_m[male], bio$Length_cm[male], col= blue, pch = 1, cex = 0.7) }
    unknown = which(bio$Sex[ind] == "U")
    if(length(unknown) > 0 ) {
      pred.u = aggregate(Length_cm ~ depth_bin, data = bio[unknown,], FUN = mean) 
      if (length(unknown) > n) { unknown = sample(unknown, n)  }
      points(bio$Depth_m[unknown], bio$Length_cm[unknown], col= grey, pch = 1, cex = 0.7) }
    if(length(bio$Sex == "F") > 0) { lines(pred.f$depth_bin, pred.f$Length_cm, col = 'red4',  lwd = 2) }
    if(length(bio$Sex == "M") > 0) { lines(pred.m$depth_bin, pred.m$Length_cm, col = 'navy', lwd = 2) }
    if(length(bio$Sex == "U") > 0) { lines(pred.u$depth_bin, pred.u$Length_cm, col = 'grey40', lwd = 2) }

    # Length by latitude    
    bio$lat = floor(bio$Latitude_dd)
    plot(bio$Latitude_dd[ind], bio$Length_cm[ind], axes = F, type = "n", pch = 20,  xlab = "", ylab = "", las = 1)    
    axis(side = 1); mtext(side = 1, "Latitude", line = 3); box()
    if(length(bio$Sex[ind] == "F") > 0){
      find = which(bio$Sex[ind] == "F")
      pred.f = aggregate(Length_cm ~ lat, data = bio[find,], FUN = mean) 
      if (length(find) > n) { find = sample(find, n) }     
      points(bio$Latitude_dd[find], bio$Length_cm[find], col= red, pch = 1, cex = 0.7) }
    if(length(bio$Sex[ind] == "M") > 0){
      find = which(bio$Sex[ind] == "M")
      pred.m = aggregate(Length_cm ~ lat, data = bio[find,], FUN = mean) 
      if (length(find) > n) { find = sample(find, n) }
      points(bio$Latitude_dd[find], bio$Length_cm[find], col= blue, pch = 1, cex = 0.7) }
    if(length(bio$Sex[ind] == "U") > 0){
      find = which(bio$Sex[ind] == "U")
      pred.u = aggregate(Length_cm ~ lat, data = bio[find,], FUN = mean) 
      if (length(find) > n) { find = sample(find, n) }
      points(bio$Latitude_dd[find], bio$Length_cm[find], col= grey, pch = 1, cex = 0.7) }    
    if(length(bio$Sex == "F") > 0) { lines(pred.f$lat, pred.f$Length_cm, col = 'red4',  lwd = 2) }
    if(length(bio$Sex == "M") > 0) { lines(pred.m$lat, pred.m$Length_cm, col = 'navy', lwd = 2) }
    if(length(bio$Sex == "U") > 0) { lines(pred.u$lat, pred.u$Length_cm, col = 'grey40', lwd = 2) }
   
  dev.off()
  }

  pn = 2
  if (pn %in% plot){
  # CPUE by latitude by year
  pngfun(wd = dir, file = paste0(name, '_NWFSC_CPUE_Lat_Year.png'), h = 8.5)
  ymax = ceiling(max(log(catch$cpue_kg_km2)))
  par(mfrow = c(5,4), mar = c(2,2,2,2), oma = c(2,2,1,1))
  for (ii in sort(unique(catch$Year))){
    ind = catch$cpue_kg_km2 != 0 & catch$Year == ii
    plot(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), type = "n", pch = 20,  xlab = "", ylab = "", 
       main = ii, las = 1, xlim = c(32, 49), ylim = c(0, ymax))
    points(catch$Latitude_dd[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
  }
  mtext(side = 1, "Latitude", outer = TRUE)
  mtext(side = 2, "log(CPUE kg/km2)", outer = TRUE)
  mtext(side = 3, name, outer = TRUE)
  dev.off()
  }

  pn = 3
  if (pn %in% plot){
  # CPUE by depth by year
  pngfun(wd = dir, file = paste0(name, '_NWFSC_CPUE_Depth_Year.png'), h = 8.5)
  ymax = ceiling(max(log(catch$cpue_kg_km2))); xmax = ceiling(max(catch$Depth_m))
  par(mfrow = c(5,4), mar = c(2,2,2,2), oma = c(2,2,1,1))
  for (ii in sort(unique(catch$Year))){
    ind = catch$cpue_kg_km2 != 0 & catch$Year == ii
    plot(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), type = "n", pch = 20,  xlab = "", ylab = "", 
       main = ii, las = 1, xlim = c(55, xmax), ylim = c(0, ymax))
    points(catch$Depth_m[ind], log(catch$cpue_kg_km2[ind]), col=gr, bg=bw, pch = 21, cex = 0.7)
  }
  mtext(side = 1, "Depth (m)", outer = TRUE)
  mtext(side = 2, "log(CPUE kg/km2)", outer = TRUE)
  mtext(side = 3, name, outer = TRUE)
  dev.off()
  }
}