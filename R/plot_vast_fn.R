#################################################################################################################
# NWFSC Combo Index plots
#################################################################################################################
#
# @ vast.dir directory where to find the vast output
# @ plot.dir directory to write the index figures
# @ name name of the species (same as folder name where vast output is saved)
#
# Writen by Chantel Wetzel
#
plot_vast_fn <- function(vast.dir, plot.dir, name, do_smooth = TRUE){

  dir.create(plot.dir)
  colors = c("black",  "blue", "darkorchid1", "red")
  pch.vec = c(21, 22, 23, 24)
  cex.vec = c(1.6, 1.4, 1.4, 1.4)
  cex.lab  = 2.1
  cex.axis = 1.6

  colors = c("black",  "blue", "darkorchid1", "red")
  dat = read.csv(paste0(vast.dir, "/Table_for_SS3.csv"), header = TRUE)
  strat = unique(dat$Fleet)
  years = unique(dat$Year)
  if("north" %in% strat) { colors = c("black", "red", "blue")}  

  pngfun(wd = plot.dir, file = paste0(name, '_NWFSC_Index_', max(years), '.png'), h = 7, w = 10)
  
  plot(0, type='n', xlim=range(years), ylim=c(0, 1.5*max(dat$Estimate_metric_tons)), xlab="", ylab="", yaxs='i', 
    main = "", cex.axis = cex.axis)
  mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  mtext(side = 2, "Index", cex = cex.lab, line = 2.5)
  mtext(side = 3, paste0("NWFSC WCGBTS: ", name), font = 2, cex = cex.lab, line = 1)
  for(b in 1:length(strat)){
    ind = dat$Fleet == strat[b]
    est = dat[ind, "Estimate_metric_tons"]
    sd  = dat[ind, "SD_log"]
    hi = qlnorm(0.975, meanlog = log(est), sdlog = sd )
    lo = qlnorm(0.025, meanlog = log(est), sdlog = sd )
    if (do_smooth == TRUE) {
      if (b == 1) { 
        pred = loess(Estimate_metric_tons ~ Year, dat[ind,])
        lines(years, predict(pred), lty = 2, col = 'snow4')
      }
    }
    if (b == 1) { 
      arrows(x0=years, y0=lo, x1=years, y1=hi, angle=90, code=3,length=0.01, col = 'grey') 
    }
    points(years, est, pch = pch.vec[b], bg = colors[b], cex = cex.vec[b])
    lines(years, est, col = colors[b], cex = 1)
  }
  legend("topleft", legend = strat, pt.bg = colors[1:b], pch = pch.vec[1:b], bty= 'n', cex = cex.lab)
  
  dev.off()

}