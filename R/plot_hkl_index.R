plot_hkl_index <- function(data, savefile, lab.spp, smooth = TRUE){
	
	years = as.numeric(colnames(data))
	est = data["Median",]
	tmp = data.frame(Year = years, 
					 Median = as.numeric(est))

  sd  <- as.numeric(data["SD.log",])
  hi <- stats::qlnorm(0.975, meanlog = log(as.numeric(est)), sdlog = sd)
  lo <- stats::qlnorm(0.025, meanlog = log(as.numeric(est)), sdlog = sd)

	grDevices::png(filename = savefile,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  	on.exit(grDevices::dev.off(), add = TRUE)

  cex.axis = 1.25
  cex.lab = 1.20

  plot(0, type = "n",
    	xlim = range(years),
    	ylim = c(0, max(hi) + 0.02),
    	xlab = "", ylab = "", yaxs = "i",
    	main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
 	graphics::mtext(side = 2, "Index", cex = cex.lab, line = 2.5)
 	graphics::mtext(side = 3, text = lab.spp,
 	  font = 2, cex = cex.lab, line = 0.25)
 	graphics::mtext(side = 3, text = "NWFSC Hook & Line",
 	  font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = years, y0 = lo, x1 = years, y1 = hi, angle = 90, code = 3,
      length = 0.01, col = "grey")
  graphics::points(years, est, pch = 16, bg = 1, cex = 1.6)
  graphics::lines(years, est, col = 1, cex = 1)

  if (smooth == TRUE) {
      pred <- stats::loess(Median ~ Year, tmp)
      graphics::lines(years, stats::predict(pred), lty = 2, col = "snow4")
  }

}