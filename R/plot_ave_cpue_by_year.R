
wd = "C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/vast/data_check"

# pull all species catch data
# aggregated cpue_kg_km2~Year + Common_name
# subbed to only species being included for evaluation

for(s in unique(sub_data$Common_name)){
ind = which(sub_data$Common_name == s)
pngfun(wd = wd, file = paste0(s, ".png"), w=7,h=7,pt=12)
max = 1.20*(max(sub_data[ind, "cpue_kg_km2"]))
plot(sub_data[ind, "Year"], sub_data[ind, "cpue_kg_km2"], xlab = "Year",
	ylab = "Mean CPUE kg km2", type = 'b', ylim = c(0, max), main = s)
points(sub_data[ind, "Year"], sub_data[ind, "cpue_kg_km2"], pch = 16, col = 1)
dev.off()
}

########################################################################
wd = "C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary"
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/VASTWestCoast")
setwd(wd)

pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

species = GetSppDefault.fn()
species_areas = GetSpp.fn(species)


for (s in species){
	dir = file.path(wd, "vast", s)

	files = list.files(dir)
	find = grep("Catch__NWFSC.Combo", files)
	load(paste0(dir, "/", files[find[1]]))
	catch = Out

	files = list.files(dir)
	find = grep("Bio_All_NWFSC.Combo", files)
	load(paste0(dir, "/", files[max(find)]))
	bio = Data

	loc = file.path(wd, "vast", "_general_plots")

	ggplot(bio, aes(Length_cm)) +
		facet_wrap(facets = c("Year")) +
		ggtitle(paste0("NWFSC WCGBT: ",unique(bio$Common_name))) +
		geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5, bw = 1)
	ggsave(filename = file.path(loc, paste0(s, "_length_distributions_by_year.png")))
}

for (s in species){
	dir = file.path(wd, "vast", s)

	files = list.files(dir)
	find = grep("Bio_All_NWFSC.Combo", files)
	load(paste0(dir, "/", files[max(find)]))
	bio = Data

	loc = file.path(wd, "vast", "_general_plots")

	pngfun(wd = loc, file = paste0(s, "_length_boxplot.png"))
	boxplot(Length_cm~Year, bio, main = paste0("NWFSC WCGBT: ", s))
	dev.off()
}