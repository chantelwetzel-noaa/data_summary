
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