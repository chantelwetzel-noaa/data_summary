
wd = "C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary"
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/HookandLineSurveyCatchAnalysis")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/JRWToolBox")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
source(file.path(wd, "R", "get_hkl_species.R"))
source(file.path(wd, "R", "plot_hkl_index.R"))
source(file.path("C:/Users/Chantel.Wetzel/Documents/GitHub/HandyCode/R/pdf_fxn.R"))

library(lattice)
library(maptools)
library(gtools) 
library(MASS)  
library(gam)
library(MCMCpack)
library(Hmisc)
library(chron)
library(RCurl)
library(AICcmodavg)

# load the hook and line data
#data = read.csv(file.path(wd, "data-raw","qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version_04042021.csv"))
data = read.csv(file.path(wd, "data-raw", "hookandline_2004_2021_draft_data.csv"))

# use the data to determine the suite of species
species_list = get_hkl_species()

for (s in species_list[5:length(species_list)]){
	setwd(file.path(wd, "hook_and_line"))
	spec_name = gsub(" ", "_", tolower(s))
	dir = file.path(wd, "hook_and_line", spec_name)
	dir.create(dir)
	setwd(dir)

	run_hkl_glm(dir = dir, data = data, species = s, area = 3, n = 1000000)
	
	results = read.csv(file = ".SS.Table.txt", sep = "", dec = ".")
	write.csv(results, file.path(getwd(), "index.csv"))
	index = read.csv(file.path(getwd(), "index.csv"), header = TRUE)
	rownames(index) = index[,1]
	index = index[,-1]
	colnames(index) = c(2004:2019, 2021)
	plot_hkl_index(data = index, 
		savefile = paste0("HKL_GLM_", spec_name, ".png"), 
		lab.spp = s, 
		smooth = TRUE)
}

for (s in species_list){
	setwd(file.path(wd, "hook_and_line"))
	spec_name = gsub(" ", "_", tolower(s))
	dir = file.path(wd, "hook_and_line", spec_name)
	dir.create(dir)
	setwd(dir)
	dir.create(file.path(dir, "forSS"))

	sub_data = data[data$common_name == s, ]

	create_hkl_comps(
		dir = dir, 
		data = sub_data,
		species = spec_name, 
		common_name = s)
}