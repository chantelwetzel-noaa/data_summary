

#devtools::install_github("kellijohnson-NOAA/FishStatsUtils")
#devtools::install_github("James-Thorson-NOAA/VAST@2034b7442f31bbe0e79b521652a58ed99e025e49")
#devtools::install_github("nwfsc-assess/VASTWestCoast")

library(VASTWestCoast)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey") #library(nwfscSurvey)
library(rnaturalearth)
library(plyr)
library(dplyr)
library(HandyCode)
#source("C:/Handy_Code/print_letter.R")
#source("C:/Handy_Code/pdf_fxn.R")

options(scipen = 999, stringsAsFactors = FALSE)

loc = "C:/Assessments/2020/survey_summary"
vast.wd  = paste0(loc, "/vast"); dir.create(vast.wd)
plot.dir = paste0(loc, "/vast_plots/"); dir.create(plot.dir)
pull.dir = paste0(loc, "/pulled_data"); dir.create(pull.dir)
comp.dir = paste0(loc, "/comp_plots"); dir.create(comp.dir)
#map.dir  = paste0(loc, "/map_plots"); dir.create(map.dir)
cpue.dir = paste0(loc, "/cpue_plots"); dir.create(cpue.dir)
sum.dir  = paste0(loc, "/sum_plots"); dir.create(sum.dir)
table.dir= paste0(loc, "/survey_tables/"); dir.create(table.dir)


# Source functions
files = list.files(paste0(loc, "/survey_summary_package/R"))
for (i in files){ source(paste0(loc, "/survey_summary_package/R/", i))}

#colors = rich.colors.short(4)
bw   <- adjustcolor(1, alpha.f<- 0.10)
gr   <- adjustcolor(1, alpha.f<- 0.50)
red  <- adjustcolor("red", alpha.f<- 0.10)
blue <- adjustcolor("blue", alpha.f<- 0.10)


#====================================================================================================
# West Coast Groundfish Bottom Trawl Survey 
#====================================================================================================

# Create the list of species 
sp.list = rbind(
              c("north_south", "shortbelly_rockfish", "Sebastes_jordani"),
              c("wa_or", "pacific_cod", "Gadus_macrocephalus"),
              c("ca", "brown_rockfish", "Sebastes_auriculatus"), 
              c("ca", "copper_rockfish", "Sebastes_caurinus"),
              c("coast", "dover_sole", "Microstomus_pacificus"),
              c("north_south", "sablefish", "Anoplopoma_fimbria"),
              c("coast", "petrale_sole", "Eopsetta_jordani"), 
              c("ca", "bank_rockfish", "Sebastes_rufus"),
              c("coast", "pacific_sanddab", "Citharichthys_sordidus"), 
              c("coast", "arrowtooth_flounder", "Atheresthes_stomias"),
              c("coast", "aurora_rockfish", "Sebastes_aurora"),
              c("ca", "blackgill_rockfish", "Sebastes_melanostomus"),
              c("north_south", "bocaccio", "Sebastes_paucispinis"),
              c("coast", "canary_rockfish", "Sebastes_pinniger"),
              c("north_south", "chilipepper", "Sebastes_goodei"), 
              c("coast", "darkblotched_rockfish", "Sebastes_crameri"),
              c("coast", "english_sole", "Parophrys_vetulus"),
              c("wa_or", "flathead_sole", "Hippoglossoides_elassodon"),
              c("coast", "greenspotted_rockfish", "Sebastes_chlorostictus"), 
              c("coast", "greenstriped_rockfish", "Sebastes_elongatus"), 
              c("coast", "lingcod", "Ophiodon_elongatus"), 
              c("coast", "longspine_thornyhead", "Sebastolobus_altivelis"), 
              c("coast", "pacific_spiny_dogfish", "Squalus_suckleyi"), 
              c("coast", "redbanded_rockfish", "Sebastes_babcocki"),
              c("coast", "rex_sole", "Glyptocephalus_zachirus"),
              c("coast", "rougheye_rockfish", "Sebastes_aleutianus"),
              c("coast", "sharpchin_rockfish", "Sebastes_zacentrus"),
              c("coast", "shortspine_thornyhead", "Sebastolobus_alascanus"),
              c("north_south", "splitnose_rockfish", "Sebastes_diploproa"),
              c("north_south", "yellowtail_rockfish", "Sebastes_flavidus"),
              c("coast", "big_skate", "Raja_binoculata"),
              c("coast", "longnose_skate", "Raja_rhina"),
              c("north_south", "pacific_ocean_perch", "Sebastes_alutus"),
              c("coast", "widow_rockfish", "Sebastes_entomelas")  
              )

colnames(sp.list) = c("strat", "name", "sci_name")
reg.name = gsub("_", " ", sp.list[,"name"])


# Run the VAST function & create plots ---------------------------------------------

for (a in 1:dim(sp.list)[1]){
  dir = paste0(vast.wd, "/", sp.list[ ,"name"][a])
  dir.create(dir)
  setwd(dir)
  
  dist = "gamma"
  do_vast_fn(dir = dir, sci_name = sp.list[,"sci_name"][a], dist = dist, strat = sp.list[,"strat"][a])
  plot_vast_fn(vast.dir = dir, plot.dir = plot.dir, name = sp.list[,"name"][a], do_smooth = TRUE)
}

# Load pulled otolith data---------------------------------------------

oto.dat = read.csv("C:/Assessments/2020/survey_summary/pulled_data/_pulled_data_04082020.csv", header = TRUE)
# Filter out bad tows or non-standard tows
keep = oto.dat$performance == "Satisfactory" & is.na(oto.dat$year_stn_invalid)
oto.dat = oto.dat[keep,]


# Create summary figures for WCGBTS data in the data warehouse -------------------------

for (a in 1:dim(sp.list)[1]){
  # Pull data from the data warehouse
  out = pull_survey_data_fn(dir = pull.dir, name = reg.name[a], load.data = FALSE)
  bio    = out$bio 
  catch  = out$catch
  strata = create_strata_fn(area = sp.list[,"strat"][a])

  num.strata = CheckStrata.fn(dat = catch, strat.vars = c("Depth_m","Latitude_dd"), strat.df = strata, verbose = TRUE)
  write.csv(num.strata$PositiveTows, file = paste0(table.dir, sp.list[,"name"][a], "_positive_tows.csv"), row.names = FALSE)

  survey_lengths_fn(bio = bio, catch = catch, all.dir, name = sp.list[,"name"][a], 
                    table.dir = table.dir, comp.dir = comp.dir)

  plot_cpue_fn(dir = cpue.dir, name = sp.list[,"name"][a], catch = catch, bio = bio )

  # Create map
  #PlotMap.fn(dir = loc, dat = catch, main = sp.list[,"name"][a], dopng = TRUE)
  #file.rename(paste0(loc, "/map_plots/NWFSC_combo_CPUE_Map_Year.png"), paste0(loc, "/map_plots/", sp.list[,"name"][a],"_NWFSC_combo_CPUE_Map_Year.png"))
  #file.rename(paste0(loc, "/map_plots/NWFSC_combo_CPUE_Map.png"), paste0(loc, "/map_plots/", sp.list[,"name"][a],"_NWFSC_combo_CPUE_Map.png"))

  #Calculate values for histogram plots
  plot_summary_fn(dir = sum.dir, name = sp.list[,"name"][a], catch = catch, all.dat = oto.dat)

}  

#====================================================================================================
# West Coast Hook & Line Survey Data
#====================================================================================================   

# Load the data
load("C:/Assessments/2020/survey_summary/pulled_data/hkl_grand_2019.Rdata")
hkl = Grand.2019
hkl = hkl[which(hkl$LENGTH < 150),]
get_all = unique(hkl$COMNAME)
remove = get_all %in% c("", "Anemone unid", "Barred Sand Bass", "Brown Smoothhound", "California Market Squid", "Giant plumose anemone",
           "Gorgonian unident (coral)", "Gray Smoothhound", "Humboldt Squid", "Invert Unidentified", "Petrale Sole",
           "Pacific Bonito", "Pacific Jack Mackerel", "Shortbelly Rockfish", "Southern Rock Sole", "Spiny Dogfish",
           "Spiny Sea Star", "Unknown Fish", "White Croaker", "Ocean Whitefish", "Pacific Mackerel", "Sanddab Unidentified")
sub = get_all[!remove]
a = table(hkl[hkl$COMNAME %in% sub, "COMNAME"]) > 85
species = sort(sub)[a] 


for (a in 1:length(species)){

  #survey_lengths_fn(bio = bio, catch = catch, all.dir, name = sp.list[,"name"][a], 
  #                  table.dir = table.dir, comp.dir = comp.dir)

  #plot_cpue_fn(dir = cpue.dir, name = sp.list[,"name"][a], catch = catch, bio = bio )

  # Create map
  #PlotMap.fn(dir = loc, dat = catch, main = sp.list[,"name"][a], dopng = TRUE)
  #file.rename(paste0(loc, "/map_plots/NWFSC_combo_CPUE_Map_Year.png"), paste0(loc, "/map_plots/", sp.list[,"name"][a],"_NWFSC_combo_CPUE_Map_Year.png"))
  #file.rename(paste0(loc, "/map_plots/NWFSC_combo_CPUE_Map.png"), paste0(loc, "/map_plots/", sp.list[,"name"][a],"_NWFSC_combo_CPUE_Map.png"))

  #Calculate values for histogram plots
  plot_summary_fn(dir = sum.dir, name = species[a], catch = hkl, all.dat = NULL, survey = "hkl")
  hkl_length_comps_fn(catch = hkl, name = species[a], comp.dir = comp.dir)
} 
