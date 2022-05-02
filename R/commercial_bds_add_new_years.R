
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")
dir <- file.path(getwd(), "data-raw")
ca <- read.csv(file.path(dir, "Confidential-BDS501-1980---2020_california.csv"))
or <- read.csv(file.path(dir, "Confidential-BDS501-1980---2020_oregon.csv"))
wa <- read.csv(file.path(dir, "Confidential-BDS501-1980---2020_washington.csv")) 

new <- read.csv(file.path(dir, "Confidential-BDS501-2020---2021.csv"))

remove = which(ca$SAMPLE_YEAR >= 2020)
ca_new <- rbind(ca[-remove, ],
			new[new$AGENCY_CODE == "C", ])
remove = which(or$SAMPLE_YEAR >= 2020)
or_new <- rbind(or[-remove, ],
			new[new$AGENCY_CODE == "O", ])
remove = which(wa$SAMPLE_YEAR >= 2020)
wa_new <- rbind(wa[-remove, ],
			new[new$AGENCY_CODE == "W", ])

write.csv(ca_new, file = file.path(dir, "Confidential-BDS501-1980---2021_california.csv"))
write.csv(or_new, file = file.path(dir, "Confidential-BDS501-1980---2021_oregon.csv"))
write.csv(wa_new, file = file.path(dir, "Confidential-BDS501-1980---2021_washington.csv"))

source("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/R/cut_commercial_bds.R")
source("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/R/summarize_commercial_bds.R")

cut_commercial_bds(data_name = "Confidential-BDS501-1980---2021_california.csv")
cut_commercial_bds(data_name = "Confidential-BDS501-1980---2021_oregon.csv")
cut_commercial_bds(data_name = "Confidential-BDS501-1980---2021_washington.csv")

data_names <- c("Confidential-BDS501-1980---2021_california.rdat",
				"Confidential-BDS501-1980---2021_oregon.rdat",
				"Confidential-BDS501-1980---2021_washington.rdat")

summarize_commercial_bds(data_names)