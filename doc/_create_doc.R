# Always: Load packages
library(sa4ss)

# Always: Specify the directory for the document
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/doc")

# Render Call:
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
