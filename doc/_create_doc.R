# Always: Load packages
library(sa4ss)

# Always: Specify the directory for the document
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/doc")

sa4ss::draft(authors = c("Chantel R. Wetzel", "Jim Hastie"),
  			 species = "Copper Rockfish",
  			 latin = "Sebastes caurinus",
  			 coast = "Oregon US West",
  			 type = c("sa"),
  			 create_dir = FALSE,
  			 edit = FALSE)


# Render Call:
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
