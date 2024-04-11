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


# 1. Add index plots
# 2. Add juvenile index plots, remove other figures, keep tables, and add information on the size being used.
# 3. Plot catches and add information on the 2025 ACL.
# 4. Decide whether to add the nwfsc hkl index
# 5. Update the intro text to disucss the CDFW ad-hoc collections
# 6. Add information about research
# 7. Check in about including the IPHC index
# 8. yellowtail rockfish NWFS hkl index
# 9. vermilion hkl index
# 10. lingcod south and north