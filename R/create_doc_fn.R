#' Create document
#' 
#' @param doc_dir
#' @param draft
#' @param authors
#'
#' @return 
#'
#' @author Chantel Wetzel 
#' @export
#'
#' 
#'
create_doc <- function(doc_dir, draft = FALSE, authors = c("Chantel R. Wetzel")){
	setwd(doc_dir)
	if(draft){
		sa4ss::draft(authors = authors,
  				 species = "West Coast",
  				 latin = "",
  				 coast = "",
  				 type = c("sa"),
  				 create_dir = FALSE,
  				 edit = FALSE)	
	}
	if(file.exists("_main.Rmd")){
		file.remove("_main.Rmd")
	}

	bookdown::render_book("00a.Rmd", 
		clean = FALSE, 
		output_dir = getwd())
}