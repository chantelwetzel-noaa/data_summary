#' Helper function to standardize species names
#' 
#' 
#' @param data add definition
#' @param name Common_name
#'
#' @author Chantel Wetzel
#' @export
#'
combine_species <- function(data, name = "Common_name"){
	
	spiny = c("pacific spiny dogfish", "dogfish shark", "spiny dogfish", "spiny dogfish shark")
	blue = c("blue and deacon rockfish", "deacon/blue rockfish unknown", "blue rockfish", "deacon rockfish", "blue/deacon rockfish")
	reye = c("rougheye and black spotted rockfish", "rougheye rockfish", "blackspotted rockfish", "rougheye/blackspotted rockfish")
	gopher = c("gopher and black and yellow rockfish", "gopher rockfish", "black and yellow rockfish", "gopher/black and yellow rockfish") 
	verm = c("vermilion and sunset rockfish", "vermilion rockfish", "sunset rockfish", "vemilion/sunset rockfish")
	yellowtail <- c("yellowtail rockfish", "yellowtail")

	find = which(tolower(data[, name]) %in% spiny)
	data[find, name] = spiny[1]
	find = which(tolower(data[, name]) %in% blue)
	data[find, name] = blue[1]
	find = which(tolower(data[, name]) %in% reye)
	data[find, name] = reye[1]
	find = which(tolower(data[, name]) %in% verm)
	data[find, name] = verm[1]
	find = which(tolower(data[, name]) %in% gopher)
	data[find, name] = gopher[1]
	find = which(tolower(data[, name]) %in% yellowtail)
	data[find, name] = yellowtail[1]
	
	find = which(data[, name] == "pacific spiny dogfish")
	data[find, name] <- "Pacific spiny dogfish"
	find = which(data[, name] %in% c("chilipepper rockfish"))
	data[find, name] <- "chilipepper"
	find = which(data[, name] %in% c("china rockfish"))
	data[find, name] <- "China rockfish"
	find = which(data[, name] %in% c("pacific ocean perch"))
	data[find, name] <- "Pacific ocean perch"
	find = which(data[, name] %in% c("pacific cod"))
	data[find, name] <- "Pacific cod"
	find = which(data[, name] %in% c("pacific sandab"))
	data[find, name] <- "Pacific sandab"
	find = which(data[, name] %in% c("dover sole"))
	data[find, name] <- "Dover sole"  
	find = which(data[, name] %in% c("california scorpionfish"))
	data[find, name] <- "California scorpionfish" 
	
	return(data)
}