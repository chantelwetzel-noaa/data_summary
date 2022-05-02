 


combine_species <- function(data, name = "SPECIES_NAME"){
	
	spiny = c("pacific spiny dogfish", "dogfish shark", "spiny dogfish", "spiny dogfish shark")
	blue = c("deacon/blue rockfish unknown", "blue rockfish", "deacon rockfish", "blue/deacon rockfish", "blue and deacon rockfish")
	reye = c("rougheye rockfish", "blackspotted rockfish", "rougheye/blackspotted rockfish", "rougheye and black spotted rockfish")
	gopher = c("gopher rockfish", "black and yellow rockfish", "gopher and black and yellow rockfish", "gopher/black and yellow rockfish") 
	verm = c("vermilion rockfish", "sunset rockfish", "vemilion/sunset rockfish", "vermilion and sunset rockfish")

	find = which(tolower(data[, name]) %in% spiny)
	data[find, name] = spiny[1]
	find = which(tolower(data[, name]) %in% blue)
	data[find, name] = blue[5]
	find = which(tolower(data[, name]) %in% reye)
	data[find, name] = reye[4]
	find = which(tolower(data[, name]) %in% verm)
	data[find, name] = verm[4]
	find = which(tolower(data[, name]) %in% gopher)
	data[find, name] = gopher[3]
	return(data)
}