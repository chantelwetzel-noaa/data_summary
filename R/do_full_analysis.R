#' Wrapper function that will conduct the full analysis starting with
#' pulling data, calculating indices, calculating length compositions,
#' create tables and figures, and the generate an updated document
#' 
#' @param 
#' 
#'
#' @return 
#'
#' @author Chantel Wetzel 
#' @export
#'
#' @examples
#' 
#' 
#do_full_analysis <- function(data_dir, index_dir, pull = TRUE){

    
    setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary")

    data_dir <- file.path("data")
    index_dir <- 
    pull <- FALSE
    devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
    all_files <- list.files("R")
    for (a in 1:length(all_files)) {
            load(file.path("R", all_files[a])       
    }

    

    # Call function to pull data
    # WCGBTS data
    data = get_all_data(data_dir, pull = pull, survey = "all")
    # Load the NWFSC HKL data
    hkl = pull_hkl(return = TRUE, load = TRUE)
    

    # Call function to format data accordingly for consistent code

    # Pull in list of species
    # Ideally would be an available data object to the package
    # wcgbt_species = # Load in trawl species list (in data file)
    # hkl_species = # Load in a hkl species list (in data file)

    # Pull in areas to calculate the index across (trawl survey only)
    index_area = nwfscSurvey::GetSpp.fn(species = wcgbt_species, unident = FALSE) 

    # Call wrapper function to calculate indices
    get_all_indices(index_dir, data = data$all_wcgbt, species_list = wcgbt_species, area = index_area)
    get_all_indices(index_dir, data = data$hkl, species_list = hkl_species)

    # Call function to generate the length composition figures
        # Generates sample by year table as well

    # Call function to create table of postive tows, lengths, and otoliths
        # Create figures
        # Create  data frame: either create csv then convert to tex or just create the tex

    # Call function to generate tables
        # Use sa4ss txt table function to create tex table files

	# Call function for any additional figures

    # Generate the document
#}