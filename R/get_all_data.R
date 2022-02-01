#' Wrapper function that will pull all survey data types
#' 
#' @param 
#' 
#'
#' @return A list with all survey data
#'
#' @author Chantel Wetzel 
#' @export
#'
#' @examples
#' 
#' 
get_all_data <- function(data_dir, pull = TRUE, survey = "all"){

    if (c("all", "wcgbt") %in% survey){
        # Pull all WCGBT catch and bio data from the survey warehouse
        if (pull){
          all_wcgbt = pull_survey_data_fn(data_dir = data_dir, survey = "NWFSC.Combo")  
        } else {
          all_wcgbt = pull_survey_data_fn(data_dir = data_dir, survey = "NWFSC.Combo", load_data = TRUE) 
        }
    }

    if (c("all", "hkl") %in% survey){
        all_hkl = all_hkl    
    }

    data = list()
    data$all_wcgbt
    data$all_hkl
    return(all_hkl)
}


}