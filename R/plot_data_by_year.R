#' 
#' 
#' 
#' @param dir Directory location to save the compbiend data frame
#' @param data
#' @param year
#'
#' @author Chantel Wetzel
#' @export
#' @md
#'
plot_data_by_year <- function(dir = here::here("plots"), data, year){
  
  for (sp in unique(data$Common_name)){
    
    data_to_plot <- data[which(data$Common_name == sp & data$Year >= year), ]
    
    lengths <- ggplot2::ggplot(data_to_plot, aes(fill = Source, x = Year)) + 
      geom_histogram(aes(y = total_lengths), position="stack", stat="identity") + 
      xlab("Year") + ylab("Number of Lengths") + 
      facet_wrap("State", ncol = 3) + 
      ggtitle(unique(data_to_plot$Common_name)) +
      scale_fill_viridis_d()
    ages <- ggplot2::ggplot(data_to_plot, aes(fill = Source, y = total_ages, x = Year)) + 
      geom_histogram(aes(y = total_ages), position="stack", stat="identity") + 
      xlab("Year") + ylab("Number of Ages") +
      facet_wrap("State", ncol = 3) + 
      scale_fill_viridis_d()
    otoliths <- ggplot2::ggplot(data_to_plot, aes(fill = Source, y = total_otolithss, x = Year)) + 
      geom_histogram(aes(y = total_otoliths), position="stack", stat="identity") + 
      xlab("Year") + ylab("Number of Otoliths") +
      facet_wrap("State", ncol = 3) + 
      scale_fill_viridis_d()
    
    cowplot::plot_grid(lengths, ages, otoliths, ncol = 1, nrow = 3)
    ggsave(file.path(dir, paste0(sp, "_state_compositions.png")), height = 12, width = 12)
    
  }
  
}