load(here::here("data-processed", "combined_data.Rdata"))
qb_ca <- read.csv(here::here("data-raw", "quillback_aged_04112024.csv"))

qb_data <- data[
  which(data$Common_name == "quillback rockfish" & !is.na(data$Age)), ]
table(qb_data$State)

qb_ca_ages <- data.frame(
  State = "California", 
  Length_cm =  qb_ca$length_cm,   
  Age = qb_ca$age_best
)

# all quillback ages
qb_ages <- rbind(
  qb_data[, c("State", "Length_cm", "Age")],
  qb_ca_ages
)
cbp1 <- c('#AA3377', "#F0E442", "#56B4E9")

ggplot(qb_ages, aes(y = Length_cm, x = Age, color = State)) +
  geom_point(size = 3) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 65) + ylim(1, 62) +
  scale_alpha_manual(guide = 'none', 
        values = list(California = 1, Oregon = 0.2, Washington = 0.2)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  #facet_grid(State~.) + 
  xlab("Age") + ylab("Length (cm)") +
  ggplot2::scale_color_manual(values = cbp1)

ggplot() + 
  geom_point(data = qb_ages |> dplyr::filter(State != "California"), 
             aes(y = Length_cm, x = Age, color = State), 
             alpha = 0.2, size = 2,
             position = "jitter") +
  geom_point(data = qb_ages |> dplyr::filter(State == "California"), 
             aes(y = Length_cm, x = Age, color = State), 
             alpha = 1, size = 2, 
             position = "jitter") +
  theme_bw() + 
  ggplot2::ggtitle("Quillback rockfish") + 
  xlim(1, 65) + ylim(1, 62) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  #facet_grid(State~.) + 
  ggplot2::scale_color_manual(values = cbp1) +
  xlab("Age") + ylab("Length (cm)") 

#===============================================================================
# California only quillback ages from Patrick and Rachel's summaries
#===============================================================================
quillback <- read.csv("C:/Assessments/2024/quillback/quillback_age_lengths_april_19_2024.csv")
quillback$Length_cm <- as.numeric(quillback$Length_cm)
quillback$Age <- as.numeric(quillback$Age)
quillback$Source[quillback$Source == "Abrams Research"] <- "Research"

quillback <- quillback[!is.na(quillback$Length_cm), ]
len_bins <- seq(8, 56, 2)
bin <- c(-999, len_bins, Inf)
quillback$bin <- bin[findInterval(quillback$Length_cm, bin, all.inside = T)]
#write.csv(quillback, 
#          file = "C:/Assessments/2024/quillback/quillback_age_lengths_april_19_2024_modifie.csv")
quillback$age_count <- ifelse(is.na(quillback$Age), 0, 1)
quillback$length_count <- 1

# Estimate the growth curve
growth <- nwfscSurvey::est_growth(
  dat = quillback[!is.na(quillback$Age),],
  Par = data.frame(K = 0.13, Linf = 45, L0 = 10, CV0 = 0.10, CV1 = 0.10),
  bySex = FALSE,
  sdFactor = 2
)
growth_parms <- nwfscSurvey::est_growth(
    dat = quillback[!is.na(quillback$Age),],
    Par = data.frame(K = 0.13, Linf = 45, L0 = 10, CV0 = 0.10, CV1 = 0.10),
    bySex = FALSE,
    return_df = FALSE
  )
# $allSex_growth
# K      Linf   L0          CV0          CV1 
# 0.1107 43.1 15.6 1.785372e-01 2.095267e-07 
outliers <- which(growth$Length_cm > growth$Lhat_high | growth$Length_cm < growth$Lhat_low)
growth[outliers, c("Source", "Length_cm", "Age", "Lhat_low", "Lhat_high", "Lhat_pred")]
plot(growth$Age, growth$Lhat_pred)

# Add WCGBT young ages from Oregon and Washington
wcgbt <- data[
  which(data$Source == "NWFSC WCGBT" & 
          data$Common_name == "quillback rockfish" & 
          !is.na(data$Age)), ]
young_wcgbt <- wcgbt[wcgbt$Age < 5 & wcgbt$State != "California", ]
expanded_data <- data.frame(
  Source = c(quillback$Source[!is.na(quillback$Age)], young_wcgbt$Source),
  State = c(rep("California", length(quillback$Source[!is.na(quillback$Age)])), young_wcgbt$State),
  Sex = c(quillback$Sex[!is.na(quillback$Age)], young_wcgbt$Sex),
  Length_cm = c(quillback$Length_cm[!is.na(quillback$Age)], young_wcgbt$Length_cm),
  Age = c(quillback$Age[!is.na(quillback$Age)], young_wcgbt$Age)
)
growth_exp <- nwfscSurvey::est_growth(
  dat = expanded_data ,
  Par = data.frame(K = 0.13, Linf = 45, L0 = 10, CV0 = 0.10, CV1 = 0.10),
  bySex = FALSE,
  sdFactor = 2
)
growth_parms_exp <- nwfscSurvey::est_growth(
  dat = expanded_data ,
  Par = data.frame(K = 0.13, Linf = 45, L0 = 10, CV0 = 0.10, CV1 = 0.10),
  bySex = FALSE,
  sdFactor = 2,
  return_df = FALSE
)
# $allSex_growth
# K      Linf    L0          CV0          CV1 
# 0.133 42.68  8.82  0.261959817  0.006549565 
outliers_exp <- which(growth_exp$Length_cm > growth_exp$Lhat_high | growth_exp$Length_cm < growth_exp$Lhat_low)
growth_exp[outliers_exp, c("Source", "Length_cm", "Age", "Lhat_low", "Lhat_high", "Lhat_pred")]
plot(growth$Age, growth$Lhat_pred, xlim = c(0, 51), ylim = c(0, 50), lwd = 2)
points(growth_exp$Age, growth_exp$Lhat_pred, pch = 17, col = 'red')

plot(growth$Age[growth$Sex == "F"], growth$Length_cm[growth$Sex == "F"], type = "p", 
     pch = 16, col = "darkgreen", xlim = c(0, 55), ylim = c(0, 50))
points(growth$Age[growth$Sex == "M"], growth$Length_cm[growth$Sex == "M"], pch = 17, 
      col = "orange")
points(growth$Age[growth$Sex == "U"], growth$Length_cm[growth$Sex == "U"], pch = 18, 
      col = "purple")
points(growth$Age[outliers], growth$Length_cm[outliers], pch = 16, col = "red")

cbp1 <- c("#56B4E9", "#0072B2", "#009E73",
          "#F0E442", "#E69F00",  "#D55E00", '#AA3377',
          '#228833',   '#AA3377')

len_age <- ggplot() +
  geom_point(
    data = quillback, 
    aes(y = Length_cm, x = Age, color = Source),
    alpha = 0.75, size = 3, position = "jitter") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  theme_bw() + 
  ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  ggplot2::scale_color_manual(values = cbp1, drop = FALSE) +
  xlab("Age") + ylab("Length (cm)") 

len <- ggplot(
    data = quillback, aes(x = bin, y = length_count, fill = Source)) +
  geom_bar(position = "stack", stat="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10), limits = c(0,55)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  ggplot2::scale_fill_manual(values = cbp1, drop = FALSE) +
  xlab("Length (cm)") + ylab("Count")

age <- ggplot(
  data = quillback, aes(x = Age, y = length_count, fill = Source)) +
  geom_bar( position = "stack", stat="identity") +
  theme_bw() + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=10), limits = c(0,55)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  ggplot2::scale_fill_manual(values = cbp1, drop = FALSE) + 
  xlab("Age (years)") + ylab("Count")


cowplot::plot_grid(len_age, len, age, ncol = 1, nrow = 3)
ggsave(file.path("C:/Assessments/2024/quillback", "quillback_length_age.png"), 
       height = 12, width = 12, dpi = 100)

#===============================================================================
# Plot Oregon-Washington with the compiled quilllback dataset
#===============================================================================
load(here::here("data-processed", "combined_data.Rdata"))

qb_data <- data[
  which(data$Common_name == "quillback rockfish" & 
        !is.na(data$Age) & data$State != "California"), ]

qb_ca_ages <- data.frame(
  State = "California", 
  Length_cm =  quillback$Length_cm,   
  Age = quillback$Age 
)

# all quillback ages
qb_ages <- rbind(
  qb_data[, c("State", "Length_cm", "Age")],
  qb_ca_ages
)
cbp1 <- c('#AA3377', "#F0E442", "#56B4E9")

ggplot() + 
  geom_point(data = qb_ages |> dplyr::filter(State != "California"), 
             aes(y = Length_cm, x = Age, color = State), 
             alpha = 0.2, size = 2,
             position = "jitter") +
  geom_point(data = qb_ages |> dplyr::filter(State == "California"), 
             aes(y = Length_cm, x = Age, color = State), 
             alpha = 1, size = 2, 
             position = "jitter") +
  theme_bw() + 
  ggplot2::ggtitle("Quillback rockfish") + 
  xlim(1, 60) + ylim(1, 65) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  ggplot2::scale_color_manual(values = cbp1) +
  xlab("Age") + ylab("Length (cm)") 

ggsave(file.path("C:/Assessments/2024/quillback", "quillback_coastwide_length_age.png"), 
       height = 8, width = 12, dpi = 100)
