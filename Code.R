library(forecast)
library(ggplot2)  
library(gridExtra)
library(readxl)
library(Matrix)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(tidyr) 
#################################################ARIMA model
# Load data and initial cleanup
data <- read.csv("XXXXXX.csv")
grouped_data <- split(data, rep(1:ceiling(nrow(data)/32), each=32, length.out=nrow(data)))
cleaned_data <- Filter(function(x) any(x$Rate != 0), grouped_data)

# Combine cleaned data
final_data <- do.call(rbind, cleaned_data)

# Filter for years 1990-2019
filtered_data <- subset(final_data, year >= 1990 & year <= 2019)

# Create time series object
ts_data <- ts(filtered_data$Rate, start = 1, end = length(filtered_data$Rate), frequency = 1)

# Split time series into chunks of 30
split_data <- split(ts_data, ceiling(seq_along(ts_data)/30))

# Initialize output data frames
combined_data <- data.frame()
combined_model_info <- data.frame(
  selected_model = character(),
  AIC_value = numeric(),
  AICc_value = numeric(),
  BIC_value = numeric(),
  accuracy_values = numeric()
)

arima_train_residuals <- data.frame()

# Loop through the split data for ARIMA model fitting and forecasting
for (i in seq_along(split_data)) {
  
  # Split data into training and test sets
  traindata <- head(split_data[[i]], 27)
  validationdata<- tail(split_data[[i]], 3)
  
  # Train ARIMA model
  forecast_model <- auto.arima(traindata)
  
  # Get fitted values and residuals
  fitted_data <- fitted(forecast_model)
  train_residuals <- residuals(forecast_model)
  
  # Store residuals
  arima_train_residuals <- rbind(arima_train_residuals, data.frame(residuals = train_residuals))
  
  # Extract model info
  model_info <- c(
    selected_model = paste(forecast_model$arma, collapse = ","),
    AIC_value = forecast_model$aic,
    AICc_value = forecast_model$aicc,
    BIC_value = forecast_model$bic,
    accuracy_values = accuracy(forecast_model)[1]
  )
  combined_model_info <- rbind(combined_model_info, model_info)
  
  # Forecast on the validationdata
  forecast_test <- forecast(forecast_model, h = length(validationdata), level = 95)
  
  # Create a data frame for forecasted data
  forecast_validationdata <- data.frame(
    Mean = forecast_test$mean,
    Lower = forecast_test$lower[, "95%"],
    Upper = forecast_test$upper[, "95%"]
  )
  
  # Combine results into the final data frame
  combined_data <- rbind(combined_data, forecast_validationdata)
}

# Print results
print(combined_model_info)
print(arima_train_residuals)

#####################################Figure 2
mydata <- read_excel("XXXXXX.xls",sheet="XXXX",na="NA")  
head(mydata)


incidence <- mydata %>%    
  filter(measure=="Incidence")

top_bottom_incidence <- top_n(incidence, 10, Difference) %>%  
  bind_rows(top_n(incidence, -10, Difference))  

p1=ggplot(incidence, aes(x = Rate, y = Difference, size = abs(Difference), color = `Parent Name2`,   
                         label = ifelse(Difference %in% top_bottom_incidence$Difference, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) + 
  xlab("ASR (per 100,000)") + ylab("Absolute rate differences (per 100,000)")+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),  
        panel.background = element_blank())+  
  ggtitle("(A) Incidence-absolute rate differences") 
p1


Prevalence <- mydata %>%    
  filter(measure=="Prevalence")

top_bottom_Prevalence<- top_n(Prevalence, 10, Difference) %>%  
  bind_rows(top_n(Prevalence, -10, Difference))  

p2=ggplot(Prevalence, aes(x = Rate, y = Difference, size = abs(Difference), color =  `Parent Name2`,   
                          label = ifelse(Difference %in% top_bottom_Prevalence$Difference, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +
  xlab("ASR (per 100,000)") + ylab("Absolute rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),  
        panel.background = element_blank())+ 
  ggtitle("(C) Prevalence-absolute rate differences") 

p2


DALYs <- mydata %>%    
  filter(measure=="DALYs")

top_bottom_DALYs<- top_n(DALYs, 10, Difference) %>%  
  bind_rows(top_n(DALYs, -10, Difference))  

p3=ggplot(DALYs, aes(x = Rate, y = Difference, size = abs(Difference), color =  `Parent Name2`,   
                     label = ifelse(Difference %in% top_bottom_DALYs$Difference, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +  
  xlab("ASR (per 100,000)") + ylab("Absolute rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),
        panel.background = element_blank())+  
  ggtitle("(E) DALYs-absolute rate differences") 

p3

Deaths<- mydata %>%    
  filter(measure=="Deaths")

top_bottom_Deaths<- top_n(Deaths, 10, Difference) %>%  
  bind_rows(top_n(Deaths, -10, Difference))  

p4=ggplot(Deaths, aes(x = Rate, y = Difference, size = abs(Difference), color =  `Parent Name2`,   
                      label = ifelse(Difference %in% top_bottom_Deaths$Difference, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +  
  xlab("ASR (per 100,000)") + ylab("Absolute rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  
        panel.background = element_blank())+  
  ggtitle("(G) Deaths-absolute rate differences") +   
  scale_y_continuous(limits = c(0, 1.5)) 

p4


incidence <- mydata %>%    
  filter(measure=="Incidence")

top_bottom_incidence <- top_n(incidence, 10, DifferenceC) %>%  
  bind_rows(top_n(incidence, -10, DifferenceC))  

p5=ggplot(incidence, aes(x = Rate, y = DifferenceC, size = abs(DifferenceC), color = `Parent Name2`,   
                         label = ifelse(DifferenceC %in% top_bottom_incidence$DifferenceC, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +    
  xlab("ASR (per 100,000)") + ylab("Relative rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),    
        panel.border = element_blank(),  
        panel.background = element_blank())+  
  ggtitle("(B) Incidence-relative rate differences")   +  scale_y_continuous(limits = c(0, 16))  
p5

Prevalence <- mydata %>%    
  filter(measure=="Prevalence")

top_bottom_Prevalence<- top_n(Prevalence, 10, DifferenceC) %>%  
  bind_rows(top_n(Prevalence, -10, DifferenceC))  

p6=ggplot(Prevalence, aes(x = Rate, y = DifferenceC, size = abs(DifferenceC), color =  `Parent Name2`,   
                          label = ifelse(DifferenceC %in% top_bottom_Prevalence$DifferenceC, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +
  xlab("ASR (per 100,000)") + ylab("Relative rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),  
        panel.background = element_blank())+  
  ggtitle("(D) Prevalence-relative rate differences")   +  scale_y_continuous(limits = c(0, 16))  

p6


DALYs <- mydata %>%    
  filter(measure=="DALYs")

top_bottom_DALYs<- top_n(DALYs, 10, DifferenceC) %>%  
  bind_rows(top_n(DALYs, -10, DifferenceC))  

p7=ggplot(DALYs, aes(x = Rate, y = DifferenceC, size = abs(DifferenceC), color =  `Parent Name2`,   
                     label = ifelse(DifferenceC %in% top_bottom_DALYs$DifferenceC, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +  
  xlab("ASR (per 100,000)") + ylab("Relative rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  theme(axis.line = element_line(color = "black"),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),  
        panel.background = element_blank())+  
  ggtitle("(F) DALYs-relative rate differences")   +  scale_y_continuous(limits = c(0, 16))  

p7

Deaths<- mydata %>%    
  filter(measure=="Deaths")

top_bottom_Deaths<- top_n(Deaths, 10, DifferenceC) %>%  
  bind_rows(top_n(Deaths, -10, DifferenceC))  

p8=ggplot(Deaths, aes(x = Rate, y = DifferenceC, size = abs(DifferenceC), color =  `Parent Name2`,   
                      label = ifelse(DifferenceC %in% top_bottom_Deaths$DifferenceC, as.character(disease), ""))) +      
  geom_point() +      
  scale_size_continuous(range = c(2, 10),guide = "none") +    
  geom_text_repel(size = 4) +  
  xlab("ASR (per 100,000)") + ylab("Relative rate differences (per 100,000)")+ 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(), 
        panel.background = element_blank())+   
  ggtitle("(H) Deaths-relative rate differences")  +  scale_y_continuous(limits = c(0, 16))    


p8

combined_plot <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4)
combined_plot

###################################################################################################Figure 3
mydata<- read_excel("XXXXXX.xls",sheet="XXXX",na="NA")#sheet="1",

mydata=mydata %>% mutate(No=factor(No),
                         Parent_Name=factor(disease))
head(mydata)

plots <- list()  
for (group_ in unique(mydata$sex_measure)) {  
  plot_data <- mydata %>% filter(sex_measure == group_)  
  min_value <- min(plot_data$Difference)  
  max_value <- max(plot_data$Difference)  
  y_min <- floor(min_value / 5) * 5  
  y_max <- ceiling(max_value / 5) * 5  
  p <- ggplot(plot_data, aes(x = reorder(No, Difference), y = Difference,fill=Parent_Name)) +  
    geom_bar(position="dodge", stat="identity",width=0.65)+    
    labs(title = group_, x = "Disease", y = "Rate differences per 100,000") +  
    theme(  
      panel.background = element_rect(fill = "white"),  
      axis.line = element_line(colour = "black", linewidth = 0.25),  
      axis.title = element_text(size = 12, color = "black"),  
      axis.text = element_text(size = 10, color = "black"),  
      legend.position = "none") +  
    #scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, (y_max-y_min)/5)) +  
    scale_fill_manual(values = c("Depressive disorders"="#FF6F61",  
                                 "Anxiety disorders"= "#88B04B",  
                                 "Upper respiratory infections"="#FF800E",  
                                 "Urinary diseases and male infertility"="#003B77", 
                                 "Diarrheal diseases"="#9B2335",  
                                 "Malaria"="#6B5B95",  
                                 "Stroke"="#F7CAC9",  
                                 "Ischemic heart disease"="#92A8D1",  
                                 "Tuberculosis"="#955251",  
                                 "Maternal disorders"="#B565A7",  
                                 "Self-harm"="#009B77",  
                                 "Sexually transmitted infections excluding HIV"="#DD4124",  
                                 "Alzheimer's disease and other dementias"="#D65076",  
                                 "Chronic obstructive pulmonary disease"= "#45B8AC",  
                                 "Stomach cancer"="#EFC050",  
                                 "Interpersonal violence"="#4F84C4",  
                                 "Meningitis"="#E0B1CB",  
                                 "Esophageal cancer"="#0072B5",  
                                 "Tracheal, bronchus, and lung cancer"="#FFBCD9",  
                                 "Fungal skin diseases"="#3B9C9C",  
                                 "Vitamin A deficiency"="#997D87",  
                                 "Falls"="#8F7C43",  
                                 "Foreign body"="#FFC0A8",  
                                 "Headache disorders"="#98B4D4",  
                                 "Cirrhosis and other chronic liver diseases"="#A0785A",  
                                 "Hemoglobinopathies and hemolytic anemias"="#F93400",  
                                 "Intestinal nematode infections"="#3B7A57",  
                                 "Blindness and vision loss"="#4A6FE3",  
                                 "Diabetes mellitus"="#8595E1",  
                                 "Age-related and other hearing loss"="#C62168"))+coord_flip() 
  plots[[as.character(group_)]] <- p  
}  


y_value_order <- c("Female_Incidence", "Male_Incidence", "Female_Prevalence",  
                   "Male_Prevalence", "Female_DALYs", "Male_DALYs", "Female_Deaths",  
                   "Male_Deaths")  


ordered_plots <- lapply(y_value_order, function(x) plots[[x]])  

ordered_plots[[1]] <- ordered_plots[[1]] + scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200))  
ordered_plots[[2]] <- ordered_plots[[2]] + scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200))  
ordered_plots[[3]] <- ordered_plots[[3]] + scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, 170))  
ordered_plots[[4]] <- ordered_plots[[4]] + scale_y_continuous(limits = c(0, 850), breaks = seq(0, 850, 170)) 
ordered_plots[[5]] <- ordered_plots[[5]] + scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, 22))  
ordered_plots[[6]] <- ordered_plots[[6]] + scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, 22)) 
ordered_plots[[7]] <- ordered_plots[[7]] + scale_y_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.28))  
ordered_plots[[8]] <- ordered_plots[[8]] + scale_y_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, 0.28)) 
global_ordered <- grid.arrange(grobs = ordered_plots, ncol = 2)  
print(global_ordered)  

#######################################Figure 4
mydata<- read_excel("XXXXXX.xls",sheet="XXXX",na="NA")#sheet="1",
head(mydata)

my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",   
               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

plots <- list()  
for (group_ in unique(mydata$age_measure)) {    
  plot_data <- mydata %>% filter(age_measure == group_)  
  plot_data <- plot_data[order(plot_data$Difference, decreasing = TRUE), ] 
  
  p <- ggplot(plot_data, aes(x = disease_, y = Difference, fill = disease_)) +  
    geom_col(show.legend = F) +  
    coord_polar() +  scale_x_discrete(position = "top")   +     
    scale_fill_manual(values = my_colors) +
    theme_bw() 
  plots[[as.character(group_)]] <- p  
}  


y_value_order <- c("5 years_Incidence", "5-14 years_Incidence", 
                   "15-49 years_Incidence", "50-69 years_Incidence",
                   "70 years_Incidence", "5 years_Prevalence", "5-14 years_Prevalence", 
                   "15-49 years_Prevalence", "50-69 years_Prevalence", "70 years_Prevalence", 
                   "5 years_DALYs", "5-14 years_DALYs", "15-49 years_DALYs", "50-69 years_DALYs",
                   "70 years_DALYs", "5 years_Deaths", "5-14 years_Deaths", "15-49 years_Deaths", "50-69 years_Deaths", 
                   "70 years_Deaths")  


ordered_plots <- lapply(y_value_order, function(x) plots[[x]])  

global_ordered <- grid.arrange(grobs = ordered_plots, ncol = 5)
global_ordered

