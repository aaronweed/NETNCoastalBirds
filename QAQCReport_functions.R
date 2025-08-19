
# Scripts for caption and plotting functions to support QAQC reporting for NETN's Coastal Breeding Bird Program 

Incubation_methods<- {print("The nesting counts presented below show the total nests counted per comparable expert observers for each Island-Segement 
                      each day a survey was conducted.")}


SumIncubation_methods<- {print("The annual nesting counts summarized in each table and figure below were derived from the annual 
                               maximum counts among multiple Primary Surveys conducted within a year (May-July) or represent the average of the maximum count values from Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands.")}

Crechmethods<- {print(paste0("The following tables and graphs summarize data collected during boat-based Common Eider (creche) surveys conducted in ", params$year, " among all Outer Islands."))}

SumCreche_Annual<- {print("The annual counts summarized below were derived from the annual maximum counts among completed, Primary Surveys within a year (May-July) or represent the average of the completed, Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}

SumCreche_Daily<- {print("The daily counts summarized below were derived from completed, Primary Surveys conducted within a year (May-July) or represent the average of this daily count value among multiple expert birders. These count values were summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}


PlotHistBands<- function(current_yr = "2025"){
  
  creche <- SumCreche(time ="date", islands = "outer") # Summarize daily Primary Surveys
  
  # calculate historic variation
  
CBB_histsum <- filter(creche, !year %in% current_yr & !variable %in% "Average creche size" & Island %in% "All Islands") |> # select data without the current year's values
  group_by(Species_Code,  CommonName,  FullLatinName, Island, variable, month, stat) |>
  summarize(num_samps = sum(!is.na(value)),
            median_val = median(value, na.rm = TRUE),
            min_val = min(value, na.rm = TRUE),
            max_val = max(value, na.rm = TRUE),
            lower_100 = ifelse(num_samps >= 3, min(value, na.rm = T), NA),
            upper_100 = ifelse(num_samps >= 3, max(value, na.rm = T), NA),
            lower_95 = ifelse(num_samps >= 3, quantile(value, 0.025, na.rm = T), NA),
            upper_95 = ifelse(num_samps >= 3, quantile(value, 0.975, na.rm = T), NA),
            lower_50 = ifelse(num_samps >= 3, quantile(value, 0.25, na.rm = T), NA),
            upper_50 = ifelse(num_samps >= 3, quantile(value, 0.75, na.rm = T), NA),
            .groups = "drop") |>
  filter(!is.na(lower_50))

CBBdat_current <- filter(creche, year %in% current_yr & !variable %in% "Average creche size" & Island %in% "All Islands") %>% 
  mutate(metric_type = "value")

CBBdat_med <- CBB_histsum |> select(Species_Code:stat, median_val) |>
  mutate(metric_type = "median")

# Pivot summary data for plotting

CBB_sum_long <- CBB_histsum |>
  select(Species_Code:stat, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
  pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
               names_to = "metric", values_to = "value") |>
  mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
         distrib = paste0("d", gsub("\\D", "", metric))) |>
  select(-metric) |>
  pivot_wider(values_from = value, names_from = metric_type) |>
  mutate(metric_type = distrib)



# Plot ribbon for each metric_type (d50, d95, d100), grouped by variable

plot_values <- c("d100" = "#E4F0F8", "d95" = "#B8D8ED", "d50" = "#7FB9DD", "median" = "#1378b5",
                "value" = "black")
  
plot_breaks <- c("d100", "d95", "d50", "median" ,
                 "value")

plot_labels <-c("d100" = "Historic range", "d95" = "Hist. 95% range",
                "d50" = "Hist. 50% range", "median" = "Hist. median",
                "value" = "Current counts")
  
line_values <-c("median" = "solid")
line_breaks <-NULL

line_labels <- NULL

# CBB_sum_long$month <-  factor(CBB_sum_long$month,
#                          levels = CBB_sum_long$month,
#                          labels = month.abb[CBB_sum_long$month], ordered = T)
# 
# CBBdat_current$month <- factor(CBBdat_current$month,
#                         levels = CBBdat_current$month,
#                         labels = month.abb[CBBdat_current$month], ordered = T)
# 
# CBBdat_med$month <-   factor(CBBdat_med$month,
#                              levels = CBBdat_med$month,
#                              labels = month.abb[CBBdat_med$month], ordered = T)

# Create plot

y1<- ggplot() +
  geom_ribbon(data = CBB_sum_long, 
              aes(x = month, 
                  ymin = lower, ymax = upper, fill = metric_type, group= metric_type), alpha = 0.2) +
 
  geom_line(data = CBBdat_med,
            aes(y = median_val, x = month, 
                color = metric_type, group = metric_type,
                text = paste0("Island: ", Island, "<br>",
                              "Month: ", month, "<br>",
                              "Historic Median: ", round(median_val, 1), "<br>")), lwd = 0.7) +
  
  geom_point(data = CBBdat_current,
             aes(y = value, 
                 x = as.numeric(format(time, "%m")) + as.numeric(format(time, "%d")) / 31,
                 color = metric_type, group = metric_type)) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    name = "Month"
  ) + 
   scale_color_manual(values = plot_values,
                     breaks = plot_breaks,
                     labels = plot_labels,
                     name = NULL) +
  scale_fill_manual(values = plot_values,
                      breaks = plot_breaks,
                    labels = plot_labels,
                    name = NULL) +
  scale_linetype_manual(values = line_values,
                        breaks = line_breaks,
                        labels = line_labels,
                        name = NULL) +
  
  facet_grid( ~ variable, scales = "free_y", switch = "y") +
  labs(
    title = paste("Historical Seasonal Pattern vs", current_yr, "Observations across all Outer Islands"),
    x = "Month",
    y = "No. of Common Eider",
    fill = "Metric Type",
    color = "Metric Type"
  ) +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

suppressWarnings(print(y1))

}

plt_var<- function(type = "boxplot", survey, species, var) {
  
  if(survey == "incubation"){ 
    
    df <- filter(incub, Species_Code == species &  stat == "max", variable == var)
    
  }
  
  if(survey == "creche"){ df<-creche
  
  df <- filter(creche, Species_Code == species &  stat == "max", variable == var)
  
  }
  
  if(survey == "ground"){ 
    
    df <- filter(nests, Species_Code == species, variable == var)
    
  }
  
  
  if(type == "boxplot") {
    
    y2<- ggplot(data= df, aes(x= Island, y= value, group= Island, color = year)) +
      geom_boxplot(alpha= 0.3, notch= F) +geom_jitter(size= 3)
    
  }
  
  if(type == "violin"){
    
    y2<- ggplot(data= df, aes(x= Island, y= value, group= Island, color = year)) + geom_violin(alpha= 0.3)+
      geom_jitter(size= 3)
  }
  
  y2 <- (y2 + 
           facet_wrap(~Island, scales = "free_x")+
           coord_flip()+
           #scale_color_manual(values = c("black", "red"))+ 
           #scale_shape_manual(values = c(19, 8))+ 
           theme_bw()+
           #guides(fill = "none") + # This removes the legend title for fill
           labs(y = "Count", x ="", color = "Year")+
           #scale_color_gradient(low = "yellow", high = "purple", name = "Year") +
           scale_color_viridis_c(
             option = "D",           # Options: "A", "B", "C", "D", "E", "F", "G", "H", etc.
             name = "Year"
           )+
           ggtitle(paste0("Variation in maximum annual counts of ", species," ", var, " from 2007 to ", params$year, "."))+
           theme(
             legend.position = "right",
             
             # Increase the height of the legend key (for vertical legend)
             legend.key.height = unit(2, "cm"),
             
             # Increase text size
             legend.title = element_text(size = 14),
             legend.text  = element_text(size = 12)
           ) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 16)) +
           theme(legend.text = element_text(size = 12))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 12 )) +
           theme(axis.text.y = element_text(size = 12 )) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
           theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
           theme(axis.text.y=element_blank())+
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")) +
           theme(plot.title=element_text(size=12, vjust=2, face= "bold")) +
           theme(strip.background= element_rect(size=10, color="gray" )))
  
  suppressWarnings(print(y2))
}

