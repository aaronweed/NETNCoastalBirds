
# Scripts for caption and plotting functions to support QAQC reporting for NETN's Coastal Breeding Bird Program 

Incubation_methods<- {print("The nesting counts presented below show the daily number of nests counted by expert observers on each Island-Segment 
                       in comparison to the historical variation. Historical variation is summarized by the standardized week of each year.")}


SumIncubation_methods<- {print("The annual nesting counts summarized in each table and figure below were derived from the annual 
                               maximum counts among multiple Primary Surveys conducted within a year (May-July) or represent the average of the maximum count values from Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands.")}

Crechmethods<- {print(paste0("The following tables and graphs summarize all Primary Survey counts collected during boat-based Common Eider (creche) surveys conducted in ", params$year, " among all Outer Islands."))}

CrechmethodsHist<- {print(paste0("The following graphs summarize all Primary Survey counts collected 
                                 during boat-based Common Eider (creche) surveys conducted in ", params$year, 
                                 " among all Outer Islands in comparison to the historical variation. Historical variation is summarized by the standardized week of each year."))}


SumCreche_Annual<- {print("The annual counts summarized below were derived from the annual maximum counts among 
                          completed, Primary Surveys within a year (May-July) or represent the average of the completed, Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}

SumCreche_Daily<- {print("The daily counts summarized below and for each life stage (scroll down) were derived from 
                         completed, Primary Surveys conducted within a year (May-July) or represent the average of this daily count value among multiple expert birders. These count values were summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}

PlotWeeklyBands<- function(survey, current_yr, species ){
  
  library(tidyverse)
  library(magrittr)
  library(NETNCoastalBirds)
  
  if(survey == "creche"){ 
    
    df <- SumCreche(time ="date", islands = "outer") %>%  # Summarize daily Primary Surveys
      
      filter(!variable %in% "Average creche size" & Island %in% "All Islands") %>% 
      
      mutate(month=lubridate::month(time), week = lubridate::isoweek(time)) 
  }
  
  
  if(survey == "incubation"){ 
    
    df <- SumIncubation(time= "date")  %>%  
      
      filter(Species_Code %in% species & !Island %in% "All Islands") %>% 
      
      mutate(month = lubridate::month(time),
             week = lubridate::isoweek(time)) 
    
  }
  
  
  # calculate historic variation
  
  df_histsum <- filter(df, !year %in% current_yr) %>% # exclude current year
    group_by(., Species_Code,  CommonName,  FullLatinName, Island, variable, month, week,  stat) %>% 
    summarize(num_samps = sum(!is.na(value)),
              median_val = median(value, na.rm = TRUE),
              min_val = min(value, na.rm = TRUE),
              max_val = max(value, na.rm = TRUE),
              lower_100 = min(value, na.rm = T),
              upper_100 = max(value, na.rm = T),
              lower_95 = ifelse(num_samps >= 3, quantile(value, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 3, quantile(value, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 3, quantile(value, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 3, quantile(value, 0.75, na.rm = T), NA),
              .groups = "drop") |>
    filter(!is.na(lower_50))
  
  df_current <- filter(df, year %in% current_yr) %>% 
    mutate(metric_type = "value")
  
  df_med <- df_histsum |> select(Species_Code:stat, median_val) |>
    mutate(metric_type = "median")
  
  # Pivot summary data for plotting
  
  df_sum_plot <- df_histsum |>
    select(Species_Code:stat, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
    pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
                 names_to = "metric", values_to = "value") |>
    mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
           distrib = paste0("d", gsub("\\D", "", metric))) |>
    select(-metric) |>
    pivot_wider(values_from = value, names_from = metric_type) |>
    mutate(metric_type = distrib)
  
  
  
  # Plot ribbon for each metric_type (d50, d95, d100), grouped by variable
  
  plot_values <- c("d100" = "#B8D8ED", "d95" = "#7FB9DD", "d50" = "#1378b5", "median" = "blue",
                   "value" = "black")
  
  plot_breaks <- c("d100", "d95", "d50", "median" ,
                   "value")
  
  plot_labels <-c("d100" = "Historic range", "d95" = "Hist. 95% range",
                  "d50" = "Hist. 50% range", "median" = "Hist. median",
                  "value" = "Current counts")
  
  line_values <-c("median" = "solid")
  line_breaks <-NULL
  
  line_labels <- NULL
  
  # df_sum_plot$month <-  factor(df_sum_plot$month,
  #                          levels = df_sum_plot$month,
  #                          labels = month.abb[df_sum_plot$month], ordered = T)
  # 
  # df_current$month <- factor(df_current$month,
  #                         levels = df_current$month,
  #                         labels = month.abb[df_current$month], ordered = T)
  # 
  # df_med$month <-   factor(df_med$month,
  #                              levels = df_med$month,
  #                              labels = month.abb[df_med$month], ordered = T)
  
  # Create plot
  
  y1<- ggplot() +
    geom_ribbon(data = df_sum_plot, 
                aes(x = week, 
                    ymin = lower, ymax = upper, fill = metric_type, group= metric_type), alpha = 0.2) +
    
    geom_line(data = df_med,
              aes(y = median_val, x = week, 
                  color = metric_type, group = metric_type,
                  text = paste0("Island: ", Island, "<br>",
                                "Month: ", month, "<br>",
                                "Historic Median: ", round(median_val, 1), "<br>")), lwd = 0.7) +
    
    geom_point(data = df_current,
               aes(y = value, 
                   x = week,
                   color = metric_type, group = metric_type)) +
    scale_x_continuous(breaks = seq(1, 52, 1), 
                       #minor_breaks = seq(1, 52, 1),
                       name = "Week of Year") + 
    #expand_limits(x= c(19, 23))+ 
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
    labs(
      title = paste("Historical Seasonal Pattern (weekly) vs", current_yr, "Observations across Outer Islands"),
      y = paste ("No. of ", df_sum_plot$CommonName[1]),
      fill = "Metric Type",
      color = "Metric Type"
    ) +
    theme_bw()+
    theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 16)) +
    theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 12 )) +
    theme(axis.text.y = element_text(size = 12 )) +
    #theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
    theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
    theme(plot.title=element_text(size=12, vjust=2, face= "bold")) +
    #theme(strip.background= element_rect(size=10, color="gray" ))+
    #theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    facet_wrap( ~ Island, scales = "free_y", ncol= 3)
  
  suppressWarnings(print(y1))
  
}
PlotWeeklyObserver<- function(survey, current_yr, species, var , print= TRUE){
  
  library(tidyverse)
  library(magrittr)
  library(NETNCoastalBirds)
  
  outer<- c("Calf", "Little Calf", "Green", "The Graves", "Middle Brewster", 
          "Outer Brewster", "Shag Rocks","Little Brewster")
  
  if(survey == "creche"){

    
    df <- GetCrecheData()  # Summarize daily Primary Surveys


    df$Island <- plyr::mapvalues(df$Island, 
                                 from = c("Roaring Bulls"), 
                                 to = c("The Graves"))
    df<- df %>% 
          filter(Survey_Primary %in% "Yes" & Survey_Complete %in% "Yes" & Island %in% outer) %>%

      mutate(
        Date = as.Date(Date),
        week = isoweek(Date),
        Species_Code = as.factor(Species_Code),
        Observer = as.factor(Observer)
      ) %>% 
      dplyr::filter(Species_Unit %in% c("F-Lone", "Chick","F-Tend" )) %>% droplevels() %>% 
      
      group_by( Island, Species_Code, Segment, Date, year, month, week, Species_Unit, Observer) %>% 
     
      summarize(Unit_Count= sum(Unit_Count, na.rm = TRUE)) # summarize segment level counts by each oberver on each day
  }
  
  
  if(survey == "incubation"){ 
    
        df <- GetIncubationData()   %>% 
      filter(Species_Code %in% species & Survey_Primary %in% "Yes" & Survey_Complete %in% "Yes" & Island %in% outer) %>% 
      mutate(
        Date = as.Date(Date),
        week = isoweek(Date),
        Species_Code = as.factor(Species_Code),
        Observer = as.factor(Observer)
  )
    
}
  
  
  # calculate historic variation
  
  df_histsum <- filter(df, !year %in% current_yr) %>% # exclude current year
    {if(survey == "creche")  filter(., Species_Unit %in% var) else . } %>% 
    {if(survey == "creche") group_by(.,Island, Species_Code,  Segment, Species_Unit, week ) else # sum by life stage if needed
      group_by(.,Island, Segment, Species_Code, week) } %>% ## first summarize data by Island
    summarize(num_samps = sum(!is.na(Unit_Count)),
              median_val = median(Unit_Count, na.rm = TRUE),
              min_val = min(Unit_Count, na.rm = TRUE),
              max_val = max(Unit_Count, na.rm = TRUE),
              lower_100 = min(Unit_Count, na.rm = T),
              upper_100 = max(Unit_Count, na.rm = T),
              lower_95 = ifelse(num_samps >= 3, quantile(Unit_Count, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 3, quantile(Unit_Count, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 3, quantile(Unit_Count, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 3, quantile(Unit_Count, 0.75, na.rm = T), NA),
              .groups = "drop") %>% 
          filter(!is.na(lower_50))
  
  df_current <- filter(df, year %in% current_yr) %>% 
    { if(survey == "creche")  filter(., Species_Unit %in% var) else .} %>%
    mutate(metric_type = "value")
  
  df_med <- df_histsum |> select(Island:week, median_val) %>% 
    {if(survey == "creche") filter(., Species_Unit %in% var) else .} %>% 
    mutate(metric_type = "median")
  
  # Pivot summary data for plotting
  
  df_sum_plot <- df_histsum |>
    select(Island:week, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
    pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
                 names_to = "metric", values_to = "value") |>
    mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
           distrib = paste0("d", gsub("\\D", "", metric))) |>
    select(-metric) |>
    pivot_wider(values_from = value, names_from = metric_type) |>
    # subset data by COEI life stage
    mutate(metric_type = distrib)
  
  
  # Plot ribbon for each metric_type (d50, d95, d100), grouped by variable
  
  plot_values <- c("d100" = "#B8D8ED", "d95" = "#7FB9DD", "d50" = "#1378b5", "median" = "blue",
                   "value" = "black")
  
  plot_breaks <- c("d100", "d95", "d50", "median" ,
                   "value")
  
  plot_labels <-c("d100" = "Historic range", "d95" = "Hist. 95% range",
                  "d50" = "Hist. 50% range", "median" = "Hist. median",
                  "value" = "Current counts")
  
  line_values <-c("median" = "solid")
  line_breaks <-NULL
  
  line_labels <- NULL
  
  
  
  # Create plot
  
  y1<- ggplot() +
    geom_ribbon(data = df_sum_plot, 
                aes(x = week, 
                    ymin = lower, ymax = upper, fill = metric_type, group= metric_type), alpha = 0.2) +
    
    geom_line(data = df_med,
              aes(y = median_val, x = week, 
                  color = metric_type, group = metric_type,
                  text = paste0("Island: ", Island, "<br>",
                                "Week: ", week, "<br>",
                                "Historic Median: ", round(median_val, 1), "<br>")), lwd = 0.7) +
    
    geom_point(data = df_current,
               aes(y = Unit_Count, 
                   x = week,
                   color = metric_type, group = metric_type)) +
    scale_x_continuous(breaks = seq(1, 52, 1), 
                       #minor_breaks = seq(1, 52, 1),
                       name = "Week of Year") + 
    #expand_limits(x= c(19, 23))+ 
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
    labs(
      title = paste("Historical Seasonal Pattern (weekly) vs", current_yr, "Observations across Outer Islands"),
      y = {if (survey == "creche") paste ("No. of ", df_sum_plot$Species_Code[1], " ", df_sum_plot$Species_Unit[1]) else paste ("No. of ", df_sum_plot$Species_Code[1])}
        ,
      fill = "Metric Type",
      color = "Metric Type"
    ) +
    theme_bw()+
    theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 16)) +
    theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 10 )) +
    theme(axis.text.y = element_text(size = 12 )) +
    #theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
    theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
    theme(plot.title=element_text(size=12, vjust=2, face= "bold")) +
    #theme(strip.background= element_rect(size=10, color="gray" ))+
    #theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    facet_wrap( ~ Island +Segment, scales = "free_y", ncol= 3)
  
  
  #### CHOOSE TO PRINT ON EXECUTION OR CREATE OBJECT; THE LATTER HELPFUL WHEN LOOPING  
  
  if(print){
    
    suppressWarnings(print(y1))
    
  }
  else{
    
    return(y1)
  
  }
}


PlotHistBands<- function(survey, current_yr ){
  
  library(tidyverse)
  library(magrittr)
  library(NETNCoastalBirds)
  
  if(survey == "creche"){ 
  
  df <- SumCreche(time ="date", islands = "outer") %>%  # Summarize daily Primary Surveys
  
    filter(!variable %in% "Average creche size" & Island %in% "All Islands")
  }
  
  
  if(survey == "incubation"){ 
    
    df <- SumIncubation(time ="date")  %>%  # Summarize daily Primary Surveys
      
      filter(Island %in% "All Islands") %>% 
      
      mutate(month=lubridate::month(time), week = lubridate::isoweek(time))
    
    }
  
  
  # calculate historic variation
  
df_histsum <- filter(df, !year %in% current_yr) %>% # exclude current year
  group_by(., Species_Code,  CommonName,  FullLatinName, Island, variable, month, stat) %>% 
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

df_current <- filter(df, year %in% current_yr) %>% 
  mutate(metric_type = "value")

df_med <- df_histsum |> select(Species_Code:stat, median_val) |>
  mutate(metric_type = "median")

# Pivot summary data for plotting

df_sum_plot <- df_histsum |>
  select(Species_Code:stat, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
  pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
               names_to = "metric", values_to = "value") |>
  mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
         distrib = paste0("d", gsub("\\D", "", metric))) |>
  select(-metric) |>
  pivot_wider(values_from = value, names_from = metric_type) |>
  mutate(metric_type = distrib)



# Plot ribbon for each metric_type (d50, d95, d100), grouped by variable

plot_values <- c("d100" = "#B8D8ED", "d95" = "#7FB9DD", "d50" = "#1378b5", "median" = "blue",
                "value" = "black")
  
plot_breaks <- c("d100", "d95", "d50", "median" ,
                 "value")

plot_labels <-c("d100" = "Historic range", "d95" = "Hist. 95% range",
                "d50" = "Hist. 50% range", "median" = "Hist. median",
                "value" = "Current counts")
  
line_values <-c("median" = "solid")
line_breaks <-NULL

line_labels <- NULL

# df_sum_plot$month <-  factor(df_sum_plot$month,
#                          levels = df_sum_plot$month,
#                          labels = month.abb[df_sum_plot$month], ordered = T)
# 
# df_current$month <- factor(df_current$month,
#                         levels = df_current$month,
#                         labels = month.abb[df_current$month], ordered = T)
# 
# df_med$month <-   factor(df_med$month,
#                              levels = df_med$month,
#                              labels = month.abb[df_med$month], ordered = T)

# Create plot

y1<- ggplot() +
  geom_ribbon(data = df_sum_plot, 
              aes(x = month, 
                  ymin = lower, ymax = upper, fill = metric_type, group= metric_type), alpha = 0.2) +
 
  geom_line(data = df_med,
            aes(y = median_val, x = month, 
                color = metric_type, group = metric_type,
                text = paste0("Island: ", Island, "<br>",
                              "Month: ", month, "<br>",
                              "Historic Median: ", round(median_val, 1), "<br>")), lwd = 0.7) +
  
  geom_point(data = df_current,
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
  
  facet_grid( ~ Species_Code +variable, scales = "free_y", switch = "y") +
  labs(
    title = paste("Historical Seasonal Pattern (monthly) vs", current_yr, "Observations across all Outer Islands"),
    x = "Month",
    y = "No. of Common Eider",
    fill = "Metric Type",
    color = "Metric Type"
  ) +
  theme_bw()+
  theme(axis.text.y = element_text(color="black", vjust= 0.5, size = 16)) +
  theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 12 )) +
  theme(axis.text.y = element_text(size = 12 )) +
  #theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
  theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F)) +
  theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F)) +
  #theme(panel.background =  element_rect(fill="white", colour="black")) +
  #theme(panel.grid.major = element_line(colour = "grey90")) +
  theme(plot.title=element_text(size=12, vjust=2, face= "bold")) 
  #theme(strip.background= element_rect(size=10, color="gray"))

  

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

