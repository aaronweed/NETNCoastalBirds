
# Scripts for caption and plotting functions to support QAQC reporting for NETN's Coastal Breeding Bird Program 

Incubation_methods<- {print("The nesting counts presented below show the total nests counted per comparable expert observers for each Island-Segement 
                      each day a survey was conducted.")}


SumIncubation_methods<- {print("The annual nesting counts summarized in each table and figure below were derived from the annual 
                               maximum counts among multiple Primary Surveys conducted within a year (May-July) or represent the average of the maximum count values from Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands.")}

Crechmethods<- {print(paste0("The following tables and graphs summarize data collected during boat-based Common Eider (creche) surveys conducted in ", params$year, " among all Outer Islands."))}

SumCreche_Annual<- {print("The annual counts summarized below were derived from the annual maximum counts among completed, Primary Surveys within a year (May-July) or represent the average of the completed, Primary Surveys among multiple expert birders. These count values are summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}

SumCreche_Daily<- {print("The daily counts summarized below were derived from completed, Primary Surveys conducted within a year (May-July) or represent the average of this daily count value among multiple expert birders. These count values were summarized for each island and among all Outer Islands. Note that counts from Roaring Bulls are combined with The Graves.")}



# Calc min/max 95% stats

current_yr<-"2025"

PlotHistBands<- function(survey , current_yr, time){
  
  if(survey == "incubation"){ 
    
    df <- filter(incub, Species_Code == species &  stat == "max", variable == var)
    
  }
  
  if(survey == "creche"){ df<-creche
  
  df <- filter(creche, Species_Code == species &  stat == "max", variable == var)
  
  }
  
  if(survey == "ground"){ 
    
    df <- filter(nests, Species_Code == species, variable == var)
    
  }
  
  # calculate historic variation
  
CBB_histsum <- filter(df, !year %in% current_yr) |> # select data without the current year's values
  group_by(Species_Code,  CommonName,  FullLatinName, Size_Units, Island, variable, stat) |>
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

CBB_current <- filter(df, year %in% current_yr) 

# Pivot data for plotting

CBB_sum_long <- wdat_sum |>
  select(SiteCode:LowerThreshold_corr, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
  pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
               names_to = "metric", values_to = "value") |>
  mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
         distrib = paste0("d", gsub("\\D", "", metric))) |>
  select(-metric) |>
  pivot_wider(values_from = value, names_from = metric_type) |>
  mutate(metric_type = distrib)

}
