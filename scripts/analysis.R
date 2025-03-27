source("clean_data.R")
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(lubridate)

# Median wage by group ---------------------------------------------------

# data on the average wage by group from 2015 to 2024
avg_wage <- clean_dt %>%
  mutate(year = year(date)) %>% 
  group_by(year, immigrant) %>% 
  summarise(avg_wage = mean(hourlyearn, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = immigrant,      # Pivot wider by immigrant status
                         values_from = avg_wage 
                         )
write.csv(avg_wage, "wage_yearly.csv", row.names = FALSE)

avg_wage_all <- clean_dt %>% 
  group_by(immigrant) %>% 
  summarise(avg_wage = mean(hourlyearn, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = immigrant,      # Pivot wider by immigrant status
              values_from = avg_wage 
  )



# Yearly wage gap by education --------------------------------------------
yearly_educ <- clean_dt %>%
  group_by(educ, immigrant) %>%       # Group by year, educational attainment, and immigrant status
  summarise(median_wage = median(hourlyearn, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = immigrant,      # Pivot wider by immigrant status
              values_from = median_wage, 
              names_prefix = "median_wage_")
write.csv(yearly_educ, "yearly_educ.csv", row.names = FALSE)

# Median wage gap by province ---------------------------------------------
region_wage <- clean_dt %>%
  group_by(region, immigrant) %>%
  summarise(median_wage = median(hourlyearn, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = immigrant, 
              values_from = median_wage,
              names_prefix = "median_wage_")
write.csv(region_wage, "region_wage.csv", row.names = FALSE)

# for all of canada
canada_wage <- clean_dt %>%
  group_by(immigrant) %>%
  summarise(median_wage = median(hourlyearn, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = immigrant, 
              values_from = median_wage,
              names_prefix = "median_wage_")

# Median wage by industry -------------------------------------------------
industry_wage <- clean_dt %>%
  group_by(naics_21, immigrant) %>%
  summarise(median_wage = median(hourlyearn, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = immigrant, 
              values_from = median_wage,
              names_prefix = "median_wage_")
write.csv(industry_wage, "industry_wage.csv", row.names = FALSE)

# Proportion by industry  
prop_industry <- clean_dt %>% 
  group_by(naics_21, immigrant) %>% 
  summarise(
    total = n()
  ) %>% 
  pivot_wider(names_from = immigrant, 
                 values_from = total,
                 names_prefix = "total")
write.csv(prop_industry, "prop_industry.csv", row.names = FALSE)

