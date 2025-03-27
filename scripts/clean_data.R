source("scripts/load_data.R")
library(dplyr)

# Filter data 
clean_dt <- combined_data %>% 
  filter(lfsstat == 1,# employed, at work
         age_12 %in% 3:8, # Core-aged only
         cowmain %in% 1:2, # public and private sector only
         ftptmain == 1, # full time
         schooln == 1) %>%  # non-students only
  mutate(
    hourlyearn = hrlyearn / 100, # convert cents to dollars
    female = as.integer(sex == 2), # 1 is female, 0 is not
    canadian = as.integer(immig == 3), # 1 is canadian, 0 is immigrant
    immigrant = case_when(
      immig == 3 ~ 0L, # canadian born
      immig == 1 ~ 1L, # recent immigrants
      immig == 2 ~ 2L # established immigrants
    )
  ) %>% 
  select(-hrlyearn, -sex, -immig, -ftptlast)

# convert categorical variables to factors
clean_dt$age_12 <- as.factor(clean_dt$age_12)
clean_dt$cma <- as.factor(clean_dt$cma)
clean_dt$estsize <- as.factor(clean_dt$estsize)
clean_dt$firmsize <- as.factor(clean_dt$firmsize)
clean_dt$marstat <- as.factor(clean_dt$marstat)
clean_dt$mjh <- as.factor(clean_dt$mjh)
clean_dt$noc_43 <- as.factor(clean_dt$noc_43)
clean_dt$union <- as.factor(clean_dt$union)

# Rename the variables
clean_dt <- clean_dt %>% 
  mutate(immigrant = recode(immigrant,
                            `0` = "Canadian",
                            `1` = "Recent immigrant",
                            `2` = "Established immigrant"),
         educ = recode(educ,
                       `0` = "0 to 8 years",
                       `1` = "Some HS",
                       `2` = "HS graduate",
                       `3` = "Some postsecondary",
                       `4` = "Postsecondary certificate or diploma",
                       `5` = "Bachelor's degree",
                       `6` = "Above bachelor's degree"),
         naics_21 = recode(naics_21,
                           `1` = "Agriculture",
                           `2` = "Forestry and logging and support activities for forestry",
                           `3` = "Fishing, hunting and trapping",
                           `4` = "Mining, quarrying, and oil and gas extraction",
                           `5` = "Utilities",
                           `6` = "Construction",
                           `7` = "Manufacturing",
                           `8` = "Manufacturing",
                           `9` = "Wholesale trade",
                           `10` = "Retail trade",
                           `11` = "Transportation and warehousing",
                           `12` = "Finance and insurance",
                           `13` = "Real estate and rental and leasing",
                           `14` = "Professional, scientific and technical services",
                           `15` = "Business, building and other support services",
                           `16` = "Educational services",
                           `17` = "Health care and social assistance",
                           `18` = "Information, culture and recreation",
                           `19` = "Accommodation and food services",
                           `20` = "Other services (except public administration)",
                           `21` = "Public administration"
                           ),
         female = recode(female, 
                         `1` = "Women",
                         `0` = "Men"),
         prov = recode(prov,
                       `10` = "Newfoundland and Labrador",
                       `11` = "Prince Edward Island",
                       `12` = "Nova Scotia",
                       `13` = "New Brunswick",
                       `24` = "Quebec",
                       `35` = "Ontario",
                       `46` = "Manitoba",
                       `47` = "Saskatchewan",
                       `48` = "Alberta",
                       `59` = "British Columbia")
  )

# Create Atlantic region
clean_dt <- clean_dt %>%
  mutate(region = case_when(
    prov %in% c("Prince Edward Island", 
                    "New Brunswick", 
                    "Nova Scotia", 
                    "Newfoundland and Labrador") ~ "Atlantic region",
    TRUE ~ prov
  ))

# rename occupations
clean_dt <- clean_dt %>% 
  mutate(occupation = recode(noc_10,
                             `1` = "Management",
                             `2` = "Business, finance, and administration",
                             `3` = "Natural and applied sciences and related",
                             `4` = "Health",
                             `5` = "Education, law and social, community and government services",
                             `6` = "Art, culture, recreation and sport",
                             `7` = "Sales and service",
                             `8` = "Trades, transport, and equipment operators",
                             `9` = "Natural resources, agriculture, and related production",
                             `10` = "Manufacturing and utilities")
  )

# Change date column to date class
clean_dt <- clean_dt %>% 
  mutate(date = as.Date(paste0(date, "-01")))
         
                       
                     