source("scripts/clean_data.R")
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(lubridate)
library(texreg)

# Linear regression
clean_dt <- clean_dt %>% 
  mutate(log_wage = log(hourlyearn))

# Add in the controls one by one to see the direction of the effect
# With no controls
mod1 <- lm(log_wage ~ immigrant,
           data = clean_dt)

# add female
mod2 <- lm(log_wage ~ immigrant + female,
           data = clean_dt)

# add education
mod3 <- lm(log_wage ~ immigrant + female + educ,
           data = clean_dt)

# add in tenure
mod4 <- lm(log_wage ~ immigrant + female + educ + tenure,
           data = clean_dt)

# add in province
mod5 <- lm(log_wage ~ immigrant + female + educ + tenure + prov, 
           data = clean_dt)

# add industry and occupation
mod6 <- lm(log_wage ~ immigrant + female + educ + tenure + prov +
             naics_21 + noc_43, 
           data = clean_dt)

# add union status
mod7 <- lm(log_wage ~ immigrant + female + educ + tenure + prov +
             naics_21 + noc_43 + union, 
           data = clean_dt)

# add family characteristics
mod8 <- lm(log_wage ~ immigrant + female + educ + tenure + prov +
             naics_21 + noc_43 + union + marstat, 
           data = clean_dt)

# All controls
mod9 <- lm(log_wage ~ immigrant + female + educ + tenure + naics_21 +
             region,
           data = clean_dt)


# Output the results into one table
results <- htmlreg(
  l = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9),
  file = "resultsv2.doc",
  single.row = TRUE,
  custom.coef.map = list("immigrantEstablished immigrant" = "Established immigrant",
                         "immigrantRecent immigrant" = "Recent immigrant"),
  stars = c(0.001, 0.01, 0.05, 0.1),
  digits = 4,
  booktabs = TRUE,
  table = TRUE,
  sideways = TRUE
  )

