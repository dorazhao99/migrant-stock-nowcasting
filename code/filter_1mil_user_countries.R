#### Subset countries with 1 million+ fb users ####

### Setup
setwd("data")
library(tidyverse)

totals <- read_csv("FB_UN_totals.csv")
agesex <- read_csv("FB_UN_age_sex.csv")

### Select country codes w/ 1 million+ users
mil_countries <- filter(totals, fb_users >= 1000000) %>% 
  .$country_code

totals_1mil <- filter(totals, country_code %in% mil_countries)
agesex_1mil <- filter(agesex, country_code %in% mil_countries)

### Export data
write_csv(totals_1mil, "FB_UN_totals_1mil.csv")
write_csv(agesex_1mil, "FB_UN_age_sex_1mil.csv")
