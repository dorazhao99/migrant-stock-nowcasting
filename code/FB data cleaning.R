#### FB cleaning ####

setwd("data")
library(tidyverse)

Facebook <- read_csv("facebookData.csv")

Facebook_expat <- select(Facebook, -c(total:female_age10)) %>%
  gather(age_sex_group, expat_pop, total_expat:female_expat_age10) %>% 
  mutate(age_sex_group = gsub("_expat", "", age_sex_group))


Facebook_total <- select(Facebook, -c(total_expat:female_expat_age10)) %>%
  gather(age_sex_group, total_pop, total:female_age10)

Facebook_tidy <- full_join(Facebook_expat, Facebook_total)

write_csv(Facebook_tidy, "Facebook_tidy.csv")
