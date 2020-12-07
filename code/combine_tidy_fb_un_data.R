#### Combine tidy facebook data and UN data ####

### Setup

setwd("data")
library(tidyverse)

Facebook <- read_csv("Facebook_tidy.csv")
UN <- read_csv("UN_data_clean.csv")

#### Create total dataset/not normalized

Facebook_totals <- filter(Facebook, age_sex_group == "total") %>% 
  select(-age_sex_group, -country) %>% 
  rename(fb_expats = expat_pop, fb_users = total_pop, 
         country_code = iso)

UN_totals <- filter(UN, !(age_group %in% c("Total", "0-4", "5-9", "10-14")), sex == "both sexes") %>% 
  select(-sex, -total_pop_2019, -data_used) %>% 
  group_by(country_name, year) %>% 
  summarise(migrant_pop = sum(migrant_pop), total_pop_2020 = sum(total_pop_2020)) %>% 
  spread(year, migrant_pop)

UN_totals <- filter(UN, year == 2019) %>% 
  select(country_code, country_name, data_used) %>% 
  unique() %>%
  full_join(UN_totals) %>% 
  rename(total_pop = total_pop_2020, migrant_pop_2015 = "2015", 
         migrant_pop_2017 = "2017", migrant_pop_2019 = "2019")

FB_UN_totals <- inner_join(UN_totals, Facebook_totals) %>% 
  mutate(fb_penetration = fb_users/total_pop)

write_csv(FB_UN_totals, "FB_UN_totals.csv")

#### Create normalized dataset

Facebook_age_sex <- filter(Facebook, age_sex_group %in% c(unique(Facebook$age_sex_group)
                                                          [c(15:length(unique(Facebook$age_sex_group)))])) %>% 
  select(-country) %>% 
  rename(fb_expats = expat_pop, fb_users = total_pop, 
         country_code = iso)

UN_age_sex <- filter(UN, !(age_group %in% c("Total", "0-4", "5-9", "10-14")), sex != "both sexes") %>% 
  select(-total_pop_2019, -data_used) %>% 
  spread(year, migrant_pop)

UN_age_sex <- filter(UN, year == 2019) %>% 
  select(country_code, country_name, data_used) %>% 
  unique() %>%
  full_join(UN_age_sex) %>% 
  rename(total_pop = total_pop_2020, migrant_pop_2015 = "2015", 
         migrant_pop_2017 = "2017", migrant_pop_2019 = "2019")

UN_age_sex <- filter(UN_age_sex, age_group %in% c("65-69", "70-74", "75+")) %>% 
  group_by(country_code, country_name, data_used, sex, un_development_lvl, oecd_member) %>% 
  summarise(total_pop = sum(total_pop), migrant_pop_2015 = sum(migrant_pop_2015), 
            migrant_pop_2017 = sum(migrant_pop_2017), migrant_pop_2019 = sum(migrant_pop_2019)) %>% 
  add_column(age_group = "65+", .before = "sex") %>% 
  bind_rows(UN_age_sex) %>% 
  filter(!(age_group %in% c("65-69", "70-74", "75+")))

UN_age_sex <- unite(UN_age_sex, "age_sex_group", sex:age_group) %>% 
  mutate(age_sex_group = as.factor(age_sex_group))

levels(UN_age_sex$age_sex_group) <- unique(Facebook_age_sex$age_sex_group)[c(12:22, 1:11)]
  
#### Join data

FB_UN_age_sex <- inner_join(UN_age_sex, Facebook_age_sex) %>% 
  .[, c(1:3, 5:6, 4, 7:12)] %>% 
  mutate(fb_penetration = fb_users/total_pop)

write_csv(FB_UN_age_sex, "FB_UN_age_sex.csv")

group_by(FB_UN_age_sex, country_code, country_name) %>% 
  summarise(total_pop = sum(total_pop), migrant_pop_2015 = sum(migrant_pop_2015), 
            migrant_pop_2017 = sum(migrant_pop_2017), migrant_pop_2019 = sum(migrant_pop_2019)) -> test

table(test$total_pop == FB_UN_totals$total_pop)
table(test$migrant_pop_2015 == FB_UN_totals$migrant_pop_2015)
table(test$migrant_pop_2017 == FB_UN_totals$migrant_pop_2017)
table(test$migrant_pop_2019 == FB_UN_totals$migrant_pop_2019)

