#### Combine tidy facebook data and UN data ####

### Setup

setwd("..")
library(tidyverse)

Facebook <- read_csv("Facebook_tidy.csv")
UN <- read_csv("UN_data_clean.csv")

#### Create total dataset/not normalized

Facebook_totals <- filter(Facebook, age_sex_group == "total") %>% 
  select(-age_sex_group, -country) %>% 
  rename(fb_expats = expat_pop, fb_users = total_pop, 
         country_code = iso)

UN_totals <- filter(UN, age_group == "Total", sex == "both sexes") %>% 
  select(-age_group, -sex, -total_pop_2019, -data_used) %>% 
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

# 
# 
# Mapes <- read_csv("model_mapes.csv")
# 
# 
# Mapes <- gather(Mapes, method, mape, rf_mape:linreg_mape) %>% 
#   filter(method != "adaboost_mape", !(split %in% c("train_high_test_low", "train_low_test_high")), 
#                                       !(model %in% c("fb_age_sex_normalized", "autoregressive_with_fb_normalized", 
#                                                      "all_preds")))
# 
# 
# 
# 
# 
# setwd("..")
# setwd("data")
# 
# FB_UN <- read_csv("facebook_un_combined_2020.csv")
