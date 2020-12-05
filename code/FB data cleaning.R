#### FB cleaning ####

setwd("data")


Facebook <- read_csv("facebookData.csv")

Facebook_expat <- select(Facebook, -c(total:female_age10)) %>%
  gather(age_sex_group, expat_pop, total_expat:female_expat_age10) %>% 
  mutate(age_sex_group = gsub("_expat", "", age_sex_group))


Facebook_total <- select(Facebook, -c(total_expat:female_expat_age10)) %>%
  gather(age_sex_group, total_pop, total:female_age10)

Facebook_tidy <- full_join(Facebook_expat, Facebook_total)

write_csv(Facebook_tidy, "Facebook_tidy.csv")

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
