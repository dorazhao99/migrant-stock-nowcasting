#### Accuracy analysis & graphing

#### Setup

setwd("code")
library(tidyverse)

#### Clean data

Mapes <- read_csv("model_mapes.csv")

Mapes <- gather(Mapes, method, mape, rf_mape:linreg_mape)

Mapes_subgroups <- filter(Mapes, model %in% c("autoregression_subgroups_added", 
                                               "fb_normalized_subgroups_added", 
                                               "autoregressive_with_fb_normalized_subgroups_added")) %>% 
  mutate(model = factor(model, levels = c("autoregression_subgroups_added",
                                          "fb_normalized_subgroups_added", 
                                          "autoregressive_with_fb_normalized_subgroups_added"), 
                        labels = c("Baseline", "Facebook", "Combined")))


# Mapes_og <- Mapes[-c(grep("subgroup", Mapes$model), 
#                      grep("data_used", Mapes$model)), ] %>% 
#   filter(model != "all_preds") %>% 
#   mutate(model = factor(model, levels = unique(Mapes_og$model))) %>% 
#   mutate(model_data = factor(case_when(model %in% c("autoregressive_baseline") ~ "Baseline", 
#                                 model %in% c("fb_naive", "fb_age_sex_normalized") ~ "Facebook", 
#                                 model %in% c("autoregressive_plus_fb", "autoregressive_with_fb_normalized") ~ "Combined"), 
#                              levels = c("Baseline", "Facebook", "Combined")), 
#          model_age_sex_adj = case_when(model %in% c("autoregressive_baseline", "fb_naive", "autoregressive_plus_fb") ~ "Not adjusted", 
#                                        model %in% c("fb_age_sex_normalized", "autoregressive_with_fb_normalized") ~ "Adjusted"))
# 
# Mapes_og <- filter(Mapes_og, model == "autoregressive_baseline") %>% 
#   mutate(model_age_sex_adj = "Adjusted") %>% 
#   bind_rows(Mapes_og) %>% 
#   unite("split_adj", model_age_sex_adj, split, remove = F)

#### Graphing

filter(Mapes_subgroups, method == "linreg_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions"))

filter(Mapes_subgroups, method == "rf_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions"))

filter(Mapes_subgroups, method == "xgboost_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions"))

# 
# 
# filter(Mapes_og, method == "linreg_mape") %>% 
#   ggplot(aes(y = mape, x = model, color = split, shape = split)) + 
#   geom_line(aes(group = split)) + 
#   geom_point() 
# 
