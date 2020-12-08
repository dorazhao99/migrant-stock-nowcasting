##### Graph scrap #####

Mapes_total <- filter(Mapes, model %in% c("autoregressive_baseline", 
                                          "fb_naive", 
                                          "autoregressive_plus_fb")) %>% 
  mutate(model = factor(model, levels = c("autoregressive_baseline",
                                          "fb_naive", 
                                          "autoregressive_plus_fb"), 
                        labels = c("Baseline", "Facebook", "Combined")))

Mapes_subgroups <- filter(Mapes, model %in% c("autoregressive_subgroups_added", 
                                              "fb_normalized_subgroups_added", 
                                              "autoregressive_with_fb_normalized_subgroups_added")) %>% 
  mutate(model = factor(model, levels = c("autoregressive_subgroups_added",
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


#### Graphing all countries effect of adjusting

## linreg 
filter(Mapes, split == "random_all", method == "Linear regression", model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model_age_sex_adj, color = model_data, shape = model_data)) + 
  geom_line(aes(group = model_data)) + 
  geom_point(size = 2) + 
  xlab("Age-sex adjusted") + ylab("MAPE") + 
  scale_shape_discrete(name = "Model") + 
  scale_color_discrete(name = "Model") + 
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300))

## rf
filter(Mapes, split == "random_all", method == "Random forest", model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model_age_sex_adj, color = model_data, shape = model_data)) + 
  geom_line(aes(group = model_data)) + 
  geom_point(size = 2) + 
  xlab("Age-sex adjusted") + ylab("MAPE") + 
  scale_shape_discrete(name = "Model") + 
  scale_color_discrete(name = "Model") + 
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300))

## xgboost
filter(Mapes, split == "random_all", method == "XGBoost", model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model_age_sex_adj, color = model_data, shape = model_data)) + 
  geom_line(aes(group = model_data)) + 
  geom_point(size = 2) + 
  xlab("Age-sex adjusted") + ylab("MAPE") + 
  scale_shape_discrete(name = "Model") + 
  scale_color_discrete(name = "Model") + 
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300))

# 
# filter(Mapes, split == "random_all") %>% 
#   ggplot(aes(y = mape, x = method, color = model_data, shape = model_age_sex_adj)) + 
#   geom_line(aes(group = model_adj, linetype = model_age_sex_adj)) + 
#   geom_point(size = 2) + 
#   xlab("Model") + ylab("MAPE") + 
#   scale_shape_discrete(name = "Method", labels = c("Linear regression", "Random forest", "XGBoost")) + 
#   scale_color_discrete(name = "Method", labels = c("Linear regression", "Random forest", "XGBoost")) + 
#   scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))
# 
# 
# filter(Mapes, split == "random_all" & model_age_sex_adj == "Not adjusted") %>% 
#   ggplot(aes(y = mape, x = model_data, color = method, shape = method)) + 
#   geom_line(aes(group = method)) + 
#   geom_point(size = 2) + 
#   xlab("Model") + ylab("MAPE") + 
#   scale_shape_discrete(name = "Method", labels = c("Linear regression", "Random forest", "XGBoost")) + 
#   scale_color_discrete(name = "Method", labels = c("Linear regression", "Random forest", "XGBoost")) + 
#   scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

#### Graphing all

filter(Mapes, method == "linreg_mape") %>% 
  ggplot(aes(y = mape, x = model_data, color = split, shape = model_age_sex_adj)) + 
  geom_line(aes(group = split_adj, linetype = model_age_sex_adj)) + 
  geom_point(size = 2) + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Age-sex adjusted") + scale_linetype_discrete(name = "Age-sex adjusted") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes, method == "rf_mape") %>% 
  ggplot(aes(y = mape, x = model_data, color = split, shape = model_age_sex_adj)) + 
  geom_line(aes(group = split_adj, linetype = model_age_sex_adj)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Age-sex adjusted") + scale_linetype_discrete(name = "Age-sex adjusted") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes, method == "xgboost_mape") %>% 
  ggplot(aes(y = mape, x = model_data, color = split, shape = model_age_sex_adj)) + 
  geom_line(aes(group = split_adj, linetype = model_age_sex_adj)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Age-sex adjusted") + scale_linetype_discrete(name = "Age-sex adjusted") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

#### Graphing total

filter(Mapes_total, method == "linreg_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes_subgroups, method == "rf_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes_subgroups, method == "xgboost_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

#### Graphing subgroups

filter(Mapes_subgroups, method == "linreg_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes_subgroups, method == "rf_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

filter(Mapes_subgroups, method == "xgboost_mape") %>% 
  ggplot(aes(y = mape, x = model, shape = split, color = split)) + 
  geom_line(aes(group = split)) + 
  geom_point() + 
  xlab("Model") + ylab("MAPE") + 
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

# 
# 
# filter(Mapes_og, method == "linreg_mape") %>% 
#   ggplot(aes(y = mape, x = model, color = split, shape = split)) + 
#   geom_line(aes(group = split)) + 
#   geom_point() 
# 
