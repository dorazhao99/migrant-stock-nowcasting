#### Accuracy analysis & graphing

#### Setup

setwd("code")
library(tidyverse)

#### Clean data

Mapes <- read_csv("model_mapes.csv")

Mapes <- gather(Mapes, method, mape, rf_mape:linreg_mape) %>% 
  unique() %>% 
  filter(!grepl("subgroups$", model))

Mapes <- mutate(Mapes, model_data = factor(case_when(model %in% c("autoregressive_baseline", "autoregressive_subgroups_added") ~ "Baseline", 
                                              model %in% c("fb_naive", "fb_normalized_subgroups_added") ~ "Facebook", 
                                              model %in% c("autoregressive_plus_fb", "autoregressive_with_fb_normalized_subgroups_added") ~ "Combined"), 
                                    levels = c("Baseline", "Facebook", "Combined")), 
                model_age_sex_adj = factor(case_when(model %in% c("autoregressive_baseline", "fb_naive", "autoregressive_plus_fb") ~ "Not adjusted", 
                                              model %in% c("autoregressive_subgroups_added", "fb_normalized_subgroups_added", 
                                                           "autoregressive_with_fb_normalized_subgroups_added") ~ "Adjusted"), 
                                           levels = c("Not adjusted", "Adjusted"))) %>% 
     unite("split_adj", model_age_sex_adj, split, remove = F) %>% 
  unite("model_adj", model_data, model_age_sex_adj, remove = F) %>% 
  mutate(method = factor(method, levels = c("linreg_mape", "rf_mape", "xgboost_mape"), 
                         labels = c("Linear regression", "Random forest", "XGBoost"))) %>% 
  unite("model_method", model_data, method, remove = F) %>% 
  unite("split_method", split, method, remove = F) %>% 
  mutate(split = factor(split, levels = c("random_all", "train_test_high", "train_test_low"),
                        labels = c("All countries", "Developed regions", "Less developed regions")))

#### Graphing all countries total- fig 1

filter(Mapes, split == "All countries", model_age_sex_adj == "Not adjusted") %>% 
  ggplot(aes(y = mape, x = method, color = model_data, shape = model_data)) + 
  geom_line(aes(group = model_data)) + 
  geom_point(size = 2) + 
  xlab("Method") + ylab("MAPE") + 
  scale_shape_discrete(name = "Model") + 
  scale_color_discrete(name = "Model") + 
  scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250))

#### Graphing accuracy of facebook when adjusting- fig 2

filter(Mapes, split == "All countries", model_data == "Facebook") %>% 
  ggplot(aes(y = mape, x = model_age_sex_adj, color = method, shape = method)) + 
  geom_line(aes(group = method)) + 
  geom_point(size = 2) + 
  xlab("Age-sex adjusted") + ylab("MAPE") + 
  scale_shape_discrete(name = "Method") + 
  scale_color_discrete(name = "Method") + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375))

#### Graphing accuracy of baseline+combined when adjusting- fig 3

filter(Mapes, split == "All countries", model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model_age_sex_adj, color = method, shape = model_data)) + 
  geom_line(aes(group = model_method, linetype = model_data)) + 
  geom_point(size = 2) + 
  xlab("Age-sex adjusted") + ylab("MAPE") + 
  scale_shape_discrete(name = "Model") + scale_linetype_discrete(name = "Model") + 
  scale_color_discrete(name = "Method") + 
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20))

#### Graphing effect of country data quality on accuracy, method facet (for baseline & combined)- fig 4

filter(Mapes, model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model, color = split, shape = split)) + 
  geom_line(aes(group =split)) + 
  geom_point(size = 2) + facet_grid(~method) + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) +
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_x_discrete(labels = c("autoregressive_baseline" = "Baseline", "autoregressive_plus_fb" = "Combined", 
                              "autoregressive_subgroups_added" = "Baseline, age-sex adj.", 
                              "autoregressive_with_fb_normalized_subgroups_added" = "Combined, age-sex adj.")) + 
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) + 
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

#### Graphing effect of method on accuracy, data quality facet (for baseline & combined)- fig 5

filter(Mapes, model_data != "Facebook") %>% 
  ggplot(aes(y = mape, x = model, color = method, shape = method)) + 
  geom_line(aes(group =method)) + 
  geom_point(size = 2) + facet_grid(~split) + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Method") +
  scale_color_discrete(name = "Method") + 
  scale_x_discrete(labels = c("autoregressive_baseline" = "Baseline", "autoregressive_plus_fb" = "Combined", 
                              "autoregressive_subgroups_added" = "Baseline, age-sex adj.", 
                              "autoregressive_with_fb_normalized_subgroups_added" = "Combined, age-sex adj.")) + 
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) + 
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

#### Graphing effect of country data quality on accuracy, method facet (facebook)- fig 6

filter(Mapes, model_data == "Facebook") %>% 
  ggplot(aes(y = mape, x = model, color = split, shape = split)) + 
  geom_line(aes(group =split)) + 
  geom_point(size = 2) + facet_grid(~method) + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) +
  scale_color_discrete(name = "Countries", labels = c("All", "Developed regions", "Less developed regions")) + 
  scale_x_discrete(labels = c("fb_naive" = "Facebook", "fb_normalized_subgroups_added" = "Facebook, age-sex adj.")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375)) + 
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

#### Graphing effect of method on accuracy, data quality facet (facebook)- fig 7

filter(Mapes, model_data == "Facebook") %>% 
  ggplot(aes(y = mape, x = model, color = method, shape = method)) + 
  geom_line(aes(group =method)) + 
  geom_point(size = 2) + facet_grid(~split) + 
  xlab("Model") + ylab("MAPE") + 
  scale_shape_discrete(name = "Method") +
  scale_color_discrete(name = "Method") + 
  scale_x_discrete(labels = c("fb_naive" = "Facebook", "fb_normalized_subgroups_added" = "Facebook, age-sex adj.")) + 
  scale_y_continuous(breaks = seq(0, 350, 50), limits = c(0, 375)) + 
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))
