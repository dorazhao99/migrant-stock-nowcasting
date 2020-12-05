# Mapes <- read_csv("model_mapes.csv")

# Mapes <- gather(Mapes, method, mape, rf_mape:linreg_mape) %>% 
#   filter(method != "adaboost_mape", !(split %in% c("train_high_test_low", "train_low_test_high")), 
#                                       !(model %in% c("fb_age_sex_normalized", "autoregressive_with_fb_normalized", 
#                                                      "all_preds")))
