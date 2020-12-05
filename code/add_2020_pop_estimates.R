#### Add UN 2020 pop estimates (estimates published in 2019) ####

#### Set up 
setwd("data")

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(countrycode)

UN_data_clean <- read_csv("UN_data_clean.csv")

#### Import 2020 pop data

### Both sexes

## Extract clean data from excel

Both_spreadsheet <- xlsx_cells("WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx")

Both_fill_colors <- xlsx_formats("WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx")$local$fill$patternFill$fgColor$rgb

Both <- filter(Both_spreadsheet, sheet == "ESTIMATES", row >= 17, !(col %in% c(1, 2, 4, 7)), !is_blank) %>% 
  mutate(fill_color = Both_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", type) %>% 
  behead("left", year) %>% 
  behead("up", age_group) %>%
  filter(type == "Country/Area" & is.na(fill_color) & year == 2020) %>% 
  select(-row, -col, -data_type, -character, -fill_color, -year, -type, -country_name, 
         total_pop_2020 = numeric) %>% 
  mutate(total_pop_2020 = total_pop_2020*1000, 
         sex = "both sexes")

## Group all 75+ together

Both <- filter(Both, age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>% 
  group_by(country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "75+", sex = "both sexes") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Both) %>% 
  filter(!(age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")))

## Get totals

Both <- group_by(Both, country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "Total", sex = "both sexes") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Both)

### Female

## Extract clean data from excel

Female_spreadsheet <- xlsx_cells("WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")

Female_fill_colors <- xlsx_formats("WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")$local$fill$patternFill$fgColor$rgb

Female <- filter(Female_spreadsheet, sheet == "ESTIMATES", row >= 17, !(col %in% c(1, 2, 4, 7)), !is_blank) %>% 
  mutate(fill_color = Female_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", type) %>% 
  behead("left", year) %>% 
  behead("up", age_group) %>%
  filter(type == "Country/Area" & is.na(fill_color) & year == 2020) %>% 
  select(-row, -col, -data_type, -character, -fill_color, -year, -type, -country_name, 
         total_pop_2020 = numeric) %>% 
  mutate(total_pop_2020 = total_pop_2020*1000, 
         sex = "female")

## Group all 75+ together

Female <- filter(Female, age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>% 
  group_by(country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "75+", sex = "female") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Female) %>% 
  filter(!(age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")))

## Get totals

Female <- group_by(Female, country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "Total", sex = "female") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Female)

### Male

## Extract clean data from excel

Male_spreadsheet <- xlsx_cells("WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")

Male_fill_colors <- xlsx_formats("WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")$local$fill$patternFill$fgColor$rgb

Male <- filter(Male_spreadsheet, sheet == "ESTIMATES", row >= 17, !(col %in% c(1, 2, 4, 7)), !is_blank) %>% 
  mutate(fill_color = Male_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", type) %>% 
  behead("left", year) %>% 
  behead("up", age_group) %>%
  filter(type == "Country/Area" & is.na(fill_color) & year == 2020) %>% 
  select(-row, -col, -data_type, -character, -fill_color, -year, -type, -country_name, 
         total_pop_2020 = numeric) %>% 
  mutate(total_pop_2020 = total_pop_2020*1000, 
         sex = "male")

## Group all 75+ together

Male <- filter(Male, age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>% 
  group_by(country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "75+", sex = "male") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Male) %>% 
  filter(!(age_group %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")))

## Get totals

Male <- group_by(Male, country_code) %>% 
  summarise(total_pop_2020 = sum(total_pop_2020)) %>% 
  mutate(age_group = "Total", sex = "male") %>% 
  select(total_pop_2020, country_code, age_group, sex) %>% 
  bind_rows(Male)

#### Wrangle data

### Combine

Totalpop_2020 <- rbind(Male, Female, Both) %>% 
  mutate(country_code = as.numeric(country_code))

### Merge w UN clean data

UN_data_clean_new <- full_join(UN_data_clean, Totalpop_2020, by = c("country_code", "age_group", "sex")) %>% 
  filter(!is.na(total_pop_2019))

#### Export data

write_csv(UN_data_clean_new, "UN_data_clean.csv")

#### Data QC

mutate(UN_data_clean_new, pct = (total_pop_2020-total_pop_2019)/total_pop_2019*100) %>% 
  View()
