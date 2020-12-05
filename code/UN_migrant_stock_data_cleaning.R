#### Clean migrant stock data ####

#### Set up 
setwd("data")

library(tidyverse)
library(tidyxl)
library(unpivotr)
library(countrycode)

#### Data cleaning

### Migrant stock

## 2019

Stock_2019_spreadsheet <- xlsx_cells("UN_MigrantStockByAgeAndSex_2019.xlsx")

stock_2019_fill_colors <- xlsx_formats("UN_MigrantStockByAgeAndSex_2019.xlsx")$local$fill$patternFill$fgColor$rgb

Stock_2019 <- filter(Stock_2019_spreadsheet, sheet == "Table 1" & row >= 15 & !is_blank & col != 2 & col != 4) %>% 
  mutate(fill_color = stock_2019_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("up-left", sex) %>% 
  behead("up", age_group) %>% 
  behead("left", Year) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", data_used) %>% 
  filter(data_type == "numeric" & is.na(fill_color) & Year == 2019) %>% 
  select(-row, -col, -data_type, -character, -fill_color, migrant_pop = numeric) %>% 
  mutate(sex = gsub("^.*\\(", "", sex)) %>% 
  mutate(sex = gsub("\\).*$", "", sex))

## 2017

Stock_2017_spreadsheet <- xlsx_cells("UN_MigrantStockByAge_2017.xlsx")

stock_2017_fill_colors <- xlsx_formats("UN_MigrantStockByAge_2017.xlsx")$local$fill$patternFill$fgColor$rgb

Stock_2017 <- filter(Stock_2017_spreadsheet, sheet == "Table 1" & row >= 15 & !is_blank & col != 2 & col != 4) %>% 
  mutate(fill_color = stock_2017_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("up-left", sex) %>% 
  behead("up", age_group) %>% 
  behead("left", Year) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", data_used) %>% 
  filter(data_type == "numeric" & is.na(fill_color) & Year == 2017) %>% 
  select(-row, -col, -data_type, -character, -fill_color, migrant_pop = numeric) %>% 
  mutate(sex = gsub("^.*\\(", "", sex)) %>% 
  mutate(sex = gsub("\\).*$", "", sex))

## 2015

Stock_2015_spreadsheet <- xlsx_cells("UN_MigrantStockByAge_2015.xlsx")

stock_2015_fill_colors <- xlsx_formats("UN_MigrantStockByAge_2015.xlsx")$local$fill$patternFill$fgColor$rgb

Stock_2015 <- filter(Stock_2015_spreadsheet, sheet == "Table 6" & row >= 15 & !is_blank & col != 1 & col != 3) %>% 
  mutate(fill_color = stock_2015_fill_colors[local_format_id]) %>% 
  select(row, col, data_type, numeric, character, fill_color) %>% 
  behead("up-left", sex) %>% 
  behead("up", age_group) %>% 
  behead("left", country_name) %>% 
  behead("left", country_code) %>% 
  behead("left", data_used) %>% 
  filter(data_type == "numeric" & is.na(fill_color)) %>% 
  select(-row, -col, -data_type, -character, -fill_color, migrant_pop = numeric)  %>% 
  mutate(sex = gsub("^.*\\(", "", sex)) %>% 
  mutate(sex = gsub("\\).*$", "", sex)) %>% 
  add_column(Year = 2015, .before = "country_name")

### Total population (in 2019)

Totalpop_2019 <- filter(Stock_2019_spreadsheet, sheet == "Table 2" & row >= 15 & !is_blank & col != 2 & col != 4) %>%
  mutate(fill_color = stock_2019_fill_colors[local_format_id]) %>%
  select(row, col, data_type, numeric, character, fill_color) %>%
  behead("up-left", sex) %>%
  behead("up", age_group) %>%
  behead("left", Year) %>%
  behead("left", country_name) %>%
  behead("left", country_code) %>%
  behead("left", data_used) %>%
  filter(data_type == "numeric" & is.na(fill_color) & Year == 2019) %>%
  select(-row, -col, -data_type, -character, -fill_color, -Year, -country_name, -data_used, 
         total_pop_2019 = numeric) %>%
  mutate(sex = case_when(grepl("both sexes", sex) ~ "both sexes",
                         grepl(" male", sex) ~ "male",
                         grepl("female", sex) ~ "female"),
         total_pop_2019 = total_pop_2019*1000)

### Level of development (as classified by the UN in 2019)

Development_2019 <- filter(Stock_2019_spreadsheet, sheet == "ANNEX" & row >= 16 & !(col %in% c(1, 3, 8:15))) %>%
  mutate(fill_color = stock_2019_fill_colors[local_format_id]) %>%
  select(row, col, data_type, numeric, character, fill_color) %>%
  behead("left", country_name) %>%
  behead("left", country_code) %>%
  behead("left", data_used) %>%
  behead("up", development_lvl) %>%
  filter(data_type == "character" & is.na(fill_color)) %>%
  select(-row, -col, -data_type, -numeric, -character, -fill_color, -country_name, -data_used) %>% 
  mutate(country_code = as.numeric(country_code))

### Join data

UN_data_clean <- rbind(Stock_2019, Stock_2017, Stock_2015) %>% 
  full_join(Totalpop_2019, by = c("country_code", "sex", "age_group")) %>% 
  full_join(Development_2019, by = "country_code") %>% 
  filter(!is.na(total_pop_2019) & !is.na(development_lvl))

### Clean final dataset

UN_data_clean <- mutate(UN_data_clean, country_name = 
                          countrycode(country_code, "iso3n", "country.name")) %>% 
  filter(!is.na(country_name)) %>% 
  select(country_name, country_code, year = Year, age_group, sex, migrant_pop, total_pop_2019, data_used, 
         un_development_lvl = development_lvl)

UN_data_clean <- group_by(UN_data_clean, country_code) %>% 
  summarise(frequency = n()) %>% 
  full_join(UN_data_clean) %>% 
  filter(frequency > 3) %>% 
  select(-frequency)

## Add new OECD variable

UN_data_clean <- mutate(UN_data_clean, 
                        oecd_member = if_else(country_name %in% c("Australia", "Austria", "Belgium", "Canada", 
                                                                    "Chile", "Colombia", "Czechia", "Colombia", 
                                                                    "Czechia", "Denmark", "Estonia", "Finland", 
                                                                    "France", "Germany", "Greece", "Hungary", 
                                                                    "Iceland", "Ireland", "Israel", "Italy", 
                                                                    "Japan", "South Korea", "Latvia", "Lithuania", 
                                                                    "Luxembourg", "Mexico", "Netherlands", "New Zealand", 
                                                                    "Norway", "Poland", "Portugal", "Slovakia", 
                                                                    "Slovenia", "Spain", "Sweden", "Switzerland", 
                                                                    "Turkey", "United Kingdom", "United States"), T, F))

#### Export dataset

write_csv(UN_data_clean, "UN_data_clean.csv")

#### Quality checks

filter(UN_data_clean, oecd_member == T) %>% 
  select(country_name) %>% unique() %>% 
  View()

mutate(UN_data_clean, migrant_pct = migrant_pop/total_pop_2019) %>% 
  select(country_name, year, age_group, sex, migrant_pct) %>% 
  View()

select(UN_data_clean, country_name, oecd_member, data_used) %>% 
  unique() %>% View()

table(UN_data_clean$data_used, UN_data_clean$oecd_member)

table(UN_data_clean$data_used, UN_data_clean$un_development_lvl)


