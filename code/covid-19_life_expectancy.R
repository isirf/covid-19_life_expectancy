##########################################################################################
############################### COVID-19 death data ######################################
### https://dc-covid.site.ined.fr/en/data/pooled-datafiles/

library(dplyr)
library(httr)
library(jsonlite)
library(dslabs)
library(stringr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(reshape2)
library(writexl)
library(ggplot2)
library(ggforce)
library(gtools)
library(forcats)

run_api = FALSE
load_api_results = TRUE

color_palette = c("#009E73", "#0072B2", "#F0E442")
color_palette_big_bar = c("#F0E442", "#0072B2", "#009E73")
color_palette_big_bar_alt = c("#0072B2", "#F0E442", "#009E73")

labels_variable = c("deaths", "lost years")

### read csv ined - covid pooled
### 30.11.2020 with read_csv
### 29.01.2021 with read_csv2
### 05.02.2021 with read_csv
ined_covid_all = read_csv("/Users/isabell/GitHub/covid_life_expectancy/data/covid_pooled_2021-02-19.csv", col_names = TRUE)
ihme_other_diseases_all = read_csv("/Users/isabell/GitHub/covid_life_expectancy/data/ihme_other_diseases.csv", col_names = TRUE)

### sweden has no data by gender for all age groups below 49 except the odd 0-49 group
### sweden also has no shares for 70-79, but 70-74 and 75-79 ->  calculate  average share of the two smaller groups and fill large group
### same for 80-89 (80-84 and 85-89 have shares)
### older groups, other way round -> calculate avg male and female ratio of the large groups -> fill smaller groups 
ined_covid_sweden = ined_covid_all %>%
  filter(country == "Sweden" & age_group %in% c("0-49", "70-74", "75-79", "80-84", "85-89")) %>%
  mutate(age_group = if_else(age_group %in% c("70-74", "75-79"), "70-79", age_group),
         age_group = if_else(age_group %in% c("80-84", "85-89"), "80-89", age_group),
         share_male = cum_death_male / cum_death_both) %>%
  group_by(country, age_group) %>%
  summarize(share_male = mean(share_male, na.rm = TRUE))

sweden_share_male_0_49 = ined_covid_sweden[ined_covid_sweden$age_group == "0-49",]$share_male
sweden_share_male_70_79 = ined_covid_sweden[ined_covid_sweden$age_group == "70-79",]$share_male
sweden_share_male_80_89 = ined_covid_sweden[ined_covid_sweden$age_group == "80-89",]$share_male

rm(ined_covid_sweden)

### calculate shares manually for england & wales - 0-19, 20-39, 40-59, 60-79, 80+, italy - 80+
ined_covid_england = ined_covid_all %>%
  filter(country == "England & Wales" & !(age_group %in% c("0-19", "20-39", "40-59", "60-79", "80+", "Total known", "Total unknown", "Total"))) %>%
  mutate(age_group = if_else(age_group %in% c("<1", "1-4", "5-9", "10-14", "15-19"), "0-19", age_group),
         age_group = if_else(age_group %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", age_group),
         age_group = if_else(age_group %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", age_group),
         age_group = if_else(age_group %in% c("60-64", "65-69", "70-74", "75-79"), "60-79", age_group),
         age_group = if_else(age_group %in% c("80-84", "85-89", "90+"), "80+", age_group),
         share_male = cum_death_male / cum_death_both) %>%
  group_by(country, age_group) %>%
  summarize(share_male = mean(share_male, na.rm = TRUE))

england_share_male_0_19 = ined_covid_england[ined_covid_england$age_group == "0-19",]$share_male
england_share_male_20_39 = ined_covid_england[ined_covid_england$age_group == "20-39",]$share_male
england_share_male_40_59 = ined_covid_england[ined_covid_england$age_group == "40-59",]$share_male
england_share_male_60_79 = ined_covid_england[ined_covid_england$age_group == "60-79",]$share_male
england_share_male_80_101 = ined_covid_england[ined_covid_england$age_group == "80+",]$share_male

rm(ined_covid_england)

### italy (odd group disappeared with the data update, 19/02)
# ined_covid_italy = ined_covid_all %>%
#   filter(country == "Italy" & age_group %in% c("80-89", "90+")) %>%
#   mutate(age_group = "80+",
#          share_male = cum_death_male / cum_death_both) %>%
#   group_by(country, age_group) %>%
#   summarize(share_male = mean(share_male, na.rm = TRUE))
# 
# italy_share_male_80_101 = ined_covid_italy[ined_covid_italy$age_group == "80+",]$share_male
# 
# rm(ined_covid_italy)

### germany oldest age group starts with 90+, then changed to 90-99 and 100+, goes back to 90+ again (dec 2020)
### -> aggregate  age groups so that it's 90+ for the whole
### 90-99 and 100+ has 300 entries -> 150 rows less in total
ined_covid_germany = ined_covid_all %>%
  subset(country == "Germany")

ined_covid_without_germany = ined_covid_all %>%
  subset(country != "Germany")

### adjust age groups (once again)
ined_covid_germany = ined_covid_germany %>%
  mutate(age_group = if_else(country == "Germany" & age_group %in% c("90-99", "100+"), "90+", age_group)) %>%
  group_by(region, country, country_no, country_code, excelsource, excelsheet, age_group, pop_date, death_reference_date, death_reference_date_type) %>%
  summarize(pop_male = sum(pop_male, na.rm = TRUE),
            pop_female = sum(pop_female, na.rm = TRUE),
            pop_both = sum(pop_both, na.rm = TRUE),
            cum_death_male = sum(cum_death_male, na.rm = TRUE),
            cum_death_female = sum(cum_death_female, na.rm = TRUE),
            cum_death_unknown = sum(cum_death_unknown, na.rm = TRUE),
            cum_death_both = sum(cum_death_both, na.rm = TRUE))

### bind again
ined_covid_all = bind_rows(ined_covid_without_germany, ined_covid_germany)

rm(ined_covid_without_germany, ined_covid_germany)

# ### some countries have odd age groups at the beginning that are only used for a limited time
# ### denmark: 0-60 (5 deaths), 0-70 (7)
# ### england & wales: 0-19 (0), 80-101 (0)
# ### germany: 0-59 (58), 90-101 (20)
# ### italy: 80-101 (0)
# ### sweden: 0-49 (35), 70-79, 80-89
# 
# ### remove age groups with zero deaths (england & wales,italy). For germany, change 90-101 to 90-99
# # ined_covid_all = ined_covid_all %>%
# #   filter(!(country == "England & Wales" & age_group %in% c("0-19", "80+"))) %>%
# #   filter(!(country == "Italy" & age_group == "80+")) %>%
# #   mutate(age_group = if_else(country == "Germany" & age_group == "90+", "90-99", age_group))
# 
# ### leaves the two groups for denmark (0-60, 0-70), one for germany (0-59) and one for sweden (0-49).

### change death_reference_date from character to date format (R standard)
ined_covid_all$death_reference_date = as.Date(parse_date_time(ined_covid_all$death_reference_date, c('dmy', 'dmy_HMS')))

### replace age_group 85+ with 85-101 and <4 with 0-4
ined_covid_all$age_group = str_replace_all(ined_covid_all$age_group, c("\\+" = "-101", "\\<" = "0-"))

### convert age group to factor so that ggplot orders them in the correct way
ined_covid_all = ined_covid_all %>%
  mutate(age_group = factor(age_group, levels = mixedsort(unique(age_group))))

### extract population numbers per country, age group and sex
ined_pop = ined_covid_all %>%
  subset(!(age_group %in% c("Total unknown", "Total", "Total known"))) %>%
  select(country, age_group, pop_male, pop_female) %>%
  unique()

ined_pop_long = ined_pop %>%
  pivot_longer(cols = starts_with("pop"),
               names_to = "pop_sex",
               names_prefix = "pop_",
               values_to = "pop")

### remove columns & rows with "Total unknown", "Total", "Total known"
ined_deaths = ined_covid_all %>% 
  select(-c("excelsource", "excelsheet", "death_reference_date_type", "pop_date", "pop_male", "pop_female", "pop_both", "region", "country_no")) %>%
  subset(!(age_group %in% c("Total unknown", "Total", "Total known")))

### set cum_death_unknown to 0 instead of NA
ined_deaths = ined_deaths %>%
  mutate(cum_death_unknown = if_else(is.na(cum_death_unknown), 0, cum_death_unknown))

### data without entries in cum_death_male/cum_death_female -> calculate shares to use them for NA later
ined_deaths = ined_deaths %>%
  mutate(share_male = ifelse(is.na(cum_death_male) | (cum_death_both - cum_death_unknown) == 0, NA, cum_death_male / (cum_death_both - cum_death_unknown)),
         share_female = ifelse(is.na(cum_death_female) | (cum_death_both - cum_death_unknown) == 0, NA, cum_death_female / (cum_death_both - cum_death_unknown)))

### fill up, then down - in some cases data is disaggregated in between but then not anymore later on
ined_deaths = ined_deaths %>%
  group_by(country, age_group) %>%
  fill(c(share_male, share_female), .direction = "updown")

ined_share_summary = ined_deaths %>%
  group_by(country, age_group) %>%
  summarize(mean_share = mean(share_male, na.rm = TRUE))

### set shares for Sweden manually as the data is not disaggregated by gender for all age groups below 49 (except the large 0-49 group),
### and not for the groups 70-79 and 80-89.
ined_deaths = ined_deaths %>%
  mutate(share_male = if_else(country == "Sweden" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49"), sweden_share_male_0_49, share_male),
         share_female = if_else(country == "Sweden" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49"), 1 - sweden_share_male_0_49, share_female),
         share_male = if_else(country == "Sweden" & age_group == "70-79", sweden_share_male_70_79, share_male),
         share_female = if_else(country == "Sweden" & age_group == "70-79", 1 - sweden_share_male_70_79, share_female),
         share_male = if_else(country == "Sweden" & age_group == "80-89", sweden_share_male_80_89, share_male),
         share_female = if_else(country == "Sweden" & age_group == "80-89", 1 - sweden_share_male_80_89, share_female))

### also set shares for England & Wales manually.
ined_deaths = ined_deaths %>%
  mutate(share_male = if_else(country == "England & Wales" & age_group == "0-19", england_share_male_0_19, share_male),
         share_female = if_else(country == "England & Wales" & age_group == "0-19", 1 - england_share_male_0_19, share_female),
         share_male = if_else(country == "England & Wales" & age_group == "20-39", england_share_male_20_39, share_male),
         share_female = if_else(country == "England & Wales" & age_group == "20-39", 1 - england_share_male_20_39, share_female),
         share_male = if_else(country == "England & Wales" & age_group == "40-59", england_share_male_40_59, share_male),
         share_female = if_else(country == "England & Wales" & age_group == "40-59", 1 - england_share_male_40_59, share_female),
         share_male = if_else(country == "England & Wales" & age_group == "60-79", england_share_male_60_79, share_male),
         share_female = if_else(country == "England & Wales" & age_group == "60-79", 1 - england_share_male_60_79, share_female),
         share_male = if_else(country == "England & Wales" & age_group == "80-101", england_share_male_80_101, share_male),
         share_female = if_else(country == "England & Wales" & age_group == "80-101", 1 - england_share_male_80_101, share_female))

### and for Italy.
# ined_deaths = ined_deaths %>%
#   mutate(share_male = ifelse(country == "Italy" & age_group == "80-101", italy_share_male_80_101, share_male),
#          share_female = ifelse(country == "Italy" & age_group == "80-101", 1 - italy_share_male_80_101, share_female))

### replace NA in cum_death_male & cum_death_female with share_male & share_female
ined_deaths = ined_deaths %>%
  mutate(cum_death_male = ifelse(is.na(cum_death_male) & cum_death_both > 0, round(share_male * cum_death_both), cum_death_male),
         cum_death_female = ifelse(is.na(cum_death_female) & cum_death_both >0, round(share_female * cum_death_both), cum_death_female))

### if cum_death_both == 0 & cum_death_female/male == NA -> replace cum_death_female/male with 0
ined_deaths = ined_deaths %>%
  mutate(cum_death_male = ifelse(cum_death_both == 0 & is.na(cum_death_male), 0, cum_death_male),
         cum_death_female = ifelse(cum_death_both == 0 & is.na(cum_death_female), 0, cum_death_female))

### new variable to distribute cum_death_unknown to cum_death_male/female 
ined_deaths = ined_deaths %>%
  mutate(cum_death_male_unk = round(cum_death_male + cum_death_unknown * share_male),
         cum_death_female_unk = round(cum_death_female + cum_death_unknown * share_female))

### check if there are too many/little deaths after distributing the unknown via share_male/female
ined_deaths = ined_deaths %>%
  mutate(control_both = cum_death_both - cum_death_male_unk - cum_death_female_unk)

### if control_both = 1 -> did more women or men die in their age bracket? -> count this person as male/female
### if control_both = -1 -> did more women or men die in their age bracket? -> subtract this person from male/female
ined_deaths = ined_deaths %>%
  group_by(country, age_group) %>%
  mutate(share_male_age_group = sum(cum_death_male, na.rm = TRUE) / (sum(cum_death_both, na.rm = TRUE) - sum(cum_death_unknown, na.rm = TRUE)),
         share_female_age_group = sum(cum_death_female, na.rm = TRUE) / (sum(cum_death_both, na.rm = TRUE) - sum(cum_death_unknown, na.rm = TRUE)))

ined_deaths = ined_deaths %>%
  mutate(cum_death_male = ifelse(control_both == 1 & share_male_age_group > share_female_age_group, (cum_death_male - 1), cum_death_male),
         cum_death_female = ifelse(control_both == 1 & share_female_age_group > share_male_age_group, (cum_death_female - 1), cum_death_female),
         cum_death_male = ifelse(control_both == -1 & share_male_age_group > share_female_age_group, (cum_death_male + 1), cum_death_male),
         cum_death_female = ifelse(control_both == -1 & share_female_age_group > share_male_age_group, (cum_death_female + 1), cum_death_female))

#########################################################################################################################################################

### new column with country names in the population.io format -> country_pop_io
### country names with spaces now have a %20 instead of a space (for the API)
ined_deaths = ined_deaths %>%
  mutate(country_pop_io = country) %>%
  mutate(country_pop_io = replace(country_pop_io, country_pop_io == "Netherlands", "The%20Netherlands"),
         country_pop_io = replace(country_pop_io, country_pop_io == "United States", "United%20States"),
         country_pop_io = replace(country_pop_io, country_pop_io == "South Korea", "Rep%20of%20Korea"),
         country_pop_io = replace(country_pop_io, country_pop_io == "England & Wales", "United%20Kingdom"),
         country_pop_io = replace(country_pop_io, country_pop_io == "Scotland", "United%20Kingdom"))

### fill missing values with previous entry (first entry will remain NA)
ined_deaths = ined_deaths %>%
  group_by(country, age_group) %>%
  fill(c(cum_death_male, cum_death_female, cum_death_unknown))

### replace NA with 0
ined_deaths = ined_deaths %>%
  replace_na(list(cum_death_male = 0, cum_death_female = 0, cum_death_unknown = 0))

### order observations by death_reference_date within each country and age_group & calculate differences between dates => daily number of deaths
ined_deaths = ined_deaths %>% 
  arrange(country_code, age_group, death_reference_date) %>%
  group_by(country_code, age_group) %>%
  mutate(daily_death_f = c(0, diff(cum_death_female, lag = 1)), 
         daily_death_m = c(0, diff(cum_death_male, lag = 1)),
         daily_death_unknown = c(0, diff(cum_death_unknown, lag = 1)),
         daily_death_both = c(0, diff(cum_death_both, lag = 1))) 

### in older age_group there are already deaths in the first observation, code above overwrites those with 0
ined_deaths = ined_deaths %>%
  group_by(country_code, age_group) %>%
  mutate(daily_death_f = ifelse(row_number() == 1, cum_death_female, daily_death_f),
         daily_death_m = ifelse(row_number() == 1, cum_death_male, daily_death_m),
         daily_death_unknown = ifelse(row_number() == 1, cum_death_unknown, daily_death_unknown),
         daily_death_both = ifelse(row_number() == 1, cum_death_both, daily_death_both))

########################################

### split age_group in a min and max + if min_age_group = 0 and max_age_group = NA -> replace NA with 0.99
ined_deaths = ined_deaths %>%
  separate(age_group, c("min_age_group", "max_age_group"), "-", remove = FALSE)

### na_if replaces certain values with NA, replace_na replaces NAs with a given value & turn min_age_group and max_age_group into numeric
ined_deaths = ined_deaths %>%
  mutate(max_age_group = replace_na(max_age_group, "0.99"),
         min_age_group = as.numeric(min_age_group),
         max_age_group = as.numeric(max_age_group))

### check if the sum of deaths for austria is correct (compared to ined_covid_all)
# sum(ined_deaths$cum_death_both[ined_deaths$country_code == "AUT" & ined_deaths$death_reference_date == "2021-02-04"])
# 
# sum(ined_deaths$cum_death_female_unk[ined_deaths$country_code == "AUT" & ined_deaths$death_reference_date == "2021-02-04"], na.rm = TRUE)
# sum(ined_deaths$cum_death_male_unk[ined_deaths$country_code == "AUT" & ined_deaths$death_reference_date == "2021-02-04"], na.rm = TRUE)
# 
# sum(ined_deaths$daily_death_f[ined_deaths$country_code == "AUT"], na.rm = TRUE)
# sum(ined_deaths$daily_death_m[ined_deaths$country_code == "AUT"], na.rm = TRUE)
# sum(ined_deaths$daily_death_both[ined_deaths$country_code == "AUT"], na.rm = TRUE)
# 
# View(ined_deaths[ined_deaths$country == "Austria", c("min_age_group", "max_age_group", "death_reference_date", "cum_death_male", "daily_death_m", "cum_death_female", "daily_death_f", "cum_death_unknown", "daily_death_unknown", "cum_death_both", "daily_death_both", "cum_death_female_unk", "cum_death_male_unk")])
# 
# ined_deaths_test = ined_deaths %>%
#   group_by(country, age_group) %>%
#   mutate(cum_daily_death_f = cumsum(daily_death_f),
#          cum_daily_death_m = cumsum(daily_death_m),
#          cum_f_test = cum_death_female - cum_daily_death_f,
#          cum_m_test = cum_death_male - cum_daily_death_m)

### convert data (sex) from wide to long - other option: pivot_longer() & mean age
ined_deaths_long = ined_deaths %>%
  select(-c("cum_death_male", "cum_death_female", "cum_death_unknown", "cum_death_both", "share_male", "share_female", 
            "share_male_age_group", "share_female_age_group", "cum_death_male_unk", "cum_death_female_unk", "control_both")) %>%
  melt(id.vars = c("country", "country_code", "country_pop_io", "age_group", "min_age_group", "max_age_group", "death_reference_date"),
       variable.name = "daily_death_sex",
       value.name = "number_of_deaths") %>%
  mutate(daily_death_sex = as.character(daily_death_sex),
         mean_age = (min_age_group + max_age_group) / 2)

### rename values in the sex column (unknown = both)
ined_deaths_long = ined_deaths_long %>%
  mutate(daily_death_sex = replace(daily_death_sex, daily_death_sex == "daily_death_f", "female"),
         daily_death_sex = replace(daily_death_sex, daily_death_sex == "daily_death_m", "male"),
         daily_death_sex = replace(daily_death_sex, daily_death_sex == "daily_death_unknown", "both"),
         daily_death_sex = replace(daily_death_sex, daily_death_sex == "daily_death_both", "both"))


# sum(ined_deaths_long$number_of_deaths[ined_deaths_long$country_code == "AUT"])


##########################################################################################################################################
############################################################ API #######################################################################

### life expectancy for every age group in every country (death data)
### assumed that the person is in the middle of the age group (e.g. 14.5 years on their date of death) -> how many years have been lost?
### Germany, 10-19, 9.5.2020 -> 14,5 years

### remaining life expectancy (from API documentation)
### TotalLifeExpectancy {
###    sex (string) = ['male' or 'female']: given sex,
###    country (string, optional): given country,
###    date (date string): given reference date, as string with date format YYYY-MM-DD,
###    age (offset string): given age, as string in offset format ##y##m##d,
###    remaining_life_expectancy (float): calculated remaining life expectancy
### }

### API example 
# rle = GET("https://***population.io***")
# data_life_exp_rem = fromJSON(rawToChar(rle$content))
# names(data_life_exp_rem)

### filter out unknown and both from daily_death_sex
ined_deaths_long = ined_deaths_long %>%
  filter(daily_death_sex %in% c("male", "female"))

### loop for variables for API
ined_deaths_long$rle = NA

if (run_api) {
  for (i in 1:nrow(ined_deaths_long)) {
    current_mean_age = ined_deaths_long$mean_age[i]
    current_date_of_death = ined_deaths_long$death_reference_date[i]
    current_country = ined_deaths_long$country_pop_io[i]
    current_sex = ined_deaths_long$daily_death_sex[i]
    
    age_string = NULL
    
    if (current_mean_age %% 1 == 0) {
      age_string = paste0(current_mean_age, "y0m")
    } else if (current_mean_age %% 1 %in% c(0.5, 0.495)) {
      age_string = paste0(floor(current_mean_age), "y6m")
    }
    
    rle = GET(paste0("***",
                     current_sex,
                     "/",
                     current_country,
                     "/",
                     current_date_of_death,
                     "/",
                     age_string,
                     "/?***"))
    remaining_life_expectancy = fromJSON(rawToChar(rle$content))
    
    ined_deaths_long$rle[i] = remaining_life_expectancy$remaining_life_expectancy
    
    if (i %% 100 == 0) {
      print(i)  
    }
  }
  
  ### save rds
  saveRDS(ined_deaths_long, "/Users/isabell/GitHub/covid_life_expectancy/data/api_results.rds")
  
  ### export to xlsx (first results)
  write_xlsx(ined_deaths_long, "/Users/isabell/GitHub/covid_life_expectancy/data/first_results.xlsx")
}


### import rds from api
if (load_api_results) {
  ined_deaths_long = readRDS("/Users/isabell/GitHub/covid_life_expectancy/data/api_results.rds")
  
  ### convert data table from rds to tibble
  ined_deaths_long = ined_deaths_long %>%
    as_tibble()
  
  ### convert age group to factor so that age groups are plotted in the correct order
  ined_deaths_long = ined_deaths_long %>%
    mutate(age_group = factor(age_group, levels = mixedsort(unique(age_group))))  
}

ined_deaths_long = ined_deaths_long %>%
  mutate(number_of_deaths = as.numeric(number_of_deaths))

### lost_year_total
ined_deaths_long = ined_deaths_long %>%
  mutate(lost_years_total = round(number_of_deaths * rle, 1))

### new object with summarized data (without switzerland or age_group 0-49/0-59) - switzerland ok after data update 19/02
lost_years_per_country_total = ined_deaths_long %>%
  #filter(!((country_code == "CHE") | (min_age_group == 0 & max_age_group == 49) | (min_age_group == 0 & max_age_group == 59))) %>%
  group_by(country) %>%
  summarize(lost_years_total = sum(lost_years_total, na.rm = TRUE),
            deaths_total = sum(number_of_deaths, na.rm = TRUE))

lost_years_per_country_total = lost_years_per_country_total %>%
  mutate(lost_years_per_death = lost_years_total / deaths_total)

### export to xlsx (country summary)
write_xlsx(lost_years_per_country_total, "/Users/isabell/GitHub/covid_life_expectancy/data/country_summary.xlsx")

### export to xlsx (first summary)
write_xlsx(ined_deaths_long, "/Users/isabell/GitHub/covid_life_expectancy/data/first_summary.xlsx")

##########################################################################################################################################
############################################################ plots #######################################################################

### total number of deaths per country & age_group
deaths_lost_years_age_group_gender_count_wide = ined_deaths_long %>%
  group_by(country, age_group, daily_death_sex) %>%
  #group_by(country, age_group) %>%
  summarize(death_total = sum(number_of_deaths, na.rm = TRUE),
            lost_years_total = sum(lost_years_total, na.rm = TRUE))

deaths_lost_years_age_group_gender_count_wide = deaths_lost_years_age_group_gender_count_wide %>%
  mutate(lost_years_per_death = if_else(lost_years_total != 0, lost_years_total / death_total, 0))

### take care of "odd groups"
### sweden: 0-49, 70-79, 80-89; germany: 0-59; denmark: 0-60, 0-70; england & wales: 0-19, 20-39, 40-59, 60-79, 80-101
deaths_lost_years_odd_age_groups = deaths_lost_years_age_group_gender_count_wide %>%
  filter((country == "Sweden" & age_group %in% c("0-49", "70-79", "80-89")) |
           (country == "Denmark" & age_group %in% c("0-60", "0-70")) |
           (country == "Germany" & age_group == "0-59") |
           (country == "England & Wales" & age_group %in% c("0-19", "20-39", "40-59", "60-79", "80-101"))) %>%
  select(-c(lost_years_total, lost_years_per_death)) %>%
  rename(death_total_odd = death_total)

deaths_lost_years_normal_age_groups = deaths_lost_years_age_group_gender_count_wide %>%
  filter(!((country == "Sweden" & age_group %in% c("0-49", "70-79", "80-89")) |
             (country == "Denmark" & age_group %in% c("0-60", "0-70")) |
             (country == "Germany" & age_group == "0-59") |
             (country == "England & Wales" & age_group %in% c("0-19", "20-39", "40-59", "60-79", "80-101"))))

deaths_lost_years_normal_age_groups = deaths_lost_years_normal_age_groups %>%
  mutate(age_group_odd = NA) %>%
  mutate(age_group_odd = ifelse(country == "Sweden" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49"), "0-49", age_group_odd),
         age_group_odd = ifelse(country == "Sweden" & age_group %in% c("70-74", "75-79"), "70-79", age_group_odd),
         age_group_odd = ifelse(country == "Sweden" & age_group %in% c("80-84", "85-89"), "80-89", age_group_odd),
         age_group_odd = ifelse(country == "Denmark" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59"), "0-60", age_group_odd),
         age_group_odd = ifelse(country == "Germany" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59"), "0-59", age_group_odd),
         age_group_odd = ifelse(country == "England & Wales" & age_group %in% c("0-1", "1-4", "5-9", "10-14", "15-19"), "0-19", age_group_odd),
         age_group_odd = ifelse(country == "England & Wales" & age_group %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", age_group_odd),
         age_group_odd = ifelse(country == "England & Wales" & age_group %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", age_group_odd),
         age_group_odd = ifelse(country == "England & Wales" & age_group %in% c("60-64", "65-69", "70-74", "75-79"), "60-79", age_group_odd),
         age_group_odd = ifelse(country == "England & Wales" & age_group %in% c("80-84", "85-89", "90-101"), "80-101", age_group_odd)) %>%
  mutate(age_group_odd_2 = NA) %>%
  mutate(age_group_odd_2 = ifelse(country == "Denmark" & age_group %in% c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69"), "0-70", age_group_odd_2))

deaths_lost_years_normal_age_groups = deaths_lost_years_normal_age_groups %>%
  group_by(country, daily_death_sex, age_group_odd) %>%
  mutate(share_odd = death_total / sum(death_total)) %>%
  ungroup() %>%
  group_by(country, daily_death_sex, age_group_odd_2) %>%
  mutate(share_odd_2 = death_total / sum(death_total)) %>%
  ungroup()

### merge normal groups with odd groups (all except denmark 0-70)
deaths_lost_years_normal_age_groups = merge(deaths_lost_years_normal_age_groups, deaths_lost_years_odd_age_groups,
                                            by.x = c("country", "age_group_odd", "daily_death_sex"),
                                            by.y = c("country", "age_group", "daily_death_sex"), all.x = TRUE) %>%
  as_tibble()

deaths_lost_years_normal_age_groups = deaths_lost_years_normal_age_groups %>%
  mutate(death_total = if_else(!is.na(age_group_odd), round(death_total + share_odd * death_total_odd), death_total)) %>%
  select(-c(age_group_odd, share_odd, death_total_odd))

### merge again for denmark 0-70
deaths_lost_years_normal_age_groups = merge(deaths_lost_years_normal_age_groups, deaths_lost_years_odd_age_groups,
                                            by.x = c("country", "age_group_odd_2", "daily_death_sex"),
                                            by.y = c("country", "age_group", "daily_death_sex"), all.x = TRUE) %>%
  as_tibble()

deaths_lost_years_normal_age_groups = deaths_lost_years_normal_age_groups %>%
  mutate(death_total = if_else(!is.na(age_group_odd_2), round(death_total + share_odd_2 * death_total_odd), death_total)) %>%
  select(-c(age_group_odd_2, share_odd_2, death_total_odd))

### calculate new lost years total
deaths_lost_years_normal_age_groups = deaths_lost_years_normal_age_groups %>%
  mutate(lost_years_total = death_total * lost_years_per_death)

deaths_lost_years_age_group_gender_count_wide = deaths_lost_years_normal_age_groups

rm(deaths_lost_years_odd_age_groups)

### merge population data to death data
deaths_lost_years_age_group_gender_count_wide = merge(deaths_lost_years_age_group_gender_count_wide, ined_pop_long,
                                                      by.x = c("country", "age_group", "daily_death_sex"),
                                                      by.y = c("country", "age_group", "pop_sex"))

### calculate share for number of deaths per country, age_group, sex & lost_years as share
deaths_lost_years_age_group_gender_count_wide = deaths_lost_years_age_group_gender_count_wide %>%
  group_by(country, daily_death_sex) %>%
  mutate(death_share = death_total / sum(death_total),
         lost_years_share = lost_years_total / sum(lost_years_total))

### calculate deaths per population for each country, age_group and sex per 100.000
deaths_lost_years_age_group_gender_count_wide = deaths_lost_years_age_group_gender_count_wide %>%
  mutate(death_pop = (death_total / pop) * 100000)

### deaths per population for each country and age_group & lost_years_total (both sexes combined)
deaths_lost_years_age_group_count_wide = deaths_lost_years_age_group_gender_count_wide %>%
  group_by(country, age_group) %>%
  summarize(daily_death_both = sum(death_total),
            lost_years_total_both = sum(lost_years_total))

### deaths per population & lost_years_total (both sexes) from wide to long
deaths_lost_years_age_group_count_long = deaths_lost_years_age_group_count_wide %>%
  melt(id.vars = c("country", "age_group"),
       variable.name = "group",
       value.name = "count") %>%
  mutate(group = as.character(group)) %>%
  mutate(group = replace(group, group == "daily_death_both", "deaths"),
         group = replace(group, group == "lost_years_total_both", "lost years"))


### calculate share for number of deaths per country, age_group & lost_years as share (both sexes combined)
deaths_lost_years_share_wide = deaths_lost_years_age_group_count_wide %>%
  mutate(death_share = daily_death_both/ sum(daily_death_both),
         lost_years_share = lost_years_total_both / sum(lost_years_total_both))

saveRDS(deaths_lost_years_share_wide, "/Users/isabell/GitHub/covid_life_expectancy/data/deaths_lost_years_share_wide.rds")

### shares from wide to long
deaths_lost_years_share_long = deaths_lost_years_share_wide %>%
  select(!c(daily_death_both, lost_years_total_both)) %>%
  melt(id.vars = c("country", "age_group"),
       variable.name = "share_group",
       value.name = "share") %>%
  mutate(share_group = as.character(share_group)) %>%
  mutate(share_group = replace(share_group, share_group == "death_share", "death"),
         share_group = replace(share_group, share_group == "lost_years_share", "lost years"))

### plot death_total & lost_years_total
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_death_total.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = death_total, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = lost_years_total, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_total/death_total_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = death_total, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Number of deaths", title = current_country) +
    scale_fill_manual(values = color_palette)
  
  print(plot)
  
  dev.off()
}

##

pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_lost_years_total.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = lost_years_total, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = lost_years_total, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_lost_years_total/lost_years_total_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = lost_years_total, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Number of years of life lost", title = current_country) +
    scale_fill_manual(values = color_palette)
  
  print(plot)
  
  dev.off()
}


### plot death_share & lost_years_share
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_death_share.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = death_share, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = death_share, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_share/deaths_share_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = death_share, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Share of number of deaths", title = current_country) +
    scale_fill_manual(values = color_palette) +
    ylim(0, 0.8)
  
  print(plot)
  
  dev.off()
}

##

pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_lost_years_share.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = lost_years_share, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = lost_years_share, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_lost_years_share/lost_years_share_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = lost_years_share, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Share of years of life lost", title = current_country) +
    scale_fill_manual(values = color_palette) +
    ylim(0, 0.8)
  
  print(plot)
  
  dev.off()
}


### plot death_pop per 100.000
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_death_pop.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = death_pop, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = death_pop, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_pop/death_pop_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = death_pop, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Number of deaths per 100.000 people", title = current_country) +
    scale_fill_manual(values = color_palette)
  
  print(plot)
  
  dev.off()
}

### plot death share & lost_years share in one plot (small number of deaths compared to high number lost years)
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_death_lost_years_share.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_share_long %>%
  ggplot(aes(x = age_group, y = share, fill = share_group)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_share_long %>%
    ggplot(aes(x = age_group, y = share, fill = share_group)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_share_long$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_lost_years_share/deaths_lost_years_share_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_share_long %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = share, fill = share_group)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Group", x = "Age group", y = "Share", title = current_country) +
    scale_fill_manual(values = color_palette, labels = labels_variable) +
    ylim(0, 0.8)
  
  print(plot)
  
  dev.off()
}


### life years lost per death per age group
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/data/first_plots_lost_years_per_age_group.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_gender_count_wide %>%
  ggplot(aes(x = age_group, y = lost_years_per_death, fill = daily_death_sex)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x")

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_gender_count_wide %>%
    ggplot(aes(x = age_group, y = lost_years_per_death, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = color_palette) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free_x", page = i)
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_lost_years_per_age_group/lost_years_per_age_group_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_gender_count_wide %>%
    subset(country == current_country) %>%
    ggplot(aes(x = age_group, y = lost_years_per_death, fill = daily_death_sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(fill = "Gender", x = "Age group", y = "Average number of life years lost per death", title = current_country) +
    scale_fill_manual(values = color_palette)
  
  print(plot)
  
  dev.off()
}


### plot of death_total_both & lost_years_total_both in one plot (small number of deaths compared to high number lost years)
### scaling factor
scaling_factor = max(deaths_lost_years_age_group_count_long$count[deaths_lost_years_age_group_count_long$group == "deaths"]) / max(deaths_lost_years_age_group_count_long$count[deaths_lost_years_age_group_count_long$group == "lost years"])

### scaling with long data
deaths_lost_years_age_group_count_long = deaths_lost_years_age_group_count_long %>%
  mutate(count = ifelse(group == "lost years", count * scaling_factor, count))

### plot
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_lost_years_total.pdf",
    width = 10, 
    height = 10)
all_pages = deaths_lost_years_age_group_count_long %>%
  ggplot(aes(x = age_group)) +
  geom_bar(aes(y = count, fill = group, group = group),
           stat = "identity", position = position_dodge(),
           color = "black", alpha = .6)  +
  facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free") +
  scale_y_continuous(name = "total deaths",
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / scaling_factor, name = "total lost years", labels = scales::comma)) +
  labs(fill = "Variable", x = "Age group") +
  theme_bw() +
  theme(legend.position = 'top',
        plot.title = element_text(color = 'black',face = 'bold',hjust = 0.5),
        axis.text = element_text(color = 'black',face = 'bold'),
        axis.title = element_text(color = 'black',face ='bold'),
        legend.text = element_text(color = 'black',face = 'bold'),
        legend.title = element_text(color = 'black',face = 'bold')) +
  ggtitle('total deaths vs. total lost years')

required_n_pages <- n_pages(all_pages)

for(i in 1:required_n_pages){
  
  plot_x = deaths_lost_years_age_group_count_long %>%
    ggplot(aes(x = age_group)) +
    geom_bar(aes(y = count, fill = group, group = group),
             stat = "identity", position = position_dodge(),
             color = "black", alpha = .6) +
    facet_wrap_paginate(~ country, ncol = 1, nrow = 4, scales = "free", page = i) +
    scale_y_continuous(name = "total deaths",labels = scales::comma,sec.axis = sec_axis(~. / scaling_factor, name = "total lost years", labels = scales::comma)) +
    labs(fill = "Variable", x = "Age group")+
    theme_bw()+
    theme(legend.position = 'top',
          plot.title = element_text(color = 'black',face = 'bold',hjust = 0.5),
          axis.text = element_text(color = 'black',face = 'bold'),
          axis.title = element_text(color = 'black',face ='bold'),
          legend.text = element_text(color = 'black',face = 'bold'),
          legend.title = element_text(color = 'black',face = 'bold')) +
    ggtitle('total deaths vs. total lost years')
  
  
  print(plot_x)
}

dev.off()

### one PDF per country
for (current_country in deaths_lost_years_age_group_gender_count_wide$country) {
  deaths_lost_years_age_group_count_long_country = deaths_lost_years_age_group_count_long %>%
    filter(country == current_country)
  
  scaling_factor = max(deaths_lost_years_age_group_count_long_country$count[deaths_lost_years_age_group_count_long_country$group == "deaths"]) / max(deaths_lost_years_age_group_count_long_country$count[deaths_lost_years_age_group_count_long_country$group == "lost years"])
  
  ### scaling with long data
  deaths_lost_years_age_group_count_long_country = deaths_lost_years_age_group_count_long_country %>%
    mutate(count = ifelse(group == "lost years", count * scaling_factor, count))
  
  
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/first_plots_death_lost_years_total/death_lost_years_total_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  plot = deaths_lost_years_age_group_count_long_country %>%
    ggplot(aes(x = age_group)) +
    geom_bar(aes(y = count, fill = group, group = group),
             stat = "identity", position = position_dodge(),
             color = "black", alpha = .6)  +
    scale_y_continuous(name = "Total deaths",
                       labels = scales::comma,
                       sec.axis = sec_axis(~. / scaling_factor, name = "Total lost years", labels = scales::comma)) +
    labs(fill = "Variable", x = "Age group") +
    scale_fill_manual(values = color_palette) +
    theme_bw() +
    theme(legend.position = 'top',
          plot.title = element_text(color = 'black',face = 'bold',hjust = 0.5),
          axis.text = element_text(color = 'black',face = 'bold'),
          axis.title = element_text(color = 'black',face ='bold'),
          legend.text = element_text(color = 'black',face = 'bold'),
          legend.title = element_text(color = 'black',face = 'bold')) +
    ggtitle(paste0(current_country, ' - total deaths vs. total lost years'))
  
  print(plot)
  
  dev.off()
}


### now the bar plot that compares the different countries
#countries -> all minus Austria, Belgium, Scotland and USA
deaths_lost_years_share_plot_wide = deaths_lost_years_share_wide %>%
  filter(!(country %in% c("Austria", "Belgium", "Scotland", "United States")))

age_group_young = "<39"
age_group_middle = "40-69"
age_group_old = "70+"

deaths_lost_years_share_plot_wide = deaths_lost_years_share_plot_wide %>%
  mutate(age_group_plot = NA) %>%
  mutate(age_group_plot = replace(age_group_plot, country == "Canada" & age_group %in% c("0-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Canada" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Canada" & age_group %in% c("70-79", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Denmark" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Denmark" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Denmark" & age_group %in% c("70-79", "80-89", "90-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "England & Wales" & age_group %in% c("0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "England & Wales" & age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "England & Wales" & age_group %in% c("70-74", "75-79", "80-84", "85-89", "90-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "France" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "France" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "France" & age_group %in% c("70-79", "80-89", "90-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Germany" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Germany" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Germany" & age_group %in% c("70-79", "80-89", "90-99", "100-101", "90-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Italy" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Italy" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Italy" & age_group %in% c("70-79", "80-89", "90-101", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Moldova" & age_group %in% c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Moldova" & age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Moldova" & age_group %in% c("70-74", "75-79", "80-84", "85-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Netherlands" & age_group %in% c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Netherlands" & age_group %in% c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Netherlands" & age_group %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Norway" & age_group %in% c("0-40"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Norway" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Norway" & age_group %in% c("70-79", "80-89", "90-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Portugal" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Portugal" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Portugal" & age_group %in% c("70-79", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_group %in% c("70-79", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Spain" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Spain" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Spain" & age_group %in% c("70-79", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Switzerland" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Switzerland" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Switzerland" & age_group %in% c("70-79", "80-101"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Sweden" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Sweden" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Sweden" & age_group %in% c("70-79", "80-89", "90-101", "70-74", "75-79", "80-84", "85-89"), age_group_old),
         age_group_plot = replace(age_group_plot, country == "Ukraine" & age_group %in% c("0-9", "10-19", "20-29", "30-39"), age_group_young),
         age_group_plot = replace(age_group_plot, country == "Ukraine" & age_group %in% c("40-49", "50-59", "60-69"), age_group_middle),
         age_group_plot = replace(age_group_plot, country == "Ukraine" & age_group %in% c("70-79", "80-89", "90-101"), age_group_old))

deaths_lost_years_share_plot_wide_agg = deaths_lost_years_share_plot_wide %>%
  filter(!is.na(age_group_plot)) %>%
  group_by(country, age_group_plot) %>%
  summarize(daily_death_both = sum(daily_death_both),
            lost_years_total_both = sum(lost_years_total_both)) %>%
  mutate(death_share = daily_death_both / sum(daily_death_both),
         lost_years_share = lost_years_total_both / sum(lost_years_total_both))

deaths_lost_years_share_plot_wide_agg = deaths_lost_years_share_plot_wide_agg %>%
  arrange(age_group_plot, lost_years_share, country)

### Rearrange data so that countries are sorted by share of lost years of youngest age group
deaths_lost_years_share_plot_wide_agg = deaths_lost_years_share_plot_wide_agg %>% 
  ungroup() %>%
  arrange(age_group_plot, lost_years_share) %>%
  mutate(country = fct_inorder(country))

### write to pdf in separate folder.
pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/plots/big_bar_plots/country_comparison_lost_years_by_age_group.pdf",
    width = 10, 
    height = 8)
ggplot(deaths_lost_years_share_plot_wide_agg) +
  geom_col(aes(x = country, y = lost_years_share, fill = age_group_plot),
           position = position_fill(reverse = TRUE)) +
  coord_flip() +
  labs(fill = "Age group", y = "Proportion of years of life lost by age group", x = "Country") +
  scale_fill_manual(values = color_palette_big_bar)
dev.off()

### Rearrange data so that countries are sorted by share of deaths of youngest age group
deaths_lost_years_share_plot_wide_agg = deaths_lost_years_share_plot_wide_agg %>% 
  ungroup() %>%
  arrange(age_group_plot, death_share) %>%
  mutate(country = fct_inorder(country))

pdf(file = "/Users/isabell/GitHub/covid_life_expectancy/plots/big_bar_plots/country_comparison_deaths_by_age_group.pdf",
    width = 10, 
    height = 8)
ggplot(deaths_lost_years_share_plot_wide_agg) +
  geom_col(aes(x = country, y = death_share, fill = age_group_plot),
           position = position_fill(reverse = TRUE)) +
  coord_flip() +
  labs(fill = "Age group", y = "Proportion of number of deaths by age group", x = "Country") +
  scale_fill_manual(values = color_palette_big_bar)
dev.off()



##########################################################################################################################################
############################################################ IHME ########################################################################

### plot share of deaths for covid, diabetes and cardiovescular by age group for certain countries

### austria, usa, south korea, belgium -> get rid off unnecessary columns
### republic of korea => South Korea
### united states of america => united states

ihme_countries_subset = ihme_other_diseases_all %>%
  filter(location_name %in% c("Austria", "Belgium", "Republic of Korea", "United States of America") &
           metric_name == "Number") %>%
  select(-c(measure_id, location_id, sex_id, age_id, cause_id, metric_id, metric_name, year, upper, lower)) %>%
  rename(number = val, country = location_name, sex = sex_name) %>%
  mutate(country = ifelse(country == "Republic of Korea", "South Korea", country),
         country = ifelse(country == "United States of America", "United States", country),
         sex = ifelse(sex == "Male", "male", sex),
         sex = ifelse(sex == "Female", "female", sex),
         sex = ifelse(sex == "Both", "both", sex))


### % = share of a certain cause in all deaths in a given age group

### age groups in covid data:
### austria: 0-4, 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85-101
### belgium: 0-24, 25-44, 45-64, 65-74, 75-84, 85-101
### south korea: 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-101
### usa: 0, 1-4, 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85-101

### age groups in ihme data:
### <1 year, 1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54,
### 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+

### data needs to be aggregated -> age groups between ihme and covid match
ihme_data_plot = ihme_countries_subset %>%
  mutate(age_group_plot = NA) %>%
  mutate(age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("<1 year", "1 to 4"), "0-4"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("5 to 9", "10 to 14"), "5-14"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("15 to 19", "20 to 24"), "15-24"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("25 to 29", "30 to 34"), "25-34"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("35 to 39", "40 to 44"), "35-44"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("45 to 49", "50 to 54"), "45-54"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("55 to 59", "60 to 64"), "55-64"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("65 to 69", "70 to 74"), "65-74"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("75 to 79", "80 to 84"), "75-84"),
         age_group_plot = replace(age_group_plot, country == "Austria" & age_name %in% c("85 plus"), "85-101"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24"), "0-24"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("25 to 29", "30 to 34", "35 to 39", "40 to 44"), "25-44"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("45 to 49", "50 to 54", "55 to 59", "60 to 64"), "45-64"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("65 to 69", "70 to 74"), "65-74"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("75 to 79", "80 to 84"), "75-84"),
         age_group_plot = replace(age_group_plot, country == "Belgium" & age_name %in% c("85 plus"), "85-101"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("<1 year", "1 to 4", "5 to 9"), "0-9"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("10 to 14", "15 to 19"), "10-19"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("20 to 24", "25 to 29"), "20-29"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("30 to 34", "35 to 39"), "30-39"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("40 to 44", "45 to 49"), "40-49"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("50 to 54", "55 to 59"), "50-59"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("60 to 64", "65 to 69"), "60-69"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("70 to 74", "75 to 79"), "70-79"),
         age_group_plot = replace(age_group_plot, country == "South Korea" & age_name %in% c("80 to 84", "85 plus"), "80-101"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("<1 year"), "0"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("1 to 4"), "1-4"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("5 to 9", "10 to 14"), "5-14"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("15 to 19", "20 to 24"), "15-24"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("25 to 29", "30 to 34"), "25-34"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("35 to 39", "40 to 44"), "35-44"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("45 to 49", "50 to 54"), "45-54"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("55 to 59", "60 to 64"), "55-64"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("65 to 69", "70 to 74"), "65-74"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("75 to 79", "80 to 84"), "75-84"),
         age_group_plot = replace(age_group_plot, country == "United States" & age_name %in% c("85 plus"), "85-101")) %>%
  filter(!is.na(age_group_plot)) %>%
  select(-age_name)

### now aggregate by covid age groups
ihme_data_plot = ihme_data_plot %>%
  group_by(measure_name, country, sex, cause_name, age_group_plot) %>%
  summarize(number = sum(number, na.rm = TRUE))

### reshape to wide format (for Deaths and YLLs (years of life Lost))
ihme_data_plot = ihme_data_plot %>%
  mutate(measure_name = ifelse(measure_name == "Deaths", "deaths_total", measure_name),
         measure_name = ifelse(measure_name == "YLLs (Years of Life Lost)", "lost_years_total", measure_name)) %>%
  pivot_wider(names_from = measure_name,
              values_from = number)

### calculate shares of deaths and lost years for each country and disease
ihme_data_plot = ihme_data_plot %>%
  group_by(country, sex, cause_name) %>%
  mutate(deaths_share = deaths_total / sum(deaths_total),
         lost_years_share = lost_years_total / sum(lost_years_total)) %>%
  ungroup()

### only for both sexes, filter that
ihme_data_plot = ihme_data_plot %>%
  filter(sex == "both") %>%
  select(-sex)

### convert age group to factor
ihme_data_plot = ihme_data_plot %>%
  rename(age_group = age_group_plot) %>%
  mutate(age_group = factor(age_group, levels = mixedsort(unique(age_group))))

### rename variables
ihme_data_plot = ihme_data_plot %>%
  rename(daily_death_both = deaths_total,
         lost_years_total_both = lost_years_total,
         death_share = deaths_share)

### filter the four countries and add variable "cause_name" to covid data
covid_data_plot = deaths_lost_years_share_wide %>%
  filter(country %in% c("Austria", "Belgium", "South Korea", "United States")) %>%
  mutate(cause_name = "COVID-19")

### bind datasets
ihme_data_plot_all = bind_rows(ihme_data_plot, covid_data_plot)

for (current_country in unique(ihme_data_plot_all$country)) {
  ihme_data_plot_country = ihme_data_plot_all %>%
    filter(country == current_country)
  
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/deaths_share_cause_comparison/cause_deaths_shares_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  
  plot = ggplot(ihme_data_plot_country, aes(fill = cause_name, y = death_share, x = age_group)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values = color_palette_big_bar_alt) +
    labs(fill = "Cause of death", y = "Share of deaths", x = "Age group") +
    ggtitle(paste0(current_country, " - comparison of share of deaths by age group"))
  
  print(plot)
  
  dev.off()
}

for (current_country in unique(ihme_data_plot_all$country)) {
  ihme_data_plot_country = ihme_data_plot_all %>%
    filter(country == current_country)
  
  pdf(file = paste0("/Users/isabell/GitHub/covid_life_expectancy/plots/life_years_lost_cause_comparisons/cause_life_years_lost_shares_", current_country, ".pdf"),
      width = 10, 
      height = 5)
  
  plot = ggplot(ihme_data_plot_country, aes(fill = cause_name, y = lost_years_share, x = age_group)) + 
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values = color_palette_big_bar_alt) +
    labs(fill = "Cause of death", y = "Share of life years lost", x = "Age group") +
    ggtitle(paste0(current_country, " - comparison of life years lost by age group"))
  
  print(plot)
  
  dev.off()
}