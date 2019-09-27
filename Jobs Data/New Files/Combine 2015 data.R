library(tidyverse)
library(readxl)

path_to_data <- "~/Documents/2018-19/ECONW/Jobs Data"
setwd(path_to_data)

## load final data, add state variable
final_data <- read_excel("Old Files/Final_Dataset_For R.xlsx") %>% 
  mutate(state = str_trim(str_extract(geography, "(?<=,).+$")))

## load 2015 data, change the variable name in accordance with the final data
data_2015 <- read_excel("New Files/2015_Data.xlsx") %>% 
  mutate(city_name = as.character(city_name)) %>% 
  rename(JOBS2015 = total_jobs) %>% na.omit %>%
  select(-state)

## join the two data frames, filter the areas of interest (cities from 4 states)
joined <- left_join(final_data, data_2015, by = c("stplcname" = "city_name")) %>% 
  filter(state %in% c("California","Colorado","Oregon","Washington"))

## get the job growth from 2002-15, 2004-15, and 2010-15
joined <- joined %>% mutate(job_growth_02to15 = JOBS2015-JOBS2002) %>%
  mutate(job_growth_04to15 = JOBS2015-JOBS2004) %>% 
  mutate(job_growth_10to15 = JOBS2015-JOBS2010)

## get the ratio of job growth to permit growth from 2010-15, 2002-15, 2004-15
joined <- joined %>% mutate(job_permit_10to15 = job_growth_10to15/permits_10to15) %>%
  mutate(job_permit_02to15 = job_growth_02to15/permits_02to15) %>%
  mutate(job_permit_04to15 = job_growth_04to15/permits_04to15)

## get the ratio of job growth to housing unit growth from 2010-15
joined <- joined %>% mutate(job_unit_10to15 = job_growth_10to15/unit_growth_10to15)

## get the ratio of job growth to population growth from 2010-15
joined <- joined %>% mutate(job_pop_10to15 = job_growth_10to15/pop_growth_10to15)

## get housing divide from 2010-14 and 2010-15
joined <- joined %>% 
  mutate(housing_divide_10to15 = job_growth_10to15-unit_growth_10to15) %>%
  mutate(housing_divide_10to14 = job_growth_10to14-unit_growth_10to14)

## get the column names from the old file and change 2014 to 2015
old_file <- read_excel("Old Files/Housing Divide Analysis - Four States.xlsx", skip = 1)
column_rename <- colnames(old_file)
column_rename <- gsub("2014","2015", column_rename)

## select the columns of interest
selected_columns <- 
  c("id2", "stplcname", "county_name", "state", "total_pop_10", "total_pop_15", "pop_growth_10to15", "unit_growth_10to15", "job_growth_10to15", "housing_divide_10to15","permits_10to15", "job_unit_10to15", "pop_unit_00to15", "job_pop_10to15", "job_permit_10to15", "permit_unit_10to15", "pop_permit_10to15")

## select the columns, get rid of state codes from county and city name, and rename the columns
city_final_data <- joined %>% 
  select(selected_columns) %>%
  mutate(county_name = gsub(",.*$", "", county_name)) %>%
  mutate(stplcname = gsub(",.*$", "", stplcname)) %>%
  rename_at(vars(selected_columns), ~ column_rename) %>% na.omit
  
write.csv(city_final_data, file = "New Files/2010-2015.csv", row.names=FALSE)
write.csv(joined , file = "New Files/2010-2015(All Columns).csv", row.names=FALSE)

comparision_columns <- 
  c("id2", "stplcname", "county_name", "state", "total_pop_10", "total_pop_14","total_pop_15", "pop_growth_10to14", "pop_growth_10to15", "unit_growth_10to14", "unit_growth_10to15", "job_growth_10to14", "job_growth_10to15", "housing_divide_10to14","housing_divide_10to15","permits_10to14","permits_10to15","job_unit_10to14","job_unit_10to15","pop_unit_00to14","pop_unit_00to15", "job_pop_10to14", "job_pop_10to15", "job_permit_10to14", "job_permit_10to15", "permit_unit_10to14", "permit_unit_10to15", "pop_permit_10to14","pop_permit_10to15")

city_comparision_data_14to15 <- 
  joined %>% 
  select(comparision_columns) %>% 
  mutate(county_name = gsub(",.*$", "", county_name)) %>%
  mutate(stplcname = gsub(",.*$", "", stplcname)) %>% na.omit

write.csv(city_comparision_data_14to15, file = "New Files/2010-2015 with Comparisons (2014 and 2015).csv", row.names=FALSE)
