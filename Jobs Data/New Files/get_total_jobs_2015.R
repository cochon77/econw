library(tidyverse)

path_to_data <- "~/Documents/2018-19/ECONW/Jobs Data/New Files/Row Data"
setwd(path_to_data)

get_city_job_totals <- function(jobs_data_path, state_crosswalk_path) {
  
  # load both files
  jobs <- read_csv(jobs_data_path)
  crosswalk <- read_csv(state_crosswalk_path)
  
  # subset and rename datasets
  tidy_jobs <- jobs %>% 
    select(w_geocode, C000) %>% 
    rename(total_jobs = C000) %>% 
    mutate(w_geocode = as.character(w_geocode))
  
  tidy_crosswalk <- crosswalk %>% 
    select(tabblk2010, stplcname) %>% 
    rename(w_geocode = tabblk2010,
           city_name = stplcname) %>% 
    mutate(w_geocode = as.character(w_geocode))
  
  # join crosswalk to jobs data
  joined <- left_join(tidy_jobs, tidy_crosswalk)
  
  # aggregate job totals to city level
  city_job_totals <- joined %>% 
    group_by(city_name) %>% 
    summarise(total_jobs = sum(total_jobs)) %>% 
    ungroup()
  
  return(city_job_totals)
  
}

get_different_state_results <- function(jobs_data_path, state_crosswalk_path) {
  
  # map get city job totals across jobs data and crosswalk
  tidy_output <- map2_df(jobs_data_path, state_crosswalk_path, get_city_job_totals)
  
  # extract state from data
  out_df <- tidy_output %>% 
    mutate(state = str_trim(str_extract(city_name, "(?<=,).+$")))
  
  return(out_df)
  
}

# need to define paths of jobs data and crosswalk data here
list_of_job_data_paths <- list.files(paste(path_to_data))[seq(1,7,by = 2)]
list_of_crosswalk_paths <- list.files(paste(path_to_data))[seq(2,8,by = 2)]

# produce the output
output_2015 <- get_different_state_results(list_of_job_data_paths, list_of_crosswalk_paths)
write.csv(output_2015, 
          file = "~/Dropbox (ECONW)/23186 Up For Growth On Call Contract/Analysis/City Job Housing Ratios/New files/2015_Data.csv", 
          row.names=FALSE)