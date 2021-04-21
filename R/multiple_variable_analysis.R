# Load tidyverse
library(tidyverse)

# Clear environment

rm(list = ls())

# Load data

df_data <- read_csv(file = "inputs_1/BRIDGE Endline Survey.csv")
df_data <- select(df_data, -contains("#"))
# df_data_K <- select(df_data, -starts_with("#"))

dim(df_data)
class(df_data)
colnames(df_data)

# summary analysis
# disaggregation  of data by gender

summary_data <-  df_data %>%
  group_by(respondent_gender) %>% 
  summarise(
    gender_disaggregation = n(),
    percentage_distribution = (gender_disaggregation/nrow(.))*100
  )

# disaggregation  of data by participant_category

summary_data <-  df_data %>%
  group_by(participant_category) %>% 
  summarise(
    participant_disaggregation = n(),
    percentage_distribution = (participant_disaggregation/nrow(.))*100 
  )

# disaggregation  of data by hh_head

summary_data <-  df_data %>%
  group_by(hh_head) %>% 
  summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )


# disaggregation  of data by hh_head in the refugee settlements

summary_data <-  df_data %>%
  filter(participant_category == "Refugee") %>% 
  group_by(hh_head) %>% 
  summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )

# disaggregation  of data by hh_head in the Host community

summary_data <-  df_data %>%
  filter(participant_category == "Host community") %>% 
  group_by(hh_head) %>% 
  summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )



# disaggregation  of data by edu_level

summary_data <-  df_data %>%
  group_by(edu_level) %>% 
  summarise(
    educ_level_disaggregation = n(),
    percentage_distribution = (educ_level_disaggregation/nrow(.))*100
  )


# disaggregation  of data by agricultural_enterprises (multiple)

summary_data <-  df_data %>%
  group_by(ag_enterprises) %>% 
  summarise(
    ag_disaggregation = n(),
    percentage_distribution = (ag_disaggregation/nrow(.))*100
  )



# disaggregation  of data by agricultural production _level

summary_data <-  df_data %>%
  group_by(ag_level) %>% 
  summarise(
    ag_level_disaggregation = n(),
    percentage_distribution = (ag_level_disaggregation/nrow(.))*100
  )


# disaggregation  of data by crop_production_kgs (filtering by dates)

summary_data <-  df_data %>% 
  group_by(crop_type) %>% 
  summarise(
    crop_production_disaggregation = mean(crop_production_kgs, na.rm=T),
    percentage_distribution = (crop_production_disaggregation/nrow(.))*100
  )


# disaggregation  of data by major income sources (multiple)

summary_data <-  df_data %>%
  group_by(in_source) %>% 
  summarise(
    
    
    
    
    # disaggregation  of data by land_size_cultivated and by settlement (needs standardising respones)
    
    
    # disaggregation  of data by cost of tillage per acre (needs grouping data)
    variable_name  (till_price)
    
    
    # disaggregation  of data by source of information (multiple) 
    variable_name (info_farming_practices)
    
    
    # disaggregation  of data by source of money to pay cost share (multiple)
    
    variable_name (cost_share)
    
    
    # disaggregation  of data by types of extension services (multiple)
    variable_name (access_extension_types)
    
    
    # disaggregation  of data by employment status
    
    summary_data <-  df_data %>%
      group_by(employ_status) %>% 
      summarise(
        employment_status_disaggregation = n(),
        percentage_distribution = (employment_status_disaggregation/nrow(.))*100
      )
    
    # disaggregation  of data by level of job satisfaction (Need to remove n/a)
    
    summary_data <-  df_data %>%
      group_by(job_satisfaction) %>% 
      summarise(
        job_satisfaction_disaggregation = n(),
        percentage_distribution = (job_satisfaction_disaggregation/nrow(.))*100
      )
    
    
    # disaggregation  of data by skills acquired (multiple + need to remove n/as)
    
    variable_name (skills)
    
    
    # disaggregation  of data by grant/capital received (Not applicable is fine)
    
    summary_data <-  df_data %>%
      group_by(capital) %>% 
      summarise(
        capital_received_disaggregation = n(),
        percentage_distribution = (capital_received_disaggregation/nrow(.))*100
      )
    
    
    # disaggregation  of data by frequency of visiting innovation center (remove nas)
    
    summary_data <-  df_data %>%
      group_by(ic_frequency) %>% 
      summarise(
        innovation_center_visits_disaggregation = n(),
        percentage_distribution = (innovation_center_visits_disaggregation/nrow(.))*100
      )
    
    
    # disaggregation  of data by household decision making (round off percentages)
    
    summary_data <-  df_data %>%
      group_by(hh_decision) %>% 
      summarise(
        hh_decision_making_disaggregation = n(),
        percentage_distribution = (hh_decision_making_disaggregation/nrow(.))*100
      )
    
    
    
    access_limits
    
    
    
    
    
    
    
    
    
    
    
    