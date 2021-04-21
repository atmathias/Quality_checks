
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

sm_data_gender <-  df_data %>%
  group_by(respondent_gender) %>% 
  summarise(
    gender_disaggregation = n(),
    percentage_distribution = (gender_disaggregation/nrow(.))*100
  )
  
# disaggregation  of data by participant_category

sm_data_participant_category <-  df_data %>%
  group_by(participant_category) %>% 
  summarise(
    participant_disaggregation = n(),
    percentage_distribution = (participant_disaggregation/nrow(.))*100 
  )

# disaggregation  of data by hh_head

sm_data_hh_head <-  df_data %>%
  group_by(hh_head) %>% 
  summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )


# disaggregation  of data by hh_head in the refugee settlements

sm_data_hh_head_refugee <-  df_data %>%
  filter(participant_category == "Refugee") %>% 
  group_by(hh_head) %>% 
    summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )

# disaggregation  of data by hh_head in the Host community

sm_data_hh_head_host <-  df_data %>%
  filter(participant_category == "Host community") %>% 
  group_by(hh_head) %>% 
  summarise(
    hh_head_disaggregation = n(),
    percentage_distribution = (hh_head_disaggregation/nrow(.))*100
  )


# disaggregation  of data by edu_level

sm_data_education_level <-  df_data %>%
  group_by(edu_level) %>% 
  summarise(
    educ_level_disaggregation = n(),
    percentage_distribution = (educ_level_disaggregation/nrow(.))*100
  )

# disaggregation  of data by method of agricultural land acquisition

sm_data_land_acquisition <-  df_data %>%
  filter(!is.na(land_aquisition)) %>% 
  group_by(land_aquisition) %>% 
  summarise(
    land_aquisition_disaggregation = n(),
    percentage_distribution = (land_aquisition_disaggregation/nrow(.))*100
  )


# disaggregation  of data by agricultural production level

sm_data_agric_production_level <-  df_data %>%
  filter(!is.na(ag_level)) %>% 
  group_by(ag_level) %>% 
  summarise(
    ag_level_disaggregation = n(),
    percentage_distribution = (ag_level_disaggregation/nrow(.))*100
  )


# disaggregation  of data by crop type 

sm_data_crop_type <-  df_data %>% 
  filter(!is.na(crop_type)) %>% 
  group_by(crop_type) %>% 
  summarise(
    crop_production_disaggregation = mean(crop_production_kgs, na.rm=T),
    percentage_distribution = (crop_production_disaggregation/nrow(.))*100
  )

   
# disaggregation  of data by land tilled (na.rm not working) 
sm_data_land_tilled <-  df_data %>%
  # filter(till_land = "Yes", "No", "Not applicable") (wrong procedure)
  # filter(till_land == "Yes" | till_land == "No" | till_land == "Not applicable") %>% 
  filter(!is.na(till_land)) %>% 
  group_by(till_land) %>% 
  summarise(
    land_tilled_disaggregation = n(),
    percentage_distribution = (land_tilled_disaggregation/nrow(.))*100
  )
 
    
# disaggregation  of data by employment type
 
 sm_data_employment_type <-  df_data %>%
   filter(!is.na(employ_status)) %>% 
   group_by(employ_status) %>% 
   summarise(
     employment_status_disaggregation = n(),
     percentage_distribution = (employment_status_disaggregation/nrow(.))*100
    )
 
# disaggregation  of data by level of job satisfaction (Need to remove n/a)
 
 sm_data_job_satisfaction <-  df_data %>%
   filter(!is.na(job_satisfaction)) %>% 
   group_by(job_satisfaction) %>% 
   summarise(
     job_satisfaction_disaggregation = n(),
     percentage_distribution = (job_satisfaction_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation  of data by grant/capital received
 
 sm_data_capital_received <-  df_data %>%
   group_by(capital) %>% 
   summarise(
     capital_received_disaggregation = n(),
     percentage_distribution = (capital_received_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation  of data by frequency of visiting innovation center (remove nas)
 
 sm_data_innovation_center_visists <-  df_data %>%
   filter(!is.na(ic_frequency)) %>% 
   group_by(ic_frequency) %>% 
   summarise(
     innovation_center_visits_disaggregation = n(),
     percentage_distribution = (innovation_center_visits_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation  of data by household decision making (round off percentages)
 
 sm_data_hh_decision_making <-  df_data %>%
   group_by(hh_decision) %>% 
   summarise(
     hh_decision_making_disaggregation = n(),
     percentage_distribution = (hh_decision_making_disaggregation/nrow(.))*100
   )
 
 # disaggregation of data by tilling method
 
 sm_data_tilling_method <-  df_data %>%
   filter(!is.na(till_method)) %>% 
   group_by(till_method) %>% 
   summarise(
     till_method_disaggregation = n(),
     percentage_distribution = (till_method_disaggregation/nrow(.))*100
   )
 
# disaggregation of data by respondents who used paper or E-vouchers to access seeds
 
 sm_data_voucher_access <-  df_data %>%
   filter(!is.na(seed_vouchers)) %>% 
   group_by(seed_vouchers) %>% 
   summarise(
     seed_voucher_disaggregation = n(),
     percentage_distribution = (seed_voucher_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation of data by respondents who had access to extension services
 
 sm_data_extension_service_access <-  df_data %>%
   filter(!is.na(access_extension)) %>% 
   group_by(access_extension) %>% 
   summarise(
     extension_service_access_disaggregation = n(),
     percentage_distribution = (extension_service_access_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation of data by respondents' employment status
 
 sm_data_employment_status <-  df_data %>%
   group_by(employed) %>% 
   summarise(
     employment_status_disaggregation = n(),
     percentage_distribution = (employment_status_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation of data by respondents' who own a phone
 
 sm_data_phone_ownership <-  df_data %>%
   group_by(phone_ownership) %>% 
   summarise(
     phone_ownership_disaggregation = n(),
     percentage_distribution = (phone_ownership_disaggregation/nrow(.))*100
   )
 
 
 # disaggregation of data by respondents' who own a phone and participant category  
 sm_data_participant_category_phone_ownership <-  df_data %>%
   filter(phone_ownership == "Yes") %>% 
   group_by(participant_category) %>% 
   summarise(
     participant_own_phone_disaggregation = n(),
     percentage_distribution = (participant_own_phone_disaggregation/nrow(.))*100
   )
 
 
 
 
 
 
 
 
 
 
 
 
 
 



