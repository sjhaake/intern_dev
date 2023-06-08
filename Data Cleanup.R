################## START ######################

#### DOWNLOAD PACKAGES ####

{
  install.packages("tidyverse")
  install.packages("lubridate")
  install.packages("janitor")
  install.packages("readxl")
}


#### LOAD PACKAGES ####

{
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(readxl)
}


#### Download Data ####

perch_data <- read_xlsx("perch_data_deidentified.xlsx")



### How to look at your data ###

perch_data
view(perch_data)
summary(perch_data)
str(perch_data)
head(perch_data,10)
print(perch_data,n=50)




##### Manage Dataset #####

# fix the awful columnnames

janitor::clean_names(perch_data)
#OR
perch_data %>% clean_names()

perch_data <- perch_data %>% clean_names()

# rename columns #

perch_data %>%
  rename("position" = "positions") -> perch_data

# fix date column type #

perch_data %>%
  mutate(date = as.Date(date)) -> perch_data

perch_data$date <- as.Date(perch_data$date)


# change order of columns #

perch_data %>%
  select(ids,position,date,exercise,everything()) -> perch_data


# check for issues #

perch_data %>% filter(position == "-")

perch_data %>%
  mutate(
    position == case_when(
      position == "-" ~ "SP",
      TRUE ~ position
    )
  ) -> perch_data



#### CLEAN DATA ####

#check out summary statistics#
summary(perch_data)

#another way of looking at things#
perch_data %>%
  ggplot(aes(mean_velocity_m_s))+
  geom_density()

perch_data %>%
  ggplot(aes(peak_power_w))+
  geom_density()



#whats acceptable#
IQR(perch_data$mean_velocity_m_s)
summary(perch_data$peak_power_w)
sd(perch_data$peak_power_w)

quantile(perch_data$mean_velocity_m_s)

1.214-.607


#how do we decide what to clean?#
perch_data %>%
  group_by(exercise) %>%
  mutate(iqr_mv = IQR(mean_velocity_m_s),
         upper_bound_mv = quantile(mean_velocity_m_s,.9),
         lower_bound_mv = quantile(mean_velocity_m_s,.1)) %>%
  filter(mean_velocity_m_s >= lower_bound_mv-(1.5*iqr_mv),
         mean_velocity_m_s <= upper_bound_mv+(1.5*iqr_mv))

perch_data %>%
  group_by(exercise) %>%
  mutate(iqr_pp = IQR(peak_power_w),
         upper_bound_pp = quantile(peak_power_w,.9),
         lower_bound_pp = quantile(peak_power_w,.1)) %>%
  filter(peak_power_w >= lower_bound_pp-(1.5*iqr_pp),
         peak_power_w <= upper_bound_pp+(1.5*iqr_pp)) %>%
  ggplot(aes(peak_power_w))+
  geom_density()



##perch_data %>%
  group_by(exercise) %>%
  mutate(sd_pp = sd(peak_power_w),
         x2_sd_pp = 2*sd_pp) %>%
  filter(
    peak_power_w >= peak_power_w-x2_sd_pp,
    peak_power_w <= peak_power_w+x2_sd_pp
  ) %>%
  ggplot(aes(peak_power_w))+
  geom_density()
