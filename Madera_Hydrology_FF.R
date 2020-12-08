# Hydrology

# Load packages and data 

library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)

#Hydrology data for SJR (willow creek, Pinedale,) and peak flow for Friant Dam

sjr_willowcreek <- read_csv("SanJoaquinR_WillowCreek.csv") %>% 
  clean_names()

SJR_Pinedale<- read.csv("SJR_Pinedale1.csv") %>% 
  clean_names()


willowcreek <- read_csv("willowcreek_hydrology_ff.csv") %>% 
  clean_names()

peakflow_friantdam <- read_csv("peakflows_FriantDam.csv") %>% 
  clean_names()

#change to proper date format

sjr_willowcreek_date <- sjr_willowcreek %>% 
  mutate(
    date_new = lubridate::mdy(date)
  )

SJR_Pineale_date <- SJR_Pinedale %>% 
  mutate(
    date_new = lubridate::mdy(date)
  ) 

willowcreek_date <- willowcreek %>% 
  mutate(date_new = lubridate::mdy(date))

peakflow_friantdam_date <- peakflow_friantdam %>% 
  mutate(date_new = lubridate::mdy(date))

#If want to parse date further 
sandiegor_parse_date <- sandiegor_date %>% 
  mutate(obs_month = lubridate::month(date_new, label = TRUE),
         obs_year = lubridate::year(date_new))

### Observations by day
ggplot(sjr_willowcreek_date, aes(x = date_new, y = discharrge_m3_s))+
  geom_line()+ 
  theme_minimal()+
  labs(x = "Date", y = "Discharge (m3/s)", title = "San Joaquin River at Willow Creek Daily Discharge Rates (2010 - 2020)")

ggplot(SJR_Pineale_date, aes(x = date_new, y = discharge_m3_s))+
  geom_line()+
  theme_minimal()+
  labs(x = "Date", y = "Discharge (m3/s)", title = "San Joaquin River at Pinedale Daily Discharge Rates (2010 - 2020)")

ggplot(willowcreek_date, aes(x = date_new, y = discharge_m3_s))+
  geom_line()+
  theme_minimal()+
  labs(x = "Date", y = "Discharge (m3/s)", title = "Willowcreek Daily Discharge Rates (2010 - 2020)")


#Sum for flow per day sjr willow creek:

sjr_willowcreek_volume <- sjr_willowcreek_date %>%
  mutate(volume = (discharrge_m3_s * 86400)) %>% 
  mutate(af = (volume/1233.48))

# graph volume acre feet per day sjr willow creek:
# friant dam conversions for peak flows:
friantdam_af <- peakflow_friantdam_date %>% 
  mutate(volume_cf = (peak_va * 86400)) %>% 
  mutate(volume_af = (volume_cf/ 43559.9)) %>% 
  filter(date_new < ("2050-11-18"))

# graph annual peak flows 
ggplot(friantdam_af, aes(x = date_new, y = volume_af))+
  geom_col()+
  geom_hline(yintercept = 33362.27)+
  theme_classic()+
  labs(x = "Date", y = "Volume (Acre Feet)", title = "Highest Daily Peak Flow per year")

### Hensley Lake Hydrology and flood frequency:

# read in data for Fresno River above and below Hensley Dam (Hensley dam is stage data not stream flow so it is in feet):

fresno_abv <- read_csv("FHL_20.csv") %>% 
  clean_names() 

fresno_abv_date <- fresno_abv %>% 
  mutate(
    date_new = lubridate::ymd_hms(date_time)
  ) %>% 
  separate(date_new, into = c("date", "time"), sep= ' ', remove = FALSE)# helps to seperate date and time into 2 different columns


fresno_abv_daily <- fresno_abv_date %>%  
  group_by(date) %>% 
  summarise(count = n(),
            daily_avg_flow = mean(cfs, na.rm = TRUE), 
            daily_max_flow = max(cfs, na.rm = TRUE)
  )

### Below reservoir is RIVER STAGE
fresno_blw <- read_csv("HIQ_1.csv") %>% 
  clean_names()

fresno_blw_date <- fresno_blw %>% 
  mutate(
    date_new = lubridate::ymd_hms(date_time)
  ) %>% 
  separate(date_new, into = c("date", "time"), sep= ' ', remove = FALSE)# helps to seperate date and time into 2 different columns


fresno_blw_daily <- fresno_blw_date %>%  
  group_by(date) %>% 
  summarise(count = n(),
            daily_avg_stage = mean(feet, na.rm = TRUE), 
            daily_max_stage = max(feet, na.rm = TRUE)
  )

# plot daily summaries
ggplot(data = fresno_abv_daily) +
  geom_point(aes(x = date, y = daily_avg_flow)) +
  geom_point(aes(x = date, y = daily_max_flow),
             color = "red",
             fill = "red") 



ggplot(data = fresno_blw_daily) +
  geom_point(aes(x = date, y = daily_avg_stage))

### find maximum daily discharge for each year - use that to find recurrence intervals and probabilities
fresno_abv_annual <- fresno_abv_daily %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% # make column for year
  group_by(year) %>% 
  summarize(
    count = n(),
    annual_avg = mean(daily_avg_flow, na.rm = TRUE),
    annual_max = max(daily_max_flow, na.rm = TRUE)
  ) %>% 
  mutate(rank_order = rank(-annual_max)) %>% ## assign rank to max flow 
  arrange(rank_order) %>% 
  mutate(recurrence_int = (1+12)/rank_order) %>% ## RI = (1+[# of years])/[rank]
  mutate(probability = 1/recurrence_int) ## prob = 1/RI

fresno_blw_annual <- fresno_blw_daily %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% 
  group_by(year) %>% 
  summarize(
    count = n(),
    annual_avg = mean(daily_avg_stage, na.rm = TRUE),
    annual_max = max(daily_max_stage, na.rm = TRUE)
  ) %>% 
  mutate(rank_order = rank(-annual_max)) %>% 
  arrange(rank_order) %>% 
  mutate(recurrence_int = (1+11)/rank_order) %>% # only 11 years of data for below
  mutate(probability = 1/recurrence_int)

# plot probability vs discharge - if enough points, can fit a linear relationship to this and determine flood stages at any probability of occurring (like 100-year flood, etc.)
ggplot(data = fresno_abv_annual) +
  geom_point(aes(x = annual_max,
                 y = probability)) +
  geom_smooth(aes(x = annual_max,
                  y = probability),
              method = "lm")

ggplot(data = fresno_blw_annual) +
  geom_point(aes(x = annual_max,
                 y = probability)) +
  geom_smooth(aes(x = annual_max,
                  y = probability),
              method = "lm")

# Hydrology for BCQ Chowchilla River Below Buchanan Dam 2010- 2020. It is river staged and is measured in Feet
bcq <- read_csv("BCQ_1.csv") %>% 
  clean_names()

# BCQ Seperating Date and Time into 2 different columns.
bcq_date <- bcq %>% 
  mutate(
    date_new = lubridate::ymd_hms(date_time)
  ) %>% 
  separate(date_new, into = c("date", "time"), sep= ' ', remove = FALSE)# helps to seperate date and time into 2 different columns

# BCQ Daily average and daily max stage (FEET)
bcq_daily <- bcq_date %>%  
  group_by(date) %>% 
  summarise(count = n(),
            daily_avg_stage = mean(value, na.rm = TRUE), 
            daily_max_stage = max(value, na.rm = TRUE)
  )

# BCQ plot daily summaries feet
ggplot(data = bcq_daily) +
  geom_point(aes(x = date, y = daily_avg_stage)) +
  geom_point(aes(x = date, y = daily_max_stage),
             color = "red",
             fill = "red")

#?
ggplot(data = bcq_daily) +
  geom_point(aes(x = date, y = daily_avg_stage))

### find maximum daily discharge for each year - use that to find recurrence intervals and probabilities
bcq_annual <- bcq_daily %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% # make column for year
  group_by(year) %>% 
  summarize(
    count = n(),
    annual_avg = mean(daily_avg_stage, na.rm = TRUE),
    annual_max = max(daily_max_stage, na.rm = TRUE)
  ) %>% 
  mutate(rank_order = rank(-annual_max)) %>% ## assign rank to max flow 
  arrange(rank_order) %>% 
  mutate(recurrence_int = (1+11)/rank_order) %>% ## RI = (1+[# of years])/[rank]
  mutate(probability = 1/recurrence_int) ## prob = 1/RI


# plot probability vs discharge - if enough points, can fit a linear relationship to this and determine flood stages at any probability of occurring (like 100-year flood, etc.)
ggplot(data = bcq_annual) +
  geom_point(aes(x = annual_max,
                 y = probability)) +
  geom_smooth(aes(x = annual_max,
                  y = probability),
              method = "lm")

# Hydrology for BHP Chowchilla 2010 - 2020 . This is Pool Evelvation Measured in Feet. Pool? Elevation is at 613
bhp <- read_csv("BHP_chowchilla.csv") %>% 
  clean_names()

# BHP Seperating Date and Time into 2 different columns.
bhp_date <- bcq %>% 
  mutate(
    date_new = lubridate::ymd_hms(date_time)
  ) %>% 
  separate(date_new, into = c("date", "time"), sep= ' ', remove = FALSE)# helps to seperate date and time into 2 different columns

# BHP Daily average and daily max stage (FEET)
bhp_daily <- bhp_date %>%  
  group_by(date) %>% 
  summarise(count = n(),
            daily_avg_stage = mean(value, na.rm = TRUE), 
            daily_max_stage = max(value, na.rm = TRUE)
  )

# BHP plot daily summaries feet
ggplot(data = bhp_daily) +
  geom_point(aes(x = date, y = daily_avg_stage)) +
  geom_point(aes(x = date, y = daily_max_stage),
             color = "red",
             fill = "red")

#?
ggplot(data = bhp_daily) +
  geom_point(aes(x = date, y = daily_avg_stage))

### find maximum daily discharge for each year - use that to find recurrence intervals and probabilities
bhp_annual <- bhp_daily %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% # make column for year
  group_by(year) %>% 
  summarize(
    count = n(),
    annual_avg = mean(daily_avg_stage, na.rm = TRUE),
    annual_max = max(daily_max_stage, na.rm = TRUE)
  ) %>% 
  mutate(rank_order = rank(-annual_max)) %>% ## assign rank to max flow 
  arrange(rank_order) %>% 
  mutate(recurrence_int = (1+11)/rank_order) %>% ## RI = (1+[# of years])/[rank]
  mutate(probability = 1/recurrence_int) ## prob = 1/RI


# plot probability vs discharge - if enough points, can fit a linear relationship to this and determine flood stages at any probability of occurring (like 100-year flood, etc.)
ggplot(data = bhp_annual) +
  geom_point(aes(x = annual_max,
                 y = probability)) +
  geom_smooth(aes(x = annual_max,
                  y = probability),
              method = "lm")

# Hydrology for HIDDEN DAM (HID) (daily outflow in cfs) There is also information on river storage in Acre Feet.

hidden_dam_2002_2020 <- read_csv("hiddendam_2.csv") %>% 
  clean_names()
hidden_dam_2002_2020$outflow_cfs[hidden_dam_2002_2020$outflow_cfs == "--"] <- NA #changing -- to NA




# HID Seperating Date and Time into 2 different columns.
hiddendam_date <- hidden_dam_2002_2020 %>%
  mutate(
    date_new = lubridate::mdy(date_time_pst))


# helps to seperate date and time into 2 different columns

# HID Daily Outflow CFS
hiddendam_daily <- hiddendam_date %>%  
  group_by(date_new) %>% 
  summarise(count = n(),
            daily_avg_flow = mean(outflow_cfs, na.rm = TRUE), 
            daily_max_flow = max(outflow_cfs, na.rm = TRUE))

# HID plot daily summaries feet
ggplot(data = hiddendam_daily) +
  geom_point(aes(x = date_new, y = daily_max_flow))

#?
ggplot(data = hiddendam_daily) +
  geom_point(aes(x = date_new, y = daily_max_flow))

### find maximum daily discharge for each year - use that to find recurrence intervals and probabilities
hiddendam_annual <- hiddendam_daily %>% 
  mutate(year = format(date_new,"%Y")) %>% # make column for year
  group_by(year) %>% 
  summarize(
    count = n(),
    annual_max = max(daily_max_flow, na.rm = TRUE)) %>% 
  mutate(rank_order = rank(- annual_max)) %>% ## assign rank to max flow 
  arrange(rank_order) %>% 
  mutate(recurrence_int = (1+19)/rank_order) %>% ## RI = (1+[# of years])/[rank]
  mutate(probability = 1/recurrence_int) ## prob = 1/RI

# plot probability vs discharge - if enough points, can fit a linear relationship to this and determine flood stages at any probability of occurring (like 100-year flood, etc.)
ggplot(data = hiddendam_annual_rank) +
  geom_point(aes(x = annual_max,
                 y = probability)) +
  geom_smooth(aes(x = annual_max,
                  y = probability),
              method = "lm")

