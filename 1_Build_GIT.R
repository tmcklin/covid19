#Build the dataset

library(tidyverse)
library(lubridate)

#Get the raw data directly from Github
dat_c <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"),
                  header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE) 
dat_c <- dat_c %>%
  select(-Lat, -Long) %>%
  gather(key = "Date", value = "Confirmed", -Province.State, -Country.Region) %>%
  mutate(Date = substring(Date, 2)) %>%
  mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
  mutate(Date = mdy(Date))

dat_d <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"),
                  header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE)
dat_d <- dat_d %>%
  select(-Lat, -Long) %>%
  gather(key = "Date", value = "Deaths", -Province.State, -Country.Region) %>%
  mutate(Date = substring(Date, 2)) %>%
  mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
  mutate(Date = mdy(Date))


dat_r <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"),
                  header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE)
dat_r <- dat_r %>%
  select(-Lat, -Long) %>%
  gather(key = "Date", value = "Recovered", -Province.State, -Country.Region) %>%
  mutate(Date = substring(Date, 2)) %>%
  mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
  mutate(Date = mdy(Date))

####Join the three files (Confirmed, Deaths, Recovered)

dat_all <- left_join(dat_c, dat_d, by=c("Province.State", "Country.Region", "Date")) %>%
  left_join(dat_r, by = c("Province.State", "Country.Region", "Date"))

####Create Active Column
dat_all <- dat_all %>%
  mutate(Active = Confirmed - (Deaths + Recovered))

####Create Difference Column
dat_all <- dat_all %>%
  arrange(Country.Region, Province.State, Date) %>%
  group_by(Country.Region, Province.State) %>%
  mutate(Difference = Confirmed - lag(Confirmed, n=1L, default = 0))

####Tidy Up
dat_all <- dat_all %>%
  pivot_longer(cols = c("Confirmed", "Deaths", "Recovered","Active"), names_to="Case_Type", values_to="Cases")

###Write it
write.csv(dat_all, "git_dat.csv", row.names=FALSE)
  
  