#Build the dataset

library(tidyverse)
library(lubridate)

dat_today <- read.csv(url("https://opendata.arcgis.com/datasets/628578697fb24d8ea4c32fa0c5ae1843_0.csv"),
                  header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE) %>%
  rename("Country" = `Country_Region`) %>%
  rename("State" = `Province_State`) %>%
  mutate(Date = as_date(Last_Update)) %>%
  filter(Country == "US") %>%
  select(State, Country, Date, Confirmed, Recovered, Deaths) %>%
  group_by(Country, Date, State) %>%
  summarize(Confirmed = sum(Confirmed, na.rm=TRUE),
         Recovered = sum(Recovered),
         Deaths = sum(Deaths, na.rm=TRUE),
         Active = Confirmed - (Recovered + Deaths)) %>%
  ungroup() %>%
  select(State, Date, Confirmed, Deaths)

abbrev <- read.csv("Support Files/abbrev.csv", header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE)

dat_daily <- read.csv(url("http://covidtracking.com/api/states/daily.csv"),
                      header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE) %>%
  rename("Code" = `state`) %>%
  left_join(., abbrev, by="Code") %>%
  mutate(Date = ymd(date)) %>%
  rename("Confirmed" = `positive`,
         "Deaths" = `death`) %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%#Convert NAs to 0
  select(State, Date, Confirmed, Deaths)

dat_all <- bind_rows(dat_daily, dat_today) %>% #remove the lesser of today's numbers
  group_by(State, Date) %>%
  summarize(Confirmed = max(Confirmed, na.rm=TRUE),
            Deaths = max(Deaths, na.rm=TRUE))

###Get population data
pop <- read_csv("Support Files/population.csv") %>% 
  rename("Pop" = POPESTIMATE2019) 

dat_all <- dat_all %>%
  left_join(., pop, by="State") %>%
  mutate(pct = round((Confirmed/Pop)*100, digits=4)) %>%
  mutate(pct = format(pct, scientific=F)) %>%
  na.omit()
  
###Write it
#write.csv(dat_all, "git_dat.csv", row.names=FALSE)

# #Get the raw data directly from Github
# dat_c <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"),
#                   header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE) 
# dat_c <- dat_c %>%
#   select(-Lat, -Long) %>%
#   gather(key = "Date", value = "Confirmed", -Province.State, -Country.Region) %>%
#   mutate(Date = substring(Date, 2)) %>%
#   mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
#   mutate(Date = mdy(Date))
# 
# dat_d <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"),
#                   header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE)
# dat_d <- dat_d %>%
#   select(-Lat, -Long) %>%
#   gather(key = "Date", value = "Deaths", -Province.State, -Country.Region) %>%
#   mutate(Date = substring(Date, 2)) %>%
#   mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
#   mutate(Date = mdy(Date))
# 
# 
# dat_r <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"),
#                   header = TRUE, sep = ",", na.strings = "-", stringsAsFactors = FALSE)
# dat_r <- dat_r %>%
#   select(-Lat, -Long) %>%
#   gather(key = "Date", value = "Recovered", -Province.State, -Country.Region) %>%
#   mutate(Date = substring(Date, 2)) %>%
#   mutate(Date = str_replace_all(Date, "[.]", "-")) %>%
#   mutate(Date = mdy(Date))
# 
# ####Join the three files (Confirmed, Deaths, Recovered)
# 
# dat_all <- left_join(dat_c, dat_d, by=c("Province.State", "Country.Region", "Date")) %>%
#   left_join(dat_r, by = c("Province.State", "Country.Region", "Date"))
# 
# ####Create Active Column
# dat_all <- dat_all %>%
#   mutate(Active = Confirmed - (Deaths + Recovered))
# 
# ####Create Difference Column
# dat_all <- dat_all %>%
#   arrange(Country.Region, Province.State, Date) %>%
#   group_by(Country.Region, Province.State) %>%
#   mutate(Difference = Confirmed - lag(Confirmed, n=1L, default = 0))
# 
# ####Tidy Up
# dat_all <- dat_all %>%
#   pivot_longer(cols = c("Confirmed", "Deaths", "Recovered","Active"), names_to="Case_Type", values_to="Cases")


  
  