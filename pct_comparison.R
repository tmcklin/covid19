library(tidyverse)
library(lubridate)
library(googlesheets4)
library(grid)
library(ggrepel)

git_dat <- read_csv("git_dat.csv", 
                    col_types = list(col_character(), col_character(), col_date(), col_integer(), col_character(), col_integer()))


#US################
git_dat <- git_dat %>%
  rename("Country" = `Country.Region`) %>%
  rename("State" = `Province.State`) %>%
  mutate(Date = as_date(Date)) 

dat_states <- git_dat %>%
  filter(Country == "US") %>%
  filter(str_detect(State, "[,]", negate = TRUE)) %>%
  select(-Difference)

###Get today's data to provide most recent stats####
arc_dat <- read_csv(url("https://opendata.arcgis.com/datasets/bbb2e4f589ba40d692fab712ae37b9ac_1.csv"))

dat_daily <- arc_dat %>%
  rename("Country" = `Country_Region`) %>%
  rename("State" = `Province_State`) %>%
  mutate(Date = as_date(Last_Update)) %>%
  filter(Country == "US") %>%
  select(State, Country, Date, Confirmed, Recovered, Deaths) %>%
  group_by(State) %>%
  mutate(Confirmed = sum(Confirmed),
         Recovered = sum(Recovered),
         Deaths = sum(Deaths),
         Active = Confirmed - (Recovered + Deaths)) %>%
  filter(Date == max(dat_states$Date)+1) %>%
  pivot_longer(cols=c("Confirmed", "Recovered", "Deaths", "Active"),
               names_to="Case_Type", values_to="Cases")

dat_states <- bind_rows(dat_states, dat_daily)

###Get population data
pop_us <- read_csv("Support Files/population.csv") %>% rename("Pop_US" = POPESTIMATE2019)
pop_world <- read_csv("Support Files/world_population.csv") %>% 
  rename(Country = name,
         Pop = pop2019) %>%
  mutate(Pop = Pop *1000) %>%
  mutate_at(vars(Country), funs(recode(.,
                                    "United States"       = 'US'))) %>%
  select(Country, Pop)

state <- c("New York", "Washington", "California", "North Carolina", "Georgia")

dat <- dat_states %>%
  arrange(State, Date) %>%
  select(State, Date, Case_Type, Cases) %>%
  filter(Date >= "2020-01-15") %>%
  pivot_wider(names_from=Case_Type, values_from=Cases) %>% 
  filter(Confirmed != 0) %>% 
  group_by (State) %>%
  mutate(days = seq_along(Date)) %>%
  filter(State %in% state) %>%
  left_join(., pop_us, by = "State") %>%
  mutate(pct = round((Confirmed/Pop)*100, digits=4)) %>%
  mutate(pct = as.numeric(format(pct, scientific=F)))

####US Comparison
ggplot(dat, aes(x = days, y = pct, group = State, color = State)) +
  geom_line() +
  labs(caption = "Source: Johns Hopkins University Coronavirus Data Stream
       [add US Census as population source and arcgis as daily source]") +
  theme_bw() +
  #ggtitle(paste0(i, ". ", state, " (", pop_pct$pct,"% of the State Population)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
  ylab("Confirmed Cases by Population Percentage") +
  geom_text(data = dat %>% filter(days == last(days)), aes(label = State, 
                                                                 x = days + .5,
                                                                 y = pct,
                                                                 color = State)) +
  scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks())
################


####World Comparison
ggplot(dat, aes(x = days, y = pct, group = State, color = State)) +
  geom_line() +
  labs(caption = "Source: Johns Hopkins University Coronavirus Data Stream
       [add US Census as population source and arcgis as daily source]") +
  theme_bw() +
  #ggtitle(paste0(i, ". ", state, " (", pop_pct$pct,"% of the State Population)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
  ylab("Confirmed Cases by Population Percentage") +
  geom_text(data = dat %>% filter(days == last(days)), aes(label = State, 
                                                           x = days + .5,
                                                           y = pct,
                                                           color = State)) +
  scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks())
################

# World population statistics from: https://worldpopulationreview.com/
countries <- c("US", "Italy", "Germany", "Spain", "Iran", "China")
dat_world <- git_dat %>%
  select(-Difference) %>%
  pivot_wider(names_from=Case_Type, values_from=Cases) %>% 
  filter(Confirmed != 0) %>%
  group_by(Country, Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active)) %>%
  left_join(., pop_world, by = "Country") %>%
  mutate(pct = round((Confirmed/Pop)*100, digits=4)) %>%
  mutate(pct = as.numeric(format(pct, scientific=F))) %>%
  filter(Country %in% countries) 

write.csv(dat_world, "world_population.csv", row.names = FALSE)

start_date <- dat_world %>%
  group_by(Country) %>%
  summarize(start_Confirmed = min(Confirmed)) 
start_date <- max(start_date$start_Confirmed)

dat_world <- dat_world %>%
  filter(Confirmed >= start_date) %>%
  group_by(Location) %>%
  mutate(days = seq_along(Date)) 
  
ggplot(dat_world, aes(x = days, y = pct, group = Country, color = Country)) +
  geom_line(show.legend = FALSE) +
  labs(caption = "Source: Johns Hopkins University Coronavirus Data Stream
       World population statistics from: https://worldpopulationreview.com") +
  theme_bw() +
  #ggtitle(paste0(i, ". ", state, " (", pop_pct$pct,"% of the State Population)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
  ylab("Confirmed Cases by Population Percentage") +
  xlab(paste0("Days Since First Confirmed Case # ", start_date)) +
  geom_text(data = dat_world %>% filter(days == last(days)), aes(label = Country, 
                                                           x = days + 1.8,
                                                           y = pct,
                                                           color = Country),
            show.legend = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks())




