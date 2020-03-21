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
pop <- read_csv("population.csv") %>% rename("Pop" = POPESTIMATE2019)

state <- c("New York", "Washington", "California", "North Carolina")

dat <- dat_states %>%
  arrange(State, Date) %>%
  select(State, Date, Case_Type, Cases) %>%
  filter(Date >= "2020-01-15") %>%
  pivot_wider(names_from=Case_Type, values_from=Cases) %>% 
  filter(Confirmed != 0) %>% 
  group_by (State) %>%
  mutate(days = seq_along(Date)) %>%
  filter(State %in% state) %>%
  left_join(., pop, by = "State") %>%
  mutate(pct = round((Confirmed/Pop)*100, digits=4)) %>%
  mutate(pct = as.numeric(format(pct, scientific=F)))


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


