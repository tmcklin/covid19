
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(grid)
library(ggrepel)

git_dat <- read_csv("git_dat.csv", 
                    col_types = list(col_character(), col_character(), col_date(), col_integer(), col_character(), col_integer()))

#remove any old plots
plots_folder <- "States_Plots"
plot_list <- list.files(plots_folder, include.dirs=F, full.names=T)
file.remove(plot_list)

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

dat_states <- bind_rows(dat_states, dat_daily) %>%
  select(-Country) %>%
  filter(Date >= "2020-01-15") %>%
  group_by(State, Date, Case_Type) %>%
  summarize(Cases = sum(Cases)) %>%
  pivot_wider(names_from=Case_Type, values_from=Cases)

###Get population data
pop <- read_csv("Support Files/population.csv") %>% 
  rename("Pop" = POPESTIMATE2019) 

dat_states <- dat_states %>%
  left_join(., pop, by="State") %>%
  mutate(pct = round((max(Confirmed)/Pop)*100, digits=4)) %>%
  mutate(pct = format(pct, scientific=F))

write.csv(dat_states, "state_population.csv", row.names = FALSE)
  
state_order <- dat_states %>%
  group_by(State) %>%
  summarize(pct = max(pct)) %>% 
  filter(pct != "NA") %>%
  mutate(pct = as.numeric(pct)) %>%
  arrange(-pct) 

states <- state_order$State
i=1
for (state in states){
#state <- "Washington"

  dat <- dat_states %>%
    filter(State == state) 

  plot_i <- ggplot(data = dat) +
    geom_col(aes(x = Date, y = Active), alpha=.5) +
    geom_hline(aes(yintercept=max(Confirmed)), linetype="dashed") +
    geom_point(aes(x = Date, y = Deaths), alpha=.5) +
    geom_line(aes(x = Date, y = Recovered), color="Green") +
    labs(caption = "Grey bars represent the number of active cases.
       Green line is the number of people who have recovered.
       Dots represent the number of deaths.
       \nSource: Johns Hopkins University Coronavirus Data Stream") +
    theme_bw() +
    ggtitle(paste0(i, ". ", state, " (", max(dat$pct),"% of the State Population)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
    theme(axis.text.x = element_text(angle = 90), text=element_text(family="serif")) +
    ylab("# Active Covid-19 Cases") +
    scale_x_date(date_breaks = "2 days") +
    annotate("text", x = min(dat$Date), y = max(dat$Confirmed), label = paste0("Confirmed Cases = ", scales::comma_format()(max(dat$Confirmed))), family="serif", vjust=1.5, hjust=.2)
    
  plot_i
  
  # ######## Save Plot
  #ggsave(filename = paste0(state, " ", min(dat$Date), " to ", max(dat$Date), ".png", sep = ""),
  state <- str_replace_all(state, " ", "_")
  ggsave(filename = paste0(i, "_", state, ".png"),
         type = "cairo",
         path = plots_folder,
         width = 8,
         height = 5,
         units = "in")
i=i+1
}

