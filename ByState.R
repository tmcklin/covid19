
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(grid)
library(ggrepel)

git_dat <- read_csv("git_dat.csv", 
                    col_types = list(col_character(), col_character(), col_date(), col_integer(), col_character(), col_integer()))

#remove any old plots
plots_folder <- "US_Plots"
plot_list <- list.files(plots_folder, include.dirs=F, full.names=T)
file.remove(plot_list)

#US################
git_dat <- git_dat %>%
  rename("Country" = `Country.Region`) %>%
  rename("State" = `Province.State`) %>%
  mutate(Date = as_date(Date)) 

dat_states <- git_dat %>%
  filter(Country == "US") %>%
  filter(str_detect(State, "[,]", negate = TRUE))

state_order <- dat_states %>%
  filter(Case_Type == "Confirmed") %>%
  group_by(State) %>%
  summarize(Cases = max(Cases)) %>%
  arrange(-Cases) 

states <- state_order$State
i=1
for (state in states){
#state <- "Washington"
  dat <- dat_states %>%
    filter(State == state) %>%
    arrange(State, Date) %>%
    select(State, Date, Case_Type, Cases) %>%
    filter(Date >= "2020-01-15") %>%
    group_by(State, Date, Case_Type) %>%
    summarize(Cases = sum(Cases)) %>%
    pivot_wider(names_from=Case_Type, values_from=Cases)
  
  
  plot_i <- ggplot(data = dat) +
    geom_col(aes(x = Date, y = Active), alpha=.5) +
    #geom_text_repel(aes(x=Date, y = Active, label=ifelse(Active>0, Active,"")), size=3, direction="y") +
    geom_point(aes(x = Date, y = Deaths), alpha=.5) +
    #geom_text(aes(x=Date, y=Deaths, label=ifelse(Deaths>0, Deaths, "")), size=2.5, vjust=-.7) +
    geom_line(aes(x = Date, y = Recovered), color="Green") +
    labs(caption = "Grey bars represent the number of active cases.
       Green line is the number of people who have recovered.
       Dots represent the number of deaths.
       \nSource: Johns Hopkins University Coronavirus Data Stream") +
    theme_bw() +
    ggtitle(paste0(i, ". ", state, " (", scales::comma_format()(max(dat$Confirmed)), " Confirmed Cases)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
    theme(axis.text.x = element_text(angle = 90)) +
    ylab("# Active Covid-19 Cases") +
    scale_x_date(date_breaks = "2 days") 
  
  plot_i
  
  # ######## Save Plot
  #ggsave(filename = paste0(state, " ", min(dat$Date), " to ", max(dat$Date), ".png", sep = ""),
  state <- str_replace_all(state, " ", "_")
  ggsave(filename = paste0(i, "_", state, ".png"),
         type = "cairo",
         path = "US_Plots",
         width = 8,
         height = 5,
         units = "in")
i=i+1
}

