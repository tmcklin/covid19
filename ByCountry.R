library(tidyverse)
library(lubridate)
library(grid)

gs_dat <- read_csv("git_dat.csv", 
                   col_types = list(col_character(), col_character(), col_date(), col_integer(), col_character(), col_integer()))

#remove any old plots
plots_folder <- "Countries_Plots"
plot_list <- list.files(plots_folder, include.dirs=F, full.names=T)
file.remove(plot_list)

#US################
gs_dat <- gs_dat %>%
  rename("Country" = `Country.Region`) %>%
  rename("State" = `Province.State`) %>%
  mutate(Date = as_date(Date)) 

top_30 <- gs_dat %>%
  filter(Case_Type == "Confirmed") %>%
  filter(Date == max(Date)) %>%
  group_by(Country) %>%
  summarize(Cases = sum(Cases)) %>%
  arrange(-Cases) %>%
  head(n = 30L)

countries <- top_30$Country 
i=1
for (country in countries){
#country <- "US"
dat <- gs_dat %>%
  filter(Country == country) %>%
  arrange(State, Date) %>%
  select(Country, Date, Case_Type, Cases) %>%
  filter(Date >= "2020-01-15") %>%
  group_by(Country, Date, Case_Type) %>%
  summarize(Cases = sum(Cases)) %>%
  pivot_wider(names_from=Case_Type, values_from=Cases)
  

plot_i <- ggplot(data = dat) +
  geom_col(aes(x = Date, y = Active), alpha=.5) +
  geom_point(aes(x = Date, y = Deaths), alpha=.5) +
  geom_line(aes(x = Date, y = Recovered), color="Green") +
  labs(caption = "Grey bars represent the number of active cases.
       Green line is the number of people who have recovered.
       Dots represent the number of deaths.
       \nSource: Johns Hopkins University Coronavirus Data Stream") +
  theme_bw() +
  ggtitle(paste0(i, ". ", country, " (", scales::comma_format()(max(dat$Confirmed)), " Confirmed Cases)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("# Active Covid-19 Cases") +
  scale_x_date(date_breaks = "2 days") 

plot_i

# ######## Save Plot
country <- str_replace_all(country, " ", "_")
ggsave(filename = paste0(i, "_", country, ".png"),
       type = "cairo",
       path = plots_folder,
       width = 8,
       height = 5,
       units = "in")
i=i+1
}

