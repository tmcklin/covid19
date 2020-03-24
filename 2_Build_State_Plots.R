
library(tidyverse)
library(lubridate)
library(grid)

# Get the State-level data
source("1_Gather_Data.R")

#remove any old plots
plots_folder <- "States_Plots"
plot_list <- list.files(plots_folder, include.dirs=F, full.names=T)
file.remove(plot_list)

state_order <- dat_all %>%
  group_by(State) %>%
  summarize(pct = max(pct)) %>% 
  filter(pct != "NA") %>%
  mutate(pct = as.numeric(pct)) %>%
  arrange(-pct) 

states <- state_order$State
i=1
for (state in states){
#state <- "Washington"

  dat <- dat_all %>%
    filter(State == state) 

  plot_i <- ggplot(data = dat) +
    geom_col(aes(x = Date, y = Confirmed), alpha=.5) +
    #geom_hline(aes(yintercept=max(Confirmed)), linetype="dashed") +
    geom_point(aes(x = Date, y = Deaths), alpha=.5) +
    #geom_line(aes(x = Date, y = Recovered), color="Green") +
    labs(caption = "Grey bars represent the number of confirmed cases.
       Dots represent the number of deaths.
       \nSource: Johns Hopkins University Coronavirus Data Stream") +
    theme_bw() +
    ggtitle(paste0(i, ". ", state, " (", max(dat$pct),"% of the State Population)"), paste0(min(dat$Date), " to ", max(dat$Date))) +
    theme(axis.text.x = element_text(angle = 90), text=element_text(family="serif")) +
    ylab("# Confirmed Covid-19 Cases") +
    scale_x_date(date_breaks = "1 day") +
    annotate("text", x = min(dat$Date), y = max(dat$Confirmed), label = paste0("Confirmed Cases = ", scales::comma_format()(max(dat$Confirmed))), family="serif", vjust=1.5, hjust=.2)
    
  plot_i
  
  # ######## Save Plot
  state <- str_replace_all(state, " ", "_")
  ggsave(filename = paste0(i, "_", state, ".png"),
         type = "cairo",
         path = plots_folder,
         width = 8,
         height = 5,
         units = "in")
i=i+1
}

