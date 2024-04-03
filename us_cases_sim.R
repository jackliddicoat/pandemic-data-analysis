library(readr)
library(usmap)
library(gganimate) # important
library(transformr) # important
library(lubridate)
library(dplyr)

# we want to look at confirmed cases and deaths early on in the pandemic

# read the data in
cases_20 <- read_csv("us-counties-2020.csv")
head(cases_20)

# filter the dates between the first day and March 15th
cases_20 <- cases_20 %>%
  filter(between(date, min(date), ymd(20200315)))

cases_20 <- cases_20 %>% 
  group_by(county, state) %>% 
  mutate(cum_cases = cumsum(cases))

#  here is how we make the animation for deaths
cases_anim <- plot_usmap(data = cases_20, regions = "counties", values = "cases", 
           linewidth = .2) +
  scale_fill_viridis_b() +
  labs(title = "Confirmed COVID Cases by Day, 2020",
       subtitle = "{closest_state}") +
  geom_polygon(data = usmapdata::us_map(regions = "states"),
               aes(x, y, group = group), fill = NA, size = .5, color = "black") +
  transition_states(date, 
                    transition_length = 4, # transitions are double in length
                    state_length = 1)
cases_anim <- animate(cases_anim, fps = 10, duration = 30)
anim_save("case_change.gif")


