library(readr)
library(usmap)
library(gganimate) # important
library(transformr) # important
library(lubridate)
library(dplyr)
library(hrbrthemes)
library(patchwork)

# we want to look at confirmed cases and deaths early on in the pandemic

# read the data in
deaths_20 <- read_csv("us-counties-2020.csv")

# confirmed deaths
deaths_20 %>%
  filter(between(date, ymd(min(date)), ymd(max(date))),
         !is.na(deaths)) %>% 
  group_by(date) %>% 
  reframe(deaths = sum(deaths),
          date = unique(date)) %>%
  ggplot() +
  geom_line(aes(date, deaths), color = "red", lwd = 2) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_ipsum() +
  labs(title = "Cumulative Confirmed COVID Deaths by Day, 2020") +
  transition_reveal(date)
anim_save("cum_deaths.gif")

# confirmed cases
deaths_20 %>%
  filter(between(date, ymd(min(date)), ymd(max(date))),
         !is.na(deaths)) %>% 
  group_by(date) %>% 
  reframe(deaths = sum(deaths),
          date = unique(date)) %>%
  ggplot() +
  geom_line(aes(date, deaths), color = "blue", lwd = 2) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_ipsum() +
  labs(title = "Cumulative Confirmed COVID Cases by Day, 2020") +
  transition_reveal(date)
# anim_save("cum_cases.gif")


# lets look at New York and Florida
deaths_20 %>%
  filter(!is.na(deaths)) %>% 
  filter(state %in% c("New York", "Florida")) %>% 
  group_by(state, date) %>% 
  reframe(deaths = sum(deaths),
          date = unique(date)) %>% 
  ggplot(aes(date, deaths, color = state)) +
  geom_line() +
  theme_ipsum() +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("blue", "chartreuse4")) +
  labs(title = "COVID Deaths in NY and FL, 2020") +
  transition_reveal(date)
# anim_save("fly_ny.gif")


