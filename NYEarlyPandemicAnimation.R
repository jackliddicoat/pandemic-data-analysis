# packages
library(ggplot2)
library(gganimate)
library(dplyr)

# data
df <- read_csv("us-counties-2020.csv")

# filter ny
ny_df <- df %>% 
  filter(state == "New York") %>% 
  filter(!is.na(fips)) %>% 
  filter(between(date, ymd(20200301), ymd(20200601)))

# look at the data
ny_df %>% glimpse()

state <- "new york"
state <- map_data("county", region = state)
state

# our map
ggplot(county, aes(long, lat, group = group))+
  geom_polygon(color = "blue", fill = "white") +
  theme(plot.title = element_text(hjust = .5, face = "bold")) +
  labs(title = "Map of NY Counties")

# county population dataframe
pop_counties_uncleaned <- read_excel("ny_counties_pop.xlsx", skip = 4)
head(pop_counties_uncleaned)

# we need to remove the first row
pop_counties_uncleaned <- pop_counties_uncleaned[-1, ]

# then lets just get names and population
pop_counties <- pop_counties_uncleaned %>% 
  dplyr::select(name, population) %>% 
  mutate(name = gsub(" County", "", name)) %>% 
  mutate(name = tolower(name)) %>% 
  rename(subregion = name)

# join the this dat frame with the state one
counties_and_pop <- left_join(state, pop_counties, by = 'subregion')

# change this to covid
covid_ny <- ny_df %>% 
  mutate(county = tolower(county)) %>% 
  rename(subregion = county)

covid_map <- left_join(covid_ny, counties_and_pop, by = 'subregion')
covid_map$cases_per_100k <- (covid_map$cases / covid_map$population)*100000
covid_map <- covid_map[order(covid_map$date),]
covid_map <- covid_map %>% 
  filter(!is.na(group))

glimpse(covid_map)

cases <- ggplot(covid_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = cases_per_100k), color = "black") +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  transition_states(date) +
  labs(title = "Covid Cases in New York", subtitle = 'Source: New York Times, Date: {closest_state}')

animate(cases, fps = 5)
