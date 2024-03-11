library(readxl)
library(lubridate)
library(ggthemes)
library(dplyr)
library(ggplot2)

# load in our dataset
icreftables <- read_excel("icreftables.xlsx", sheet = "1", skip = 5)

# lets remove columns and rows we don't care about
df <- icreftables %>% 
  filter(row_number() < 34) %>% 
  select(-`NUTS Code`)
head(df)

# We'll grab the dates instead of week numbers from the same data (dif sheet)
weeks <- read_excel("icreftables.xlsx", sheet = "Week number dates ", skip = 3)

# It has a time so lets get rid of that using ymd()
weeks$`End date` <- ymd(weeks$`End date`)

# convert to character vector
weeks_char <- as.character(weeks$`End date`)

# make the colnames from df
colnames(df) <- c("Country", weeks_char)

# transpose the data
df <- df %>% 
  pivot_longer(cols = c(2:401),
               names_to = "week_end",
               values_to = "asmr") %>% 
  mutate(asmr = as.numeric(asmr),
         week_end = as.Date(week_end))
head(df)
  
# lets look at all countries 
df %>% 
  filter(week_end > ymd(20180101),
         !is.na(Country),
         Country != "Country") %>% 
  ggplot(aes(week_end, asmr)) +
  geom_line() +
  facet_wrap(~ Country) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = ymd(20200301),
             col = 'red', lty = 2, lwd = .5) +
  labs(title = "Pandemic-Era Age-Standardized Mortality Rates by Country")

# Lets look at the nordic countries
# a three-month moving average might be better because the data is highly variable
df %>% 
  filter(week_end > ymd(20150101),
         Country %in% c("Sweden", "Norway", "Finland")) %>%
  mutate(asmr_ma = rollmean(asmr, 13, fill = NA, align = "right")) %>% 
  ggplot(aes(week_end, asmr_ma, color = Country)) +
  geom_line() +
  geom_vline(xintercept = ymd(20200301),
             col = 'black', lty = 2, lwd = .5) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pandemic-Era Age-Standardized Mortality Rates by Country",
       subtitle = "3-Month Moving Average", x = "year", y = "asmr") +
  scale_y_continuous(limits = c(12, 22))

# Lets look at western and eastern europe
df %>% 
  filter(!is.na(asmr)) %>% 
  mutate(region = ifelse(Country %in% c("Bulgaria", "Croatia", "Czechia",
                                     "Estonia", "Latvia", "Lithuania",
                                     "Hungary", "Slovakia", "Slovania",
                                     "Romania", "Poland"), "Eastern Europe", 
                      "Western Europe")) %>% 
  group_by(region, week_end) %>% 
  summarise(avg_asmr = mean(asmr)) %>% 
  ggplot(aes(week_end, avg_asmr, group = region, color = region)) +
  geom_line() +
  theme_clean() +
  labs(title = "Western vs Eastern Europe During the Pandemic",
       y = "weekly asmr", x = "year") +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(10, 45))
