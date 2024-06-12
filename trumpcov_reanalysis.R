library(tidyverse)
library(dplyr)
library(lmtest)
library(car)
library(patchwork)

#drop the popsqrt variable for the cor matrix
study1 = subset(study, select = -c(county, popsqrt))
study1
#table 1. cor matrix
res = cor(study1)
round(res, 3)
cor.test(study1$m_income, study1$pct_trump)
cor.test(study1$elderly, study1$pct_trump)
cor.test(study1$pop_dens, study1$pct_trump)
cor.test(study1$vaxrate, study1$pct_trump)
cor.test(study1$pct_black, study1$pct_trump)

#OLS Regression
lm1 = lm(deaths ~ pct_trump + m_income + elderly + pop_dens + pct_black, 
         data = study1)
summary(lm1)
bptest(lm1)
vif(lm1)

#OLS Regression with control for vaccination rate
lm2 = lm(deaths ~  pct_trump + m_income + elderly + pop_dens + vaxrate +
           pct_black, data = study)
bptest(lm2)
summary(lm2)

study3 <- study %>% 
  filter(!is.na(deaths))
ols2 <- lm(deaths ~ pct_trump + m_income + elderly + pop_dens +
             vaxrate + pct_black, data = study3)
summary(ols2)
bptest(ols2)
wt <- 1 / lm(abs(ols2$residuals) ~ ols2$fitted.values)$fitted.values^2
wls1 <- lm(deaths ~ pct_trump + m_income + elderly + pop_dens +
             vaxrate + pct_black, weights = wt, data = study3)
bptest(wls1)
summary(wls1)
vif(wls1)


study %>% 
  arrange(deaths) %>% 
  print(n = 100)

# Relationship between county 
study2 <- study %>% 
  filter(!is.na(deaths))
cor(study2$deaths, study2$vaxrate)
cor.test(study2$deaths, study2$vaxrate)
study2$red <- ifelse(study2$pct_trump > 50.0, 1, 0)
t.test(study2$deaths[study2$red==1],
       study2$deaths[study2$red==0])

#robustness check
dummy <- study %>% 
  mutate(pct_trump = ifelse(pct_trump > 50.0, 1, 0))
dummy <- dummy %>% 
  rename(trump_dummy = pct_trump)
dummy.model = lm(deaths ~  trump_dummy + m_income + elderly + pop_dens +
           pct_black, data = dummy)
summary(dummy.model)
bptest(dummy.model)
vif(dummy.model)

dummy <- dummy %>% 
  filter(!is.na(deaths))
dummy.model2 = lm(deaths ~  trump_dummy + m_income + elderly + pop_dens +
                   pct_black + vaxrate, data = dummy)
wt <- 1 / lm(abs(dummy.model2$residuals) ~ dummy.model2$fitted.values)$fitted.values^2
dummy.model.wt = lm(deaths ~  trump_dummy + m_income + elderly + pop_dens +
                    pct_black + vaxrate, weights = wt, data = dummy)
summary(dummy.model.wt)
bptest(dummy.model.wt)
vif(dummy.model.wt)


t.test(dummy$vaxrate[dummy$trump_dummy==0],
       dummy$vaxrate[dummy$trump_dummy==1])

cor(dummy$vaxrate, dummy$trump_dummy)


# some more tests
library(readr)
study <- read_csv("Downloads/study.csv")
View(study)
study$margin_of_victory = study$pct_trump - (100-study$pct_trump)
study_margin = subset(study, select = c(county, pct_trump, deaths, margin_of_victory))
study_margin %>% 
  View()

# counties where the margin within 10%
study_margin_10 <- subset(study_margin, study_margin$margin_of_victory < 10 &
                            study_margin$margin_of_victory > -10)
study_margin_10$biden = ifelse(study_margin_10$margin_of_victory < 0, 1, 0)
head(study_margin_10)

# counties where the margin was 10-20%
study_margin_10_20 <- subset(study_margin, (study_margin$margin_of_victory > 10 &
                                              study_margin$margin_of_victory <20) |
                               (study_margin$margin_of_victory < -10 &
                                  study_margin$margin_of_victory > -20))
study_margin_10_20$biden = ifelse(study_margin_10_20$margin_of_victory < 0, 1, 0)
head(study_margin_10_20)

# counties where the margin was 20-30%
study_margin_20_30 <- subset(study_margin, (study_margin$margin_of_victory > 20 &
                                              study_margin$margin_of_victory < 30) |
                               (study_margin$margin_of_victory < -20 &
                                  study_margin$margin_of_victory > -30))
study_margin_20_30$biden = ifelse(study_margin_20_30$margin_of_victory < 0, 1, 0)
head(study_margin_20_30)

# counties where the margin was >30%
study_margin_gre_30 <- subset(study_margin, (study_margin$margin_of_victory >= 30) |
                                  (study_margin$margin_of_victory <= -30))
study_margin_gre_30$biden = ifelse(study_margin_gre_30$margin_of_victory < 0, 1, 0)
head(study_margin_gre_30)

study_margin_10$biden = ifelse(study_margin_10$biden==1, "Biden", "Trump")
study_margin_10_20$biden = ifelse(study_margin_10_20$biden==1, "Biden", "Trump")
study_margin_20_30$biden = ifelse(study_margin_20_30$biden==1, "Biden", "Trump")
study_margin_gre_30$biden = ifelse(study_margin_gre_30$biden==1, "Biden", "Trump")

study_margin_10 %>% 
  View()

p1 = ggplot(data = study_margin_10, aes(biden, deaths)) +
  geom_boxplot(aes(fill = biden), show.legend = F) +
  scale_fill_manual(values = c("deepskyblue4", "red")) +
  labs(title = "Within 10%", y = "Deaths/100k") +
  theme(axis.title.x = element_blank())

p2 = ggplot(data = study_margin_10_20, aes(biden, deaths)) +
  geom_boxplot(aes(fill = biden), show.legend = F) +
  scale_fill_manual(values = c("deepskyblue4", "red")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Within 10 to 20%")

p3 = ggplot(data = study_margin_20_30, aes(biden, deaths)) +
  geom_boxplot(aes(fill = biden), show.legend = F) +
  scale_fill_manual(values = c("deepskyblue4", "red")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Within 20 to 30%")

p4 = ggplot(data = study_margin_gre_30, aes(biden, deaths)) +
  geom_boxplot(aes(fill = biden), show.legend = F) +
  scale_fill_manual(values = c("deepskyblue4", "red")) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Greater than 30%")

p1+p2+p3+p4

study$vote_dummy = ifelse(study$pct_trump > 50, "Trump", "Biden")

ggplot(data = study, aes(vote_dummy, deaths)) +
  geom_boxplot(aes(fill = vote_dummy), show.legend = F) +
  scale_fill_manual(values = c("deepskyblue4", "red")) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Deaths/100k") +
  annotate("text", x = 1.5, y = 160, label = "Mean difference: 22.7/100k\nt = 2.14")

ggplot(data = study, aes(vaxrate, deaths, col = vote_dummy)) +
  geom_point() +
  geom_smooth(aes(vaxrate, deaths, col = vote_dummy),
              method = "lm", se = F) +
  theme(legend.position = "top") +
  labs(y = "Deaths/100k", x = "% vaccinated") +
  scale_color_manual(name = "county_vote", values = c("deepskyblue4", "red"))

