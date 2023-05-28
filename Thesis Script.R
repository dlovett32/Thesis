# --------------- Loading Packages -------------------#

library(tidyverse)
library(readr)
library(moderndive) 
library(skimr) 
library(ISLR)
library(texreg)
library(ggeffects)

rm(list = ls())
theme_set(theme_minimal())


# --------------- Loading and Wrangling Data -------------------#

    #Independent Variables#
      #domicil - Which phrase on this card best describes the area where you live? 1 = big city, 2 = suburb, 3 = town/small city, 4 = country village, 5 = farm/countryside
    
    #Dependent Variables#
      #contplt- contacted a politician, government or local government official in last 12 months? 1 = yes, 2 = no
      #wrkprty - worked in a political party or action group? 1 = yes, 2 = no
      #wrkorg - worked in another organisation or association? 1 = yes, 2 = no
      #badge - worn or displayed a campaign badge/sticker? 1 = yes, 2 = no
      #sgnptit - Signed petition last 12 months? 1 = yes, 2 = no 
      #pbldmn - Taken part in lawful public demonstration last 12 months? 1 = yes, 2 = no
      #bctprd - Boycotted certain products last 12 months? 1 = yes, 2 = no
    
    #Control Variables#
      #gndr = gender (1 = male, 2 = Female)
      #eisced - highest level of education? (Want to limit to answers from 1 (low) - 7 (high))
      #hinctnta - households total net income from 1 (low) - 10 (high) (1 - 12 for 2006 wave and called hinctnt)
      #agea = age
      #marsts = Marital Status, 1 = married, 2 = civil union, 3 = separated, 4 = divorced, 5 = widowed, 6 = none of these (maritala for 2006)
      #lrscale = placement on left (0) - right (10) scale
      #cntry = country


ess_2010 <- read_csv("ESS 2010 Dataset.csv")
ess_2014 <- read_csv("ESS 2014 Dataset.csv")
ess_2018 <- read_csv("ESS 2018 Dataset.csv")


## 2010 ##

ess_2010 <- ess_2010 %>% 
  select(domicil, 
         contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd, 
         gndr, eisced, hinctnta, lrscale, cntry, agea) %>% 
  filter(domicil <= 5, 
         contplt <=4, wrkprty <=4, wrkorg <=4, badge <=4, sgnptit <=4, pbldmn <=4, bctprd <=4,
         eisced <= 7, eisced >= 1, lrscale <= 10, hinctnta <= 10, agea <=150, gndr <= 3,
         cntry %in% c("CH", "DE", "DK", "ES", "FR", "NL", "NO", "PL", "BE", "CZ", "GB", "IE", "SE")) %>% 
  mutate(suburban = as.factor(domicil),
         suburban = recode(suburban, "1" = "Big City", "2" = "Suburb", "3" = "Town/Small City", "4" = "Country Village", "5" = "Farm/Countryside"),
         contplt = abs(contplt - 2),
         wrkprty = abs(wrkprty -2),
         wrkorg = abs(wrkorg -2),
         badge = abs(badge -2),
         sgnptit = abs(sgnptit -2),
         pbldmn = abs(pbldmn -2),
         bctprd = abs(bctprd -2),
         engagement = (contplt + wrkprty + wrkorg + badge + sgnptit + pbldmn + bctprd),
         engagement_standard = (engagement - mean(engagement))/sd(engagement),
         engagement_log = log10(engagement + 0.5),
         gender = as.factor(gndr), 
         gender = recode(gender, "1" = "Male", "2" = "Female"),
         country = as.factor(cntry),
         country = recode(country,"CH" = "Switzerland", "DE" = "Germany", "DK" = "Denmark", "ES" = "Spain", 
                          "FR" = "France", "NL" = "Netherlands", "NO" = "Norway", "PL" = "Poland", 
                          "BE" = "Belgium", "CZ" = "Czechia", "GB" = "United Kingdom", "IE" = "Ireland", "SE" = "Sweden"),
         education = eisced,
         education_standard = (education - mean(education))/sd(education),
         income = hinctnta, 
         income_standard = (income - mean(income))/sd(income),
         ideology = lrscale, 
         ideology_standard = (ideology - mean(ideology))/sd(ideology),
         age = agea,
         age_standard = (age - mean(age))/sd(age)) 

## 2014 ##

ess_2014 <- ess_2014 %>% 
  select(domicil, 
         contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd, 
         gndr, eisced, hinctnta, lrscale, cntry, agea) %>% 
  filter(domicil <= 5, 
         contplt <=4, wrkprty <=4, wrkorg <=4, badge <=4, sgnptit <=4, pbldmn <=4, bctprd <=4,
         eisced <= 7, eisced >= 1, lrscale <= 10, hinctnta <= 10, agea <=150, gndr <= 3,
         cntry %in% c("CH", "DE", "DK", "ES", "FR", "NL", "NO", "PL", "BE", "CZ", "GB", "IE", "SE")) %>% 
  mutate(suburban = as.factor(domicil),
         suburban = recode(suburban, "1" = "Big City", "2" = "Suburb", "3" = "Town/Small City", "4" = "Country Village", "5" = "Farm/Countryside"),
         contplt = abs(contplt - 2),
         wrkprty = abs(wrkprty -2),
         wrkorg = abs(wrkorg -2),
         badge = abs(badge -2),
         sgnptit = abs(sgnptit -2),
         pbldmn = abs(pbldmn -2),
         bctprd = abs(bctprd -2),
         engagement = (contplt + wrkprty + wrkorg + badge + sgnptit + pbldmn + bctprd),
         engagement_standard = (engagement - mean(engagement))/sd(engagement),
         engagement_log = log10(engagement + 0.5),
         gender = as.factor(gndr), 
         gender = recode(gender, "1" = "Male", "2" = "Female"),
         country = as.factor(cntry),
         country = recode(country,"CH" = "Switzerland", "DE" = "Germany", "DK" = "Denmark", "ES" = "Spain", 
                          "FR" = "France", "NL" = "Netherlands", "NO" = "Norway", "PL" = "Poland", 
                          "BE" = "Belgium", "CZ" = "Czechia", "GB" = "United Kingdom", "IE" = "Ireland", "SE" = "Sweden"),
         education = eisced,
         education_standard = (education - mean(education))/sd(education),
         income = hinctnta, 
         income_standard = (income - mean(income))/sd(income),
         ideology = lrscale, 
         ideology_standard = (ideology - mean(ideology))/sd(ideology),
         age = agea,
         age_standard = (age - mean(age))/sd(age)) 

## 2018 ##

ess_2018 <- ess_2018 %>% 
  select(domicil, 
         contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd, 
         gndr, eisced, hinctnta, lrscale, cntry, agea) %>% 
  filter(domicil <= 5, 
         contplt <=4, wrkprty <=4, wrkorg <=4, badge <=4, sgnptit <=4, pbldmn <=4, bctprd <=4,
         eisced <= 7, eisced >= 1, lrscale <= 10, hinctnta <= 10, agea <=150, gndr <= 3,
         cntry %in% c("CH", "DE", "DK", "ES", "FR", "NL", "NO", "PL", "BE", "CZ", "GB", "IE", "SE")) %>% 
  mutate(suburban = as.factor(domicil),
         suburban = recode(suburban, "1" = "Big City", "2" = "Suburb", "3" = "Town/Small City", "4" = "Country Village", "5" = "Farm/Countryside"),
         contplt = abs(contplt - 2),
         wrkprty = abs(wrkprty -2),
         wrkorg = abs(wrkorg -2),
         badge = abs(badge -2),
         sgnptit = abs(sgnptit -2),
         pbldmn = abs(pbldmn -2),
         bctprd = abs(bctprd -2),
         engagement = (contplt + wrkprty + wrkorg + badge + sgnptit + pbldmn + bctprd),
         engagement_standard = (engagement - mean(engagement))/sd(engagement),
         engagement_log = log10(engagement + 0.5),
         gender = as.factor(gndr), 
         gender = recode(gender, "1" = "Male", "2" = "Female"),
         country = as.factor(cntry),
         country = recode(country,"CH" = "Switzerland", "DE" = "Germany", "DK" = "Denmark", "ES" = "Spain", 
                          "FR" = "France", "NL" = "Netherlands", "NO" = "Norway", "PL" = "Poland", 
                          "BE" = "Belgium", "CZ" = "Czechia", "GB" = "United Kingdom", "IE" = "Ireland", "SE" = "Sweden"),
         education = eisced,
         education_standard = (education - mean(education))/sd(education),
         income = hinctnta, 
         income_standard = (income - mean(income))/sd(income),
         ideology = lrscale, 
         ideology_standard = (ideology - mean(ideology))/sd(ideology),
         age = agea,
         age_standard = (age - mean(age))/sd(age)) 


# --------------- Exploratory Analysis -------------------#

## 2010 ##

library(psych)
index_2010 <- ess_2010 %>% 
  select(contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd)
alpha(index_2010)

summary(ess_2010$engagement)
sd(ess_2010$engagement)
ggplot(ess_2010, aes(x = engagement)) +
  geom_histogram() +
  labs(x = "Civic Engagement Score", y = "Count", title = "Distribution of Civic Engagement Scores (2010)") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7))

summary(ess_2010$engagement_log)
ggplot(ess_2010, aes(x = engagement_log)) +
  geom_histogram() +
  labs(x = "Logged Civic Engagement Score", y = "Count", title = "Distribution of Logged Civic Engagement Scores (2010)")

summary(ess_2010$contplt)
summary(ess_2010$wrkprty)
summary(ess_2010$wrkorg)
summary(ess_2010$badge)
summary(ess_2010$sgnptit)
summary(ess_2010$pbldmn)
summary(ess_2010$bctprd)

summary(ess_2010$suburban)

summary(ess_2010$gender)
summary(ess_2010$country)
summary(ess_2010$age)
sd(ess_2010$age)
summary(ess_2010$income)
sd(ess_2010$income)
summary(ess_2010$ideology)
sd(ess_2010$ideology)
summary(ess_2010$education)
sd(ess_2010$education)

ess_2010_by_hood <- ess_2010 %>%  
  group_by(suburban) %>% 
  summarize(avg_civic = mean(engagement),
            med_civic = median(engagement),
            std_civic = sd(engagement),
            min_civic = min(engagement),
            max_civic = max(engagement),
            iqr_civic = IQR(engagement))
ggplot(ess_2010_by_hood, aes(x = suburban, y = avg_civic)) +
  geom_col() +
  labs(x = "Neighborhood Type", y = "Mean Civic Engagement Score", title = "Mean Civic Engagement Score by Neighborhood Type (2010)")

## 2014 ##

index_2014 <- ess_2014 %>% 
  select(contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd)
alpha(index_2014)

summary(ess_2014$engagement)
sd(ess_2014$engagement)
ggplot(ess_2014, aes(x = engagement)) +
  geom_histogram() +
  labs(x = "Civic Engagement Score", y = "Count", title = "Distribution of Civic Engagement Scores (2014)") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7))

summary(ess_2014$contplt)
summary(ess_2014$wrkprty)
summary(ess_2014$wrkorg)
summary(ess_2014$badge)
summary(ess_2014$sgnptit)
summary(ess_2014$pbldmn)
summary(ess_2014$bctprd)

summary(ess_2014$suburban)

summary(ess_2014$gender)
summary(ess_2014$country)
summary(ess_2014$age)
sd(ess_2014$age)
summary(ess_2014$income)
sd(ess_2014$income)
summary(ess_2014$ideology)
sd(ess_2014$ideology)
summary(ess_2014$education)
sd(ess_2014$education)

ess_2014_by_hood <- ess_2014 %>%  
  group_by(suburban) %>% 
  summarize(avg_civic = mean(engagement),
            med_civic = median(engagement),
            std_civic = sd(engagement),
            min_civic = min(engagement),
            max_civic = max(engagement),
            iqr_civic = IQR(engagement))
ggplot(ess_2014_by_hood, aes(x = suburban, y = avg_civic)) +
  geom_col() +
  labs(x = "Neighborhood Type", y = "Mean Civic Engagement Score", title = "Mean Civic Engagement Score by Neighborhood Type (2014)")

## 2018 ##

index_2018 <- ess_2018 %>% 
  select(contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd)
alpha(index_2018)

summary(ess_2018$engagement)
sd(ess_2018$engagement)
ggplot(ess_2018, aes(x = engagement)) +
  geom_histogram() +
  labs(x = "Civic Engagement Score", y = "Count", title = "Distribution of Civic Engagement Scores (2018)") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7))

summary(ess_2018$contplt)
summary(ess_2018$wrkprty)
summary(ess_2018$wrkorg)
summary(ess_2018$badge)
summary(ess_2018$sgnptit)
summary(ess_2018$pbldmn)
summary(ess_2018$bctprd)

summary(ess_2018$suburban)

summary(ess_2018$gender)
summary(ess_2018$country)
summary(ess_2018$age)
sd(ess_2018$age)
summary(ess_2018$income)
sd(ess_2018$income)
summary(ess_2018$ideology)
sd(ess_2018$ideology)
summary(ess_2018$education)
sd(ess_2018$education)

ess_2018_by_hood <- ess_2018 %>%  
  group_by(suburban) %>% 
  summarize(avg_civic = mean(engagement),
            med_civic = median(engagement),
            std_civic = sd(engagement),
            min_civic = min(engagement),
            max_civic = max(engagement),
            iqr_civic = IQR(engagement))
ggplot(ess_2018_by_hood, aes(x = suburban, y = avg_civic)) +
  geom_col() +
  labs(x = "Neighborhood Type", y = "Mean Civic Engagement Score", title = "Mean Civic Engagement Score by Neighborhood Type (2018)")


# --------------- Regression Analysis -------------------#

## Linear Regression ##

## 2010 ##

model_2010 <- lm(engagement_standard ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)

prediction_2010 <- ggpredict(model_2010, terms = "suburban")

plot(prediction_2010) + 
  labs(title = "Predicted Values of Civic Engagement (2010)", y = "Civic Engagement", x = "Neighborhood Type")

## 2014 ##

model_2014 <- lm(engagement_standard ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)

prediction_2014 <- ggpredict(model_2014, terms = "suburban")

plot(prediction_2014) + 
  labs(title = "Predicted Values of Civic Engagement (2014)", y = "Civic Engagement", x = "Neighborhood Type")

## 2018 ##

model_2018 <- lm(engagement_standard ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)

prediction_2018 <- ggpredict(model_2018, terms = "suburban")

plot(prediction_2018) + 
  labs(title = "Predicted Values of Civic Engagement (2018)", y = "Civic Engagement", x = "Neighborhood Type")


screenreg(list(model_2010, model_2014, model_2018), 
          custom.model.names = c("2010", "2014", "2018"), 
          omit.coef = c("country"),
          custom.coef.names = c("Intercept (Big City, Female)",
                                "Suburb",
                                "Town/Small City",
                                "Country Village",
                                "Countryside",
                                "Gender (Male)",
                                "Education",
                                "Income",
                                "Ideology",
                                "Age"))

 
## Logistic Regression ##

## 2010 ##

petition_2010 <- glm(sgnptit ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
boycott_2010 <- glm(bctprd ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
org_2010 <- glm(wrkorg ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
politician_2010 <- glm(contplt ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
badge_2010 <- glm(badge ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
rally_2010 <- glm(pbldmn ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)
party_2010 <- glm(wrkprty ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)

wordreg(list(petition_2010, boycott_2010, org_2010, politician_2010, badge_2010, rally_2010, party_2010), 
          custom.model.names = c("Petition", "Boycott", "Org", "Politician", "Badge", "Rally", "Party"),
        file = "logistic_2010.doc")

## 2014 ##

petition_2014 <- glm(sgnptit ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
boycott_2014 <- glm(bctprd ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
org_2014 <- glm(wrkorg ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
politician_2014 <- glm(contplt ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
badge_2014 <- glm(badge ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
rally_2014 <- glm(pbldmn ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)
party_2014 <- glm(wrkprty ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)

screenreg(list(petition_2014, boycott_2014, org_2014, politician_2014, badge_2014, rally_2014, party_2014), 
          custom.model.names = c("Petition", "Boycott", "Org", "Politician", "Badge", "Rally", "Party"))

## 2018 ##
petition_2018 <- glm(sgnptit ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
boycott_2018 <- glm(bctprd ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
org_2018 <- glm(wrkorg ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
politician_2018 <- glm(contplt ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
badge_2018 <- glm(badge ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
rally_2018 <- glm(pbldmn ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)
party_2018 <- glm(wrkprty ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)

screenreg(list(petition_2018, boycott_2018, org_2018, politician_2018, badge_2018, rally_2018, party_2018), 
          custom.model.names = c("Petition", "Boycott", "Org", "Politician", "Badge", "Rally", "Party"))


# --------------- Robustiness Testing -------------------#

## 2010 ##

log_model_2010 <- lm(engagement_log ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2010)

log_prediction_2010 <- ggpredict(log_model_2010, terms = "suburban")

plot(log_prediction_2010) + 
  labs(title = "Predicted Values of Logged Civic Engagement (2010)", y = "Logged Civic Engagement", x = "Neighborhood Type")

## 2014 ##

log_model_2014 <- lm(engagement_log ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2014)

log_prediction_2014 <- ggpredict(log_model_2014, terms = "suburban")

plot(log_prediction_2014) + 
  labs(title = "Predicted Values of Logged Civic Engagement (2014)", y = "Logged Civic Engagement", x = "Neighborhood Type")

## 2018 ##

log_model_2018 <- lm(engagement_log ~ suburban + gender + education_standard + income_standard + ideology_standard + age_standard + country, data = ess_2018)

log_prediction_2018 <- ggpredict(log_model_2018, terms = "suburban")

plot(log_prediction_2018) + 
  labs(title = "Predicted Values of Logged Civic Engagement (2018)", y = "Logged Civic Engagement", x = "Neighborhood Type")


screenreg(list(log_model_2010, log_model_2014, log_model_2018), 
          custom.model.names = c("2010", "2014", "2018"))
