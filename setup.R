
library(tidyverse)

covid19 = read_csv("covid19.csv")
View(covid19)

#remove county_fips and date_updated
#the dataset is already processed pretty nicely 
covid19 = covid19 %>%
  select(county, state:`covid-19_community_level`) %>%
  rename('Covid19 Level' = 'covid-19_community_level', 
         'Covid Cases (per 100k)' = covid_cases_per_100k,
         'Covid19 Hospital Admissions (per 100k)' = covid_hospital_admissions_per_100k)


write_csv(x = covid19, file = "data/covid19.csv")


covid19 %>%
  filter(state == 'Illinois') %>%
  filter(county == 'DuPage County') %>%
  filter(health_service_area == "Cook (Chicago), IL - DuPage, IL") %>%
  ggplot(aes(x=`Covid19 Hospital Admissions (per 100k)`, y = `Covid Cases (per 100k)`, color = `Covid19 Level`)) +
  geom_point(size = 3) +
  theme_bw()







