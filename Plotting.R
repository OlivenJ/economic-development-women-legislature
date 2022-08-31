source("helper.R")
library(tidyverse)
library(corrplot)
library(tidymodels)

correlation <- cor(joint_data_lagLaw %>% select(hc2total, lawmaker,gdp, oldAge,youngAge, ODA,TB, labPar, democra))
corrplot(correlation, method = "color")

joint_data %>%  ggplot(aes(x = year, y = healthExp))+
  geom_line()+
  facet_wrap("country", nrow = 14)

joint_data %>% filter(country %in% c("China", "Japan","Rwanda","United Arab Emirates")) %>% 
  ggplot(aes(x = year,y = lawmaker))+
  geom_smooth()+
  geom_point()+
  facet_wrap("country")

OECD_joint  %>% ggplot(aes(x = year, y = hc2total, color = country))+
  geom_line()
  

Emerging_joint  %>% ggplot(aes(x = year, y = hc2total, color = country))+
  geom_line()

LDC_joint  %>% ggplot(aes(x = year, y = hc2total, color = country))+
  geom_line()
  
  
joint_data %>% filter(country == "China") %>% 
  pivot_longer(3:10, values_to = "values", names_to = "variables") %>% 
  ggplot(aes(x = year, y = values))+
  geom_line()+
  facet_wrap("variables", nrow = 4,scales = "free")

coplot(lawmaker ~year| country ,data = joint_data)


joint_data %>% group_by(year) %>% summarize(avghc2total = mean(hc2total)) %>% 
  ggplot(aes(x = year, y =avghc2total ))+
  geom_point()+
  theme_bw()+
  theme(panel.background = element_rect(fill='transparent'))+
  xlab("Year")+
  ylab("Average Health Care Expenditure to Budget")

joint_data %>% group_by(year) %>% summarize(avglawmaker = mean(lawmaker)) %>% 
  ggplot(aes(x = year, y = avglawmaker))+
  geom_point()+
  theme_bw()+
  xlab("Year")+
  ylab("Average Ratio of Female Lawmakers")


joint_data  %>% 
  head(480) %>% 
  ggplot(aes(x = lawmaker, y = hc2total))+
  geom_jitter()+
  facet_wrap("country", scales = "free")

joint_name <- joint_data %>% names()

joint_name[3:14]

joint_data %>% 
  select(-lawmaker_average) %>% 
  pivot_longer(joint_name[3:14], names_to = "name", values_to = "value") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap("name", scale = "free")

HI_joint %>% ggplot(aes(x = lawmaker, y = hc2total,color = year))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x)+
  facet_wrap("country")

Emerging_joint %>% ggplot(aes(x = lawmaker, y = hc2total,color = year))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x)+
  facet_wrap("country")

LDC_joint %>% ggplot(aes(x = lawmaker, y = hc2total,color = year))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x)+
  facet_wrap("country",scale= 'free')

joint_data %>% ggplot(aes(x = lawmaker, y = hc2total))+
  geom_jitter()+
  geom_smooth(method = "lm", formula = y~x)

joint_data_lagLaw %>% ggplot(aes(gdp)) +
  geom_histogram()+
  facet_grid("class")

joint_data %>% ggplot(aes(x = hc2total, y = democra))+
  geom_point()+
  geom_smooth(method = "lm")
