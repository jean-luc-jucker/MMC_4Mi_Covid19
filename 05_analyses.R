library(tidyverse)
library(scales)
library(lubridate)
library(viridis)
library(ggpubr)
getwd()
load("rda/05_cleaned_data_20200609.rda")
dim(data) #4,124 obs, 207 vars
View(data) 


#NUMBER OF INTERVIEWS####
data %>% group_by(Region) %>%
  tally()
data %>% group_by(Region, Q13_1c) %>% 
  tally()
#nas
summary(data$c1) #total nas = 9

#GENDER AND AGE####
data %>% 
  group_by(Q25) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100)
data %>% 
  group_by(Region, Q13_1c, Q25) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100) %>% filter(Q25=="Female")


data %>% 
  summarise(Mean=mean(Q23))
data %>% 
  group_by(Region, Q13_1c) %>% 
  summarise(Mean=mean(Q23))


#INTERVIEWS OVER TIME####
#bar, with updates cutoff
data %>% 
  group_by(Date, Region) %>% 
  tally() %>% 
  ggplot(aes(x=as.factor(Date), y=n, fill=Region))+
  geom_col()+
  geom_vline(xintercept = 16.5)+
  geom_vline(xintercept = 22.5)+
  geom_vline(xintercept = 36.5)+
  geom_vline(xintercept = 51.5)+
  geom_vline(xintercept = 64.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

#line
data %>% 
  group_by(Date, Region) %>% 
  tally() %>% 
  ggplot(aes(x=as.factor(Date), y=n, color=Region))+
  geom_line(aes(group=Region))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

#stacked
stacked <- data %>% 
  group_by(Region, Date) %>% 
  tally()  %>%
  mutate(cumulative=cumsum(n))
stacked %>%   ggplot(aes(x=Date, y=cumulative, fill=Region))+
  geom_area()+
  geom_vline(xintercept = as.Date('2020-04-20'))+
  geom_vline(xintercept = as.Date('2020-04-26'))+
  geom_vline(xintercept = as.Date('2020-05-10'))+
  geom_vline(xintercept = as.Date('2020-05-25'))+
  geom_vline(xintercept = as.Date('2020-06-08'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
  scale_x_date(date_breaks="1 day")+
  scale_y_continuous(breaks = seq(0, 4500, by = 100))

#CREATE PERIODS####
#split
data  <- data %>% 
  mutate(Period = ifelse(Date >= '2020-04-06' & Date <= '2020-05-05', 1,
                ifelse(Date >= '2020-05-06' & Date <= '2020-05-20', 2,
                3)))
mean(data$Period)
dim(data) #208 cols

#count
data %>% group_by(Period, Region) %>% tally()
data %>% group_by(Period, Region) %>% tally() %>% 
  mutate(Percent=n/sum(n)*100) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=Region))+
  geom_col(width = 0.5)


#add n by period
data <- data %>% 
  group_by(Period) %>% 
  mutate(N_period = length(Period))
dim(data) #209 cols
#add n by period and region
data <- data %>% 
  group_by(Period, Region) %>% 
  mutate(N_period_region = length(Region))
dim(data) #210 cols
#add n by period and country
data <- data %>% 
  group_by(Period, Q13_1c) %>% 
  mutate(N_period_country = length(Q13_1c))
dim(data) #211 cols
View(data)

#export data
write.csv(data, "data_outputs/05_periods_20200610.csv")
#export colnames
columns <- colnames(data)
View(columns)
View(data)
write.csv(columns, "data_outputs/05_variables_names_20200609.csv")

#ITEMS####

#c3 I am worried about catching coronavirus (na=9)####
levels(data$c3)
data$c3 <- factor(data$c3, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
#overall
data %>% 
  group_by(Period, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% print(n=21)
data %>% 
  group_by(Period, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c3)) +
  geom_point(aes(group=c3))+
  geom_line(aes(group=c3))
#region
data %>%
  group_by(Period, Region, N_period_region, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=82)
data %>%
  group_by(Period, Region, N_period_region, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c3))+
  geom_col()+
  facet_grid(~Region)
#country
data %>%
  group_by(Period, Q13_1c, N_period_country, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>% print(n=161)
data %>%
  group_by(Period, Q13_1c, N_period_country, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c3))+
  geom_col()+
  facet_grid(~Q13_1c)

#c4 I am worried about transmitting coronavirus (na=9)####
levels(data$c4)
data$c4 <- factor(data$c4, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
#overall
data %>% 
  group_by(Period, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% print(n=21)
data %>% 
  group_by(Period, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c4)) +
  geom_point(aes(group=c4))+
  geom_line(aes(group=c4))
#region
data %>%
  group_by(Period, Region, N_period_region, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=86)
data %>%
  group_by(Period, Region, N_period_region, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c4))+
  geom_col()+
  facet_grid(~Region)
#country
data %>%
  group_by(Period, Q13_1c, N_period_country, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>% print(n=173)
data %>%
  group_by(Period, Q13_1c, N_period_country, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c4))+
  geom_col()+
  facet_grid(~Q13_1c)

#c5 I know about coronavirus and how to protect myself and others (na=8)####
levels(data$c5)
data$c5 <- factor(data$c5, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
#overall
data %>% 
  group_by(Period, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% print(n=21)
data %>% 
  group_by(Period, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c5)) +
  geom_point(aes(group=c5))+
  geom_line(aes(group=c5))
#region
data %>%
  group_by(Period, Region, N_period_region, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=86)
data %>%
  group_by(Period, Region, N_period_region, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c5))+
  geom_col()+
  facet_grid(~Region)
#country
data %>%
  group_by(Period, Q13_1c, N_period_country, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>% print(n=173)
data %>%
  group_by(Period, Q13_1c, N_period_country, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c5))+
  geom_col()+
  facet_grid(~Q13_1c)


#c6 If you have received information on coronavirus and how to protect yourself, who did you receive it from? (n=4,115, na=9)####

#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 55:70) %>% #this excludes nones
  pivot_longer(cols = 3:18, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>% print(n=47)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 55:70) %>% #this excludes nones
  pivot_longer(cols = 3:18, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>% print(n=47) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer))+
  geom_line(aes(group=Answer))

#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 55:71) %>% #this includes nones
  pivot_longer(cols = 4:20, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>% print(n=210)

data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 55:71) %>% #this includes nones
  pivot_longer(cols = 4:20, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer))+
  facet_wrap(~Region)

data %>% ungroup() %>% #need to ungroup first, which is odd
    select(208, 205, 210, 55:71) %>% #this includes nones
    pivot_longer(cols = 4:20, names_to="Options", values_to="Answer") %>% 
    filter(!is.na(Answer)) %>% 
    group_by(Period, Region, N_period_region, Answer) %>% 
    tally() %>% 
    mutate(Percent=round(n/N_period_region*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  facet_wrap(~Answer)


#c13 What are you currently doing to protect yourself against coronavirus? (multi-select, n=4,115, na=9)####

#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 34:43) %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>% print(n=29)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 34:43) %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer))+
  geom_line(aes(group=Answer))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 34:43) %>%
  pivot_longer(cols = 4:13, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>% print(n=130)
data %>% ungroup() %>% 
  select(208, 205, 210, 34:43) %>%
  pivot_longer(cols = 4:13, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer))+
  facet_wrap(~Region)
data %>% ungroup() %>% 
  select(208, 205, 210, 34:43) %>%
  pivot_longer(cols = 4:13, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  facet_wrap(~Answer)


#c17 If you had coronavirus and needed healthcare, would you be able to access health services today?####
#overall
data %>% 
  group_by(Period, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% print(n=21)
data %>% 
  group_by(Period, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c17)) +
  geom_point(aes(group=c17))+
  geom_line(aes(group=c17))
#region
data %>%
  group_by(Period, Region, N_period_region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=86)
data %>%
  group_by(Period, Region, N_period_region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c17))+
  geom_col()+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  facet_wrap(~c17)
#country
data %>%
  group_by(Period, Q13_1c, N_period_country, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>% print(n=173)
data %>%
  group_by(Period, Q13_1c, N_period_country, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c17))+
  geom_col()+
  facet_grid(~Q13_1c)
data %>%
  group_by(Period, Q13_1c, N_period_country, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_country*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c17))+
  geom_line(aes(group=c17))+
  facet_wrap(~Q13_1c)

#c18 What are the barriers to accessing health services? (multi-select, n=3,282, na=8)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 115:127) %>% #this excludes nones
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>% print(n=39)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 115:127) %>% #this excludes nones
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer))+
  geom_line(aes(group=Answer))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 115:128) %>% #this includes nones
  pivot_longer(cols = 4:17, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>% print(n=172)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 115:128) %>% #this includes nones
  pivot_longer(cols = 4:17, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 115:128) %>% #this includes nones
  pivot_longer(cols = 4:17, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  facet_wrap(~Answer)

#c26 Are you in need of extra help since the coronavirus outbreak began?####
#overall
data %>% 
  group_by(Period, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=21)
data %>% 
  group_by(Period, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c26)) +
  geom_point(aes(group=c26), size=3)+
  geom_line(aes(group=c26), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")
#region
data %>%
  group_by(Period, Region, N_period_region, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=55)
data %>%
  group_by(Period, Region, N_period_region, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c26))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_viridis(discrete=TRUE, na.value = "grey50")+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")+
  facet_wrap(~c26)

#c27 What kind of extra help? (multi-select, n=1,770 / 1,012 / 782, total = 3,564)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 157:169) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>%
  select(208, 157:169) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 205, 157:169) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>% 
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% print(n=174)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 205, 157:169) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>% 
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 205, 157:169) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  filter(Answer != "Refused") %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>% 
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region))+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")
  
#c23 Have you received additional assistance since the coronavirus crisis began?####
#overall
data %>% 
  group_by(Period, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=21)
data %>% 
  group_by(Period, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c23)) +
  geom_point(aes(group=c23), size=3)+
  geom_line(aes(group=c23), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")
#region
data %>%
  group_by(Period, Region, N_period_region, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=55)
data %>%
  group_by(Period, Region, N_period_region, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c23))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_viridis(discrete=TRUE, na.value = "grey50")+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")+
  facet_wrap(~c23)




#c24 What assistance was that? (multi-select, n=960)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% view()#discard those who don't need help
  select(208, 131:143) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 131:143) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))

#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 131:143) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%   mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% print(n=180)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 131:143) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%   mutate(Percent=round(n/n_region_p*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 131:143) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%   mutate(Percent=round(n/n_region_p*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1)+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")


#c25 Who did you receive it from? (multi-select, n=960)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>%#discard those who don't need help
  select(208, 145:154) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:11, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>%#discard those who don't need help
  select(208, 145:154) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>% 
  pivot_longer(cols = 2:11, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 145:154) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% print(n=180)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 145:154) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c23 == "Yes") %>% #discard those who don't need help
  select(208, 205, 145:154) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1)+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")

#c20 What impact has the crisis had on your day-to-day life? (multi-select, n=4,115 na=9)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 171:177) %>% #this excludes nones
  pivot_longer(cols = 3:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1))
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 171:177) %>% #this excludes nones
  pivot_longer(cols = 3:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 171:178) %>% #this includes nones
  pivot_longer(cols = 4:11, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=172)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 171:178) %>% #this includes nones
  pivot_longer(cols = 4:11, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%   ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 171:178) %>% #this includes nones
  pivot_longer(cols = 4:11, names_to="Options", values_to="Answer") %>%   
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1)+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")

#c21 Have you lost income due to coronavirus restrictions?####
#overall
data %>% 
  group_by(Period, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=21)
data %>% 
  group_by(Period, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c21)) +
  geom_point(aes(group=c21), size=3)+
  geom_line(aes(group=c21), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")
#region
data %>%
  group_by(Period, Region, N_period_region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=58)
data %>%
  group_by(Period, Region, N_period_region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c21))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_viridis(discrete=TRUE, na.value = "grey50")+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")+
  facet_wrap(~c21)
  
#c22 What impact has the loss of income had? (multi-select, n=2,368)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard not impacted
  select(208, 181:188) %>%
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard not impacted
  select(208, 181:188) %>%
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))

#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard those who don't need help
  select(208, 205, 181:188) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:10, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% print(n=98)


data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard those who don't need help
  select(208, 205, 181:188) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:10, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
  
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard those who don't need help
  select(208, 205, 181:188) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:10, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1.5)+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")


#C19 What impact has the coronavirus crisis had on your migration journey? (multi-select, n=4,115, na=9)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 190:200) %>% #this excludes nones
  pivot_longer(cols = 3:13, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>%  print(n=33)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 190:200) %>% #this excludes nones
  pivot_longer(cols = 3:13, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "I've been delayed because I was sick, or because I had to stop and take care of people who got sick"="Delayed because I or other people got sick")) %>%
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>%
ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer), size=3)+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))
#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 190:200) %>%#this includes nones
  pivot_longer(cols = 4:14, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=141)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 190:200) %>%#this includes nones
  pivot_longer(cols = 4:14, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "I've been delayed because I was sick, or because I had to stop and take care of people who got sick"="Delayed because I or other people got sick")) %>%
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer))+
  geom_line(aes(group=Answer), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  facet_wrap(~Region)
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 190:200) %>%#this includes nones
  pivot_longer(cols = 4:14, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "I've been delayed because I was sick, or because I had to stop and take care of people who got sick"="Delayed because I or other people got sick")) %>%
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1)+
  facet_wrap(~Answer)+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")

#C29 Have you changed your plans as a result of the coronavirus outbreak?####
data %>% 
  group_by(Period, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=24)
data %>% 
  group_by(Period, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=c29)) +
  geom_point(aes(group=c29), size=3)+
  geom_line(aes(group=c29), size=1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")
#region
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=98)
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c29))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_viridis(discrete=TRUE, na.value = "grey50")+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")+
  facet_wrap(~c29)



#CHOSEN GRAPHS#############################################

#c3 I am worried about catching coronavirus (na=9)####
levels(data$c3)
data$c3 <- factor(data$c3, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

#overall
data %>% 
  mutate(c3 = recode(c3, "Strongly agree" = "Agree")) %>% 
  group_by(Period, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c3=="Agree")

#region
c3 <- data %>% 
  mutate(c3 = recode(c3, "Strongly agree" = "Agree")) %>% 
  group_by(Period, Region, N_period_region, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  filter(c3=="Agree") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c3="Agree", n=1820, Percent=88.9) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c3="Agree", n=1051, Percent=88.8) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c3="Agree", n=785, Percent=88) 
c3

c3$Region <- factor(c3$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig1 <- c3 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig1


#c4 I am worried about transmitting coronavirus (na=9)####
levels(data$c4)
data$c4 <- factor(data$c4, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

#overall
data %>% 
  mutate(c4 = recode(c4, "Strongly agree" = "Agree")) %>% 
  group_by(Period, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c4=="Agree")

#region
c4 <- data %>% 
  mutate(c4 = recode(c4, "Strongly agree" = "Agree")) %>% 
  group_by(Period, Region, N_period_region, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  filter(c4=="Agree") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c4="Agree", n=1435, Percent=70.1) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c4="Agree", n=820, Percent=69.3) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c4="Agree", n=602, Percent=67.5) 
c4

c4$Region <- factor(c4$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig2 <- c4 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig2

fig1and2  <- ggarrange(fig1, fig2, labels = c("C", "T"), ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
fig1and2

#c5 I know about coronavirus and how to protect myself and others (na=8)####
levels(data$c5)
data$c5 <- factor(data$c5, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))

#overall
data %>% 
  mutate(c5 = recode(c5, "Strongly agree" = "Agree")) %>% 
  group_by(Period, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c5=="Agree")

#region
c5 <- data %>% 
  mutate(c5 = recode(c5, "Strongly agree" = "Agree")) %>% 
  group_by(Period, Region, N_period_region, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  filter(c5=="Agree") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c5="Agree", n=1729, Percent=84.4) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c5="Agree", n=985, Percent=83.2) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c5="Agree", n=774, Percent=86.8) 
c5

c5$Region <- factor(c5$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig3 <- c5 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig3

#c13 What are you currently doing to protect yourself against coronavirus? (multi-select, n=4,115, na=9)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 34:43) %>%
  pivot_longer(cols = 3:12, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>% 
  filter(Answer == "Nothing")
#region
c13 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 34:43) %>%
  pivot_longer(cols = 4:13, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer, .drop = FALSE) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  filter(Answer == "Nothing") %>%
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, Answer="Nothing", n=168, Percent=8.2) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, Answer="Nothing", n=100, Percent=8.4) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, Answer="Nothing", n=65, Percent=7.3) 
c13

c13$Region <- factor(c13$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig4 <- c13 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig4



#c17 If you had coronavirus and needed healthcare, would you be able to access health services today?####
#overall
data %>% 
  group_by(Period, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c17=="Yes" | c17=="No" | c17=="Don´t know")
  
#region
c17  <- data %>%
  group_by(Period, Region, N_period_region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  filter(c17=="Yes" | c17=="No" | c17=="Don´t know") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c17="Don´t know", n=569, Percent=27.8) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c17="Don´t know", n=405, Percent=34.2) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c17="Don´t know", n=246, Percent=27.6) %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c17="No", n=627, Percent=30.6) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c17="No", n=362, Percent=30.6) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c17="No", n=259, Percent=29) %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c17="Yes", n=792, Percent=38.7) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c17="Yes", n=401, Percent=33.9) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c17="Yes", n=376, Percent=42.2)
c17 %>% print(n=54)

c17$Region <- factor(c17$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))
c17$c17 <- factor(c17$c17,levels = c("Yes", "No", "Don´t know"))

fig5 <- c17 %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))+
  facet_wrap(~c17)+
  theme(strip.text = element_text(size=14))
fig5
  

#c18 What are the barriers to accessing health services? (multi-select, n=3,282, na=8)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 115:128) %>% #this excludes nones
  pivot_longer(cols = 3:16, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>% print(n=42)

library(RColorBrewer)
display.brewer.all()

fig6 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 115:128) %>% #this excludes nones
  pivot_longer(cols = 3:16, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100)) %>%
  filter(Answer != "Refused") %>% 
  filter(Answer != "Don't know") %>%
  filter(Answer != "Other (specify)") %>%
  mutate(Answer = recode(Answer, "I don't know where to go for healthcare"="I don't know where to go", "The advice for testing and treating coronavirus is unclear here"="The advice is unclear","I don't have the money to pay for health services"="I don't have the money","I don't have the right or the legal documentation to access health services here"="I don't have the right", "I am afraid of being reported to authorities, or arrest, or deportation"="Afraid of being reported", "Discrimination against foreigners limits access to services"="Discrimination against foreigners","General insecurity and conflict prevent me from accessing healthcare"="General insecurity and conflict","There are no health services here"="There are no health services","Services are overwhelmed and access is difficult for everyone"="Services are overwhelmed",)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_point(aes(group=Answer))+
  geom_line(aes(group=Answer), size=2.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  scale_color_brewer(palette = "Paired")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig6



#c26 Are you in need of extra help since the coronavirus outbreak began?####
#overall
data %>% 
  group_by(Period, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=21) %>%
  filter(c26=="Yes")
#region
c26  <- data %>%
  group_by(Period, Region, N_period_region, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%  
  filter(c26=="Yes") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c26="Yes", n=1770, Percent=86.4) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c26="Yes", n=1012, Percent=85.5) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c26="Yes", n=782, Percent=87.7)
c26

c26$Region <- factor(c26$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig7 <- c26 %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig7


#c27 What kind of extra help? (multi-select, n=1,770 / 1,012 / 782, total = 3,564)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 157:169) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
fig8  <- data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>%
  select(208, 157:169) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  filter(Answer != "Refused") %>% 
  filter(Answer != "Other") %>% 
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_line(aes(group=Answer), size=2.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  scale_color_brewer(palette = "Paired")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig8

data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 157:169) %>% 
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:14, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  filter(Answer=="Cash") %>% print()
#region
c27 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c26 == "Yes") %>% #discard those who don't need help
  select(208, 205, 157:169) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:15, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>% 
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% 
  filter(Answer=="Cash") %>%
  ungroup() %>% 
  add_row(Period=1, Region="Overall", n_region_p=1770, Answer="Cash", n=1327, Percent=75) %>% 
  add_row(Period=2, Region="Overall", n_region_p=1012, Answer="Cash", n=806, Percent=79.6) %>%
  add_row(Period=3, Region="Overall", n_region_p=782, Answer="Cash", n=608, Percent=77.5)
c27

c27$Region <- factor(c27$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig9  <- c27 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig9

#c23 Have you received additional assistance since the coronavirus crisis began?####
#overall
data %>% 
  group_by(Period, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c23=="Yes")
#region
c23 <- data %>%
  group_by(Period, Region, N_period_region, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  filter(c23=="Yes") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c23="Yes", n=415, Percent=20.3) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c23="Yes", n=251, Percent=21.2) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c23="Yes", n=294, Percent=33)
c23  

c23$Region <- factor(c23$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig10  <- c23 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig10  



#c20 What impact has the crisis had on your day-to-day life? (multi-select, n=4,115 na=9)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 171:177) %>% #this excludes nones
  pivot_longer(cols = 3:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>% print(n=21)

fig11 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 171:177) %>% #this excludes nones
  pivot_longer(cols = 3:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_line(aes(group=Answer), size=2.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 5))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  scale_color_brewer(palette = "Paired")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig11
#region-xenophobia
c20  <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 171:178) %>% #this includes nones
  pivot_longer(cols = 4:11, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  filter(Answer=="Increased racism and xenophobia")%>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, Answer="Increased racism and xenophobia", n=406, Percent=19.8) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, Answer="Increased racism and xenophobia", n=198, Percent=16.7) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, Answer="Increased racism and xenophobia", n=156, Percent=17.5) 
c20

c20$Region <- factor(c20$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig12  <- c20 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig12
#region-basic goods
c20_bis <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 171:178) %>% #this includes nones
  pivot_longer(cols = 4:11, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  filter(Answer=="Reduced availability of basic goods") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, Answer="Reduced availability of basic goods", n=1039, Percent=50.7) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, Answer="Reduced availability of basic goods", n=555, Percent=46.9) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, Answer="Reduced availability of basic goods", n=441, Percent=49.4) 

c20_bis$Region <- factor(c20_bis$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig13  <- c20_bis %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig13


#c21 Have you lost income due to coronavirus restrictions?####
#overall
data %>% 
  group_by(Period, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  filter(c21=="Yes")
#region
c21 <- data %>%
  group_by(Period, Region, N_period_region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%  
  filter(c21=="Yes") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, c21="Yes", n=1226, Percent=59.9) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, c21="Yes", n=637, Percent=53.8) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, c21="Yes", n=505, Percent=56.6)
c21

c21$Region <- factor(c21$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig14 <- c21 %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig14



#c22 What impact has the loss of income had? (multi-select, n=2,368)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard not impacted
  select(208, 181:188) %>%
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% print(n=39)
fig15 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard not impacted
  select(208, 181:188) %>%
  group_by(Period) %>% 
  mutate(n_period = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 2:9, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Answer, n_period, .drop = FALSE) %>% tally() %>%
  mutate(Percent=round(n/n_period*100, digits = 1)) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Answer)) +
  geom_line(aes(group=Answer), size=2.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  scale_color_brewer(palette = "Paired")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig15

c22 <- data %>% ungroup() %>% #need to ungroup first, which is odd
  filter(c21 == "Yes") %>% #discard those who don't need help
  select(208, 205, 181:188) %>% 
  group_by(Period, Region) %>% 
  mutate(n_region_p = length(Period)) %>% ungroup() %>%
  pivot_longer(cols = 3:10, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(Period, Region, Answer, n_region_p) %>% tally() %>%
  mutate(Percent=round(n/n_region_p*100, digits = 1)) %>% 
  filter(Answer=="I am unable to afford basic goods" | Answer=="I am unable to pay remittances" | Answer=="I am unable to continue my journey") %>%
  ungroup() %>% 
  add_row(Period=1, Region="Overall", n_region_p=1226, Answer="I am unable to afford basic goods", n=819, Percent=66.8) %>% 
  add_row(Period=2, Region="Overall", n_region_p=637, Answer="I am unable to afford basic goods", n=389, Percent=61.1) %>%
  add_row(Period=3, Region="Overall", n_region_p=505, Answer="I am unable to afford basic goods", n=357, Percent=70.7) %>% 
  add_row(Period=1, Region="Overall", n_region_p=1226, Answer="I am unable to pay remittances", n=474, Percent=38.7) %>% 
  add_row(Period=2, Region="Overall", n_region_p=637, Answer="I am unable to pay remittances", n=216, Percent=33.9) %>%
  add_row(Period=3, Region="Overall", n_region_p=505, Answer="I am unable to pay remittances", n=199, Percent=39.4) %>% 
  add_row(Period=1, Region="Overall", n_region_p=1226, Answer="I am unable to continue my journey", n=419, Percent=34.2) %>% 
  add_row(Period=2, Region="Overall", n_region_p=637, Answer="I am unable to continue my journey", n=231, Percent=36.3) %>%
  add_row(Period=3, Region="Overall", n_region_p=505, Answer="I am unable to continue my journey", n=130, Percent=25.7)
c22 %>% print(n=54)

c22$Region <- factor(c22$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig16 <- c22 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))+
  facet_wrap(~Answer)+
  theme(strip.text = element_text(size=14))
fig16

#C19 What impact has the coronavirus crisis had on your migration journey? (multi-select, n=4,115, na=9)####
#overall
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 209, 190:200) %>% 
  pivot_longer(cols = 3:13, names_to="Options", values_to="Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Period, N_period, Answer) %>%
  tally() %>% 
  mutate(Percent=round(n/N_period*100, digits = 1)) %>%  print(n=33)

#region
data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 190:200) %>%#this includes nones
  pivot_longer(cols = 4:14, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=141)

c19  <- data %>% ungroup() %>% #need to ungroup first, which is odd
  select(208, 205, 210, 190:200) %>%#this includes nones
  pivot_longer(cols = 4:14, names_to="Options", values_to="Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Period, Region, N_period_region, Answer) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% 
  filter(Answer=="None") %>% 
  ungroup() %>% 
  add_row(Period=1, Region="Overall", N_period_region=2048, Answer="None", n=413, Percent=20.2) %>% 
  add_row(Period=2, Region="Overall", N_period_region=1184, Answer="None", n=278, Percent=23.5) %>%
  add_row(Period=3, Region="Overall", N_period_region=892, Answer="None", n=307, Percent=34.4)
c19

c19$Region <- factor(c19$Region,levels = c("Asia", "East Africa", "Latin America", "North Africa", "West Africa", "Overall"))

fig17  <- c19 %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region)) +
  geom_line(aes(group=Region, linetype=Region), size=2.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "dashed"))+
  scale_color_manual(values=c("#CC79A7", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000"))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig17


#C29 Have you changed your plans as a result of the coronavirus outbreak?####
data %>% 
  group_by(Period, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=24)
fig18 <- data %>% 
  group_by(Period, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c29)) %>%
  mutate(c29 = recode(c29, "Yes I have changed my intended destination"="Yes, changed destination", "Yes I have changed my planned routes but my intended destination remains the same"="Yes, changed routes", "Yes I have decided to return home"="Yes, decided to return home", "Yes I have stopped here for a time because I am stuck"="Yes, stopped for a while", "Yes other (specify)"=" Yes, other")) %>% 
  ggplot(aes(x=as.factor(Period), y=Percent, color=c29)) +
  geom_line(aes(group=c29), size=2.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  xlab("Time Period")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(axis.title.y = element_text(size=13, vjust= 3))+
  theme(axis.title.x = element_text(size=13, vjust = -2))+
  theme(axis.title = element_text(size = 12))+
  scale_color_brewer(palette = "Paired")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))
fig18



#region
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=98)
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, fill=c29))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_viridis(discrete=TRUE, na.value = "grey50")+
  facet_grid(~Region)
data %>%
  group_by(Period, Region, N_period_region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>%
  ggplot(aes(x=as.factor(Period), y=Percent, color=Region))+
  geom_line(aes(group=Region), size=1.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_color_viridis(discrete=TRUE, na.value = "grey50")+
  facet_wrap(~c29)









#Export figures####
# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("fig18.emf")
# produce the desired graph(s)
plot(fig18)
dev.off() #turn off device and finalize file
# }


#WAY 2#####################################################
View(data)
View(colnames(data))

#c17 If you had coronavirus and needed healthcare, would you be able to access health services today?####
#overall
data %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100))
data %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_line(aes(group=c17)) +
  geom_point()+
  geom_smooth(method = "auto")+
  facet_wrap(~c17)
data %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c17)) %>% 
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_point()+
  geom_smooth(method = "auto")+
  facet_wrap(~c17)

?geom_smooth

#region
data %>% 
  group_by(Date, Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% print()
data %>% 
  group_by(Date, Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% 
  filter(!is.na(c17)) %>% 
  filter(c17 != "Refused") %>% 
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_smooth(method = "auto", se = FALSE)+
  facet_wrap(~Region)
data %>% 
  group_by(Date, Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% 
  filter(!is.na(c17)) %>% 
  filter(c17 != "Refused") %>% 
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_smooth(method = "auto", se = FALSE)+
  facet_grid(rows = vars(Region))
data %>% 
  group_by(Date, Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>% 
  filter(!is.na(c17)) %>% 
  filter(c17 != "Refused") %>% 
  ggplot(aes(x=Date, y=Percent, color=Region)) +
  geom_smooth(method = "auto", se = FALSE)+
  facet_grid(rows = vars(c17))

#Harsh####
#weighted loess
#apply weights
#lm linear
#gam is linear but adds smoothing (the epsilon thing)
#loess (polynomial regression, so not linear)
#use time series packages
#add more periods
#add periods every 3 day
#in the same plot do it for 1 day, 3 days, 5 days
#points don't lie in grey band, so it's too noisy
#grey band is the error

#Using week####

#Overall
data %>% 
  mutate(Date=week(ymd(Date))) %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c17)) %>% View()
data %>% 
  mutate(Date=week(ymd(Date))) %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c17)) %>% 
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_line(aes(group=c17)) +
  geom_point()+
  geom_smooth(method = "auto")+
  facet_wrap(~c17)
data %>% 
  mutate(Date=week(ymd(Date))) %>% 
  group_by(Date, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c17)) %>% 
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_point()+
  geom_smooth(method = "auto")+
  facet_wrap(~c17)

#Region
data %>% 
  mutate(Date=week(ymd(Date))) %>% 
  group_by(Date, Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100)) %>%
  filter(!is.na(c17)) %>% 
  filter(c17 != "Refused") %>% 
  filter(c17 != "Don´t know") %>%
  ggplot(aes(x=Date, y=Percent, color=c17)) +
  geom_point()+
  geom_smooth(method = "auto", se = FALSE)+
  facet_wrap(~Region)






