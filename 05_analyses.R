library(tidyverse)
library(scales)
library(lubridate)
library(viridis)
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
  mutate(Percent=round(n/N_period_region*100, digits = 1)) %>% print(n=58)
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






