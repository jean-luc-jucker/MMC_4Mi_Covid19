library(tidyverse)
library(scales)
getwd()
load("rda/04_cleaned_data_20200528.rda")
dim(data) #3,290 obs, 207 vars
 
#NUMBER OF INTERVIEWS####
data %>% group_by(Region) %>%
  tally()
data %>% group_by(Region, Q13_1c) %>% 
  tally()
#nas
summary(data$c1) #total nas = 8


#PROFILES####
#q25 sex####
data %>% 
  group_by(Q25) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100)
data %>%
  group_by(Region, Q25) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100)
data %>% 
  group_by(Region, Q13_1c, Q25) %>% 
  tally() %>% 
  mutate(Percent=n/sum(n)*100) %>% print(n=25)

#q23 age####
data %>% 
  summarise(Mean=mean(Q23))
data %>% 
  group_by(Region) %>% 
  summarise(Mean=mean(Q23))
data %>% 
  group_by(Region, Q13_1c) %>% 
  summarise(Mean=mean(Q23))

#Q20 Have you reached the end of your journey?####
data %>% 
  group_by(Q20) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, Q20) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, Q20) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=38)


#AWARENESS####

#c1 Have you heard of coronavirus?####
data %>% 
  group_by(c1) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c1) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c1) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=30)


#c2 Have you seen people acting more cautiously####
data %>% 
  group_by(c2) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c2) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c2) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=36)




#c3 I am worried about catching coronavirus (na=8)####
levels(data$c3)
data$c3 <- factor(data$c3, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=30)
data %>% 
  group_by(Region, Q13_1c, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=61)


#c4 I am worried about transmitting coronavirus (na=8)####
levels(data$c4)
data$c4 <- factor(data$c4, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c4) %>% 
  tally() %>%
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=32)
data %>% 
  group_by(Region, Q13_1c, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=66)


#c5 I know about coronavirus and how to protect myself and others (na=8)####
levels(data$c5)
data$c5 <- factor(data$c5, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=28)
data %>% 
  group_by(Region, Q13_1c, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=57)

#c11 What are the symptoms of coronavirus? (multi-select, n=3,282, na=8)####
data %>% 
  select(13:21) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 13:21) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=41)
data %>% 
  select(6, 207, 13:21) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=93)

#c12 Which groups are most at risk from the disease? (multi-select, n=3,282, na=8)####
data %>% 
  select(23:32) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 23:32) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=47)
data %>% 
  select(6, 207, 23:32) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)


#c13 What are you currently doing to protect yourself against coronavirus? (multi-select, n=3,282, na=8)####
data %>% 
  select(34:43) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 34:43) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 207, 34:43) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=102)

#c14 If you aren't taking measures, what are the reasons? (multi-select, n=272)####
data %>% 
  select(45:50) %>% 
  pivot_longer(cols = 1:6, names_to = "Options", values_to = "Answer") %>%  
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/272*100, digits = 1))

data %>%
  filter(`c13/Nothing`=="Nothing") %>% #select those who did nothing
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 45:50) %>%
  pivot_longer(cols = 3:8, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=17)

data %>%
  filter(`c13/Nothing`=="Nothing") %>% #select those who did nothing
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by region
  select(6, 208, 45:50) %>%
  pivot_longer(cols = 3:8, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=52)

#c15 Have you been tested for coronavirus?####
data %>% 
  group_by(c15) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c15) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c15) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=30)

#c31 Do you think you are able to practice the recommended 1.5 metre of distance between people?####
data %>% 
  group_by(c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=40)

#c32 Where do you currently live?####
data %>% 
  group_by(c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=22)
data %>% 
  group_by(Region, Q13_1c, c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=48)

#INFORMATION####

#c6 If you have received information on coronavirus and how to protect yourself, who did you receive it from? (n=3,282, na=8)####
data %>% 
  select(55:70) %>% #note this excludes 18 nones
  pivot_longer(cols = 1:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1)) %>% 
  print(n=16)
data %>% 
  select(205, 206, 55:71) %>% #note this includes nones
  pivot_longer(cols = 3:19, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=75)
data %>% 
  select(6, 207, 55:71) %>% #note this includes nones
  pivot_longer(cols = 3:19, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=161)

#c7 Through what means did you receive the information (type of media)? (multi-select, n=3,282, na=8, note inconsistency in survey design)####
data %>% 
  select(73:81) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 73:81) %>%
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=39)
data %>% 
  select(6, 207, 73:81) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=94)

#c8  What kinds of social media? (multi-select, N = 2,003)####
data %>% 
  select(83:94) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/2003*100, digits = 1))
data %>% 
  filter(!is.na(c8)) %>% #discard those that don't use social media
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 83:94) %>%
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=51)
data %>% 
  filter(!is.na(c8)) %>% #discard those that don't use social media
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 208, 83:94) %>%
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=107)

#c10 Who do you think is a trustworthy source of information on coronavirus? (multi-select, n=3,282, na=8)####
data %>% 
  select(96:111) %>% #note this excludes 24 nones
  pivot_longer(cols = 1:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 96:112) %>% #note this includes 24 nones
  pivot_longer(cols = 3:19, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=76)
data %>% 
  select(6, 207, 96:112) %>% #note this includes 24 nones
  pivot_longer(cols = 3:19, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=163)

#HEALTHCARE####

#c17 If you had coronavirus and needed healthcare, would you be able to access health services today?####
data %>% 
  group_by(c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c17) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=43)

#c18 What are the barriers to accessing health services? (multi-select, n=3,282, na=8)####
data %>% 
  select(115:127) %>% #excludes 250 nones
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 115:128) %>% #includes nones
  pivot_longer(cols = 3:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=61)
data %>% 
  select(6, 207, 115:128) %>% 
  pivot_longer(cols = 3:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=131)

#ASSISTANCE####

#c26 Q55 Are you in need of extra help since the coronavirus outbreak began?####
data %>% 
  group_by(c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c26) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=39)

#c27 What kind of extra help? (multi-select, n=2,835)####
data %>% 
  select(157:169) %>% 
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/2835*100, digits = 1)) %>% 
  print(n=13)
data %>% 
  filter(c26 == "Yes") %>% #discard those that don't need extra help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 157:169) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=61)
data %>% 
  filter(c26 == "Yes") %>%#discard those that don't need extra help
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 208, 157:169) %>% 
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=132)

#c23 Have you received additional assistance since the coronavirus crisis began?####
data %>% 
  group_by(c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c23) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=31)

#c24 What assistance was that? (multi-select, n=668)####
data %>% 
  select(131:143) %>% 
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/668*100, digits = 1))
data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 131:143) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 208, 131:143) %>% 
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=90)

#c25 Who did you receive it from? (multi-select, n=668)####
data %>% 
  select(145:154) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/668*100, digits = 1))
data %>% 
  filter(c23 == "Yes") %>% #discard those that didn't receive help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 145:154) %>%
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=39)
data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 208, 145:154) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=100)

#IMPACT ON MIGRANTS' LIVES####

#c20 What impact has the crisis had on your day-to-day life? (multi-select, n=3,282, na=8)####
data %>% 
  select(171:177) %>% #excludes 96 none
  pivot_longer(cols = 1:7, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 171:178) %>% #includes 96 none
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=37)
data %>% 
  select(6, 207, 171:178) %>% #includes 96 none
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=81)

#c21 Have you lost income due to coronavirus restrictions?####
data %>% 
  group_by(c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=21)
data %>% 
  group_by(Region, Q13_1c, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=44)

#c22 Q51 What impact has the loss of income had? (multi-select, n=1,875)####
data %>% 
  select(181:188) %>% 
  pivot_longer(cols = 1:8, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>%
  mutate(Percent = round(n/1875*100, digits = 1))
data %>% 
  filter(c21 == "Yes") %>%#discard those that didn't lose income
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 181:188) %>%
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=35)
data %>% 
  filter(c21 == "Yes") %>%#discard those that didn't lose income
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 208, 181:188) %>% 
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=71)

#IMPACT ON JOURNEY####

#C19 What impact has the coronavirus crisis had on your migration journey? (multi-select, n=3,282, na=8)####
data %>% 
  select(190:200) %>% 
  pivot_longer(cols = 1:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/3282*100, digits = 1))
data %>% 
  select(205, 206, 190:200) %>%
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 207, 190:200) %>% 
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=106)

#C29 Have you changed your plans as a result of the coronavirus outbreak?####
data %>% 
  group_by(c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print(n=36)
data %>% 
  group_by(Region, Q13_1c, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=75)

#FIGURES####

#Figure 1, 1.5 metre distance####
fig1_data <- data %>%
  group_by(Region, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print()
fig1  <- fig1_data %>% 
  mutate(c31=fct_relevel(c31, "Refused", "Don´t know", "No", "Yes")) %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  ggplot(aes(fill=c31, y=Percent))+
  geom_col(aes(x=Region, y=Percent), width = 0.42)+
  xlab("")+
  theme_bw()+
  labs(caption = "N =
  Asia:    238
  East Africa:    100
  Latin America:    459
  North Africa: 1,500
       West Africa:    993")+
  coord_flip()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.65, 0.96), legend.direction = "horizontal", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig1

#Figure 2, Who do you think is a trustworthy source of information?####
fig2_data <- data %>% 
  select(205, 206, 96:112) %>% #note this includes 24 nones
  pivot_longer(cols = 3:19, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=76)
fig2  <- fig2_data %>%
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption= "N =
  Asia:     238
  East Africa:       99
  Latin America:     459
  North Africa: 1,493
       West Africa:    993")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig2

#Figure 3, Barriers to healthcare####
fig3_data  <- data %>% 
  select(205, 206, 115:128) %>%
  pivot_longer(cols = 3:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1))

fig3  <- fig3_data %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  mutate(Answer = recode(Answer, 
                          'Discrimination against foreigners limits access to services' = "Discrimination against foreigners",
                          'General insecurity and conflict prevent me from accessing healthcare' = "General insecurity",
                          'I am afraid of being reported to authorities, or arrest, or deportation' = "Afraid of being reported",
                          'I don\'t have the money to pay for health services' = "I don't have the money",
                          'I don\'t have the right or the legal documentation to access health services here' = "I don't have the right/documentation",
                          'The advice for testing and treating coronavirus is unclear here' = "The advice is unclear here",
                          'Services are overwhelmed and access is difficult for everyone' = "Services are overwhelmed",
                          'I don\'t know where to go for healthcare' = "I don't know where to go", 'Other (specify)'="Other")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption= "N =
  Asia:     238
  East Africa:       99
  Latin America:     459
  North Africa: 1,493
       West Africa:    993")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  scale_x_discrete(limits=c("Refused", "None", "Other", "Don't know", "General insecurity", "There are no health services here", "Services are overwhelmed", "The advice is unclear here", "Afraid of being reported", "I don't speak the language", "Discrimination against foreigners", "I don't have the right/documentation", "I don't know where to go", "I don't have the money"))+
   coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig3
  
#Figure 4, Assistance####
fig4_received <- data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Region, .drop = FALSE) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 131:143) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=60)

fig4_needed  <- data %>% 
  filter(c26 == "Yes") %>% #discard those that don't need extra help
  group_by(Region, .drop = FALSE) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 157:169) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=65)

fig4  <- fig4_received %>% 
  ungroup() %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption = "Number of respondents
  Asia: received = 78, needed = 205
  East Africa: received = 10, needed = 89
  Latin America: received = 146, needed = 408
  North Africa: received = 245, needed = 1.252
       West Africa: received = 189, needed = 881")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  facet_grid(rows = vars(Region))+
  geom_point(data = fig4_needed, aes(x=Answer, y=Percent))+
  geom_line(data = fig4_needed, aes(x=Answer, y=Percent, group=Region))+
  scale_x_discrete(labels=c("Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", "Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself"="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other"))+
  scale_fill_manual(values=c("#DC71FA", "#00B8E5", "#00BE6C","#A3A500", "#F8766D"))
fig4



#Figure 5, Impact on life####
fig5_data  <- data %>% 
  select(205, 206, 171:178) %>% #includes 60 none
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
fig5 <- fig5_data %>% 
  ungroup() %>% 
  mutate(Answer = recode(Answer, 'Other (specify)' = "Other")) %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption= "N =
  Asia:     238
  East Africa:       99
  Latin America:     459
  North Africa: 1,493
       West Africa:    993")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig5

#Figure 6, Impact of loss of income NEW NEW NEW#### 
fig6_data <- data %>% 
  filter(c21 == "Yes") %>% #discard those that didn't lose income
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(205, 208, 181:188) %>%
  pivot_longer(cols = 3:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=52)
fig6 <- fig6_data %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  mutate(Answer = recode(Answer, 'Other (specify)' = "Other")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption= "N =
  Asia:     238
  East Africa:       99
  Latin America:     459
  North Africa: 1,493
       West Africa:    993")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  scale_x_discrete(limits=c("Refused", "None", "Other", "Loss of housing", "I am unable to continue my journey", "I am unable to pay remittances", "I am unable to afford basic goods", "Increased worry and anxiety"))+
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig6

#Figure 7, Impact on journey####  
fig7_data <- data %>% 
  select(205, 206, 190:200) %>%
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>%
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
fig7 <- fig7_data %>% 
  ungroup() %>% 
  mutate(Answer = recode(Answer, 'Other (specify)' = "Other", 'I\'ve been delayed because I was sick, or because I had to stop and take care of people who got sick' = "Delayed because I or other people were sick",
                         'I feel too afraid to move (to continue my journey or return)' = "I feel to afraid to move",
                         'Increased difficulty moving around inside countries'="Increased difficulty moving around",
                         'I was going to be resettled, but this is now delayed'="About to be resettled, but now delayed",
                         'Disembarked / deported back to previous country'="Deported back to previous country")) %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America", "East Africa", "Asia")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption= "N =
  Asia:     238
  East Africa:       99
  Latin America:     459
  North Africa: 1,493
       West Africa:    993")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_discrete(limits=c("Refused", "None", "Other", "Delayed because I or other people were sick", "Deported back to previous country", "About to be resettled, but now delayed", "Reduced access to smugglers", "Increased risk of detention and deportation", "I feel to afraid to move", "Increased difficulty moving around", "Increased difficulty crossing borders"))+
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.375), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig7

levels(fig7_data$Answer)

#Export figures####
# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("fig6.emf")
# produce the desired graph(s)
plot(fig6)
dev.off() #turn off device and finalize file
# }

#TABLE####
table_demographics <- data.frame(Region=c("Asia", "", "Latin America", "", "North Africa", "", "West Africa", "", "", "Overall"), Country=c("India", "Indonesia", "Colombia", "Peru", "Libya", "Tunisia", "Burkina Faso", "Mali", "Niger", ""), n=c(58, 68, 292, 90, 442, 515, 204, 234, 207, 2110), `Percent women` = c(43, 40, 72, 54, 29, 35, 41, 18, 28, 38), `Mean age`=c(34, 28, 34, 33, 30, 28, 28, 27, 31, 30)) %>% print()
?kable

#TREND ANALYSIS####
load("rda/01_cleaned_data_20200421.rda")
load("rda/02_cleaned_data_20200430.rda")
load("rda/03_cleaned_data_20200512.rda") 
up1 <- data
up2  <- data
up3 <- data 

up1 <- up1 %>% 
  filter(Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began. == "Yes") %>% #n = 598
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(182, 184, 155:166) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  filter(Answer != "") %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  mutate(Update = "1")
View(up1)
str(up1)

up2 <- up2 %>% 
  filter(c26 == "Yes") %>% #n = 1,014
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(189, 192, 154:164, 166) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  mutate(Update = "2")
View(up2)
str(up2)

up3 <- up3 %>% 
  filter(c26 == "Yes") %>% #n = 1,827
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(193, 196, 157:167, 169) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  mutate(Update = "3")
View(up3)
str(up3)

trend <- rbind(up1, up2, up3)
str(trend)
numerics <- c(2, 4, 5)
trend[numerics] <- lapply(trend[numerics], as.numeric)
factors  <- c(1, 3, 6)
trend[factors] <- lapply(trend[factors], as.factor)
str(data)
View(trend)

#plot--by answer
trend %>% 
  filter(Region != "Asia") %>% 
  filter(Answer != "Refused") %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>%
  mutate(Answer = reorder(Answer, -Percent)) %>%
  ggplot(aes(x=Update, y=Percent, color=Region))+ 
  geom_line(aes(group=Region))+
  facet_wrap(~Answer)

#plot--by region
trend %>% 
  filter(Region != "Asia") %>% 
  filter(Answer != "Refused") %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>% 
  ggplot(aes(x=Update, y=Percent, color=Answer))+ 
  geom_line(aes(group=Answer))+
  facet_grid(rows = vars(Region))
  
#plot--overall (without region)
#prepare data
up1bis <- up1 %>% 
  filter(Q55.Are.you.in.need.of.extra.help.since.the.coronavirus.outbreak.began. == "Yes") %>% #n = 598
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(182, 184, 155:166) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  filter(Answer != "") %>% 
  group_by(Answer) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/598*100, digits = 1)) %>% 
  mutate(Update = "1")
View(up1bis)

up2bis <- up2 %>% 
  filter(c26 == "Yes") %>% #n = 1,014
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(189, 192, 154:164, 166) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Answer) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1014*100, digits = 1)) %>% 
  mutate(Update = "2")
View(up2bis)

up3bis <- up3 %>% 
  filter(c26 == "Yes") %>% #n = 1,827
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% 
  select(193, 196, 157:167, 169) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Answer) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1827*100, digits = 1)) %>% 
  mutate(Update = "3")
View(up3bis)

trend2 <- rbind(up1bis, up2bis, up3bis)
str(trend2)
numerics <- c(2, 3)
trend2[numerics] <- lapply(trend2[numerics], as.numeric)
factors  <- c(1, 4)
trend2[factors] <- lapply(trend2[factors], as.factor)
str(trend2)

#plot
trend2 %>% 
  ungroup() %>% 
  filter(Answer != "Refused") %>% 
  mutate(Answer = recode(Answer, "Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", 'Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself'="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other")) %>% 
  mutate("Assistance needed"=Answer) %>% 
  ggplot(aes(x=Update, y=Percent, color= `Assistance needed`))+ 
  geom_line(aes(group=`Assistance needed`))+
  theme_bw()+
  labs(caption = "Number of respondents
  Update 1:     598
  Update 2: 1,014
  Update 3: 1,827")

#EXPORT QUALITATIVE DATA####
data %>% select(start, Region, Q13_1c, Q23, Q25, Q29, c34, c35) %>%
  write.csv("data_outputs/quotes/04_qualitative_items_20200529.csv")


#SHINY APP####
library(shiny)

ui <- fluidPage(
  radioButtons(inputId = "location",
              label = "Location",
              choices = levels(data$Region)),
  plotOutput("bar")
)

server <- function(input, output) {
  output$bar  <- renderPlot({
    fig1_data %>% 
      mutate(c31=fct_relevel(c31, "Refused", "Don´t know", "No", "Yes")) %>% 
      ungroup() %>% 
      ggplot(aes(fill=c31, y=Percent))+
      geom_col(aes(x=input$location, y=Percent), width = 0.47, position = "dodge")+
      xlab("")+
      theme_bw()+
      theme(legend.title = element_blank())+
      theme(legend.position = c(0.70, 0.95), legend.direction = "horizontal", legend.title = element_blank())+
      scale_fill_discrete(guide = guide_legend(reverse = TRUE))+
      coord_flip()
      })
}

shinyApp(ui = ui, server = server)

#SHINY APP 2####
library(shiny)

ui <- fluidPage(
  radioButtons(inputId = "location",
               label = "Location",
               choices = levels(data$Region)),
  plotOutput("bar")
)

server <- function(input, output) {
  output$bar  <- renderPlot({
    data %>%
      group_by(Region, c31) %>% 
      tally() %>% 
      mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
      mutate(c31=fct_relevel(c31, "Refused", "Don´t know", "No", "Yes")) %>% 
      ungroup() %>% 
      ggplot(aes(fill=c31, y=Percent))+
      geom_col(aes(x=input$location, y=Percent), width = 0.47, position = "dodge")+
      xlab("")+
      theme_bw()+
      theme(legend.title = element_blank())+
      theme(legend.position = c(0.70, 0.95), legend.direction = "horizontal", legend.title = element_blank())+
      scale_fill_discrete(guide = guide_legend(reverse = TRUE))+
      coord_flip()
      })
}

shinyApp(ui = ui, server = server)

#app template####
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist  <- renderPlot({
    hist(rnorm(input$num)) #note we take 'num' from inputId above--this is the number defined by the user. Yes in other words, the input of the user defines the output to return
  })
}

shinyApp(ui = ui, server = server)







View(data)








