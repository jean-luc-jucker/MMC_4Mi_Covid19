library(tidyverse)
library(scales)
getwd()
load("rda/02_cleaned_data_20200430.rda")
dim(data)
View(data)
names(data)


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
  mutate(Percent=n/sum(n)*100)

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
  print(n=24)


#AWARENESS####

#c1 Have you heard of coronavirus?####
summary(data$c1)
#All have replied yes


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
  print(n=24)

#c3 I am worried about catching coronavirus####
levels(data$c3)
data$c3 <- factor(data$c3, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c3) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=50)


#c4 I am worried about transmitting coronavirus####
levels(data$c4)
data$c4 <- factor(data$c4, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c4) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=50)


#c5 I know about coronavirus and how to protect myself and others####
levels(data$c5)
data$c5 <- factor(data$c5, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Refused"))
data %>% 
  group_by(c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c5) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=50)

#c11 What are the symptoms of coronavirus? (multi-select)####
data %>% 
  select(13:21) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 13:21) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 13:21) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=60)

#c12 Which groups are most at risk from the disease? (multi-select)####
data %>% 
  select(23:32) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 23:32) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 23:32) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=61)


#c13 What are you currently doing to protect yourself against coronavirus? (multi-select)####
data %>% 
  select(34:43) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 34:43) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 34:43) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=61)

#c14 If you aren't taking measures, what are the reasons? (multi-select)####
data %>% 
  select(45:50) %>% 
  pivot_longer(cols = 1:6, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/57*100, digits = 1)) %>% 
  print(n=50)

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
  print(n=24)

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
  print(n=24)

#c32 Where do you currently live?####
data %>% 
  group_by(c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c32) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=60)

#INFORMATION####

#c6 If you have received information on coronavirus and how to protect yourself, who did you receive it from?####
data %>% 
  select(55:70) %>% 
  pivot_longer(cols = 1:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 55:70) %>%
  pivot_longer(cols = 3:18, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 55:70) %>% 
  pivot_longer(cols = 3:18, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

#c7 Through what means did you receive the information (type of media)? (multi-select)####
data %>% 
  select(72:80) %>% 
  pivot_longer(cols = 1:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 72:80) %>%
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 72:80) %>% 
  pivot_longer(cols = 3:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

#c8  What kinds of social media? (multi-select, N = 700)####
data %>% 
  select(82:93) %>% 
  pivot_longer(cols = 1:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/700*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(!is.na(c8)) %>% #discard those that don't use social media
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 82:93) %>%
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(!is.na(c8)) %>% #discard those that don't use social media
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 192, 82:93) %>% 
  pivot_longer(cols = 3:14, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=100)

#c10 Who do you think is a trustworthy source of information on coronavirus? (multi-select)####
data %>% 
  select(95:110) %>% 
  pivot_longer(cols = 1:16, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 95:110) %>%
  pivot_longer(cols = 3:18, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 95:110) %>% 
  pivot_longer(cols = 3:18, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

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
  print(n=25)

#c18 What are the barriers to accessing health services? (multi-select)####
data %>% 
  select(113:125) %>% 
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 113:125) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 113:125) %>% 
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

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
  print(n=25)

#c27 What kind of extra help? (multi-select, n=1,014)####
data %>% 
  select(154:166) %>% 
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1014*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c26 == "Yes") %>% #discard those that don't need extra help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 154:166) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c26 == "Yes") %>%#discard those that don't need extra help
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 192, 154:166) %>% 
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=100)

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
  print(n=25)

#c24 What assistance was that? (multi-select, n=249)####
data %>% 
  select(128:140) %>% 
  pivot_longer(cols = 1:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/249*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 128:140) %>%
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
  select(6, 192, 128:140) %>% 
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=100)

#c25 Who did you receive it from? (multi-select, n=249)####
data %>% 
  select(142:151) %>% 
  pivot_longer(cols = 1:10, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/249*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c23 == "Yes") %>% #discard those that didn't receive help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 142:151) %>%
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Q13_1c) %>% 
  mutate(N_country_part = length(Q13_1c)) %>% #get n by country
  select(6, 192, 142:151) %>% 
  pivot_longer(cols = 3:12, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country_part*100, digits = 1)) %>% 
  print(n=100)

#IMPACT ON MIGRANTS' LIVES####

#c20 What impact has the crisis had on your day-to-day life? (multi-select)####
data %>% 
  select(168:174) %>% 
  pivot_longer(cols = 1:7, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 168:174) %>%
  pivot_longer(cols = 3:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 168:174) %>% 
  pivot_longer(cols = 3:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

#c21 Have you lost income due to coronavirus restrictions?####
data %>% 
  group_by(c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c21) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=25)

#IMPACT ON JOURNEY####

#C19 What impact has the coronavirus crisis had on your migration journey? (multi-select)####
data %>% 
  select(177:187) %>% 
  pivot_longer(cols = 1:11, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/1173*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(189, 190, 177:187) %>%
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
data %>% 
  select(6, 191, 177:187) %>% 
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Q13_1c, N_country) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_country*100, digits = 1)) %>% 
  print(n=100)

#C29 Have you changed your plans as a result of the coronavirus outbreak?####
data %>% 
  group_by(c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>%
  group_by(Region, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))
data %>% 
  group_by(Region, Q13_1c, c29) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% 
  print(n=50)

#FIGURES####

#Figure 1, 1.5 metre distance####
fig1_data <- data %>%
  group_by(Region, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print()
fig1  <- fig1_data %>% 
  mutate(c31=fct_relevel(c31, "Refused", "Don´t know", "No", "Yes")) %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
  ggplot(aes(fill=c31, y=Percent))+
  geom_col(aes(x=Region, y=Percent), width = 0.47)+
  xlab("")+
  theme_bw()+
  coord_flip()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.70, 0.95), legend.direction = "horizontal", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig1

#Figure 2, Who did you receive information from?####
fig2_data <- data %>% 
  select(189, 190, 55:70) %>%
  pivot_longer(cols = 3:18, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
fig2  <- fig2_data %>%
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig2

#Figure 3, Barriers to healthcare####
fig3_data  <- data %>% 
  select(189, 190, 113:125) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)

fig3  <- fig3_data %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
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
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig3
  
#Figure 4, Assistance####
fig4_received <- data %>% #N=249
  filter(c23 == "Yes") %>%#discard those that didn't receive help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 128:140) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)

fig4_needed  <- data %>% #N=1,014
  filter(c26 == "Yes") %>%#discard those that don't need extra help
  group_by(Region) %>% 
  mutate(N_region_part = length(Region)) %>% #get n by region
  select(189, 192, 154:166) %>%
  pivot_longer(cols = 3:15, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region_part) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region_part*100, digits = 1)) %>% 
  print(n=50)

fig4  <- fig4_received %>%
  ungroup() %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  labs(caption = "Number of respondents
  Latin America: received = 99, needed = 277
  North Africa: received = 78, needed = 441
       West Africa: received = 72, needed = 296")+
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  facet_grid(rows = vars(Region))+
  geom_point(data = fig4_needed, aes(x=Answer, y=Percent))+
  geom_line(data = fig4_needed, aes(x=Answer, y=Percent, group=Region))+
  scale_x_discrete(labels=c("Access to health services"="Health services", "Distribution of sanitary items (sanitizer/ mask/ gloves/ etc)"="Sanitizer, masks, gloves", "Cash to pay for health services"="Cash for health services", "Documentation to access health services"="Documentation", "Information about the virus: symptoms/ what to do if I have symptoms/ how to protect myself"="Information", "Other basic needs: food, water, shelter"="Food, water, shelter", "Access to work and livelihoods"="Access to work", "Psychological assistance"="Psychological support", "Other (specify)"="Other"))
fig4

#Figure 5, Impact on life####
fig5_data  <- data %>% 
  select(189, 190, 168:174) %>%
  pivot_longer(cols = 3:9, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
fig5 <- fig5_data %>% 
  ungroup() %>% 
  mutate(Answer = recode(Answer, 'Other (specify)' = "Other")) %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig5
  
#Figure 6, Impact on journey####  
fig6_data <- data %>% 
  select(189, 190, 177:187) %>%
  pivot_longer(cols = 3:13, names_to = "Options", values_to = "Answer") %>% 
  filter(!is.na(Answer)) %>% 
  group_by(Region, N_region) %>% 
  count(Answer) %>% 
  mutate(Percent = round(n/N_region*100, digits = 1)) %>% 
  print(n=50)
fig6 <- fig6_data %>% 
  ungroup() %>% 
  mutate(Answer = recode(Answer, 'Other (specify)' = "Other", 'I\'ve been delayed because I was sick, or because I had to stop and take care of people who got sick' = "Delayed because I or other people were sick",
                         'I feel too afraid to move (to continue my journey or return)' = "I feel to afraid to move",
                         'Increased difficulty moving around inside countries'="Increased difficulty moving around",
                         'I was going to be resettled, but this is now delayed'="About to be resettled, but now delayed",
                         'Disembarked / deported back to previous country'="Deported back to previous country")) %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
  mutate(Answer = reorder(Answer, Percent)) %>% 
  ggplot(aes(fill=Region))+
  geom_col(aes(x=Answer, y=Percent), width = 0.6, position = position_dodge2(preserve = "single", padding = 0))+
  ylab("Percent")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  coord_flip()+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.80, 0.15), legend.direction = "vertical", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig6


# NOT RUN {
require(devEMF)
# }
# NOT RUN {
# open file "bar.emf" for graphics output
emf("fig4.emf")
# produce the desired graph(s)
plot(fig4)
dev.off() #turn off device and finalize file
# }

#TABLE####
table_demographics <- data.frame(Region=c("Latin America", "", "North Africa", "", "West Africa", "", "", "Overall"), Country=c("Colombia", "Peru", "Libya", "Tunisia", "Burkina Faso", "Mali", "Niger", ""), n=c(250, 63, 211, 305, 98, 101, 145, 1173), `Percent women` = c(71, 51, 30, 33, 42, 24, 27, 41), `Mean age`=c(34, 32, 31, 29, 29, 27, 30, 30)) %>% print()
?kable



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


data %>%
  group_by(Region, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1))


fig1_data <- data %>%
  group_by(Region, c31) %>% 
  tally() %>% 
  mutate(Percent=round(n/sum(n)*100, digits = 1)) %>% print()
fig1  <- fig1_data %>% 
  mutate(c31=fct_relevel(c31, "Refused", "Don´t know", "No", "Yes")) %>% 
  ungroup() %>% 
  mutate(Region=fct_relevel(Region, "West Africa", "North Africa", "Latin America")) %>% 
  ggplot(aes(fill=c31, y=Percent))+
  geom_col(aes(x=Region, y=Percent), width = 0.47)+
  xlab("")+
  theme_bw()+
  coord_flip()+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.70, 0.95), legend.direction = "horizontal", legend.title = element_blank())+
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))
fig1


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
















